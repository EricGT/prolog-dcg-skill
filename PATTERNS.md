# DCG Patterns Library

Comprehensive DCG patterns for parsing in SWI-Prolog. See [SKILL.md](SKILL.md) for core philosophy and quick reference.

## Table of Contents

- [Common DCG Patterns](#common-dcg-patterns)
- [Position Tracking Patterns](#position-tracking-patterns)
- [Integration with File I/O](#integration-with-file-io)

## Common DCG Patterns

### Pattern: Tab-Separated Fields

```prolog
% Parse tab-separated line
tsv_line([Field|Fields]) -->
    tsv_field(Field),
    `\t`, !,
    tsv_line(Fields).
tsv_line([Field]) -->
    tsv_field(Field),
    line_end.

tsv_field(Field) -->
    field_codes(FieldCodes),
    { atom_codes(Field, FieldCodes) }.

field_codes([C|Cs]) -->
    [C], { C \= 9, C \= 10, C \= 13 }, !,  % Not tab, LF, CR
    field_codes(Cs).
field_codes([]) --> [].
```

### Pattern: C Identifiers

```prolog
% Parse C-style identifier
c_identifier(Id) -->
    [First],
    {
        code_type(First, alpha)
    ;
        First = 0'_
    },
    c_id_rest(Rest),
    { atom_codes(Id, [First|Rest]) }.

c_id_rest([C|Cs]) -->
    [C],
    {
        code_type(C, alnum)
    ;
        C = 0'_
    },
    !,
    c_id_rest(Cs).
c_id_rest([]) --> [].
```

### Pattern: Accumulator-Based Parsing

**IMPORTANT Accumulator Rules:**
1. **Always use in/out pairs**: `parse(..., Acc0, Acc)`
2. **Place at end of argument list**: Regular args first, then accumulator pairs
3. **Pair accumulators together**: `Line0, Line, Offset0, Offset` (not mixed)
4. **In first, Out second**: Consistent ordering

**IMPORTANT Accumulator Naming Convention:**
- **In variable**: Name ending in `0` (e.g., `Seen0`, `Line0`, `Offset0`)
- **Out variable**: Name without number (e.g., `Seen`, `Line`, `Offset`)
- **Intermediate variables**: Increment number, except final has no number (e.g., `Seen0 → Seen1 → Seen`)
- **Multiple accumulators**: Use consistent numbering (e.g., `Line0, Line, Offset0, Offset`)

**IMPORTANT Head|Tail List Naming Convention:**
- **For descriptive names**: Use singular/plural pairs: `[Item|Items]`, `[Record|Records]`, `[Line|Lines]`
- **For single letters**: Use letter with 's' suffix: `[X|Xs]`, `[C|Cs]`, `[D|Ds]`
- **Avoid**: Generic words like `[Head|Tail]`, `[H|T]` (unless using single letter convention)
- **Benefit**: Self-documenting code that clarifies what the list contains

```prolog
% ❌ AVOID: Wrong accumulator naming and position
parse_lines(Seen, [Item|Items]) -->
    parse_line(Seen, Item), !,
    { update_seen(Seen, Item, Seen1) },
    parse_lines(Seen1, Items).

% ✅ PREFER: Correct naming (Seen0/Seen) and position (at end)
parse_lines([Item|Items], Seen0, Seen) -->
    parse_line(Item, Seen0, Seen1), !,
    parse_lines(Items, Seen1, Seen).
parse_lines([], Seen, Seen) --> [].

% Parse single line with accumulator pair at end
parse_line(Item, Seen0, Seen) -->
    field(Name),
    { \+ memberchk(Name, Seen0) },  % Use memberchk/2, not member/2
    `\t`,
    rest_of_line(Data),
    { Item = item(Name, Data),
      Seen = [Name|Seen0] }. % Build list (DCG forward accumulation is better)
```

### Pattern: Multiple Accumulators

When tracking multiple values (line, offset), keep pairs together at end with consistent numbering:

```prolog
% ✅ CORRECT: Accumulator pairs at end, 0-suffix for input
parse_file(Result, Line0, Line, Offset0, Offset) -->
    parse_line(Item, Line0, Line1, Offset0, Offset1), !,
    parse_file(Rest, Line1, Line, Offset1, Offset),
    { Result = [Item|Rest] }.
parse_file([], Line, Line, Offset, Offset) --> [].

% Parse line: result first, then accumulator pairs
parse_line(Item, Line0, Line, Offset0, Offset) -->
    field(File), `\t`,
    rest_of_line(Content),
    line_end,
    { Item = data(File, pos(Line0, Offset0), Content),
      succ(Line0, Line),
      string_length(Content, Len),
      Offset is Offset0 + Len + 1 }.  % +1 for newline
```

**See "PATTERN 2: Line-Oriented Tracking (Line + Column offset)" below for a comprehensive example** demonstrating multiple accumulator pairs in a real-world parsing scenario.

### Pattern: Quantifier Naming Conventions

**IMPORTANT**: Use suffix notation to indicate cardinality, with quotes around predicate names.

```prolog
% Optional (zero or one) - use ? suffix
'sign?'(Code) --> [Code], { memberchk(Code, [0'+, 0'-]) }, !.
'sign?'(_) --> [].

% Zero or more - use * suffix
'digits*'([D|Ds]) --> digit(D), !, 'digits*'(Ds).
'digits*'([]) --> [].

% One or more - use + suffix
'digits+'([D|Ds]) --> digit(D), !, 'digits*'(Ds).

% Example: parsing a number
number(Number) -->
    'sign?'(Sign),
    'digits+'(IntCodes),
    'fraction?'(FracCodes),
    'exponent?'(ExpCodes),
    { combine_number_parts(Sign, IntCodes, FracCodes, ExpCodes, Number) }.

'fraction?'(FracCodes) --> `.`, !, 'digits+'(FracCodes).
'fraction?'([]) --> [].

'exponent?'(ExpCodes) --> [E], { memberchk(E, [0'e, 0'E]) }, !,
                        'sign?'(_), 'digits+'(ExpCodes).
'exponent?'([]) --> [].
```

**Benefits:**
- Clear cardinality at a glance
- Consistent naming convention
- Mirrors regex quantifiers (?, *, +)
- Remember to quote predicate names: `'name?'`, `'name*'`, `'name+'`

### Pattern: DCG String Syntax

**IMPORTANT**: Understand Prolog string/code syntax and module flags that control interpretation.

**Prolog Flags (check these in your module):**
```prolog
% Back quotes produce codes (default)
?- current_prolog_flag(back_quotes, Val).
Val = codes.

% Double quotes produce strings (default)
?- current_prolog_flag(double_quotes, Val).
Val = string.
```

**In DCG Rules:**
```prolog
% Double quotes "abc" create strings (default flag setting)
% But in DCG context, string literals will NOT match against code lists
parse_keyword --> "function".  % Will not match when input converted to character codes when it is a string for "function"

% Back quotes `abc` create code lists directly
parse_keyword --> `function`.  % Explicit code list [102, 117, 110, 99, 116, 105, 111, 110]

% Single quotes 'abc' create an atom
parse_name(Name) -->
    identifier(NameCodes),
    { atom_codes(Name, NameCodes) }.  % Name is an atom like 'foo'

% Character code notation with 0' prefix
digit(D) --> [D], { D >= 0'0, D =< 0'9 }.  % 0'0 is character code for '0' (48)

% Single character codes with comments
tab_char --> [9].      % ASCII tab
newline --> [10].      % ASCII newline (LF)
cr --> [13].           % ASCII carriage return (CR)
space --> [32].        % ASCII space
```

**Guidelines:**
- **Double quotes** `"abc"`: Creates string, but in DCG context does not match against character code lists
- **Back quotes** `` `abc` ``: Creates code list `[97, 98, 99]` directly
- **Single quotes** `'abc'`: Creates atom
- **Character codes**: Use `0'X` notation (e.g., `0'a` = 97, `0'\t` = 9) or numeric codes `[9]` with comments
- Always comment numeric character codes (e.g., `[9]  % ASCII tab`)
- **PREFER codes for Unicode/non-ASCII**: Using character codes makes it easier to work with Unicode, UTF-8, and international character sets correctly
- Parse as codes in DCG, convert to atoms/strings only at output boundaries

### Pattern: Disjunction Formatting

**IMPORTANT**: Format disjunctions (semicolon `;`) with the semicolon on its own line to make it clearly visible.

```prolog
% ❌ AVOID: Semicolon hidden inline
c_id_rest([C|Cs]) -->
    [C],
    {
      code_type(C, alnum)
    ;
      C = 0'_
    },
    !,
    c_id_rest(Cs).

% ✅ PREFER: Semicolon on separate line for visibility
c_id_rest([C|Cs]) -->
    [C],
    {
        code_type(C, alnum)
    ;
        C = 0'_
    },
    !,
    c_id_rest(Cs).

% Multiple disjunctions
parse_with_fallback(Result) -->
    (
        parse_primary(Result), !
    ;
        parse_secondary(Result), !
    ;
        parse_raw_line(Result)
    ).
```

**Benefits:**
- Semicolon is immediately visible at start of line
- Easier to scan alternatives in complex conditionals
- Consistent with if-then-else formatting conventions
- Reduces chance of missing a disjunction when reading code

### Pattern: Conditional Control Flow - Multiple Clauses vs If-Then-Else

**RECOMMENDATION**: Prefer multiple clauses with guards over if-then-else (`->` with `;`) for conditional logic in DCGs.

#### ✅ PREFER: Multiple Clauses with Guards

```prolog
% Idiomatic and extensible
parse_value(positive) --> [C], { C > 0 }, !.
parse_value(zero) --> [0], !.
parse_value(negative) --> [C], { C < 0 }.

% Easy to extend with new cases
parse_token(identifier) --> c_identifier(_), !.
parse_token(number) --> integer(_), !.
parse_token(operator) --> operator_symbol(_).
```

#### ❌ AVOID: If-Then-Else (Use Sparingly)

```prolog
% Less idiomatic - harder to extend
parse_value(Result) -->
    [C],
    (
      C > 0
    ->
      { Result = positive }
    ;
      C =:= 0
    ->
      { Result = zero }
    ;
      { Result = negative }
    ).
```

#### When to Use Multiple Clauses

**Reasons to prefer multiple clauses:**

1. **More Idiomatic**: Leverages Prolog's core strength - pattern matching and unification. Follows declarative style (defining what, not how).

2. **Easier to Extend**: Adding new cases is simple - just add another clause. No need to modify nested conditionals.

3. **Better Readability**: Each case is clearly separated. Easier to understand at a glance which patterns are handled.

4. **Simpler Debugging**: Easier to trace which clause matched. Can comment out individual clauses for testing.

5. **Less Error-Prone**: Changes to one case don't affect others. No risk of forgetting the `;` between alternatives.

#### When to Use If-Then-Else

Use `->` with `;` only when:
- You need **guaranteed determinism** without explicit cuts
- The logic is **truly binary** and won't need extension
- **Performance is critical** and you want to avoid choice points (though cuts in clauses achieve similar effect)

**Note**: The `->` operator commits once the condition succeeds (acts like a cut), eliminating backtracking to alternative branches. However, multiple clauses with strategic cuts are equally deterministic and more maintainable.

### Pattern: Equality Operators in Conditionals and Guards

**CRITICAL**: Use comparison operators (`==/2`, `\==/2`) instead of unification operators (`=/2`, `\=/2`) when testing values in conditionals or guards.

This is a common mistake in LLM-generated code and legacy code, where unification is used when comparison is intended.

#### Understanding the Operators

**Unification operators** (can bind variables):
- `=/2`: Unification - makes two terms equal by binding variables
- `\=/2`: Not unifiable - succeeds if terms cannot be unified

**Comparison operators** (no binding, just testing):
- `==/2`: Term equality - checks if terms are structurally identical (no binding)
- `\==/2`: Term inequality - checks if terms are not structurally identical (no binding)
- `=:=/2`: Arithmetic equality - evaluates arithmetic expressions and compares values
- `=\=/2`: Arithmetic inequality - evaluates and compares arithmetic values

#### ❌ AVOID: Using =/2 in Conditionals (Common LLM Training Data Error)

```prolog
% WRONG: Uses unification in guard - can unexpectedly bind variables!
parse_line(Result, Offset, Line) -->
    { (Line = 42 -> gtrace ; true) },  % ❌ BAD: Uses =/2
    field(F1), `\t`,
    field(F2),
    { Result = data(F1, F2) }.

% WRONG: Uses unification to check value
parse_field(Field, Offset, Line) -->
    field_codes(FieldCodes),
    { atom_codes(Field, FieldCodes),
      (Field = problematic_symbol -> gtrace ; true) },  % ❌ BAD: Uses =/2
    ...
```

**Problems:**
- If `Line` were unbound, `Line = 42` would bind it to 42 (not test it!)
- Confuses intent: are we testing or binding?
- Common in LLM training data - many examples use wrong operator
- Can cause subtle bugs when variables are not fully instantiated

#### ✅ PREFER: Using ==/2 for Value Testing

```prolog
% CORRECT: Uses term equality for comparison
parse_line(Result, Offset, Line) -->
    { (Line == 42 -> gtrace ; true) },  % ✅ GOOD: Uses ==/2
    field(F1), `\t`,
    field(F2),
    { Result = data(F1, F2) }.

% CORRECT: Uses term equality to check value
parse_field(Field, Offset, Line) -->
    field_codes(FieldCodes),
    { atom_codes(Field, FieldCodes),
      (Field == problematic_symbol -> gtrace ; true) },  % ✅ GOOD: Uses ==/2
    ...

% CORRECT: Testing if list is non-empty
parse_required_field(Field, Line0, Line, Offset0, Offset) -->
    field_codes(FieldCodes, Line0, Line, Offset0, Offset),
    {
      FieldCodes \== []  % ✅ GOOD: Uses \==/2 for comparison
    ->
      string_codes(Field, FieldCodes)
    ;
      throw(error(existence_error(field, required), context(...)))
    }.
```

#### Guidelines for Choosing the Right Operator

**Use `==/2` or `\==/2` when:**
- Testing if a bound variable has a specific value in a conditional
- Checking structural equality in guards (`->`)
- Comparing values without side effects
- The intent is to TEST, not to BIND

**Use `=/2` only when:**
- Actually performing unification (binding variables)
- Pattern matching in clause heads or goals
- Constructing terms by binding variables

**Use `\=/2` when:**
- Testing if two terms fundamentally cannot unify
- Note: `\==/2` is usually clearer for simple value checks

**Use `=:=/2` or `=\=/2` when:**
- Comparing arithmetic expressions: `X + 1 =:= Y`
- Need arithmetic evaluation: `Count =:= 0`

#### Common Patterns

```prolog
% Character code comparison - use comparison, not unification
digit_char(D) --> [D], { D >= 0'0, D =< 0'9 }.  % ✅ Arithmetic comparison
not_tab([C|Cs]) --> [C], { C \== 9 }, !, not_tab(Cs).  % ✅ Could use \== or \=

% Testing specific values in guards
process_line(Line, Data) -->
    parse_data(Data),
    {
      Line == 1  % ✅ GOOD: Comparison
    ->
      process_header(Data)
    ;
      process_regular(Data)
    }.

% Wrong: unification in test
process_line(Line, Data) -->
    parse_data(Data),
    {
      Line = 1  % ❌ BAD: Unification (would bind Line if unbound!)
    ->
      process_header(Data)
    ;
      process_regular(Data)
    }.
```

#### Summary

- **In guards/conditionals**: Use `==/2` and `\==/2` for testing
- **For binding/construction**: Use `=/2`
- **For arithmetic**: Use `=:=/2` and `=\=/2`
- **Watch for LLM-generated code**: Often incorrectly uses `=/2` in guards

### Pattern: memberchk/2 vs member/2

**IMPORTANT**: Prefer `memberchk/2` over `member/2` when you only need to check membership once (don't need backtracking).

```prolog
% ❌ AVOID: member/2 creates unnecessary choice points
parse_line(Item, Seen0, Seen) -->
    field(Name),
    { \+ member(Name, Seen0) },  % Creates choice points
    rest_of_line(Data),
    { Item = item(Name, Data),
      Seen = [Name|Seen0] }.

% ✅ PREFER: memberchk/2 is deterministic
parse_line(Item, Seen0, Seen) -->
    field(Name),
    { \+ memberchk(Name, Seen0) },  % Deterministic, no choice points
    rest_of_line(Data),
    { Item = item(Name, Data),
      Seen = [Name|Seen0] }.

% Check for valid keyword
keyword(Kw) -->
    identifier(Id),
    { memberchk(Id, [if, then, else, while, for]) },  % Deterministic check
    { Kw = Id }.
```

**Guidelines:**
- Use `memberchk/2` for simple membership testing
- Use `member/2` only when you need backtracking over alternatives
- `memberchk/2` stops at first match, more efficient

### Pattern: succ/2 for Incrementing

**IMPORTANT**: Prefer `succ/2` over `is/2` for simple +1 increments/decrements.

```prolog
% ❌ AVOID: Using is/2 for simple +1 increment
parse_field([C|Cs], Offset0, Offset) -->
    [C], { C \= 9 }, !,
    { Offset1 is Offset0 + 1 },  % Arithmetic evaluation
    parse_field(Cs, Offset1, Offset).

% ✅ PREFER: Using succ/2 for increment
parse_field([C|Cs], Offset0, Offset) -->
    [C], { C \= 9 }, !,
    { succ(Offset0, Offset1) },  % Successor relation
    parse_field(Cs, Offset1, Offset).

% ✅ PREFER: succ/2 for line tracking
update_line_position(Line0, Line, _LineOffset0, LineOffset) :-
    succ(Line0, Line),
    LineOffset = 0.

% ❌ AVOID: is/2 for line tracking
update_line_position(Line0, Line, _LineOffset0, LineOffset) :-
    Line is Line0 + 1,
    LineOffset = 0.

% ✅ Complex arithmetic still uses is/2
calculate_offset(Start, Length, Offset) :-
    Offset is Start + Length + 1.  % Multiple operations - use is/2

% ❌ AVOID: Manual counting of multi-character sequences
line_end(Offset0, Offset) -->
    `\r\n`, !,
    { Offset is Offset0 + 2 }.  % Error-prone: Must manually count characters

% ✅ PREFER: Break down to single-character increments
line_end(Offset0, Offset) -->
    [13],  % CR
    { succ(Offset0, Offset1) },
    [10],  % LF
    { succ(Offset1, Offset) }, !.
```

**Benefits:**
- **More declarative**: `succ/2` works in multiple modes (can decrement too)
- **Type-safe**: Ensures integer values
- **Clearer intent**: Expresses successor relationship explicitly
- **More efficient**: Optimized for simple +1/-1 operations
- **Bidirectional**: Can be used for both incrementing and decrementing
- **Error-resistant**: Breaking down multi-character sequences eliminates manual counting errors

**Guidelines:**
- Use `succ/2` for simple +1 operations: `succ(N0, N1)`
- Use `succ/2` for -1 operations: `succ(N1, N0)` (reversed arguments)
- **IMPORTANT**: Break multi-character sequences (like `\r\n`) into individual character increments to avoid manual counting errors
- Use `is/2` for complex arithmetic: `N is N0 + Length + 1`
- Use `is/2` when incrementing by values other than 1

### Pattern: Character-Level DCG Composition

**IMPORTANT**: Create reusable single-character DCG primitives with position tracking, then compose them into complex patterns.

```prolog
% ❌ AVOID: Inline character matching in complex patterns
line_end(Offset0, Offset) -->
    [13], [10], !,  % CR LF - but no position tracking!
    { Offset is Offset0 + 2 }.
line_end(Offset0, Offset) -->
    [10], !,
    { succ(Offset0, Offset) }.

% ✅ PREFER: Character-level DCG primitives
% Each primitive handles ONE character and its position increment
tab(Offset0, Offset) -->
    [9],  % ASCII tab
    { succ(Offset0, Offset) }.

cr(Offset0, Offset) -->
    [13],  % CR
    { succ(Offset0, Offset) }.

lf(Offset0, Offset) -->
    [10],  % LF
    { succ(Offset0, Offset) }.

space(Offset0, Offset) -->
    [32],  % Space
    { succ(Offset0, Offset) }.

% ✅ PREFER: Compose primitives into complex patterns
line_end(Offset0, Offset) -->
    cr(Offset0, Offset1),
    lf(Offset1, Offset), !.
line_end(Offset0, Offset) -->
    lf(Offset0, Offset), !.
line_end(Offset0, Offset) -->
    cr(Offset0, Offset).

% ✅ Reuse primitives in different contexts
parse_tsv_field(Field, Offset0, Offset) -->
    field_codes(Codes, Offset0, Offset1),
    tab(Offset1, Offset),  % Reuse tab primitive
    { atom_codes(Field, Codes) }.

parse_csv_field(Field, Offset0, Offset) -->
    field_codes(Codes, Offset0, Offset1),
    (
      comma(Offset1, Offset)  % Different delimiter
    ;
      line_end(Offset1, Offset)
    ),
    { atom_codes(Field, Codes) }.

comma(Offset0, Offset) -->
    [44],  % Comma
    { succ(Offset0, Offset) }.
```

**Benefits:**
- **Reusability**: Character primitives can be used across different parsing contexts
- **Composability**: Build complex patterns from simple, tested building blocks
- **Maintainability**: Single point of truth for each character's position tracking
- **Clarity**: Each DCG has a single, clear responsibility
- **Error-resistant**: Position tracking handled once per character, not per complex pattern
- **Testing**: Easy to test individual character primitives independently

**Guidelines:**
- Create a DCG primitive for each significant character: tab, cr, lf, space, comma, etc.
- Each primitive should consume exactly one character and handle its position increment
- Compose primitives to build complex patterns like `line_end`, `whitespace`, etc.
- Reuse primitives across different parsing contexts (TSV, CSV, etc.)
- Name primitives after the character they match: `tab`, `cr`, `lf`, not `tab_char`, `cr_byte`

### Pattern: DCG Forward Accumulation Instead of append/3

**IMPORTANT**: Use DCG forward accumulation for list building. Avoid `append/3` which is O(n) per call.

```prolog
% ❌ AVOID: append/3 in recursive building (O(n²))
build_list([X|Xs], Acc0, Result) :-
    process(X, Item),
    append(Acc0, [Item], Acc1),  % O(n) each time!
    build_list(Xs, Acc1, Result).
build_list([], Result, Result).

% ✅ PREFER: DCG with forward accumulation
parse_items([Item|Items]) -->
    parse_item(Item), !,  % Natural O(1) cons with [Item|Items]
    parse_items(Items).
parse_items([]) --> [].

% Or just use append once if unavoidable
combine_once(Part1, Part2, Result) :-
    append(Part1, Part2, Result).  % OK for single use
```

**Guidelines:**
- Use DCG forward accumulation with `[Item|Items]` pattern for incremental building
- DCGs naturally build lists forward - prefer DCG notation
- Avoid multiple `append/3` calls in loops/recursion
- Single `append/3` at end is acceptable if needed

### Pattern: Rest of Line

```prolog
% Consume rest of line (everything until line ending)
rest_of_line(Line) -->
    rest_codes(LineCodes),
    line_end,
    { atom_codes(Line, LineCodes) }.

rest_codes([C|Cs]) -->
    [C], { C \= 10, C \= 13 }, !,  % Not LF, CR
    rest_codes(Cs).
rest_codes([]) --> [].
```

### Pattern: Just-in-Time Clause Indexing

**IMPORTANT**: Prefer clause ordering that enables just-in-time indexing when it makes sense.

SWI-Prolog automatically indexes on the first argument. Order clauses to take advantage of this:

```prolog
% ✅ PREFER: Most specific cases first, indexed on first argument
parse_line([], Seen, Seen) --> [].
parse_line([Item|Items], SeenIn, SeenOut) -->
    parse_item(Item, SeenIn, Seen1), !,
    parse_line(Items, Seen1, SeenOut).

% Pattern matching on structure
handle_def(def(File, Symbol, Line, Context)) -->
    % Handle definition
    ...
handle_def(call(File, Caller, Line, Context, Callee)) -->
    % Handle call
    ...
handle_def(include(File, Header)) -->
    % Handle include
    ...

% DCG alternatives indexed by lookahead
parse_declaration -->
    `typedef`, !, parse_typedef.
parse_declaration -->
    `struct`, !, parse_struct.
parse_declaration -->
    `enum`, !, parse_enum.
parse_declaration -->
    `#define`, !, parse_macro.
```

**Benefits:**
- Prolog indexes on first argument automatically
- Deterministic choice points with cuts after specific patterns
- Faster lookup for specific cases
- Clearer pattern matching intent

### Pattern: String/Atom/Codes Conversions

**IMPORTANT**: Always parse as codes. Only convert to strings/atoms at the end when passing output results back.

**Why prefer codes:**
- **Unicode/non-ASCII support**: Character codes handle UTF-8 and international text correctly without flag-dependent interpretation issues
- **Consistency**: Integer comparison and manipulation is explicit and predictable
- **Performance**: Direct code manipulation avoids string conversion overhead

```prolog
% ✅ PREFER: Parse as codes, convert once at end
parse_tsv_line(record(F1, F2, F3)) -->
    field(Codes1), `\t`,
    field(Codes2), `\t`,
    field(Codes3),
    line_end,
    { atom_codes(F1, Codes1),      % Convert once, all parts available
      atom_codes(F2, Codes2),
      string_codes(F3, Codes3) }.

% ❌ AVOID: Converting during parsing
parse_tsv_line(record(F1, F2, F3)) -->
    { string_codes(Input, Codes) },  % Premature conversion
    field_codes(C1),
    { atom_codes(F1, C1) },          % Multiple conversions
    `\t`,
    field_codes(C2),
    { string_codes(S2, C2),          % Convert then convert again
      atom_string(F2, S2) },
    ...

% ✅ PREFER: Combine multiple results before converting
parse_qualified_name(QName) -->
    identifier(Codes1),
    `::`,
    identifier(Codes2),
    { append(Codes1, [58, 58|Codes2], AllCodes),  % 58 = ':', combine as codes
      atom_codes(QName, AllCodes) }.              % Single conversion

% ✅ PREFER: Keep as codes until all parts available
parse_fields([F1, F2, F3]) -->
    field(C1), `\t`,
    field(C2), `\t`,
    field(C3),
    { maplist(atom_codes, [F1, F2, F3], [C1, C2, C3]) }.  % Convert all at once
```

**Guidelines:**
- Parse with DCGs working on code lists
- Combine/manipulate as codes
- Convert once when all parts are available
- Only convert at output boundaries

### Pattern: DCG Forward Accumulation to Avoid reverse/2

**IMPORTANT**: Use DCG forward accumulation to build lists in correct order instead of using reverse/2.

```prolog
% ❌ AVOID: Building list backwards, then reversing
parse_lines(Lines, Result) :-
    parse_lines_acc(Lines, [], Rev),
    reverse(Rev, Result).

parse_lines_acc([], Acc, Acc).
parse_lines_acc([L|Ls], Acc, Result) :-
    parse_line(L, Item),
    parse_lines_acc(Ls, [Item|Acc], Result).

% ✅ PREFER: DCG with forward accumulation using unification
parse_lines([Item|Items]) -->
    parse_line(Item), !,
    parse_lines(Items).
parse_lines([]) --> [].
```

**Benefits:**
- O(n) instead of O(n) + O(n) for reverse
- More efficient memory usage
- Natural DCG style builds lists forward automatically
- Cleaner code without explicit reverse/2

### Pattern: Difference Lists for Pure Streaming Transformation

**IMPORTANT**: Use explicit difference lists for TRUE streaming transformations where input flows continuously to output with minimal buffering.

This is the PUREST form of difference list output accumulation where:
- **Each input character is processed exactly once**
- **Output flows continuously through difference list**
- **No reordering** - input order ≈ output order
- **Minimal buffering** - only what's necessary for lookahead
- **True O(n)** - single pass, no backtracking

**The Core Pattern:**
```prolog
% Stream input directly to output through difference list

% Handle special pattern
expand_stream(Hole0-Hole) -->
    special_pattern,
    !,
    process_special(Hole0-Hole1),
    expand_stream(Hole1-Hole).

% Stream character directly
expand_stream(Hole0-Hole) -->
    [C],
    !,
    { Hole0 = [C|Hole1] },
    expand_stream(Hole1-Hole).

% End of input
expand_stream(Hole-Hole) --> [].
```

**Complete Working Example** ([examples/template_expansion_dcg.pl](examples/template_expansion_dcg.pl)):

```prolog
%% Template Variable Expansion
%% Input:  "Hello ${name}, you are ${age} years old"
%% Bindings: [name-"John", age-"25"]
%% Output: "Hello John, you are 25 years old"

expand_template(TemplateString, Bindings, OutputString) :-
    string_codes(TemplateString, TemplateCodes),
    phrase(expand_stream(Bindings, OutputCodes-[]), TemplateCodes),
    string_codes(OutputString, OutputCodes).

% Main streaming loop - pure continuous flow

% Found variable - parse and expand
expand_stream(Bindings, Hole0-Hole) -->
    `${`,
    !,
    expand_variable(Bindings, Hole0-Hole1),
    expand_stream(Bindings, Hole1-Hole).

% Regular character - stream directly to output
expand_stream(Bindings, Hole0-Hole) -->
    [C],
    !,
    { Hole0 = [C|Hole1] },
    expand_stream(Bindings, Hole1-Hole).

% End of input
expand_stream(_, Hole-Hole) --> [].

% Parse variable name and expand it
expand_variable(Bindings, Hole0-Hole) -->
    variable_name(VarNameCodes),
    `}`,
    !,
    {
        atom_codes(VarName, VarNameCodes),
        expand_variable_value(VarName, Bindings, VarNameCodes, Hole0-Hole)
    }.

% Helper: Found in bindings - stream replacement to output
expand_variable_value(VarName, Bindings, _, Hole0-Hole) :-
    memberchk(VarName-Value, Bindings),
    !,
    string_codes(Value, ValueCodes),
    add_codes_dl(ValueCodes, Hole0-Hole).

% Helper: Not found - stream original ${varname} to output
expand_variable_value(_, _, VarNameCodes, Hole0-Hole) :-
    Hole0 = [0'$, 0'{|Hole1],
    add_codes_dl(VarNameCodes, Hole1-Hole2),
    Hole2 = [0'}|Hole].

% Helper: Convert code list to difference list (NO append/3)
add_codes_dl([], Hole-Hole).
add_codes_dl([C|Cs], [C|More]-Hole) :-
    add_codes_dl(Cs, More-Hole).

% Parse variable name (minimal buffering for lookup)
variable_name([C|Cs]) -->
    [C],
    {
      code_type(C, alnum)
    ;
      C = 0'_
    },
    !,
    variable_name(Cs).
variable_name([]) --> [].
```

**Usage:**
```prolog
?- expand_template("Hello ${name}, you are ${age} years old",
                   [name-"John", age-"25"], Out).
Out = "Hello John, you are 25 years old".
```

**Why This Is Pure Streaming:**

1. **Character flow**: `H → e → l → l → o →  → $ → {...var...} → , ...`
2. **Direct streaming**: Regular characters go straight to output: `Out = [C|Out1]`
3. **Minimal buffering**: Only variable name buffered (bounded, necessary for lookup)
4. **No reordering**: Output order matches input order
5. **Single pass**: Each character touched exactly once

**Key Benefits:**

1. **True O(n)**: Each character processed exactly once
2. **Continuous flow**: Characters stream through without accumulation
3. **Memory efficient**: No intermediate structures
4. **Predictable performance**: Linear in input size
5. **Elegant composition**: Clear streaming pattern

**When to Use Pure Streaming:**

- ✅ Format conversions that preserve structure (comment style changes)
- ✅ Text transformations (escaping, case conversion)
- ✅ Template expansion with in-place replacement
- ✅ Any transformation where output order ≈ input order
- ❌ Transformations requiring reordering (use next pattern)
- ❌ Complex restructuring (use parsing + transformation phases)

### Pattern: Difference Lists for Output Accumulation with Reordering

**IMPORTANT**: Use explicit difference lists when building output that requires **reordering** or **buffering** parsed elements. Use the **`_dl` suffix convention** for all rules that emit via difference lists, and **buffer elements AS difference lists** for O(1) emission.

This pattern is for transformations where:
- **Input structure is reordered** in output
- **Parsed elements are buffered AS difference lists** for later O(1) emission
- **Multiple segments combined** efficiently with uniform `_dl` rules
- **ALL emission rules** use `_dl` suffix for composability

**⚠️ NOTE**: This is NOT a pure streaming pattern! Characters may be buffered and reordered.

**The Core Pattern with `_dl` Suffix:**
```prolog
% Parse and reorder - buffer AS difference list for O(1) emission!
convert_dl(Hole0-Hole) -->
    parse_part_A_dl(BufferedDL),    % Parse and buffer AS difference list!
    parse_part_B_dl(Hole0-Hole1),   % Output part B first
    parse_part_C_dl(Hole1-Hole2),   % Then part C
    { BufferedDL = Hole2-Hole }.    % Emit buffered - O(1) unification!
```

**Complete Working Example** ([examples/sql_reordering_dcg_v2.pl](examples/sql_reordering_dcg_v2.pl)):

```prolog
%% SQL Dialect Translation - Pure Difference List Pattern with _dl Suffix
%% Input:  "SELECT TOP 10 name FROM users WHERE active = 1"
%% Output: "SELECT name FROM users WHERE active = 1 LIMIT 10"
%%
%% KEY INNOVATION: All rules use _dl suffix, buffered elements stored AS
%% difference lists for O(1) emission!

convert_sql(InputSQL, OutputSQL) :-
    string_codes(InputSQL, InputCodes),
    phrase(convert_statement_dl(OutputCodes-[]), InputCodes),
    string_codes(OutputSQL, OutputCodes).

% Main conversion - all _dl rules for uniform composition
convert_statement_dl(Hole0-Hole) -->
    `SELECT`, 'whites+',
    top_clause_dl(TopDL),           % Buffer TOP AS difference list!
    'whites*',
    convert_fields_dl(Hole0-Hole1), % 1: Emit "SELECT fields"
    'whites+',
    convert_from_dl(Hole1-Hole2),   % 2: Emit " FROM table"
    (
      'whites+',
      convert_where_dl(Hole2-Hole3), !
    ;
      { Hole3 = Hole2 }
    ),
    add_limit_dl(TopDL, Hole3-Hole). % 4: Emit buffered - O(1)!

% Parse TOP - return AS difference list (not closed list!)
top_clause_dl(DigitsDL) -->
    `TOP`, 'whites+',
    'digits_dl+'(DigitsDL),         % DigitsDL = [0'1,0'0|Hole]-Hole
    !.
top_clause_dl(none) --> [].

% Emit LIMIT clause - O(1) unification!
add_limit_dl(none, Hole-Hole) --> [].
add_limit_dl(TopDL, Hole0-Hole) -->
    { TopDL \= none },
    literal_space_dl(Hole0-Hole1),
    keyword_limit_dl(Hole1-Hole2),
    literal_space_dl(Hole2-Hole3),
    { TopDL = Hole3-Hole }.         % O(1) unification! No traversal!

% Keyword emission DCGs (no input parsing, just emit)
keyword_select_dl([0'S,0'E,0'L,0'E,0'C,0'T|Hole]-Hole) --> [].
keyword_limit_dl([0'L,0'I,0'M,0'I,0'T|Hole]-Hole) --> [].
literal_space_dl([0' |Hole]-Hole) --> [].

% Parse identifier from input, emit to output difference list
identifier_dl(Hole0-Hole) -->
    [First],
    {
      code_type(First, alpha)
    ;
      First = 0'_
    },
    { Hole0 = [First|Hole1] },
    'identifier_rest_dl*'(Hole1-Hole).

'identifier_rest_dl*'(Hole0-Hole) -->
    [C],
    {
      code_type(C, alnum)
    ;
      C = 0'_
    },
    !,
    { Hole0 = [C|Hole1] },
    'identifier_rest_dl*'(Hole1-Hole).
'identifier_rest_dl*'(Hole-Hole) --> [].

% Parse digits, emit via difference list (for buffering!)
'digits_dl+'(Hole0-Hole) -->
    [D],
    { code_type(D, digit) },
    !,
    { Hole0 = [D|Hole1] },
    'digits_dl*'(Hole1-Hole).

'digits_dl*'(Hole0-Hole) -->
    [D],
    { code_type(D, digit) },
    !,
    { Hole0 = [D|Hole1] },
    'digits_dl*'(Hole1-Hole).
'digits_dl*'(Hole-Hole) --> [].
```

**Usage:**
```prolog
?- convert_sql("SELECT TOP 10 name FROM users WHERE active = 1", Out).
Out = "SELECT name FROM users WHERE active = 1 LIMIT 10".
```

#### The `_dl` Suffix Pattern Innovation

**KEY INSIGHT**: Use the `_dl` suffix for ALL rules that emit via difference list. This creates a uniform, composable pattern where buffered elements are stored AS difference lists for O(1) emission.

**IMPORTANT NAMING CONVENTION**: The `_dl` suffix comes **before** quantifiers:
- ✅ `'digits_dl+'` - "difference list version of digits, one or more"
- ✅ `'digits_dl*'` - "difference list version of digits, zero or more"
- ✅ `'identifier_rest_dl*'` - "difference list version of identifier_rest, zero or more"
- ❌ `'digits+_dl'` - WRONG: confusing, suggests "digits plus something called dl"

Pattern: `base_dl` + quantifier (`+`, `*`, `?`)

**Traditional Approach (requires O(n) traversal):**
```prolog
% Parse and return closed list
top_clause(Digits) -->
    'digits+'(Digits).              % Digits = [0'1, 0'0] (closed list)

% Later: Must traverse to emit
add_limit(Digits, Hole0-Hole) -->
    {
        Hole0 = [0' ,0'L,0'I,0'M,0'I,0'T,0' |LimitStart],
        add_codes_dl(Digits, LimitStart-Hole)  % O(n) traversal!
    }.
```

**Improved Approach with `_dl` Suffix (O(1) emission):**
```prolog
% Parse and return AS difference list
top_clause_dl(DigitsDL) -->
    'digits_dl+'(DigitsDL).         % DigitsDL = [0'1,0'0|Hole]-Hole (DL!)

% Later: Just unify - O(1)!
add_limit_dl(TopDL, Hole0-Hole) -->
    literal_space_dl(Hole0-Hole1),
    keyword_limit_dl(Hole1-Hole2),
    literal_space_dl(Hole2-Hole3),
    { TopDL = Hole3-Hole }.         % O(1) unification! Just connect holes!
```

**Why This Works:**
```prolog
% TopDL is stored as: [0'1, 0'0 | Hole]-Hole
% When emitting at Hole3-Hole:
%   Unify: [0'1, 0'0 | Hole]-Hole = Hole3-Hole
%   Result: Hole3 = [0'1, 0'0 | Hole], variables unified
% No list traversal - pure pointer manipulation!
```

**Benefits of `_dl` Suffix Pattern:**

1. **O(1) Buffered Emission**: Emitting stored elements is unification, not traversal
2. **Uniform Composition**: All `_dl` rules have consistent `Hole0-Hole` signature
3. **Clear Naming**: `_dl` suffix + Hole terminology explicitly marks difference lists
4. **Clause Indexing**: Consistent argument patterns enable just-in-time indexing
5. **No Helper Needed**: No `add_codes_dl/2` helper for buffered elements

**Common `_dl` Rule Patterns:**
```prolog
% Keyword emission (no input parsing, just emit to output)
keyword_select_dl([0'S,0'E,0'L,0'E,0'C,0'T|Hole]-Hole) --> [].

% Literal emission (no input parsing)
literal_space_dl([0' |Hole]-Hole) --> [].
literal_comma_dl([0',,Hole]-Hole) --> [].

% Parse from input AND emit to output simultaneously
identifier_dl(Hole0-Hole) -->
    [C],                            % Parse from input
    { Hole0 = [C|Hole1] },          % Emit to output DL
    more_dl(Hole1-Hole).            % Recurse

% Base case - close difference list
base_dl(Hole-Hole) --> [].
```

**The "Magic" of Difference List Threading:**

```prolog
% Hole0-Hole1 represents a "list with a hole":
% Hole0 = [part1_item1, part1_item2, ... | Hole1]
%                                          ^^^^^
%                                          The "hole"

% Hole1 becomes the start of the next segment:
% Hole1 = [part2_item1, part2_item2, ... | Hole2]
%                                          ^^^^^
%                                          New hole

% Creates ONE continuous list:
% Hole0 = [part1_item1, part1_item2, part2_item1, part2_item2, ... | Hole]
```

**Key Characteristics:**

1. **`_dl` suffix pattern**: All emission rules consistently named
2. **Reordering**: TOP parsed at position 2, output at position 5
3. **Buffered AS difference lists**: TopDL = `[0'1,0'0|Hole]-Hole`, not `[0'1,0'0]`
4. **O(1) emission**: `{ TopDL = Hole3-Hole }` is pure unification
5. **No re-processing**: Digits kept as codes, never converted

**Key Benefits:**

1. **O(1) buffered emission**: No traversal when emitting stored elements
2. **True O(n) end-to-end**: Input parsing + output emission both O(n)
3. **Uniform composition**: All `_dl` rules have consistent signature
4. **Flexible reordering**: Parse in one order, output in another
5. **Just-in-time indexing**: Consistent patterns enable clause indexing
6. **No helper predicates**: No `add_codes_dl/2` needed for buffered elements

**When to Use Reordering Pattern:**

- ✅ Converting between formats with structural differences
- ✅ Building output with reordered components
- ✅ Combining many segments efficiently
- ✅ Any situation requiring buffering of parsed elements
- ❌ Pure streaming transformations (use previous pattern)
- ❌ Complex multi-pass transformations (consider separate phases)

**Trade-offs:**

- ✅ O(1) emission of buffered elements (vs O(n) traditional approach)
- ✅ Uniform composable pattern with `_dl` suffix
- ✅ Bounded buffering (not full document)
- ⚠️ Not pure streaming - reordering breaks continuous flow
- ⚠️ Buffered elements must fit in memory
- ⚠️ Requires discipline to use `_dl` suffix consistently

**Guidelines for Both Patterns:**

1. **Use `_dl` suffix** for all rules that emit via difference list
2. **Use `Hole0-Hole` naming** consistently in DCG signatures for pedagogy
3. Initialize with `Output-[]` at top level
4. Thread through operations as `Hole0-Hole1`, `Hole1-Hole2`, etc.
5. Base case unifies tail with remainder: `Hole-Hole`
6. **Buffer elements AS difference lists** for O(1) emission (reordering pattern)
7. Keep parsed elements as codes (no unnecessary conversions)
8. Choose pattern based on whether reordering is needed

#### Naming Convention: Hole0-Hole Pattern

**IMPORTANT**: Use consistent `Hole0-Hole` naming throughout difference list DCG code for clarity and pedagogy.

**The Pattern:**

Difference list variables should follow the "Hole" naming convention with numbered threading that matches the traditional accumulator pattern (`Acc0`, `Acc1`, `Acc`):

```prolog
% DCG signature uses Hole0-Hole
rule_dl(Hole0-Hole) -->
    part_A_dl(Hole0-Hole1),    % First: Hole0 → Hole1
    part_B_dl(Hole1-Hole2),    % Second: Hole1 → Hole2
    part_C_dl(Hole2-Hole).     % Final: Hole2 → Hole (closes the chain)

% Base case: Hole-Hole (no numbering, same variable twice)
base_case_dl(Hole-Hole) --> [].

% Single step emission: Hole0-Hole1
single_step_dl(Hole0-Hole1) -->
    [C],
    { Hole0 = [C|Hole1] }.
```

**Why This Convention:**

1. **Pedagogical Clarity**: "Hole" explicitly represents the unbound tail of a difference list
2. **Matches Accumulator Pattern**: `Hole0`, `Hole1`, `Hole2`, `Hole` mirrors `Acc0`, `Acc1`, `Acc2`, `Acc`
3. **Threading Visibility**: Numbered holes show data flow through the DCG chain
4. **Consistent with `_dl` Suffix**: Both naming conventions reinforce difference list usage

**Hole Numbering Rules:**

1. **Start with `Hole0`**: First argument in sequence is `Hole0-Hole`
2. **Number intermediate holes**: Each step increments: `Hole0-Hole1`, `Hole1-Hole2`, etc.
3. **End with `Hole`**: Final variable is unnumbered `Hole` (like `Acc` in accumulator pattern)
4. **Base case uses `Hole-Hole`**: When no numbering needed, use same variable name twice

**Complete Example:**

```prolog
% Top-level: OutputCodes-[] initializes the difference list
convert_sql(InputSQL, OutputSQL) :-
    string_codes(InputSQL, InputCodes),
    phrase(convert_statement_dl(OutputCodes-[]), InputCodes),
    string_codes(OutputSQL, OutputCodes).

% Main conversion: Hole0-Hole with numbered threading
convert_statement_dl(Hole0-Hole) -->
    keyword_select_dl(Hole0-Hole1),     % Emit "SELECT"
    literal_space_dl(Hole1-Hole2),      % Emit " "
    field_list_dl(Hole2-Hole3),         % Emit fields
    from_clause_dl(Hole3-Hole).         % Emit FROM clause

% Keyword emission: Hole-Hole (base case pattern)
keyword_select_dl([0'S,0'E,0'L,0'E,0'C,0'T|Hole]-Hole) --> [].

% Literal emission: Hole-Hole (base case pattern)
literal_space_dl([0' |Hole]-Hole) --> [].
```

**Benefits:**

- Makes difference list "magic" explicit and learnable
- Distinguishes difference list code from traditional DCG code
- Facilitates code review and maintenance
- Consistent with Prolog community conventions for accumulators

#### Pattern: Sentinel Values for Optional Elements

**IMPORTANT**: Use sentinel values (like `none`) for optional parsed elements instead of empty difference lists or unbound variables.

When parsing optional elements that may or may not be present, use a sentinel value to represent absence. This makes code clearer and easier to work with.

**Problem with Empty Difference Lists:**
```prolog
% Hard to work with - awkward structure matching
top_clause_dl([]-[]) --> [].

% Later: Complex pattern matching and guards
add_limit_dl([]-[], Hole-Hole) --> [].
add_limit_dl(TopDL, Hole0-Hole) -->
    { TopDL \= ([]-[]) },  % Awkward structural guard
    ...
```

**Solution with Sentinel Value:**
```prolog
% Clean and explicit
top_clause_dl(none) --> [].

% Later: Simple pattern matching
add_limit_dl(none, Hole-Hole) --> [].
add_limit_dl(TopDL, Hole0-Hole) -->
    { TopDL \= none },  % Simple guard
    ...
```

**When to Use Different Sentinels:**

**1. Simple `none` (most common):**
Use when only one type of absence in the given context.
```prolog
% Context (argument position) distinguishes meaning
top_clause_dl(none) --> [].
where_clause_dl(none) --> [].
order_by_clause_dl(none) --> [].

% Later usage is unambiguous
add_limit_dl(TopDL, ...) -->        % TopDL is none or difference list
    ...
add_where_dl(WhereDL, ...) -->      % WhereDL is none or difference list
    ...
```

**2. Namespaced Sentinels (for clarity/debugging):**
Use when multiple optional elements in same structure or when debugging needs clarity.
```prolog
% Namespace by clause type - explicit in traces
top_clause_dl(top(none)) --> [].
where_clause_dl(where(none)) --> [].
order_by_clause_dl(order_by(none)) --> [].

% Later: Clearer what's absent
add_limit_dl(top(none), Hole-Hole) --> [].  % Explicit: TOP is absent
```

**3. Descriptive Atoms (distinguishing absence reasons):**
Use when need to distinguish different absence scenarios.
```prolog
% Different reasons for absence
top_clause_dl(absent) --> [].           % Not present in input
top_clause_dl(empty) -->                % Present but empty value
    `TOP`, 'whites+', 'whites*'.
top_clause_dl(default(10)) -->          % Use default value
    { DefaultLimit = 10 }.
```

**4. Compound Terms (with metadata):**
Use when absence carries additional information.
```prolog
% Error with details
optional_clause_dl(error(parse_failed, Line, Msg)) --> ...

% Default with provenance
optional_value_dl(default(value(10), from(config))) --> ...
```

**Benefits of Sentinel Pattern:**

1. **Clearer Semantics**: `none` explicitly means "optional element not present"
2. **Easier Pattern Matching**: Direct match on atom, not complex structure
3. **Simpler Guards**: `TopDL \= none` more readable than structural checks
4. **Better Debugging**: Seeing `none` in trace clearer than unbound variables
5. **Type-Like Checking**: Acts as tagged union (value is `none` OR actual data)
6. **Prevents Errors**: Can't accidentally unify with partial difference list

**Guidelines:**

- ✅ Use simple `none` when context distinguishes meaning (most cases)
- ✅ Use namespaced `clause(none)` when need clarity in debugging
- ✅ Use descriptive atoms when distinguishing absence reasons
- ✅ Use compound terms when carrying metadata about absence
- ❌ Avoid empty difference lists `[]-[]` for optional elements
- ❌ Avoid leaving variables unbound to represent absence

**Example from SQL Translation:**
```prolog
% TOP is optional - use none sentinel
top_clause_dl(DigitsDL) -->
    `TOP`, 'whites+',
    'digits_dl+'(DigitsDL),
    !.
top_clause_dl(none) --> [].  % Clean: TOP not present

% Later: Simple to handle
add_limit_dl(none, Hole-Hole) --> [].  % No LIMIT when no TOP
add_limit_dl(TopDL, Hole0-Hole) -->    % Add LIMIT when TOP present
    { TopDL \= none },
    literal_space_dl(Hole0-Hole1),
    keyword_limit_dl(Hole1-Hole2),
    literal_space_dl(Hole2-Hole3),
    { TopDL = Hole3-Hole }.  % O(1) unification
```

**Related Patterns:**
- Combines with **DCG Forward Accumulation** (build result lists)
- Complements **String/Atom/Codes Conversions** (parse as codes, build as codes)
- Works with **Multiple Accumulators** when tracking position + building output
- Uses **Sentinel Values for Optional Elements** (see pattern above) for cleaner optional parsing

### Pattern: Partial Parsing with phrase/3

**IMPORTANT**: Use `phrase/3` during development for error recovery. Switch to `phrase/2` when DCG works correctly.

```prolog
% Development: Use phrase/3 to see where parse fails
test_parse_development(Input, Result, Remainder) :-
    string_codes(Input, Codes),
    phrase(parse_line(Result), Codes, Remainder),
    format('Parsed: ~w~n', [Result]),
    format('Remaining: ~s~n', [Remainder]).

% Production: Use phrase/2 for complete parse
parse_line_production(Input, Result) :-
    string_codes(Input, Codes),
    phrase(parse_line(Result), Codes).

% Error recovery pattern during development
parse_with_recovery(Input, Result, ErrorPos) :-
    string_codes(Input, Codes),
    phrase(parse_line(Result), Codes, Remainder),
    process_parse_result(Remainder, Codes, ErrorPos).

process_parse_result([], _, none) :- !.
process_parse_result(Remainder, Codes, ErrorPos) :-
    length(Codes, Total),
    length(Remainder, Left),
    ErrorPos is Total - Left,
    format('Parse incomplete at position ~w~n', [ErrorPos]),
    format('Remaining input: ~s~n', [Remainder]).

% Try alternative parsers on remainder
parse_with_fallback(Result) -->
    (
      parse_primary(Result), !
    ;
      parse_secondary(Result), !
    ;
      parse_raw_line(Result)
    ).
```

**Usage Pattern:**
1. **Development**: Use phrase/3 to identify where parsing fails
2. **Debugging**: Inspect remainder to understand why parse stopped
3. **Production**: Switch to phrase/2 once DCG is working correctly
4. **Error Recovery**: Use phrase/3 to parse partial input and report position

### Pattern: Lookahead and Backtracking Control

**IMPORTANT**: Use lookahead to make parsing decisions without consuming input. Control backtracking with cuts.

```prolog
% Peek at next character without consuming
% Note: This naturally fails if no character is available (safe)
peek(C) --> [C], [C].

peek_char(C), [C] --> [C].

% Lookahead for decision making
parse_declaration -->
    peek(0'#), !,      % Look ahead for '#'
    parse_macro.
parse_declaration -->
    peek(C),
    { code_type(C, alpha) }, !,
    parse_identifier_decl.

% Check for specific sequence without consuming
lookahead(Seq), Seq --> Seq.

% Commit after lookahead
parse_function_or_variable -->
    identifier(Name),
    (
      lookahead(`(`), !,    % Lookahead then commit
      parse_function(Name)
    ;
      parse_variable(Name)
    ).

% Negative lookahead - ensure something is NOT present
not_followed_by(Pattern), Rest, Rest -->
    Rest,
    { \+ phrase(Pattern, Rest, _) }.

% Use negative lookahead
string_without_quote([C|Cs]) -->
    [C],
    { C \= 0'" },         % Not a quote
    not_followed_by(`*/`),  % Not followed by comment end
    string_without_quote(Cs).
string_without_quote([]) --> [].

% Multi-token lookahead for error reporting
parse_field_with_check(Field, Offset, Line) -->
    peek_tokens(3, Tokens),
    { validate_tokens(Tokens, Offset, Line) }, !,
    parse_field(Field).
parse_field_with_check(error(Pos, Tokens), Offset, Line) -->
    peek_tokens(3, Tokens),
    { Pos = pos(Line, Offset) }.

% IMPORTANT: peek_tokens safely returns as many tokens as are available (up to N)
% without error if fewer than N tokens remain
peek_tokens(0, []) --> [].
peek_tokens(N, [T|Ts]), [T] -->
    { N > 0 },
    [T], !,  % Try to get one token, cut if successful
    { succ(N1, N) },  % Decrement N by 1
    peek_tokens(N1, Ts).
peek_tokens(_N, []) --> [].  % Fewer than N tokens available, return what we got
```

**Benefits:**
- Make parsing decisions without consuming input
- Commit to choices after lookahead for determinism
- Negative lookahead for complex constraints
- Multi-token lookahead for better error messages
- Control backtracking explicitly with cuts
- **Bounds safety**: `peek_tokens/2` returns available tokens without error if input is shorter than requested

### Pattern: Bounds Checking for Peek Operations

**IMPORTANT**: Always ensure peek operations check that there are enough characters remaining. Accessing beyond the end of input causes errors.

```prolog
% ✅ PREFER: Safe single-character peek
% Naturally fails if no character is available
peek(C) --> [C], [C].

% ✅ PREFER: Safe multi-character peek with bounds checking
% Returns as many characters as available (up to N)
peek_chars(0, []) --> [].
peek_chars(N, [C|Cs]), [C] -->
    { N > 0 },
    [C], !,  % Try to get one character, cut if successful
    { succ(N1, N) },  % Decrement N by 1
    peek_chars(N1, Cs).
peek_chars(_N, []) --> [].  % Fewer than N chars available

% ✅ PREFER: Check length before peeking specific number
peek_exactly_n(N, Chars) -->
    peek_chars(N, Chars),
    { length(Chars, N) }.  % Verify we got exactly N

% ❌ AVOID: Unsafe peek that errors on short input
% This will throw an error if fewer than N characters remain
unsafe_peek(N, Chars) -->
    sequence(N, Chars), sequence(N, Chars).

% ✅ PREFER: Safe lookahead with multiple clauses
parse_with_lookahead(Result) -->
    peek_chars(2, [0'/, 0'*]),  % Check for /*
    !,
    parse_comment(Result).
parse_with_lookahead(Result) -->
    parse_code(Result).
```

**Guidelines:**
- Simple `peek(C) --> [C], [C]` is safe - fails gracefully when no input remains
- For multi-character peek, use a clause that handles short input: `peek_chars(_N, []) --> []`
- Add cut after successful match to commit: `[C], !, peek_chars(N1, Cs)`
- Check result length if you need exactly N characters
- Peek operations should never throw errors on short input

### Pattern: Avoiding Redundant Peek - Check the Invocation Chain

**IMPORTANT**: Before adding `peek`, check if the called predicate already performs the same check. Most unnecessary `peek` calls can be eliminated by examining the invocation chain.

**The Anti-Pattern: Redundant Peek**

```prolog
% ❌ AVOID: Redundant peek when called predicate already checks
value_dl(Out-OutRest) -->
    peek(C),                      % ❌ Unnecessary lookahead
    { code_type(C, digit) },      % ❌ Duplicate check
    !,
    'digits_dl+'(Out-OutRest).    % Already checks for digit!
value_dl(Out-OutRest) -->
    identifier_dl(Out-OutRest).

% The problem: digits_dl+ ALREADY checks the first character:
'digits_dl+'(Hole0-Hole) -->
    [D],                          % <-- Consumes first char
    { code_type(D, digit) },      % <-- ALREADY checks if digit!
    !,
    { Hole0 = [D|Hole1] },
    'digits_dl*'(Hole1-Hole).
```

**The Solution: Natural DCG Alternatives**

```prolog
% ✅ PREFER: Let the called predicate self-determine applicability
value_dl(Out-OutRest) -->
    'digits_dl+'(Out-OutRest), !.  % Tries first, fails naturally if not digit
value_dl(Out-OutRest) -->
    identifier_dl(Out-OutRest).    % Falls back automatically

% No peek needed! digits_dl+ checks the first character itself.
% If it's not a digit, it fails and we try identifier_dl.
```

**The "Check Before Peek" Decision Tree**

Before adding `peek(C)`, ask these questions in order:

1. **Does the called predicate already check the first character?**
   - If YES → Remove the peek, use natural DCG alternatives
   - Example: `digits_dl+`, `identifier_dl`, most parsing predicates

2. **Will the alternative clause handle the failure gracefully?**
   - If YES → No peek needed, let backtracking work naturally
   - If NO → Consider if a cut after success is sufficient

3. **Is backtracking acceptable here?**
   - If YES → Use natural DCG alternatives with optional cut
   - If NO → Evaluate if peek is truly necessary

**When Peek IS Necessary**

Use `peek` only in these specific cases:

**A. Lookahead WITHOUT consuming input for later use**
```prolog
% ✅ Appropriate: Need to see "--" but not consume it yet
parse_token -->
    peek(0'-), peek_ahead(0'-, 1),  % Check for "--"
    !,                               % But don't consume yet
    skip_line_comment.               % This will consume it
```

**B. The called predicate is too generic to self-check**
```prolog
% ✅ Appropriate: [C] is too generic
parse_operator -->
    peek(C),
    { member(C, [0'+, 0'-, 0'*, 0'/]) },
    !,
    [C],  % Now consume it
    emit_operator(C).
```

**C. Complex dispatch logic based on lookahead**
```prolog
% ✅ PREFER: Complex multi-way dispatch with helper predicate
parse_literal -->
    peek(C),
    { classify_literal_type(C, Type) },
    parse_by_type(Type).  % Different parsers for each

classify_literal_type(0'", string) :- !.
classify_literal_type(0'', char) :- !.
classify_literal_type(C, number) :- code_type(C, digit), !.
classify_literal_type(_, identifier).
```

**Red Flag Detection Rule**

🚨 **If you see this pattern:**
```prolog
rule -->
    peek(C),
    { some_check(C) },
    !,
    called_predicate.  % <-- STOP! Check this predicate
```

**Ask: Does `called_predicate` already perform `some_check(C)` on its first character?**
- If YES → Remove the peek
- If NO → Peek may be appropriate

**Example from Real Code**

From `examples/sql_reordering_dcg_v2.pl` before fix:
```prolog
% ❌ BEFORE: Unnecessary peek
value_dl(Hole0-Hole) -->
    peek(C),
    { code_type(C, digit) },
    !,
    'digits_dl+'(Hole0-Hole).
value_dl(Hole0-Hole) -->
    identifier_dl(Hole0-Hole).
```

After applying this pattern:
```prolog
% ✅ AFTER: Natural DCG alternatives
value_dl(Hole0-Hole) -->
    'digits_dl+'(Hole0-Hole), !.
value_dl(Hole0-Hole) -->
    identifier_dl(Hole0-Hole).
```

**Decision Flowchart Summary**

```
Need to parse alternatives?
├─ Does each alternative self-check its applicability?
│  ├─ YES → Use natural DCG clauses (no peek needed)
│  └─ NO → Consider refactoring or using peek
│
└─ Need lookahead without consumption?
   ├─ YES → peek is appropriate
   └─ NO → Don't use peek
```

**Guidelines:**
- Default to natural DCG alternatives - let predicates fail naturally
- Add `peek` only when you have a clear reason from the "When Peek IS Necessary" list
- Always check the invocation chain before adding peek
- Most parsing predicates self-check their applicability - trust them
- Use explicit cuts after successful alternatives if determinism is needed

## Position Tracking Patterns

### Pattern: Position Tracking

**IMPORTANT**: Always track character and line offsets for debugging. Large files (megabytes) require precise error locations for grepping to the problem.

Choose between two patterns based on your needs:

#### Pattern 1: Byte-Oriented Tracking (Simple)

Use when you only need absolute character position from start of file.

**Conventions:**
- Position accumulators go at **END** of argument list: `Offset0, Offset`
- **Offset increments by 1** for EACH character consumed (character-level precision)
- **IMPORTANT**: Use `succ/2` for each single-character increment
- **IMPORTANT**: Break multi-character sequences (like `\r\n`) into individual character increments to avoid manual counting errors
- **Every DCG rule** threads these through, even single-character parsers
- Simpler than line-oriented tracking - use when line/column info not needed
- This provides invaluable debugging for development (can be stripped for production)

#### Pattern 2: Line-Oriented Tracking (Line + Column)

Use when you need both line number and column offset (resets at each new line).

**Conventions:**
- Position accumulators go at **END** of argument list: `Line0, Line, LineOffset0, LineOffset`
- **Line increments by 1** when line ending is encountered
- **LineOffset resets to 0** when line ending is encountered
- **LineOffset increments by 1** for each character consumed within a line
- **IMPORTANT**: Use `succ/2` for each single-character increment
- **IMPORTANT**: Break multi-character sequences (like `\r\n`) into individual character increments to avoid manual counting errors
- **Every DCG rule** threads these through, even single-character parsers
- **IMPORTANT**: Use incremental variables `Line0, Line1, Line2...` NOT `Line, Line`
  - Using `Line, Line` assumes Line never changes, creating brittle code
  - If you later modify a DCG rule to handle embedded newlines, `Line, Line` will cause failure
  - Incremental variables future-proof the code against such changes
- **Use helper predicate** for consistency when updating both Line and LineOffset
- This provides invaluable debugging for development (can be stripped for production)

```prolog
% ============================================================================
% PATTERN 1: Byte-Oriented Tracking (Simple - absolute offset only)
% ============================================================================

% Parse file with absolute byte offset tracking
parse_file_byte_oriented(File, Result) :-
    phrase_from_file(file_content_bytes(Result, 0, _), File).

% Track only absolute offset from start of file
file_content_bytes([Item|Items], Offset0, Offset) -->
    file_item(Item, Offset0, Offset1), !,
    file_content_bytes(Items, Offset1, Offset).
file_content_bytes([], Offset, Offset) --> eos.

% Parse TSV line with byte offset only
parse_line_bytes(data(F1, F2, F3, Pos), Offset0, Offset) -->
    { Pos = offset(Offset0) },  % Simple offset position
    field_bytes(F1, Offset0, Offset1),
    tab(Offset1, Offset2),
    field_bytes(F2, Offset2, Offset3),
    tab(Offset3, Offset4),
    field_bytes(F3, Offset4, Offset5),
    line_end_bytes(Offset5, Offset).

% Character-level DCG primitives with position tracking
% IMPORTANT: Create reusable single-character DCGs, then compose them
tab(Offset0, Offset) -->
    [9],  % ASCII tab
    { succ(Offset0, Offset) }.

cr(Offset0, Offset) -->
    [13],  % CR
    { succ(Offset0, Offset) }.

lf(Offset0, Offset) -->
    [10],  % LF
    { succ(Offset0, Offset) }.

% Composed DCG: line ending built from character-level primitives
line_end_bytes(Offset0, Offset) -->
    cr(Offset0, Offset1),
    lf(Offset1, Offset), !.
line_end_bytes(Offset0, Offset) -->
    lf(Offset0, Offset), !.
line_end_bytes(Offset0, Offset) -->
    cr(Offset0, Offset).

% Field parsing with byte offset tracking
field_bytes(Field, Offset0, Offset) -->
    field_codes_bytes(FieldCodes, Offset0, Offset),
    { atom_codes(Field, FieldCodes) }.

field_codes_bytes([C|Cs], Offset0, Offset) -->
    [C], { C \= 9, C \= 10, C \= 13 }, !,  % Not tab, LF, CR
    { succ(Offset0, Offset1) },  % Increment by 1 per character
    field_codes_bytes(Cs, Offset1, Offset).
field_codes_bytes([], Offset, Offset) --> [].

% ============================================================================
% PATTERN 2: Line-Oriented Tracking (Line + Column offset)
% ============================================================================

% Parse file with line and column tracking
parse_file_line_oriented(File, Result) :-
    phrase_from_file(file_content_lines(Result, 1, _, 0, _), File).

% Track: Line number and LineOffset (column within line)
% Accumulators at END: Line0, Line, LineOffset0, LineOffset
file_content_lines([Item|Items], Line0, Line, LineOffset0, LineOffset) -->
    file_line(Item, Line0, Line1, LineOffset0, LineOffset1), !,
    file_content_lines(Items, Line1, Line, LineOffset1, LineOffset).
file_content_lines([], Line, Line, LineOffset, LineOffset) --> eos.

% Parse one line with position tracking
file_line(Item, Line0, Line, LineOffset0, LineOffset) -->
    parse_line_oriented(Item, Line0, Line0, LineOffset0, LineOffset1),
    line_end_oriented(Line0, Line, LineOffset1, LineOffset).

% Helper predicate: Update line and reset line offset
% IMPORTANT: Use this helper for consistency when line changes
update_line_position(Line0, Line, _LineOffset0, LineOffset) :-
    succ(Line0, Line),
    LineOffset = 0.  % Reset to column 0 at start of new line

% Line ending increments Line by 1, resets LineOffset to 0
line_end_oriented(Line0, Line, LineOffset0, LineOffset) -->
    `\r\n`, !,
    { update_line_position(Line0, Line, LineOffset0, LineOffset) }.
line_end_oriented(Line0, Line, LineOffset0, LineOffset) -->
    `\n`, !,
    { update_line_position(Line0, Line, LineOffset0, LineOffset) }.
line_end_oriented(Line0, Line, LineOffset0, LineOffset) -->
    `\r`, !,
    { update_line_position(Line0, Line, LineOffset0, LineOffset) }.

% Parse TSV line with line-oriented tracking
% IMPORTANT: Use incremental variables (Line0, Line1, Line2...) not (Line, Line)
% This ensures code won't break if rules are later modified to change Line
parse_line_oriented(data(F1, F2, F3, Pos), Line0, Line, LineOffset0, LineOffset) -->
    { Pos = pos(Line0, LineOffset0) },  % Line and column position
    field_oriented(F1, Line0, Line1, LineOffset0, LineOffset1),
    tab_oriented(Line1, Line2, LineOffset1, LineOffset2),
    field_oriented(F2, Line2, Line3, LineOffset2, LineOffset3),
    tab_oriented(Line3, Line4, LineOffset3, LineOffset4),
    field_oriented(F3, Line4, Line, LineOffset4, LineOffset).

% Tab character increments line offset by 1
tab_oriented(Line0, Line, LineOffset0, LineOffset) -->
    [9],  % ASCII tab
    { Line = Line0, succ(LineOffset0, LineOffset) }.

% Field parsing with line-oriented tracking
field_oriented(Field, Line0, Line, LineOffset0, LineOffset) -->
    field_codes_oriented(FieldCodes, Line0, Line, LineOffset0, LineOffset),
    { atom_codes(Field, FieldCodes) }.

field_codes_oriented([C|Cs], Line0, Line, LineOffset0, LineOffset) -->
    [C], { C \= 9, C \= 10, C \= 13 }, !,  % Not tab, LF, CR
    { succ(LineOffset0, LineOffset1) },  % Increment column by 1 per character
    field_codes_oriented(Cs, Line0, Line, LineOffset1, LineOffset).
field_codes_oriented([], Line, Line, LineOffset, LineOffset) --> [].
```

**Benefits:**
- **Character-level precision**: Know exact character position of errors in megabyte files
- **Consistent convention**: Line/Offset accumulators always at end in same order
- **Every rule threads position**: No gaps in tracking, complete audit trail
- **Development aid**: Can be stripped out for production after debugging
- **Error reporting**: Include precise `pos(Line, Offset)` in error terms

## Integration with File I/O

### Using phrase_from_file/2

```prolog
% Best for parsing entire file
parse_file(Filename, Result) :-
    phrase_from_file(file_grammar(Result), Filename).

file_grammar(Lines) -->
    lines(Lines).

lines([L|Ls]) -->
    line(L), !,
    lines(Ls).
lines([]) --> eos.
```

### Using phrase/2 with read_string

```prolog
% When you need to process stream first
parse_stream(Stream, Result) :-
    read_string(Stream, _, Content),
    string_codes(Content, Codes),
    phrase(file_grammar(Result), Codes).
```

### Using catch/3 for File Reading

**IMPORTANT**: Prefer `catch/3` when reading files to handle I/O errors gracefully.

```prolog
:- use_module(library(error)).

% ✅ PREFER: Wrap file operations in catch/3
parse_file_safe(Filename, Result) :-
    catch(
        phrase_from_file(file_grammar(Result), Filename),
        Error,
        handle_file_error(Filename, Error, Result)
    ).

handle_file_error(Filename, Error, []) :-
    print_message(error, Error),
    format(user_error, 'Failed to parse ~w~n', [Filename]).

% Reading with explicit stream management
parse_file_with_stream(Filename, Result) :-
    catch(
        (
            open(Filename, read, Stream, [encoding(utf8)]),
            call_cleanup(
                phrase_from_stream(file_grammar(Result), Stream),
                close(Stream)
            )
        ),
        Error,
        handle_file_error(Filename, Error, Result)
    ).
```

### Error Types to Handle

```prolog
% Handle specific error types
parse_file_safe(Filename, Result) :-
    catch(
        phrase_from_file(file_grammar(Result), Filename),
        Error,
        handle_parse_error(Error, Filename, Result)
    ).

handle_parse_error(error(existence_error(source_sink, Filename), _), Filename, []) :-
    format(user_error, 'File not found: ~w~n', [Filename]).

handle_parse_error(error(permission_error(open, source_sink, Filename), _), Filename, []) :-
    format(user_error, 'Permission denied: ~w~n', [Filename]).

handle_parse_error(Error, _, []) :-
    print_message(error, Error).
```
