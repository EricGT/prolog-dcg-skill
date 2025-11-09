# DCG Parsing Skill

## Purpose
Guide Claude in writing efficient, idiomatic Prolog DCGs (Definite Clause Grammars) following best practices for single-pass parsing and pure declarative style.

**See "Complete Integrated Example" section for a comprehensive demonstration of all patterns working together.**

## Scope and Disclaimer

**IMPORTANT**: This skill document is tailored specifically to **SWI-Prolog** and represents the **creator's preferences** for DCG parsing patterns. There are many valid approaches to parsing with DCGs, and this is not the only way.

The patterns and conventions documented here reflect:
- Specific SWI-Prolog predicates and features
- Preferences for code style and structure
- Trade-offs chosen for this project's specific needs

When applying these patterns, consider your project's requirements and constraints. What works well for one use case may not be optimal for another.

## Core Philosophy

### Prefer: Single-Pass DCG Parsing
Parse data once using DCG rules that handle the entire structure from start to finish, including line endings, whitespace, and field delimiters.

### Prefer: Character Codes for Text Processing
**IMPORTANT**: Always work with character codes (lists of integers) rather than strings or atoms during parsing. This provides:
- **Unicode support**: Handles non-ASCII characters and international text correctly
- **Consistent behavior**: Avoids flag-dependent string interpretation issues
- **Better performance**: Direct integer comparison and manipulation
- **Explicit conversion**: Convert to atoms/strings only at output boundaries

### Avoid: Multiple Passes Over Data
Don't split/convert data first and then parse. Each conversion or traversal adds overhead and obscures the grammar.

**Note:** This rule applies to DCG parsing logic, often invoked via `phrase/2` or `phrase/3`. It should NOT be applied to:
- Reading input from a file and converting to codes for use with `phrase/N`
- Formatting output from `phrase/N` and converting codes for output

### Avoid: External Parsing Libraries
- **Do NOT use library(pcre)** - No regular expressions. Use pure DCG rules instead.
- **Do NOT use library(dcg/basics)** - Write explicit DCG rules to maintain full control and clarity.

Write all parsing logic using pure DCG notation. This ensures the grammar is explicit, maintainable, and doesn't hide complexity behind library abstractions.

## Pattern: Pure DCG vs Mixed Approach

### ❌ AVOID: Mixed String/List/DCG Processing

```prolog
% Multiple passes - AVOID THIS
parse_file(File, Result) :-
    read_file_to_string(File, Content, []),          % Pass 1: read
    split_string(Content, "\n", "\r", Lines),        % Pass 2: split lines
    maplist(process_line, Lines, Results).           % Pass 3: process

process_line(Line, Result) :-
    split_string(Line, "\t", "", [F1, F2, F3]),      % Pass 4: split fields
    string_codes(F3, Codes),                          % Pass 5: convert to codes
    phrase(parse_field(Data), Codes),                 % Pass 6: finally parse with DCG
    Result = data(F1, F2, Data).
```

**Problems:**
- 6 passes over the data
- Mixes string operations, list operations, and DCGs
- Hard to see the actual grammar
- Inefficient memory usage with intermediate structures

### ✅ PREFER: Pure DCG Single-Pass

```prolog
% Single pass - PREFER THIS
parse_file(File, Result) :-
    phrase_from_file(file_content(Result), File).

% Parse entire file structure in one DCG
file_content([Line|Lines]) -->
    file_line(Line), !,
    file_content(Lines).
file_content([]) -->
    eos.

% Parse a single line including newline
file_line(data(F1, F2, Data)) -->
    field(F1), `\t`,
    field(F2), `\t`,
    parse_field(Data),
    line_end.

% Parse field up to delimiter
field(Field) -->
    field_codes(FieldCodes),
    { string_codes(Field, FieldCodes) }.

field_codes([C|Cs]) -->
    [C], { C \= 9 },  % 9 = tab
    !,
    field_codes(Cs).
field_codes([]) --> [].

% Parse specific field content with DCG
parse_field(result(X, Y)) -->
    whites,
    identifier(X),
    whites, `(`,
    identifier(Y),
    `)`.

% Handle different line ending types
line_end --> `\r\n` | `\n` | `\r`.

```

**Benefits:**
- Single pass through data
- Clear grammar structure visible in DCG rules
- Efficient - no intermediate conversions
- Declarative - what to parse, not how to split/convert
- Easy to debug - can test individual DCG rules

## Essential DCG Helpers

Since we avoid `library(dcg/basics)`, define these common helpers explicitly:

### Whitespace Handling

**IMPORTANT**: Prefer `code_type/2` for single character classification when possible.

```prolog
% Optional whitespace (zero or more whitespace chars)
'whites*' --> [C], { code_type(C, space) }, !, 'whites*'.
'whites*' --> [].

% Required whitespace (one or more whitespace chars)
'whites+' --> [C], { code_type(C, space) }, 'whites*'.

% Single whitespace character
white --> [C], { code_type(C, space) }.
```

### Line Endings

```prolog
% Any line ending
line_end --> `\r\n`, !.
line_end --> `\n`.
line_end --> `\r`.
line_end --> eos.  % End of stream

% End of stream
% Note: eos is defined in library(dcg/basics) but should not be used from there.
% Instead, provide both DCG eos//0 and predicate eos_/2 explicitly.
eos --> call(eos_).
eos_([], []).

% NOTE: Usually eos pattern is not needed! A successful deterministic
% parse will naturally consume all input. Use eos only when you need
% explicit end-of-stream checking.
```

### Character Classes

**IMPORTANT**: Prefer `code_type/2` for single character type checking when possible.

```prolog
% Digit - using code_type/2
digit(D) --> [D], { code_type(D, digit) }.

% Letter
alpha(C) --> [C], { code_type(C, alpha) }.

% Alphanumeric
alnum(C) --> [C], { code_type(C, alnum) }.

% Specific character checks
is_tab(9).
is_newline(10).
is_cr(13).

% Non-whitespace character
non_white(C) --> [C], { \+ code_type(C, space) }.

% Check character type without consuming
% Note: Safely fails if no character is available
peek(C) --> [C], [C].
```

% HERE HERE

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

## Error Handling and Debugging in DCGs

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

### Pattern: Position-Aware Error Reporting

```prolog
% Report errors with exact position
% Accumulators at END: Line0, Line, Offset0, Offset
parse_with_error_reporting(Result, Line0, Line, Offset0, Offset) -->
    parse_item(Result, Line0, Line, Offset0, Offset), !.
parse_with_error_reporting(error(Pos, Context), Line0, Line, Offset0, Offset) -->
    { Pos = pos(Line0, Offset0) },
    rest_of_line_codes(Context, Line0, Line, Offset0, Offset),
    { debug(parse, 'Parse error at line ~w, offset ~w: ~s', [Line0, Offset0, Context]) }.

% Rest of line with position tracking
% Line doesn't change while reading characters within a line
rest_of_line_codes([C|Cs], Line0, Line, Offset0, Offset) -->
    [C], { C \= 10, C \= 13 }, !,  % Not LF, CR
    { succ(Offset0, Offset1) },
    rest_of_line_codes(Cs, Line0, Line, Offset1, Offset).
rest_of_line_codes([], Line, Line, Offset, Offset) --> [].
```

### Pattern: Using library(debug)

**IMPORTANT**: Use `library(debug)` for debugging DCG parsing. Enable/disable debug output without changing code.

```prolog
:- use_module(library(debug)).

% Define debug topics
:- debug(parse).          % Enable: ?- debug(parse).
:- debug(parse_line).     % Disable: ?- nodebug(parse_line).

% Use debug/3 in DCG rules with position tracking
parse_line(Result, Line0, Line, Offset0, Offset) -->
    { debug(parse_line, 'Parsing line ~w at offset ~w', [Line0, Offset0]) },
    field(F1, Line0, Line1, Offset0, Offset1),
    tab(Line1, Line2, Offset1, Offset2),
    field(F2, Line2, Line3, Offset2, Offset3),
    tab(Line3, Line4, Offset3, Offset4),
    { debug(parse_line, 'Parsed fields: ~w, ~w', [F1, F2]) },
    parse_data(Data, Line4, Line, Offset4, Offset),
    { Result = data(F1, F2, Data) },
    { debug(parse_line, 'Result: ~w', [Result]) }.

% Enable debugging for specific sections
parse_file(File, Result) :-
    debug(parse, 'Starting parse of ~w', [File]),
    phrase_from_file(file_grammar(Result, 1, _, 0, _), File),
    debug(parse, 'Completed parse: ~w items', [Result]).
```

### Pattern: Using library(error) for Error Reporting

**IMPORTANT**: Prefer `library(error)` for throwing structured, standardized errors.

```prolog
:- use_module(library(error)).

% Throw structured errors with must_be/2
parse_field(Field, Line0, Line, Offset0, Offset) -->
    field_codes(FieldCodes, Line0, Line, Offset0, Offset),
    { must_be(codes, FieldCodes) },  % Validate type
    { string_codes(Field, FieldCodes) }.

% Domain errors for invalid values
validate_symbol_kind(Kind, Line0, Line, Offset0, Offset) -->
    { must_be(oneof([function, macro, typedef, struct]), Kind) },
    { Line = Line0, Offset = Offset0 },
    !.

% Type errors with context
parse_number(Number, Line0, Line, Offset0, Offset) -->
    digits(DigitCodes, Line0, Line, Offset0, Offset),
    {   catch(
            number_codes(Number, DigitCodes),
            _,
            throw(error(
                type_error(integer, DigitCodes),
                context(parse_number/5, 'at line ~w, offset ~w'-[Line0, Offset0])
            ))
        )
    }.

% Existence errors with position
parse_required_field(Field, Line0, Line, Offset0, Offset) -->
    field_codes(FieldCodes, Line0, Line, Offset0, Offset),
    {
      FieldCodes \= []
    ->
      string_codes(Field, FieldCodes)
    ;
      throw(
        error(
          existence_error(field, required),
          context(parse_required_field/5, 'at line ~w, offset ~w'-[Line0, Offset0])
        )
      )
    }.
```

### Pattern: Error Terms with Position Context

```prolog
% Create error terms with position information
throw_parse_error(Type, Context, Line, Offset) :-
    Pos = pos(Line, Offset),
    throw(error(
        Type,
        context(_, 'Parse error at ~w: ~w'-[Pos, Context])
    )).

% Use in DCG rules
parse_definition(Result, Line0, Line, Offset0, Offset) -->
    parse_kind(Kind, Line0, Line1, Offset0, Offset1), !,
    parse_symbol(Symbol, Line1, Line, Offset1, Offset),
    { Result = definition(Kind, Symbol) }.
parse_definition(_, Line0, Line, Offset0, Offset) -->
    rest_of_line_codes(Context, Line0, Line, Offset0, Offset),
    { throw_parse_error(
        syntax_error(invalid_definition),
        Context,
        Line0,
        Offset0
    ) }.
```

### Pattern: Graceful Failure with Position

```prolog
% Parse line, fail gracefully on bad input, record position
parse_line(valid(Data), Line0, Line, Offset0, Offset) -->
    field(F1, Line0, Line1, Offset0, Offset1),
    tab(Line1, Line2, Offset1, Offset2),
    field(F2, Line2, Line3, Offset2, Offset3),
    tab(Line3, Line4, Offset3, Offset4),
    parse_data(Data, Line4, Line5, Offset4, Offset5),
    line_end(Line5, Line, Offset5, Offset), !.
parse_line(invalid(Pos, Content), Line0, Line, Offset0, Offset) -->
    { Pos = pos(Line0, Offset0) },
    rest_of_line_codes(Content, Line0, Line, Offset0, Offset),  % Capture invalid line with position
    { debug(parse, 'Invalid line at ~w: ~s', [Pos, Content]) }.

## Complete Integrated Example

This example demonstrates all best practices: single-pass DCG parsing, position tracking with accumulator pairs, DCG forward accumulation, library(debug), library(error), and proper string/atom/codes conversions.

```prolog
:- module(tsv_parser, [
    parse_tsv_file/2,           % parse_tsv_file(+File, -Records)
    parse_tsv_file_safe/2       % parse_tsv_file_safe(+File, -Records)
]).

:- use_module(library(debug)).
:- use_module(library(error)).

%! parse_tsv_file(+Filename, -Records) is det.
%
% Parse TSV file with position tracking and debugging support.
% Records are returned as list of record(Name, Type, Value, Pos) terms.

parse_tsv_file(Filename, Records) :-
    debug(tsv, 'Starting parse of ~w', [Filename]),
    phrase_from_file(tsv_file(Records), Filename),
    length(Records, Count),
    debug(tsv, 'Completed parse: ~w records', [Count]).

%! parse_tsv_file_safe(+Filename, -Records) is det.
%
% Safe version with error handling.

parse_tsv_file_safe(Filename, Records) :-
    catch(
        parse_tsv_file(Filename, Records),
        Error,
        handle_parse_error(Filename, Error, Records)
    ).

handle_parse_error(Filename, Error, []) :-
    print_message(error, Error),
    format(user_error, 'Failed to parse ~w~n', [Filename]).

% ============================================================================
% Main Grammar - Single Pass with Position Tracking
% ============================================================================

%! tsv_file(-Records)// is det.
%
% Parse entire TSV file in single pass with position tracking.
% Uses DCG forward accumulation to avoid reverse/2.

tsv_file(Records) -->
    tsv_file(Records, 1, 0, _, _).

tsv_file(Records, LineIn, OffsetIn, LineOut, OffsetOut) -->
    tsv_line(Record, LineIn, Line1, OffsetIn, Offset1), !,
    { debug(tsv_line, 'Parsed line ~w: ~w', [LineIn, Record]) },
    tsv_file(RestRecords, Line1, Offset1, LineOut, OffsetOut),
    { Records = [Record|RestRecords] }.
tsv_file([], Line, Offset, Line, Offset) -->
    eos.

%! tsv_line(-Record, +LineIn, -LineOut, +OffsetIn, -OffsetOut)// is det.
%
% Parse single TSV line with position tracking.
% Threads line and offset accumulators through parsing.

tsv_line(Record, LineIn, LineOut, OffsetIn, OffsetOut) -->
    { LineStart = LineIn, OffsetStart = OffsetIn },
    parse_fields(Fields, LineIn, LineIn, OffsetIn, Offset1), !,
    line_end,
    { succ(LineIn, LineOut),
      LineLen is Offset1 - OffsetIn,
      succ(Offset1, OffsetOut),  % +1 for newline
      Fields = [NameCodes, TypeCodes, ValueCodes],
      % Convert codes to atoms/strings once at end
      atom_codes(Name, NameCodes),
      atom_codes(Type, TypeCodes),
      string_codes(Value, ValueCodes),
      Pos = pos(LineStart, OffsetStart),
      Record = record(Name, Type, Value, Pos),
      debug(tsv_line, 'Line ~w (~w chars): ~w', [LineStart, LineLen, Record]) }.
tsv_line(error(Pos, Context), LineIn, LineOut, OffsetIn, OffsetOut) -->
    { Pos = pos(LineIn, OffsetIn) },
    rest_of_line_codes(Context),
    line_end,
    { succ(LineIn, LineOut),
      string_codes(ContextStr, Context),
      OffsetOut is OffsetIn + length(Context) + 1,
      debug(tsv_error, 'Invalid line at ~w: ~s', [Pos, ContextStr]) }.

%! parse_fields(-Fields, +LineIn, -LineOut, +OffsetIn, -OffsetOut)// is det.
%
% Parse tab-separated fields with position tracking.
% Uses DCG forward accumulation pattern to build list forward.

parse_fields(Fields, LineIn, LineOut, OffsetIn, OffsetOut) -->
    parse_fields_dl(Fields, [], LineIn, LineOut, OffsetIn, OffsetOut).

% DCG forward accumulation - builds forward, no reverse needed
parse_fields_dl(Fields, Acc, LineIn, LineOut, OffsetIn, OffsetOut) -->
    field(FieldCodes, OffsetIn, Offset1),
    { append(Acc, [FieldCodes], Acc1) },
    parse_fields_continuation(Fields, FieldCodes, Acc1, LineIn, LineOut, Offset1, OffsetOut).

parse_fields_continuation([_|Rest], _, Acc, LineIn, LineOut, Offset1, OffsetOut) -->
    `\t`,
    !,
    { succ(Offset1, Offset2) },
    parse_fields_dl(Rest, Acc, LineIn, LineOut, Offset2, OffsetOut).
parse_fields_continuation([Field], Field, _, LineIn, LineIn, OffsetIn, OffsetIn) -->
    [].

parse_fields_dl([], _Acc, Line, Line, Offset, Offset) --> [].

%! field(-FieldCodes, +OffsetIn, -OffsetOut)// is det.
%
% Parse single field as codes, track character offset.

field(FieldCodes, OffsetIn, OffsetOut) -->
    field_codes(FieldCodes),
    { length(FieldCodes, Len),
      OffsetOut is OffsetIn + Len,
      debug(tsv_field, 'Field at offset ~w (~w chars)', [OffsetIn, Len]) }.

field_codes([C|Cs]) -->
    [C], { C \= 9, C \= 10, C \= 13 }, !,  % Not tab, LF, CR
    field_codes(Cs).
field_codes([]) --> [].

%! line_end// is det.
%
% Parse line ending (handles \n, \r\n, \r).

line_end --> `\r\n`, !.
line_end --> `\n`, !.
line_end --> `\r`, !.
line_end --> eos.

%! eos// is semidet.
%
% End of stream check.
% Note: eos is defined in library(dcg/basics) but should not be used from there.
% Instead, provide both DCG and predicate explicitly.

eos --> call(eos_).
eos_([], []).

%! rest_of_line_codes(-LineCodes)// is det.
%
% Consume rest of line as codes for error reporting.

rest_of_line_codes([C|Cs]) -->
    [C], { C \= 10, C \= 13 }, !,  % Not LF, CR
    rest_of_line_codes(Cs).
rest_of_line_codes([]) --> [].

% ============================================================================
% Validation with library(error)
% ============================================================================

%! validate_record(+Record, +LineNum) is det.
%
% Validate parsed record, throw structured errors on invalid data.

validate_record(record(Name, Type, Value, Pos), LineNum) :-
    must_be(atom, Name),
    must_be(oneof([string, integer, float]), Type),
    validate_type_value(Type, Value, LineNum).

validate_type_value(integer, Value, LineNum) :-
    !,
    validate_integer(Value, LineNum).
validate_type_value(_, _, _).

validate_integer(Value, LineNum) :-
    atom_number(Value, Num),
    integer(Num),
    !.
validate_integer(Value, LineNum) :-
    throw(error(
        type_error(integer, Value),
        context(validate_record/2, 'at line ~w'-[LineNum])
    )).
```

**Key Features Demonstrated:**

1. **Single-Pass Parsing**: Entire file parsed in one DCG traversal
2. **Position Tracking**: Line and offset accumulators threaded through all predicates
   - `tsv_file//5`: `LineIn, LineOut, OffsetIn, OffsetOut`
   - `tsv_line//5`: Threads position through field parsing
   - `field//3`: Tracks character offset within line
3. **DCG Forward Accumulation**: `parse_fields_dl//6` builds list forward, no reverse/2
4. **Codes Until End**: Parse as codes, convert to atoms/strings only at end
5. **library(debug)**: Debug topics (tsv, tsv_line, tsv_field, tsv_error)
6. **library(error)**: Structured error terms with position context
7. **Error Recovery**: Graceful failure captures invalid lines with position
8. **catch/3**: Safe parsing with error handling

**Usage:**

```prolog
?- debug(tsv).
?- parse_tsv_file('data.tsv', Records).
% Starting parse of data.tsv
% Parsed line 1: record(foo, string, "hello", pos(1, 0))
% Line 1 (15 chars): record(foo, string, "hello", pos(1, 0))
% ...
% Completed parse: 42 records

?- nodebug(tsv).  % Disable debugging

?- parse_tsv_file_safe('data.tsv', Records).  % Safe version
```

## Testing DCGs with Prolog Unit Tests

**IMPORTANT**: All DCG clauses MUST have test cases using [Prolog Unit Tests](https://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/plunit.html%27)) in separate test files.

### Test File Organization

```
src/prolog/parser.pl          % DCG grammar rules
test/prolog/test_parser.pl    % Unit tests for parser
```

### Pattern: Unit Test Structure

```prolog
% test/prolog/test_parser.pl
:- begin_tests(parser).
:- use_module('../../src/prolog/parser').

% Test individual DCG rules
test(field_simple, [true(F == hello)]) :-
    phrase(field(F), `hello`, Rest),
    Rest == [].

test(field_with_remainder, [true(F == hello)]) :-
    phrase(field(F), `hello\tworld`, Rest),
    Rest == [9, 119, 111, 114, 108, 100].  % \tworld

test(tsv_line_three_fields, [true(Fields == [a, b, c])]) :-
    phrase(tsv_line(Fields), `a\tb\tc\n`).

test(tsv_line_empty, [fail]) :-
    phrase(tsv_line(_), ``).

test(tsv_line_no_newline, [true(Fields == [a, b])]) :-
    phrase(tsv_line(Fields), `a\tb`).

:- end_tests(parser).
```

### Pattern: Testing Edge Cases

```prolog
:- begin_tests(parser_edge_cases).

% Empty input
test(empty_file, [true(Result == [])]) :-
    phrase(file_content(Result), ``).

% Single line, no newline
test(single_line_no_newline, [true(Result == [line(data)])]) :-
    phrase(file_content(Result), `data`).

% Multiple line endings
test(crlf_endings, [true(length(Lines, 2))]) :-
    phrase(file_lines(Lines), `line1\r\nline2\r\n`).

test(mixed_endings, [true(length(Lines, 3))]) :-
    phrase(file_lines(Lines), `line1\nline2\r\nline3\r`).

% Invalid input handling
test(invalid_line_graceful, [true(Result = invalid(_, _))]) :-
    phrase(parse_line(0, 1, Result), `bad\tdata`).

:- end_tests(parser_edge_cases).
```

### Pattern: Testing with Position Tracking

```prolog
:- begin_tests(parser_positions).

test(position_tracking_first_line, [true(Pos == pos(1, 0))]) :-
    phrase(parse_line(0, 1, data(_, Pos, _), _), `field1\tfield2\n`).

test(position_tracking_offset, [true(Offset == 15)]) :-
    Input = `field1\tfield2\n`,  % 15 chars
    phrase(parse_line(0, 1, _, Offset), Input).

test(error_position_reported, [true(Pos == pos(5, 120))]) :-
    phrase(parse_with_error_reporting(120, 5, error(Pos, _)), `bad input`).

:- end_tests(parser_positions).
```

### Pattern: Testing Complex Grammars

```prolog
:- begin_tests(c_grammar).

test(c_identifier_valid, [true(Id == foo_bar123)]) :-
    phrase(c_identifier(Id), `foo_bar123`).

test(c_identifier_starts_underscore, [true(Id == '_private')]) :-
    phrase(c_identifier(Id), `_private`).

test(c_identifier_no_digit_start, [fail]) :-
    phrase(c_identifier(_), `123foo`).

test(global_definition_macro, [true((Sym == MY_MACRO, Kind == macro))]) :-
    phrase(global_definition(Sym, Kind), `#define MY_MACRO 42`).

test(global_definition_typedef_struct, [true((Sym == MyStruct, Kind == typedef))]) :-
    phrase(global_definition(Sym, Kind), `typedef struct MyStruct {`).

:- end_tests(c_grammar).
```

### Pattern: Property-Based Testing

```prolog
:- begin_tests(parser_properties).

% Roundtrip: parse then unparse should give original
test(roundtrip_tsv_line, [forall(gen_tsv_line(Input))]) :-
    phrase(tsv_line(Fields), Input),
    phrase(unparse_tsv_line(Fields), Output),
    Input == Output.

% Helper to generate test inputs
gen_tsv_line(`a\tb\tc\n`).
gen_tsv_line(`field1\tfield2\n`).
gen_tsv_line(`x\ty\tz\tw\n`).

:- end_tests(parser_properties).
```

### Running Tests

```prolog
% Run all tests
?- run_tests.

% Run specific test suite
?- run_tests(parser).

% Run with verbose output
?- run_tests(parser, [verbose(true)]).

% Run and show only failures
?- run_tests([silent(true)]).
```

### Pattern: Test Helpers

```prolog
% test/prolog/test_helpers.pl
:- module(test_helpers, [
    codes_from_string/2,
    must_parse/3,
    must_fail_parse/2
]).

% Convert string to codes for testing
codes_from_string(String, StringCodes) :-
    string_codes(String, StringCodes).

% Assert that parsing succeeds
must_parse(Grammar, Input, Expected) :-
    phrase(Grammar, Input, Rest),
    assertion(Rest == []),
    assertion(Expected).

% Assert that parsing fails
must_fail_parse(Grammar, Input) :-
    \+ phrase(Grammar, Input).
```

### Test Coverage Goals

For each DCG rule, ensure tests cover:
- ✅ **Happy path**: Valid input parsing correctly
- ✅ **Edge cases**: Empty input, single element, boundary conditions
- ✅ **Invalid input**: Graceful failure or error reporting
- ✅ **Position tracking**: Correct offset and line number reporting
- ✅ **Whitespace handling**: Different whitespace combinations
- ✅ **Line endings**: `\n`, `\r\n`, `\r`, no ending
- ✅ **Large inputs**: Performance with realistic data sizes

## Performance Considerations

### ✅ Efficient Patterns

```prolog
% Tail recursive - good
parse_many([X|Xs]) -->
    parse_one(X), !,
    parse_many(Xs).
parse_many([]) --> [].

% Cut after successful parse - good
parse_field(F) -->
    identifier(F), !.
parse_field(default) --> [].

% Character class checking - good
is_digit(C) :- code_type(C, digit).
```

### ❌ Inefficient Patterns

```prolog
% Non-tail recursive - avoid
parse_many([X|Xs]) -->
    parse_one(X),
    parse_many(Xs), !.  % ! too late

% No cut, excessive backtracking - avoid
parse_field(F) -->
    (
      identifier(F)
    ;
      default_field(F)
    ).

% Repeated conversions - avoid
parse_field(F) -->
    codes(Cs),
    { atom_codes(Temp, Cs),
      atom_string(Temp, F) }.  % Why convert twice?
```

## Real-World Example: Cscope Output

### Before: Mixed String/DCG Processing

```prolog
% AVOID: Multiple passes
process_cscope_output(Stream, Results) :-
    read_string(Stream, _, Content),              % Pass 1
    split_string(Content, "\n", "\r", Lines),     % Pass 2
    maplist(parse_cscope_line, Lines, Results).   % Pass 3

parse_cscope_line(Line, result(File, Sym, Num, Context)) :-
    split_string(Line, " ", "", Parts),           % Pass 4
    Parts = [File, Sym, Num | ContextParts],
    atomics_to_string(ContextParts, " ", Context). % Pass 5
```

### After: Pure DCG

```prolog
% PREFER: Single pass
process_cscope_output(Stream, Results) :-
    phrase_from_stream(cscope_lines(Results), Stream).

cscope_lines([Line|Lines]) -->
    cscope_line(Line), !,
    cscope_lines(Lines).
cscope_lines([]) --> eos.

cscope_line(result(File, Sym, Num, Context)) -->
    space_field(File), ` `,
    space_field(Sym), ` `,
    space_field(Num), ` `,
    rest_of_line(Context).

space_field(Field) -->
    non_space_codes(FieldCodes),
    { atom_codes(Field, FieldCodes) }.

non_space_codes([C|Cs]) -->
    [C], { C \= 32, C \= 10, C \= 13 }, !,  % Not space, LF, CR
    non_space_codes(Cs).
non_space_codes([]) --> [].
```

## Common Mistakes to Avoid

### 1. Converting to String and Back

```prolog
% ❌ AVOID
field(F) -->
    codes(FieldCodes),
    { atom_codes(A, FieldCodes),
      atom_string(A, S),
      string_codes(S, NewFieldCodes),
      ... }.

% ✅ PREFER
field(F) -->
    codes(FieldCodes),
    { atom_codes(F, FieldCodes) }.
```

### 2. Using split_string Before DCG

```prolog
% ❌ AVOID
parse_input(Input, Result) :-
    split_string(Input, "\n", "", Lines),
    phrase(parse_lines(Result), Lines).  % Lines are strings, not codes!

% ✅ PREFER
parse_input(Input, Result) :-
    string_codes(Input, Codes),
    phrase(parse_lines(Result), Codes).
```

### 3. Mixing Paradigms

```prolog
% ❌ AVOID - mixing string ops with DCG
parse_line(Line, Result) -->
    { split_string(Line, "\t", "", [F1, F2]) },  % String op in DCG
    parse_field(F2, Result).

% ✅ PREFER - pure DCG
parse_line(Result) -->
    field(F1), `\t`,
    parse_field(Result).
```

## Debugging Support for Generated Code

When generating DCG parsers, include debugging support to help users diagnose issues.

### Pattern: Inserting gtrace for Interactive Debugging

**IMPORTANT**: When users report parse errors, suggest inserting `gtrace/0` at strategic points. Note that GUI debugger requires launching SWI-Prolog with GUI support.

```prolog
% Insert gtrace before suspected error location
parse_problematic_line(Result, Offset, Line) -->
    field(F1),
    { gtrace },  % Drop into debugger here
    `\t`,
    field(F2),
    { Result = data(F1, F2) }.

% Conditional debugging based on position
parse_line(Result, Offset, Line) -->
    { (Line == 42 -> gtrace ; true) },  % Debug only line 42
    field(F1), `\t`,
    field(F2),
    { Result = data(F1, F2) }.

% Conditional debugging on specific values
parse_field(Field, Offset, Line) -->
    field_codes(FieldCodes),
    { atom_codes(Field, FieldCodes),
      (Field == problematic_symbol -> gtrace ; true) },  % Debug specific symbol
    ...
```

**To suggest to users:**
- On Windows: Launch with `"C:\Program Files\swipl\bin\swipl-win.exe" --win_app`
- Insert `gtrace` before suspected error location in code
- Use conditional gtrace for specific lines/values
- Remove gtrace once issue is identified

### Pattern: Understanding DCG Expansion

**IMPORTANT**: When helping users debug DCGs, show the expanded Prolog code to clarify what the DCG actually does.

```prolog
% DCG notation - what user writes
field(Field) -->
    field_codes(FieldCodes),
    { atom_codes(Field, FieldCodes) }.

field_codes([C|Cs]) -->
    [C], { C \= 9 }, !,  % Not tab
    field_codes(Cs).
field_codes([]) --> [].

% Expanded Prolog - what actually runs
field(Field, S0, S) :-
    field_codes(FieldCodes, S0, S1),
    atom_codes(Field, FieldCodes),
    S1 = S.

field_codes([C|Cs], [C|S0], S) :-
    C \= 9, !,  % Not tab
    field_codes(Cs, S0, S).
field_codes([], S, S).
```

**To show users:**
- DCG rule `-->` becomes regular predicate with 2 extra arguments
- Input/output difference list threaded through: `S0` (input), `S` (output)
- Terminal `[C]` matches and consumes from input list
- Goals in `{...}` are regular Prolog, don't touch difference list
- Understanding expansion helps debug unexpected behavior

**Use `listing/1` to see actual expanded code:**
```prolog
?- listing(field/3).
?- listing(field_codes/3).
```

### Common DCG Pitfalls to Watch For

**1. Forgetting Cut in Recursive Rules**
```prolog
% ❌ BAD: No cut, creates choice points
parse_many([X|Xs]) -->
    parse_one(X),
    parse_many(Xs).
parse_many([]) --> [].

% ✅ GOOD: Cut after successful parse
parse_many([X|Xs]) -->
    parse_one(X), !,
    parse_many(Xs).
parse_many([]) --> [].
```

**2. Using Regular Prolog Unification on Difference List**
```prolog
% ❌ BAD: Tries to unify S0 = [C|S1] incorrectly
bad_consume(C, [C|S1], S1).  % Not a DCG!

% ✅ GOOD: Use DCG notation
good_consume(C) --> [C].
```

**3. Not Consuming Input in Escape Hatches**
```prolog
% ❌ BAD: Escapes DCG but needs to consume
parse_item(Item) -->
    { parse_complex(Item) }.  % Doesn't consume input!

% ✅ GOOD: Explicitly manage difference list
parse_item(Item, S0, S) :-
    parse_complex(Item, S0, S).  % Or use DCG inside
```

**4. Mixing String Operations with DCG**
```prolog
% ❌ BAD: Splitting string then trying to parse
parse_line(Result, Line) -->
    { split_string(Line, "\t", "", Parts) },  % Wrong!
    ...

% ✅ GOOD: Parse delimiters in DCG
parse_line(Result) -->
    field(F1), `\t`,
    field(F2).
```

**5. Accumulator Position Wrong**
```prolog
% ❌ BAD: Accumulator not at end
parse_line(SeenIn, SeenOut, Result) -->  % Wrong position
    ...

% ✅ GOOD: Accumulators at end, in/out pairs
parse_line(Result, SeenIn, SeenOut) -->  % Correct position
    ...
```

**6. Not Threading Position Through All Rules**
```prolog
% ❌ BAD: Position tracking lost
parse_line(Record, LineIn, LineOut) -->
    parse_fields(Fields),  % Lost offset tracking!
    { Record = record(Fields, LineIn) }.

% ✅ GOOD: Thread position through
parse_line(Record, LineIn, LineOut, OffsetIn, OffsetOut) -->
    parse_fields(Fields, LineIn, LineIn, OffsetIn, OffsetOut),
    { Record = record(Fields, pos(LineIn, OffsetIn)),
      succ(LineIn, LineOut) }.
```

**7. Building Lists Backward Without Reverse**
```prolog
% ❌ BAD: Build list backward, then need reverse/2 at higher level
parse_file(Items) -->
    parse_items_backward([], ItemsBackward),
    { reverse(ItemsBackward, Items) }.  % Need reverse!

parse_items_backward(Acc, Result) -->
    parse_item(Item), !,
    parse_items_backward([Item|Acc], Result).  % Builds backward
parse_items_backward(Items, Items) --> [].

% ✅ GOOD: Build list forward with [Item|Items] - no reverse needed
parse_file(Items) -->
    parse_items(Items).

parse_items([Item|Items]) -->
    parse_item(Item), !,
    parse_items(Items).
parse_items([]) --> [].
```

**8. EOS Check Not at End**
```prolog
% ❌ BAD: Checks eos too early
parse_file([L|Ls]) -->
    eos, !,  % Wrong! Checks before attempting parse
    line(L),
    parse_file(Ls).

% ✅ GOOD: EOS check at base case
parse_file([L|Ls]) -->
    line(L), !,
    parse_file(Ls).
parse_file([]) --> eos.
```

### Debugging Strategy When Helping Users

1. **Ask for error message and position**: What line/offset failed?
2. **Show expanded DCG code**: Help user understand what DCG does
3. **Suggest gtrace insertion**: Add gtrace before error position
4. **Check common pitfalls**: Review list above
5. **Verify position threading**: Ensure offsets/lines thread correctly
6. **Test with phrase/3**: Use remainder to see where parse stopped
7. **Add debug/3 calls**: Strategic debug output for parse trace

## Summary Checklist

When writing DCG parsers, ensure:

### Parsing
- ✅ Use `phrase_from_file/2` or `phrase/2` with codes, not intermediate string splits
- ✅ Parse line endings within DCG rules
- ✅ Parse field delimiters within DCG rules
- ✅ Keep all parsing logic in DCG rules
- ✅ Use cuts appropriately to prevent backtracking
- ✅ Use tail recursion for lists
- ✅ Convert to atoms/strings only at the end of parsing
- ❌ Don't split strings before parsing with DCG
- ❌ Don't convert between string/atom/codes multiple times
- ❌ Don't mix string operations with DCG rules
- ❌ Don't parse in multiple passes when single pass is possible
- ❌ Don't use library(pcre) - use pure DCG rules instead of regex
- ❌ Don't use library(dcg/basics) - write explicit DCG rules

### Accumulators
- ✅ **MUST** use in/out pairs for accumulators: `parse(..., Acc0, Acc)`
- ✅ **MUST** place accumulator pairs at end of argument list
- ✅ **MUST** keep accumulator pairs together: `Line0, Line, Offset0, Offset`
- ✅ **MUST** use consistent ordering: in first, out second
- ✅ **MUST** use naming convention: input ends in `0`, output has no number
- ✅ **MUST** increment intermediate variables: `Acc0 → Acc1 → Acc`
- ❌ Don't use single accumulator values passed back
- ❌ Don't mix accumulators with other variables
- ❌ Don't separate accumulator pairs

### Naming Conventions
- ✅ **PREFER** quantifier suffixes: `'sign?'` (optional), `'digits*'` (zero+), `'digits+'` (one+)
- ✅ Quote predicate names with special suffixes: `'name?'`, `'name*'`, `'name+'`
- ✅ Accumulator input: name ending in `0` (e.g., `Seen0`, `Line0`)
- ✅ Accumulator output: name without number (e.g., `Seen`, `Line`)
- ✅ Intermediate accumulators: increment number (e.g., `Seen1`, `Seen2`)
- ✅ **PREFER** Head|Tail naming:
  - Descriptive names: singular/plural pairs `[Item|Items]`, `[Record|Records]`, `[Line|Lines]`
  - Single letters: letter with 's' suffix `[X|Xs]`, `[C|Cs]`, `[D|Ds]`
- ❌ Don't use generic words like `[Head|Tail]`, `[H|T]` (unless single letter)

### DCG Syntax
- ✅ **IMPORTANT**: Know Prolog flags - `back_quotes` (default: codes), `double_quotes` (default: string)
- ✅ In DCG, `"abc"` is string but does not matches code lists, `` `abc` `` is code list directly
- ✅ Use `'abc'` (single quotes) to create atoms
- ✅ Use `0'X` notation for single character codes (e.g., `0'a` = 97, `0'\t` = 9)
- ✅ Use numeric codes `[9]` with comments (e.g., `[9]  % ASCII tab`)
- ✅ Parse as codes in DCG, convert to atoms/strings at the end
- ✅ **Format disjunctions**: Put semicolon `;` on its own line at the beginning for visibility

### Code Organization
- ✅ **IMPORTANT**: Order DCG rules from highest level to lowest level (top-down reading)
- ✅ File-level parsing rules at the top (e.g., `parse_file//1`)
- ✅ Line/record parsing rules next (e.g., `parse_line//1`, `parse_record//1`)
- ✅ Field/token parsing rules in the middle (e.g., `parse_field//1`, `identifier//1`)
- ✅ Character primitives at the bottom (e.g., `tab//2`, `cr//2`, `lf//2`, `digit//1`)
- ✅ This creates a readable narrative: structure first, details last
- ❌ Don't mix high-level and low-level rules together
- ❌ Don't prioritize execution order over readability (Prolog doesn't require forward declarations)

### DCG Composition
- ✅ **IMPORTANT**: Create character-level DCG primitives (e.g., `tab//2`, `cr//2`, `lf//2`)
- ✅ Each primitive handles ONE character and its position increment with `succ/2`
- ✅ Compose primitives into complex patterns (e.g., `line_end//2` from `cr//2` + `lf//2`)
- ✅ Reuse primitives across different parsing contexts (TSV, CSV, etc.)
- ✅ Name primitives after the character: `tab`, `cr`, `lf`, not `tab_char`, `cr_byte`
- ❌ Don't inline character matching in complex patterns
- ❌ Don't duplicate position tracking logic across similar patterns

### Character Classification
- ✅ **PREFER** `code_type/2` for single character classification when possible
- ✅ Define specific character checks: `is_tab(9)`, `is_newline(10)`

### Clause Indexing
- ✅ **PREFER** just-in-time clause indexing when it makes sense
- ✅ Order clauses with most specific cases first
- ✅ Take advantage of automatic indexing on first argument
- ✅ Use deterministic cuts after specific patterns

### Lookahead and Peek
- ✅ **IMPORTANT**: Check invocation chain before adding peek - does the called predicate already check the first character?
- ✅ **PREFER**: Natural DCG alternatives over peek - let predicates fail naturally and backtrack
- ✅ **IMPORTANT**: Ensure peek operations check bounds to avoid accessing beyond input end
- ✅ Simple peek `peek(C) --> [C], [C]` is safe (fails gracefully)
- ✅ Multi-character peek must handle short input: `peek_chars(_N, []) --> []`
- ✅ Add cut after successful peek to commit: `[C], !, peek_chars(N1, Cs)`
- ✅ Peek operations should return as many characters as available without error
- ❌ Don't use peek when called predicate already checks the first character
- ❌ Don't assume N characters are available when peeking N

### Position Tracking
- ✅ **IMPORTANT**: Position accumulators go at END of argument list
- ✅ **MUST** use ordering: `Line0, Line, Offset0, Offset` (Line before Offset)
- ✅ **MUST** use incremental variables: `Line0, Line1, Line2...` NOT `Line, Line`
- ✅ **Line increments by 1** when line ending encountered
- ✅ **Offset increments by 1** for EACH character consumed (character-level precision)
- ✅ **Every DCG rule** threads position through, even single-character parsers
- ✅ Invaluable for debugging large files (can strip for production)
- ❌ Don't use `Line, Line` - assumes Line never changes, creates brittle code
- ❌ Don't increment offset by field length - increment by 1 per character
- ❌ Don't put position accumulators at beginning of argument list

### List Building
- ✅ **PREFER** DCG forward accumulation to avoid using `reverse/2`
- ✅ **PREFER** DCG forward accumulation instead of `append/3` for O(n) building
- ✅ Build lists forward using DCG pattern: `[Item|Items]`
- ✅ DCGs naturally build lists forward - use this
- ✅ Use `memberchk/2` instead of `member/2` when applicable (deterministic)
- ❌ Don't build lists backward then reverse
- ❌ Don't use `append/3` in recursive list building (O(n²))
- ❌ Don't use `member/2` when you don't need backtracking

### Arithmetic
- ✅ **PREFER** `succ/2` for simple +1 increments: `succ(N0, N1)`
- ✅ **PREFER** `succ/2` for -1 decrements: `succ(N1, N0)` (reversed arguments)
- ✅ **IMPORTANT**: Break multi-character sequences (like `\r\n`) into individual character increments with `succ/2` to avoid manual counting errors
- ✅ Use `is/2` for complex arithmetic: `N is N0 + Length + 1`
- ✅ Use `is/2` when incrementing by values other than 1
- ❌ Don't use `is/2` for simple +1: use `succ/2` instead
- ❌ Don't manually count characters in multi-character sequences: break down into single increments

### Error Handling
- ✅ **PREFER** `library(error)` for throwing structured errors
- ✅ Use `must_be/2` for type validation
- ✅ Use standard error terms: `type_error/2`, `domain_error/2`, `existence_error/2`
- ✅ Include position context in error terms

### File I/O
- ✅ **PREFER** `catch/3` when reading files
- ✅ Wrap `phrase_from_file/2` in catch/3 for error handling
- ✅ Handle specific errors: `existence_error`, `permission_error`
- ✅ Use `call_cleanup/2` for explicit stream management

### Testing
- ✅ **MUST** write unit tests for all DCG clauses using plunit
- ✅ **MUST** place tests in separate test files (test/prolog/test_*.pl)
- ✅ Test happy path, edge cases, invalid input, and large inputs
- ✅ Test different line endings: `\n`, `\r\n`, `\r`, no ending
- ✅ Use `phrase/2` for incremental testing during development
- ✅ Run tests frequently: `?- run_tests.`

### Debugging
- ✅ **MUST** track character offset and line number for error locations
- ✅ **MUST** use `library(debug)` with debug topics (e.g., `debug(parse)`)
- ✅ Include position information (line, offset) in parsed results
- ✅ Report parse errors with exact position: `error(pos(Line, Offset), Context)`
- ✅ Enable/disable debug output without changing code: `?- debug(parse).`
- ✅ For large files (megabytes), positions enable grepping to problem location

## References

### Documentation
- SWI-Prolog DCG documentation: https://www.swi-prolog.org/pldoc/man?section=DCG
- phrase_from_file/2: Direct file parsing with DCGs
- Prolog Unit Tests (plunit): https://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/plunit.html%27)
- library(debug): https://www.swi-prolog.org/pldoc/man?section=debug
- library(error): https://www.swi-prolog.org/pldoc/man?section=error
- code_type/2: https://www.swi-prolog.org/pldoc/man?predicate=code_type/2

### Best Practices
- Performance: Single-pass parsing is both clearer and faster
- Testing: All DCG clauses must have unit tests
- Debugging: Track positions for error reporting in large files
- Error Handling: Use structured error terms with position context
- Accumulators: In/out pairs at end of argument list

## Libraries to Use

- ✅ **library(debug)**: For debugging with toggleable debug topics
- ✅ **library(error)**: For throwing structured, standardized errors (`must_be/2`, error terms)
- ✅ **Prolog Unit Tests (plunit)**: For testing DCG clauses

## Libraries NOT to Use

- ❌ **library(pcre)**: Use pure DCG rules, not regular expressions
- ❌ **library(dcg/basics)**: Write explicit DCG rules for full control and clarity

## Key Preferences Summary

1. **Character Classification**: Use `code_type/2` when possible
2. **Clause Indexing**: Prefer just-in-time clause indexing when it makes sense
3. **List Building**: Use DCG forward accumulation to avoid `reverse/2` and `append/3`
4. **Membership Testing**: Use `memberchk/2` instead of `member/2` when deterministic
5. **Arithmetic**: Use `succ/2` for simple +1/-1 operations; use `is/2` for complex arithmetic
6. **Equality in Guards**: Use `==/2` and `\==/2` for testing values in conditionals; use `=/2` only for actual unification/binding; avoid LLM training data errors where `=/2` is used in guards
7. **Accumulator Naming**: Input ends in `0`, output has no number (e.g., `Acc0, Acc`)
8. **Quantifier Naming**: Use `'name?'` (optional), `'name*'` (zero+), `'name+'` (one+)
9. **Head|Tail Naming**: Descriptive names use singular/plural (e.g., `[Item|Items]`), single letters use 's' suffix (e.g., `[X|Xs]`)
10. **DCG String Syntax**: Know Prolog flags (`back_quotes`=codes, `double_quotes`=string); use `` `abc` `` for codes, `"abc"` for strings (matches codes in DCG), `'abc'` for atoms; comment numeric codes `[9]  % ASCII tab`
11. **Disjunction Formatting**: Put semicolon `;` on its own line at the beginning for visibility
12. **Peek Usage**: Check invocation chain before adding peek - most parsing predicates self-check first character; prefer natural DCG alternatives; only use peek when lookahead is needed without consumption; ensure peek operations check bounds and return available characters without error; multi-char peek must handle short input with `peek_chars(_N, []) --> []`
13. **Position Tracking**: Position accumulators at END in order `Line0, Line, Offset0, Offset`; use incremental variables `Line0, Line1, Line2...` NOT `Line, Line`; use `succ/2` for incrementing; thread through every DCG rule
14. **Error Handling**: Use `library(error)` with structured error terms
15. **File I/O**: Use `catch/3` to wrap file operations
16. **Accumulators**: Use in/out pairs at end of argument list
17. **Testing**: Write plunit tests for all DCG clauses in separate files
18. **Debugging**: Track line and character offset, use `library(debug)`
