# Basic DCG Patterns

Core DCG parsing patterns for everyday use. See [../SKILL.md](../SKILL.md) for core philosophy and [index.md](index.md) for all pattern categories.

## Pattern: Tab-Separated Fields

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

## Pattern: C Identifiers

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

## Pattern: Partial Consumption with phrase/3

### The Problem

DCG rules often match prefixes rather than complete input. Using `phrase/2` with such rules causes unexpected failures because `phrase/2` requires consuming the entire input.

### The Solution

Use `phrase/3` to allow unconsumed remainder:

```prolog
% phrase/3 returns remaining unconsumed input in third argument
phrase(Grammar, Input, Remainder)
```

### Example: Parsing C Macro Definitions

```prolog
% DCG that matches "#define SYMBOL" but stops after identifier
% Does not consume the macro value
macro_definition(Symbol) -->
    'whites*', `#define`, 'whites+',
    c_identifier(Symbol).

% Test with phrase/3 - allows remainder
test(macro_with_value) :-
    string_codes("#define MAX_SIZE 1024", Codes),
    phrase(macro_definition(Symbol), Codes, Rest),
    Symbol == 'MAX_SIZE',
    string_codes(" 1024", Rest).  % Remainder not consumed

% Test can also ignore remainder
test(macro_simple) :-
    string_codes("#define MAX_SIZE", Codes),
    phrase(macro_definition(Symbol), Codes, _Rest),
    Symbol == 'MAX_SIZE'.
```

### When to Use phrase/3

Use `phrase/3` when:

✅ DCG matches a prefix pattern (e.g., `#define SYMBOL` without caring about rest)
✅ Testing that DCG stops at correct boundary
✅ Multiple DCG rules will process different parts sequentially
✅ You want to verify what remains after parsing

### When to Use phrase/2

Use `phrase/2` when:

✅ DCG should consume entire input
✅ Validating complete structure (e.g., complete line with line ending)
✅ Parsing a complete token or record
✅ Any remainder indicates an error

### Pattern: Testing Boundaries

Explicitly test where parsing stops:

```prolog
test(stops_at_space) :-
    string_codes("identifier next_token", Codes),
    phrase(c_identifier(Id), Codes, Rest),
    Id == identifier,
    string_codes(" next_token", Rest).  % Verify boundary

test(stops_at_paren) :-
    string_codes("func(args)", Codes),
    phrase(c_identifier(Id), Codes, Rest),
    Id == func,
    string_codes("(args)", Rest).

test(stops_at_semicolon) :-
    string_codes("variable;", Codes),
    phrase(c_identifier(Id), Codes, Rest),
    Id == variable,
    Rest == [0';].
```

### Common Mistake

```prolog
% WRONG - phrase/2 expects complete consumption
test(partial_parse) :-
    string_codes("#define MAX 100", Codes),
    phrase(macro_definition(S), Codes).  % FAILS - " 100" unconsumed

% RIGHT - phrase/3 allows remainder
test(partial_parse) :-
    string_codes("#define MAX 100", Codes),
    phrase(macro_definition(S), Codes, _).  % SUCCEEDS
```

### Real-World Usage

This pattern is essential for:
- Parsing file formats where each line has optional trailing data
- Implementing prefix matchers (keywords, identifiers, operators)
- Building parsers that compose multiple DCG rules sequentially
- Testing individual DCG components in isolation

## Pattern: Quantifier Naming Conventions

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

## Pattern: DCG String Syntax

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

## Pattern: Disjunction Formatting

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

## Pattern: Conditional Control Flow - Multiple Clauses vs If-Then-Else

**RECOMMENDATION**: Prefer multiple clauses with guards over if-then-else (`->` with `;`) for conditional logic in DCGs.

### ✅ PREFER: Multiple Clauses with Guards

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

### ❌ AVOID: If-Then-Else (Use Sparingly)

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

### When to Use Multiple Clauses

**Reasons to prefer multiple clauses:**

1. **More Idiomatic**: Leverages Prolog's core strength - pattern matching and unification. Follows declarative style (defining what, not how).

2. **Easier to Extend**: Adding new cases is simple - just add another clause. No need to modify nested conditionals.

3. **Better Readability**: Each case is clearly separated. Easier to understand at a glance which patterns are handled.

4. **Simpler Debugging**: Easier to trace which clause matched. Can comment out individual clauses for testing.

5. **Less Error-Prone**: Changes to one case don't affect others. No risk of forgetting the `;` between alternatives.

### When to Use If-Then-Else

Use `->` with `;` only when:
- You need **guaranteed determinism** without explicit cuts
- The logic is **truly binary** and won't need extension
- **Performance is critical** and you want to avoid choice points (though cuts in clauses achieve similar effect)

**Note**: The `->` operator commits once the condition succeeds (acts like a cut), eliminating backtracking to alternative branches. However, multiple clauses with strategic cuts are equally deterministic and more maintainable.

## Pattern: Equality Operators in Conditionals and Guards

**CRITICAL**: Use comparison operators (`==/2`, `\==/2`) instead of unification operators (`=/2`, `\=/2`) when testing values in conditionals or guards.

This is a common mistake in LLM-generated code and legacy code, where unification is used when comparison is intended.

### Understanding the Operators

**Unification operators** (can bind variables):
- `=/2`: Unification - makes two terms equal by binding variables
- `\=/2`: Not unifiable - succeeds if terms cannot be unified

**Comparison operators** (no binding, just testing):
- `==/2`: Term equality - checks if terms are structurally identical (no binding)
- `\==/2`: Term inequality - checks if terms are not structurally identical (no binding)
- `=:=/2`: Arithmetic equality - evaluates arithmetic expressions and compares values
- `=\=/2`: Arithmetic inequality - evaluates and compares arithmetic values

### ❌ AVOID: Using =/2 in Conditionals (Common LLM Training Data Error)

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

### ✅ PREFER: Using ==/2 for Value Testing

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

### Guidelines for Choosing the Right Operator

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

### Common Patterns

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

### Summary

- **In guards/conditionals**: Use `==/2` and `\==/2` for testing
- **For binding/construction**: Use `=/2`
- **For arithmetic**: Use `=:=/2` and `=\=/2`
- **Watch for LLM-generated code**: Often incorrectly uses `=/2` in guards

## Pattern: Rest of Line

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

## Pattern: String/Atom/Codes Conversions

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

## Pattern: Partial Parsing with phrase/3

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

## Pattern: Lookahead and Backtracking Control

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

## Pattern: Bounds Checking for Peek Operations

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

## Pattern: Avoiding Redundant Peek - Check the Invocation Chain

**IMPORTANT**: Before adding `peek`, check if the called predicate already performs the same check. Most unnecessary `peek` calls can be eliminated by examining the invocation chain.

### The Anti-Pattern: Redundant Peek

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

### The Solution: Natural DCG Alternatives

```prolog
% ✅ PREFER: Let the called predicate self-determine applicability
value_dl(Out-OutRest) -->
    'digits_dl+'(Out-OutRest), !.  % Tries first, fails naturally if not digit
value_dl(Out-OutRest) -->
    identifier_dl(Out-OutRest).    % Falls back automatically

% No peek needed! digits_dl+ checks the first character itself.
% If it's not a digit, it fails and we try identifier_dl.
```

### The "Check Before Peek" Decision Tree

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

### When Peek IS Necessary

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

### Red Flag Detection Rule

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

### Guidelines

- Default to natural DCG alternatives - let predicates fail naturally
- Add `peek` only when you have a clear reason from the "When Peek IS Necessary" list
- Always check the invocation chain before adding peek
- Most parsing predicates self-check their applicability - trust them
- Use explicit cuts after successful alternatives if determinism is needed

## See Also

- [accumulators.md](accumulators.md) - Accumulator-based parsing patterns
- [difference-lists.md](difference-lists.md) - Efficient list building and transformation
- [position-tracking.md](position-tracking.md) - Position tracking for debugging
- [optimization.md](optimization.md) - Performance patterns
- [file-io.md](file-io.md) - File I/O integration