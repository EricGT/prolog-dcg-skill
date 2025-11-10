# DCG Reference

Advanced topics including performance optimization, common mistakes, and debugging. See [SKILL.md](SKILL.md) for core concepts.

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

## Additional Resources

For comprehensive documentation including:
- All DCG patterns (30+ patterns)
- Position tracking details
- File I/O integration
- Complete examples

See [PATTERNS.md](PATTERNS.md), [EXAMPLES.md](EXAMPLES.md), [TESTING.md](TESTING.md), and [ERROR_HANDLING.md](ERROR_HANDLING.md).
