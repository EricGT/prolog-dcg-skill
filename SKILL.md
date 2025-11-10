---
name: dcg-parsing
description: Guide Claude in writing efficient, idiomatic SWI-Prolog DCGs (Definite Clause Grammars) following best practices for single-pass parsing, character codes, pure declarative style, and accumulator patterns. Use when working with Prolog parsing tasks.
---

# DCG Parsing Skill

## Purpose
Guide Claude in writing efficient, idiomatic Prolog DCGs (Definite Clause Grammars) following best practices for single-pass parsing and pure declarative style.

**See [EXAMPLES.md](EXAMPLES.md) for a comprehensive demonstration of all patterns working together.**

## Scope and Disclaimer

**IMPORTANT**: This skill document is tailored specifically to **SWI-Prolog** and represents the **creator's preferences** for DCG parsing patterns. There are many valid approaches to parsing with DCGs, and this is not the only way.

The patterns and conventions documented here reflect:
- Specific SWI-Prolog predicates and features
- Preferences for code style and structure
- Trade-offs chosen for this project's specific needs

When applying these patterns, consider your project's requirements and constraints. What works well for one use case may not be optimal for another.

## Additional Resources

This skill is organized into multiple files for progressive disclosure:

- **[PATTERNS.md](PATTERNS.md)** - Comprehensive DCG patterns library (30+ patterns: tab-separated fields, C identifiers, accumulators, position tracking, etc.)
- **[ERROR_HANDLING.md](ERROR_HANDLING.md)** - Error handling and debugging strategies with position tracking
- **[EXAMPLES.md](EXAMPLES.md)** - Complete integrated examples and real-world use cases
- **[TESTING.md](TESTING.md)** - Testing strategies and debugging techniques
- **[REFERENCE.md](REFERENCE.md)** - Performance considerations, common mistakes, and detailed references

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
    'whites*',
    identifier(X),
    'whites*', `(`,
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

## Most Common Patterns

For quick reference, here are the most frequently used patterns. For comprehensive patterns, see [PATTERNS.md](PATTERNS.md).

### Tab-Separated Fields

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

### C Identifiers

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

### Accumulator-Based Parsing

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

```prolog
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
      Seen = [Name|Seen0] }.
```

## Summary Checklist

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
6. **Accumulator Naming**: Input ends in `0`, output has no number (e.g., `Acc0, Acc`)
7. **Position Tracking**: Position accumulators at END in order `Line0, Line, Offset0, Offset`
8. **Error Handling**: Use `library(error)` with structured error terms
9. **File I/O**: Use `catch/3` to wrap file operations
10. **Testing**: Write plunit tests for all DCG clauses in separate files
11. **Debugging**: Track line and character offset, use `library(debug)`

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
