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
    read_file_to_string(File, Content, []),          % Pass 1
    split_string(Content, "\n", "\r", Lines),        % Pass 2
    maplist(process_line, Lines, Results).           % Pass 3
```

**Problems:** 6 passes, mixes string operations with DCGs, inefficient

### ✅ PREFER: Pure DCG Single-Pass

```prolog
% Single pass - PREFER THIS
parse_file(File, Result) :-
    phrase_from_file(file_content(Result), File).

file_content([Line|Lines]) --> file_line(Line), !, file_content(Lines).
file_content([]) --> eos.

file_line(data(F1, F2, Data)) -->
    field(F1), `\t`, field(F2), `\t`, parse_field(Data), line_end.
```

**Benefits:** Single pass, clear grammar, efficient, easy to debug

**See [PATTERNS.md](PATTERNS.md) for detailed examples and variations**

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

### ⚠️ WARNING: code_type/2 and Unicode

**IMPORTANT**: `code_type(C, alpha)` accepts **Unicode letters** (ü, é, ñ, etc.), not just ASCII letters (a-z, A-Z).

This is often **not** what you want when parsing programming language identifiers (C, Java, classic languages).

#### The Problem

```prolog
% Accepts Unicode - probably wrong for C/Java/etc.
c_identifier_char(C) --> [C], { code_type(C, alpha) }.

% This will SUCCEED but shouldn't for C identifiers!
?- string_codes("über", Codes), phrase(c_identifier_char(_), Codes, _).
true.  % 'ü' is accepted as alpha - WRONG for C!
```

#### Solutions by Use Case

**For ASCII-Only Languages (C, Java, classic languages):**

Use explicit ASCII range checks:

```prolog
% C identifier - ASCII letters only
ascii_letter(C) -->
    [C],
    {
        (C >= 0'a, C =< 0'z)
    ;   (C >= 0'A, C =< 0'Z)
    }.

% C identifier start character
c_id_start(C) -->
    [C],
    {
        (C >= 0'a, C =< 0'z)
    ;   (C >= 0'A, C =< 0'Z)
    ;   C = 0'_
    }.

% C identifier continuation character
c_id_rest(C) -->
    [C],
    {
        (C >= 0'a, C =< 0'z)
    ;   (C >= 0'A, C =< 0'Z)
    ;   (C >= 0'0, C =< 0'9)
    ;   C = 0'_
    }.
```

**For Unicode-Accepting Contexts:**

Use `code_type/2` with awareness:

```prolog
% Natural language processing - Unicode is fine
word_char(C) --> [C], { code_type(C, alpha) }.

% Modern language identifiers (Rust, Swift allow Unicode)
modern_identifier_char(C) --> [C], { code_type(C, alnum) }.
```

**For Hybrid Approach:**

Combine `code_type/2` with ASCII restriction:

```prolog
% Use code_type but restrict to ASCII range
ascii_alpha(C) -->
    [C],
    {
        code_type(C, alpha),
        C < 128  % ASCII only
    }.
```

#### When to Use Each Approach

**Use explicit ASCII checks when:**
- ✅ Parsing C, Java, or classic programming languages
- ✅ Implementing strict specifications (ANSI C, POSIX, etc.)
- ✅ Ensuring cross-platform portability
- ✅ Matching legacy tooling behavior (cscope, ctags, etc.)

**Use code_type/2 when:**
- ✅ Parsing natural language text
- ✅ Accepting international identifiers (modern languages)
- ✅ Processing human-readable content
- ✅ Implementing Unicode-aware specifications

#### Testing Recommendation

Always include a Unicode rejection test for ASCII-only parsers:

```prolog
:- begin_tests(identifier_unicode).

% Verify non-ASCII is rejected for C identifiers
test(unicode_rejected, [fail]) :-
    string_codes("über", Codes),
    phrase(c_identifier(_), Codes, _).

test(ascii_only_accepted, [true(Id == valid)]) :-
    string_codes("valid", Codes),
    phrase(c_identifier(Id), Codes, _).

:- end_tests(identifier_unicode).
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

## Quick Reference

**For comprehensive checklists, see [REFERENCE.md](REFERENCE.md)**

### Core Principles:
- Single-pass DCG parsing with `phrase_from_file/2`
- Work with character codes, convert at output boundaries
- Accumulators: in/out pairs (`Acc0, Acc`) at END of arguments
- Position tracking: `Line0, Line, Offset0, Offset` at END
- Use `memberchk/2` (not `member/2`) and `succ/2` (not `is/2` for +1)
- Order rules top-down: file → line → field → character primitives

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
