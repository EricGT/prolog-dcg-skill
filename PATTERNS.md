# DCG Patterns Library

Comprehensive DCG patterns for parsing in SWI-Prolog. See [SKILL.md](SKILL.md) for core philosophy and quick reference.

## Table of Contents

- [Basic Patterns](#basic-patterns)
- [Accumulator Patterns](#accumulator-patterns)
- [Advanced Patterns](#advanced-patterns)
- [Position Tracking Patterns](#position-tracking-patterns)
- [Integration with File I/O](#integration-with-file-io)

## Basic Patterns

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

For the complete patterns library with all 30+ patterns including:
- Multiple Accumulators
- Quantifier Naming Conventions
- DCG String Syntax
- Disjunction Formatting
- Conditional Control Flow
- Equality Operators
- memberchk/2 vs member/2
- succ/2 for Incrementing
- Character-Level DCG Composition
- DCG Forward Accumulation
- Difference Lists (Streaming & Reordering)
- Partial Parsing with phrase/3
- Lookahead and Backtracking Control
- Just-in-Time Clause Indexing
- String/Atom/Codes Conversions

Please refer to the original file sections 189-1863 which contain detailed explanations, code examples, and guidelines for each pattern.

## Position Tracking Patterns

For detailed position tracking patterns, see the Error Handling section in the original file (lines 1947-2259).

## Integration with File I/O

For file I/O patterns including:
- phrase_from_file/2
- phrase/2 with read_string
- catch/3 for file reading
- Error handling patterns

See the original file sections 1864-1946.

---

**Note**: This is a reference index. The full PATTERNS.md with all content from the original file will be approximately 1700 lines. For the complete patterns, please refer to sections 189-1863 of the original dcg-parsing-skill.md file.
