# Position Tracking Patterns

Patterns for tracking parse positions for debugging and error reporting. See [../SKILL.md](../SKILL.md) for core philosophy and [index.md](index.md) for all pattern categories.

## Overview

**IMPORTANT**: Always track character and line offsets for debugging. Large files (megabytes) require precise error locations for grepping to the problem.

Choose between two patterns based on your needs:

## Pattern 1: Byte-Oriented Tracking (Simple)

Use when you only need absolute character position from start of file.

**Conventions:**
- Position accumulators go at **END** of argument list: `Offset0, Offset`
- **Offset increments by 1** for EACH character consumed (character-level precision)
- **IMPORTANT**: Use `succ/2` for each single-character increment
- **IMPORTANT**: Break multi-character sequences (like `\r\n`) into individual character increments to avoid manual counting errors
- **Every DCG rule** threads these through, even single-character parsers
- Simpler than line-oriented tracking - use when line/column info not needed
- This provides invaluable debugging for development (can be stripped for production)

### Example: Byte-Oriented Tracking

```prolog
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
```

## Pattern 2: Line-Oriented Tracking (Line + Column)

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

### Example: Line-Oriented Tracking

```prolog
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

## Pattern: succ/2 for Incrementing

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

## Pattern: Character-Level DCG Composition

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

## Position Tracking Best Practices

### 1. Accumulator Placement

**IMPORTANT**: Position accumulators ALWAYS go at the END of the argument list, in consistent order.

```prolog
% ✅ CORRECT: Position accumulators at end
parse_field(Field, Line0, Line, Offset0, Offset) --> ...

% ❌ WRONG: Position accumulators not at end
parse_field(Line0, Line, Offset0, Offset, Field) --> ...
```

### 2. Accumulator Ordering

**IMPORTANT**: When using both Line and Offset, always use this order:

```prolog
% ✅ CORRECT: Line0, Line, Offset0, Offset
parse_data(Result, Line0, Line, Offset0, Offset) --> ...

% ❌ WRONG: Other orderings
parse_data(Result, Offset0, Offset, Line0, Line) --> ...
parse_data(Result, Line0, Offset0, Line, Offset) --> ...
```

### 3. Use Incremental Variables

**IMPORTANT**: Use `Line0, Line1, Line2...` NOT `Line, Line`.

```prolog
% ✅ CORRECT: Incremental variables
parse_line(Item, Line0, Line, Offset0, Offset) -->
    field(F1, Line0, Line1, Offset0, Offset1),
    tab(Line1, Line2, Offset1, Offset2),
    field(F2, Line2, Line, Offset2, Offset).

% ❌ WRONG: Assumes Line never changes
parse_line(Item, Line, Line, Offset0, Offset) -->  % Brittle!
    field(F1, Line, Line, Offset0, Offset1),
    tab(Line, Line, Offset1, Offset2),
    field(F2, Line, Line, Offset2, Offset).
```

**Why incremental variables matter:**
- Makes code robust to future changes
- If a rule is later modified to handle embedded newlines, `Line, Line` will fail
- Incremental variables clearly show data flow
- Consistent with accumulator patterns

### 4. Thread Through All Rules

**IMPORTANT**: Position tracking must thread through EVERY DCG rule, including single-character parsers.

```prolog
% ✅ CORRECT: Every rule threads position
parse_line(Result, Line0, Line, Offset0, Offset) -->
    field(F1, Line0, Line1, Offset0, Offset1),      % Threads
    tab(Line1, Line2, Offset1, Offset2),            % Threads
    field(F2, Line2, Line3, Offset2, Offset3),      % Threads
    line_end(Line3, Line, Offset3, Offset).         % Threads

% ❌ WRONG: Position tracking lost in some rules
parse_line(Result, Line0, Line, Offset0, Offset) -->
    field(F1, Line0, Line1, Offset0, Offset1),
    `\t`,  % ❌ Position lost! No threading
    field(F2, Line1, Line, Offset1, Offset).
```

## Error Reporting with Position

Include position information in parse results and error messages:

```prolog
% Include position in parsed results
parse_line(line(Data, Pos), Line0, Line, Offset0, Offset) -->
    { Pos = pos(Line0, Offset0) },
    parse_data(Data, Line0, Line, Offset0, Offset).

% Throw errors with position context
parse_required_field(Field, Line0, Line, Offset0, Offset) -->
    field_codes(Codes, Line0, Line, Offset0, Offset),
    {
      Codes == []
    ->
      throw(error(
        existence_error(field, required),
        context(parse_required_field/5, pos(Line0, Offset0))
      ))
    ;
      atom_codes(Field, Codes)
    }.

% Format error messages with position
format_parse_error(Pos, Message) :-
    Pos = pos(Line, Offset),
    format(user_error, 'Parse error at line ~w, column ~w: ~w~n',
           [Line, Offset, Message]).
```

## Summary

**Key Rules:**
1. **Choose pattern**: Byte-oriented (simple) or Line-oriented (line + column)
2. **Position at END**: Accumulators always at end of argument list
3. **Ordering**: `Line0, Line, Offset0, Offset` (Line before Offset)
4. **Use incremental variables**: `Line0, Line1, Line2...` NOT `Line, Line`
5. **Use succ/2**: For all single-character increments
6. **Character-level primitives**: Create reusable DCGs for each character
7. **Thread through all**: Every DCG rule must thread position
8. **Helper predicate**: Use for updating line and resetting offset

**Benefits:**
- **Character-level precision**: Know exact position of errors
- **Invaluable for debugging**: Especially in large files (megabytes)
- **Error reporting**: Include precise positions in error messages
- **Development aid**: Can be stripped for production

**Common Pitfalls:**
- Using `Line, Line` instead of incremental variables
- Forgetting to thread position through some rules
- Using `is/2` instead of `succ/2` for +1
- Manual counting multi-character sequences
- Position accumulators not at end

## See Also

- [accumulators.md](accumulators.md) - General accumulator patterns
- [basic.md](basic.md) - Character classification and basic patterns
- [optimization.md](optimization.md) - Performance patterns with position tracking
- [file-io.md](file-io.md) - File I/O with position tracking