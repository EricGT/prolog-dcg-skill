# Error Handling and Debugging in DCGs

Comprehensive guide to error handling, position tracking, and debugging DCG parsers in SWI-Prolog. See [SKILL.md](SKILL.md) for core concepts.

## Table of Contents

- [Pattern: Position Tracking](#pattern-position-tracking)
- [Pattern: Position-Aware Error Reporting](#pattern-position-aware-error-reporting)
- [Pattern: Using library(debug)](#pattern-using-librarydebug)
- [Pattern: Using library(error) for Error Reporting](#pattern-using-libraryerror-for-error-reporting)
- [Pattern: Error Terms with Position Context](#pattern-error-terms-with-position-context)
- [Pattern: Graceful Failure with Position](#pattern-graceful-failure-with-position)

## Pattern: Position Tracking

**IMPORTANT**: Always track character and line offsets for debugging. Large files (megabytes) require precise error locations for grepping to the problem.

Choose between two patterns based on your needs:

### Pattern 1: Byte-Oriented Tracking (Simple)

Use when you only need absolute character position from start of file.

**Conventions:**
- Position accumulators go at **END** of argument list: `Offset0, Offset`
- **Offset increments by 1** for EACH character consumed (character-level precision)
- **IMPORTANT**: Use `succ/2` for each single-character increment
- **IMPORTANT**: Break multi-character sequences (like `\r\n`) into individual character increments to avoid manual counting errors
- **Every DCG rule** threads these through, even single-character parsers
- Simpler than line-oriented tracking - use when line/column info not needed
- This provides invaluable debugging for development (can be stripped for production)

### Pattern 2: Line-Oriented Tracking (Line + Column)

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

## Pattern: Position-Aware Error Reporting

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

## Pattern: Using library(debug)

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

## Pattern: Using library(error) for Error Reporting

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

## Pattern: Error Terms with Position Context

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

## Pattern: Graceful Failure with Position

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
```
