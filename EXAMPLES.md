# DCG Examples

Complete working examples demonstrating DCG best practices. See [SKILL.md](SKILL.md) for core concepts.

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

## Additional Examples

For more examples including:
- Real-world C code parsing
- Template expansion with difference lists
- SQL dialect conversion

See the [examples](../examples/) directory.
