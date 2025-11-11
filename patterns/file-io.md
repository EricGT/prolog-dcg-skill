# File I/O Integration

Patterns for integrating DCG parsers with file operations. See [../SKILL.md](../SKILL.md) for core philosophy and [index.md](index.md) for all pattern categories.

## Pattern: Using phrase_from_file/2

**Best for**: Parsing entire files directly with DCGs.

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

% EOS (end of stream) helper
% Note: eos is defined in library(dcg/basics) but should not be used from there.
% Instead, provide both DCG eos//0 and predicate eos_/2 explicitly.
eos --> call(eos_).
eos_([], []).
```

**Benefits:**
- Direct file-to-DCG parsing
- No intermediate string conversions
- Handles file encoding automatically
- Memory efficient for large files

**Example with position tracking:**
```prolog
parse_file_with_positions(Filename, Result) :-
    phrase_from_file(file_content(Result, 1, _, 0, _), Filename).

file_content([Line|Lines], Line0, Line, Offset0, Offset) -->
    file_line(Line, Line0, Line1, Offset0, Offset1), !,
    file_content(Lines, Line1, Line, Offset1, Offset).
file_content([], Line, Line, Offset, Offset) --> eos.

file_line(Item, Line0, Line, Offset0, Offset) -->
    parse_line(Item, Line0, Line0, Offset0, Offset1),
    line_end(Line0, Line, Offset1, Offset).
```

## Pattern: Direct File Parsing with phrase_from_file/2

### Overview

`phrase_from_file/2` enables parsing files directly without reading to strings or splitting into lines. This achieves true single-pass parsing with no intermediate data structures.

### The Transformation

**Before (Multi-pass):**

```prolog
% AVOID: Multiple passes
extract_definitions(SymbolsFile, Definitions, Options) :-
    read_file_to_string(SymbolsFile, Content, []),     % Pass 1: File → String
    split_string(Content, "\n", "\r", Lines),          % Pass 2: String → Lines
    extract_defs_from_lines(Lines, [], Definitions).   % Pass 3: Lines → Data
```

**Problems:**
- Three passes over the data
- Two intermediate data structures (string, line list)
- Multiple memory allocations
- Cannot maintain state across lines efficiently

**After (Single-pass):**

```prolog
% PREFER: Single pass
extract_definitions(SymbolsFile, Definitions, Options) :-
    phrase_from_file(symbol_file_lines(Definitions, []), SymbolsFile).
```

**Benefits:**
- Single pass over file
- No intermediate structures
- Direct character code processing
- Clean accumulator threading for state

### File-Level DCG Structure

```prolog
% File parser with accumulator for tracking seen symbols
symbol_file_lines(Defs, Seen0) -->
    symbol_line(Def, Seen0, Seen1),
    !,
    {
        (   Def = def(_, _, _, _)        % Valid definition
        ->  Defs = [Def|RestDefs]
        ;   Defs = RestDefs,              % Skip this line
            Seen1 = Seen0
        )
    },
    symbol_file_lines(RestDefs, Seen1).
symbol_file_lines([], _) -->
    [].
```

### Line-Level Parser

```prolog
% Empty line - skip
symbol_line(none, Seen, Seen) -->
    'whites*',
    newline,
    !.

% Valid line - parse fields
symbol_line(Def, Seen0, Seen) -->
    file_field(File), `\t`,
    scope_field(Scope), `\t`,
    line_number_field(LineNum), `\t`,
    field_until_newline(ContextCodes),
    newline,
    !,
    {
        string_codes(Context, ContextCodes),
        process_definition(File, Scope, LineNum, Context,
                          Def, Seen0, Seen)
    }.

% Failed parse - skip line gracefully
symbol_line(none, Seen, Seen) -->
    skip_until_newline,
    newline.
```

### Accumulator Threading

**Key principles for file parsing with accumulators:**

```prolog
% Input accumulator: Acc0, Seen0, State0
% Intermediate: Acc1, Seen1, State1
% Output accumulator: Acc, Seen, State (no number)

file_lines(Results, Acc0) -->
    line_parser(Result, Acc0, Acc1),
    !,
    { process_result(Result, Results, RestResults, Acc0, Acc1) },
    file_lines(RestResults, Acc1).
file_lines([], Acc) -->
    [],
    { finalize_accumulator(Acc) }.
```

**Naming Convention:**
- Input ends in `0`: `Seen0`, `Count0`, `State0`
- Intermediate increments: `Seen1`, `Count1`, `State1`
- Output has no number: `Seen`, `Count`, `State`

**Position:**
- Always at END of argument list
- Keep related accumulators together
- Maintain consistent pairing

```prolog
% ✅ CORRECT: Accumulators at end, properly paired
parse_lines(Results, Seen0, Seen, Count0, Count) --> ...

% ❌ WRONG: Accumulators scattered
parse_lines(Results, Seen0, Count0, Seen, Count) --> ...
```

### Complete Example

```prolog
% Top-level: parse file with state tracking
extract_function_calls(SymbolsFile, Functions, Options) :-
    phrase_from_file(function_names_file(Functions, []), SymbolsFile).

% File-level parser
function_names_file(Functions, Acc0) -->
    function_name_line(Name, Acc0),
    !,
    {
        (   Name \= none,
            Name \= '<global>',
            \+ memberchk(Name, Acc0)
        ->  Acc1 = [Name|Acc0]
        ;   Acc1 = Acc0
        )
    },
    function_names_file(Functions, Acc1).
function_names_file(Functions, Functions) -->
    [].

% Line-level parser
function_name_line(Name, _Acc) -->
    'whites*',
    newline,
    !,
    { Name = none }.

function_name_line(Name, _Acc) -->
    skip_until_tab, `\t`,
    field_until_tab(NameCodes), `\t`,
    skip_until_newline,
    newline,
    !,
    { atom_codes(Name, NameCodes) }.

function_name_line(none, _Acc) -->
    skip_until_newline,
    newline.
```

### Performance Benefits

**Before:**
- File → String conversion: 1 full memory allocation
- String → Lines split: 1 full memory allocation + N string allocations
- Lines → Data: N line processing operations
- Total: 3+ passes, 2+ N memory allocations

**After:**
- File → Codes → Results: Single pass
- No intermediate structures
- Streaming character processing
- Total: 1 pass, minimal allocations

### When to Use phrase_from_file/2

Use `phrase_from_file/2` when:

✅ Parsing entire files with structured format
✅ Need to maintain state across lines (accumulators)
✅ Single-pass parsing is possible
✅ File size fits comfortably in memory
✅ Want to eliminate string manipulation overhead

### When NOT to Use phrase_from_file/2

Consider line-by-line reading when:

❌ Files are extremely large (multi-GB)
❌ Need incremental processing with early exit
❌ Cannot predict file size (unbounded streams)
❌ Memory constraints are critical

## Pattern: Using phrase/2 with read_string

**Use when**: You need to process the stream first or read from non-file streams.

```prolog
% When you need to process stream first
parse_stream(Stream, Result) :-
    read_string(Stream, _, Content),
    string_codes(Content, Codes),
    phrase(file_grammar(Result), Codes).

% Parse with known content
parse_string(Content, Result) :-
    string_codes(Content, Codes),
    phrase(file_grammar(Result), Codes).

% Parse codes directly
parse_codes(Codes, Result) :-
    phrase(file_grammar(Result), Codes).
```

**When to use each:**
- `phrase_from_file/2`: Direct file parsing (most common)
- `phrase/2` with `read_string/3`: When need to pre-process content
- `phrase/2` with codes: When content already in code list format

## Pattern: Using catch/3 for Error Handling

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

## Pattern: Handling Specific Error Types

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

## Pattern: Stream Management with call_cleanup/2

**Use when**: Need explicit control over stream lifecycle.

```prolog
% Explicit stream management
parse_file_with_cleanup(Filename, Result) :-
    open(Filename, read, Stream, [encoding(utf8)]),
    call_cleanup(
        parse_stream_safe(Stream, Result),
        close(Stream)
    ).

parse_stream_safe(Stream, Result) :-
    catch(
        phrase_from_stream(file_grammar(Result), Stream),
        Error,
        handle_stream_error(Error, Result)
    ).

handle_stream_error(Error, []) :-
    print_message(error, Error).
```

**Benefits:**
- Stream always closed, even if error occurs
- Explicit resource management
- Can set specific encoding options

## Pattern: Encoding Handling

```prolog
% Parse file with specific encoding
parse_file_utf8(Filename, Result) :-
    phrase_from_file(file_grammar(Result), Filename, [encoding(utf8)]).

% Parse file with automatic encoding detection
parse_file_auto_encoding(Filename, Result) :-
    catch(
        phrase_from_file(file_grammar(Result), Filename, [encoding(utf8)]),
        Error,
        try_other_encoding(Filename, Error, Result)
    ).

try_other_encoding(Filename, _Error, Result) :-
    phrase_from_file(file_grammar(Result), Filename, [encoding(iso_latin_1)]).
```

## Pattern: Large File Handling

```prolog
% Process large files in chunks
process_large_file(Filename, Result) :-
    open(Filename, read, Stream, [encoding(utf8)]),
    call_cleanup(
        process_chunks(Stream, [], Result),
        close(Stream)
    ).

process_chunks(Stream, Acc, Result) :-
    read_string(Stream, 65536, Chunk),  % Read 64KB chunks
    (
      Chunk == end_of_file
    ->
      reverse(Acc, Result)
    ;
      string_codes(Chunk, Codes),
      phrase(parse_chunk(Items), Codes),
      append(Items, Acc, Acc1),
      process_chunks(Stream, Acc1, Result)
    ).
```

## Common File I/O Patterns

### 1. Parse File or Return Empty on Error

```prolog
parse_file_or_empty(Filename, Result) :-
    catch(
        phrase_from_file(file_grammar(Result), Filename),
        _Error,
        Result = []
    ).
```

### 2. Parse File with Default Encoding

```prolog
parse_file_default(Filename, Result) :-
    phrase_from_file(
        file_grammar(Result),
        Filename,
        [encoding(utf8)]
    ).
```

### 3. Parse Multiple Files

```prolog
parse_files(Filenames, Results) :-
    maplist(parse_file_safe, Filenames, Results).

parse_file_safe(Filename, result(Filename, Content)) :-
    catch(
        phrase_from_file(file_grammar(Content), Filename),
        Error,
        (Content = error(Error))
    ).
```

### 4. Parse with Progress Reporting

```prolog
parse_file_with_progress(Filename, Result) :-
    format('Parsing ~w...~n', [Filename]),
    catch(
        (
            phrase_from_file(file_grammar(Result), Filename),
            length(Result, Count),
            format('Parsed ~w items from ~w~n', [Count, Filename])
        ),
        Error,
        (
            format(user_error, 'Error parsing ~w: ~w~n', [Filename, Error]),
            Result = []
        )
    ).
```

## Error Handling Best Practices

### 1. Always Catch File Errors

```prolog
% ✅ GOOD: Handles file errors
parse_file_safe(File, Result) :-
    catch(
        phrase_from_file(grammar(Result), File),
        Error,
        handle_error(Error, Result)
    ).

% ❌ BAD: No error handling
parse_file_unsafe(File, Result) :-
    phrase_from_file(grammar(Result), File).  % Will crash on error
```

### 2. Provide Meaningful Error Messages

```prolog
% ✅ GOOD: User-friendly error messages
handle_error(error(existence_error(source_sink, File), _), []) :-
    format(user_error, 'Error: File not found: ~w~n', [File]).

% ❌ BAD: Generic error output
handle_error(Error, []) :-
    format(user_error, 'Error: ~w~n', [Error]).  % Not user-friendly
```

### 3. Clean Up Resources

```prolog
% ✅ GOOD: Uses call_cleanup
parse_with_stream(File, Result) :-
    open(File, read, Stream),
    call_cleanup(
        parse_stream(Stream, Result),
        close(Stream)
    ).

% ❌ BAD: May leak stream on error
parse_with_stream_unsafe(File, Result) :-
    open(File, read, Stream),
    parse_stream(Stream, Result),
    close(Stream).  % May not be called if parse_stream fails
```

## Integration with Position Tracking

```prolog
% Parse file with full position tracking
parse_file_tracked(Filename, Result) :-
    catch(
        phrase_from_file(
            file_content(Result, 1, EndLine, 0, EndOffset),
            Filename
        ),
        Error,
        handle_parse_error_with_position(Error, Filename, Result)
    ),
    format('Parsed ~w: ~w lines, ~w bytes~n',
           [Filename, EndLine, EndOffset]).

handle_parse_error_with_position(Error, Filename, []) :-
    (
      Error = error(parse_error(Pos, Msg), _)
    ->
      Pos = pos(Line, Offset),
      format(user_error, 'Parse error in ~w at line ~w, column ~w: ~w~n',
             [Filename, Line, Offset, Msg])
    ;
      format(user_error, 'Error parsing ~w: ~w~n', [Filename, Error])
    ).
```

## Pattern: Single-Pass Stream Processing

### Overview

When processing streams from external sources (pipes, network), read the entire stream into codes, parse once with DCG, then write results. This eliminates line-by-line I/O overhead.

### The Transformation

**Before (Line-by-line):**

```prolog
% AVOID: Line-by-line processing
read_and_reformat_lines(In, Out, Acc, Total) :-
    read_line_to_string(In, Line),           % N read operations
    (   Line == end_of_file
    ->  Total = Acc
    ;   reformat_cscope_line(Line, Reformatted),
        format(Out, '~s~n', [Reformatted]),  % N write operations
        Acc1 is Acc + 1,
        read_and_reformat_lines(In, Out, Acc1, Total)
    ).
```

**Problems:**
- N read operations (one per line)
- N write operations (one per line)
- String conversion on each line
- Recursion overhead

**After (Single-pass):**

```prolog
% PREFER: Single-pass stream processing
read_and_reformat_lines(In, Out, _Acc, Total) :-
    read_stream_to_codes(In, Codes),             % 1 read operation
    phrase(cscope_stream_lines(Lines), Codes),   % Single parse pass
    write_reformatted_lines(Out, Lines, 0, Total). % Batch write
```

**Benefits:**
- 1 read operation (entire stream)
- Single DCG parsing pass
- Batch writing (more efficient)
- No line-by-line overhead

### Stream DCG Parser

```prolog
% Parse entire stream into list of lines
cscope_stream_lines([Line|Lines]) -->
    parse_cscope_line(File, Symbol, LineNum, Context),
    newline,
    !,
    {
        normalize_path(File, NormFile),
        atomics_to_string([NormFile, Symbol, LineNum, Context], "\t", Line)
    },
    cscope_stream_lines(Lines).
cscope_stream_lines([]) -->
    [].

% Parse single line from stream
parse_cscope_line(File, Symbol, LineNum, Context) -->
    field_until_space(FileCodes), 'whites+',
    field_until_space(SymbolCodes), 'whites+',
    field_until_space(LineNumCodes), 'whites+',
    field_until_newline(ContextCodes),
    {
        atom_codes(File, FileCodes),
        atom_codes(Symbol, SymbolCodes),
        atom_codes(LineNum, LineNumCodes),
        string_codes(Context, ContextCodes)
    }.
```

### Batch Writer

```prolog
% Write all results at once
write_reformatted_lines(_, [], Total, Total).
write_reformatted_lines(Out, [Line|Lines], Acc, Total) :-
    format(Out, '~s~n', [Line]),
    succ(Acc, Acc1),
    write_reformatted_lines(Out, Lines, Acc1, Total).
```

### Complete Example

```prolog
% Process cscope output stream
generate_cscope_data(SymbolsFile, Options) :-
    setup_call_cleanup(
        open(SymbolsFile, write, Out, [encoding(utf8)]),
        process_cscope_output(Out),
        close(Out)
    ).

process_cscope_output(Out) :-
    process_create(
        path(cscope),
        ['-d', '-L', '-1', '.*'],
        [stdout(pipe(In)), stderr(null)]
    ),
    setup_call_cleanup(
        true,
        read_and_reformat_lines(In, Out, 0, Total),
        close(In)
    ),
    debug(cscope, 'Processed ~w lines', [Total]).

read_and_reformat_lines(In, Out, _Acc, Total) :-
    read_stream_to_codes(In, Codes),
    phrase(cscope_stream_lines(Lines), Codes),
    write_reformatted_lines(Out, Lines, 0, Total).
```

### Performance Benefits

**Before (Line-by-line):**
- System calls: N reads + N writes
- String allocations: N
- Recursion depth: N
- Format operations: N

**After (Stream processing):**
- System calls: 1 read + N writes (batched)
- Code processing: Single pass
- Recursion depth: For writing only
- Format operations: N (but batched)

### When to Use Stream Processing

Use this pattern when:

✅ Processing external command output (pipes)
✅ Stream size fits in memory
✅ Entire stream needed before processing
✅ Can afford single-pass memory usage
✅ Batch writing is acceptable

### When to Use Line-by-Line

Use line-by-line when:

✅ Streams are unbounded or very large
✅ Need incremental processing with early exit
✅ Memory is constrained
✅ Real-time processing required

## Summary

### Key Patterns:
1. **phrase_from_file/2**: Direct file parsing (most common)
2. **phrase/2 with read_string**: When preprocessing needed
3. **catch/3**: Always wrap file operations
4. **call_cleanup/2**: Ensure resources released
5. **Specific error handling**: Handle existence_error, permission_error separately

### Best Practices:
- ✅ Always use `catch/3` for file operations
- ✅ Provide user-friendly error messages
- ✅ Clean up resources with `call_cleanup/2`
- ✅ Specify encoding explicitly
- ✅ Include position information in errors
- ❌ Don't let file operations crash without handling
- ❌ Don't leak file streams on errors

### Common Encodings:
- `utf8`: Unicode/international text (most common)
- `iso_latin_1`: Western European text
- `ascii`: Plain ASCII text
- `utf16`: Windows Unicode text

## See Also

- [position-tracking.md](position-tracking.md) - Include positions in error messages
- [basic.md](basic.md) - Basic parsing patterns
- [../ERROR_HANDLING.md](../ERROR_HANDLING.md) - Error handling strategies
- [../REFERENCE.md](../REFERENCE.md) - Common mistakes and debugging