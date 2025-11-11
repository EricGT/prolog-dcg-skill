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