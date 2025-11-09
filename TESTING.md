# Testing DCGs

Guide to testing DCG parsers using Prolog Unit Tests. See [SKILL.md](SKILL.md) for core concepts.

## Overview

**IMPORTANT**: All DCG clauses MUST have test cases using [Prolog Unit Tests](https://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/plunit.html%27)) in separate test files.

## Test File Organization

```
src/prolog/parser.pl          % DCG grammar rules
test/prolog/test_parser.pl    % Unit tests for parser
```

## Pattern: Unit Test Structure

```prolog
% test/prolog/test_parser.pl
:- begin_tests(parser).
:- use_module('../../src/prolog/parser').

% Test individual DCG rules
test(field_simple, [true(F == hello)]) :-
    phrase(field(F), `hello`, Rest),
    Rest == [].

test(field_with_remainder, [true(F == hello)]) :-
    phrase(field(F), `hello\tworld`, Rest),
    Rest == [9, 119, 111, 114, 108, 100].  % \tworld

test(tsv_line_three_fields, [true(Fields == [a, b, c])]) :-
    phrase(tsv_line(Fields), `a\tb\tc\n`).

test(tsv_line_empty, [fail]) :-
    phrase(tsv_line(_), ``).

test(tsv_line_no_newline, [true(Fields == [a, b])]) :-
    phrase(tsv_line(Fields), `a\tb`).

:- end_tests(parser).
```

## Pattern: Testing Edge Cases

```prolog
:- begin_tests(parser_edge_cases).

% Empty input
test(empty_file, [true(Result == [])]) :-
    phrase(file_content(Result), ``).

% Single line, no newline
test(single_line_no_newline, [true(Result == [line(data)])]) :-
    phrase(file_content(Result), `data`).

% Multiple line endings
test(crlf_endings, [true(length(Lines, 2))]) :-
    phrase(file_lines(Lines), `line1\r\nline2\r\n`).

test(mixed_endings, [true(length(Lines, 3))]) :-
    phrase(file_lines(Lines), `line1\nline2\r\nline3\r`).

% Invalid input handling
test(invalid_line_graceful, [true(Result = invalid(_, _))]) :-
    phrase(parse_line(0, 1, Result), `bad\tdata`).

:- end_tests(parser_edge_cases).
```

## Pattern: Testing with Position Tracking

```prolog
:- begin_tests(parser_positions).

test(position_tracking_first_line, [true(Pos == pos(1, 0))]) :-
    phrase(parse_line(0, 1, data(_, Pos, _), _), `field1\tfield2\n`).

test(position_tracking_offset, [true(Offset == 15)]) :-
    Input = `field1\tfield2\n`,  % 15 chars
    phrase(parse_line(0, 1, _, Offset), Input).

test(error_position_reported, [true(Pos == pos(5, 120))]) :-
    phrase(parse_with_error_reporting(120, 5, error(Pos, _)), `bad input`).

:- end_tests(parser_positions).
```

## Pattern: Testing Complex Grammars

```prolog
:- begin_tests(c_grammar).

test(c_identifier_valid, [true(Id == foo_bar123)]) :-
    phrase(c_identifier(Id), `foo_bar123`).

test(c_identifier_starts_underscore, [true(Id == '_private')]) :-
    phrase(c_identifier(Id), `_private`).

test(c_identifier_no_digit_start, [fail]) :-
    phrase(c_identifier(_), `123foo`).

test(global_definition_macro, [true((Sym == MY_MACRO, Kind == macro))]) :-
    phrase(global_definition(Sym, Kind), `#define MY_MACRO 42`).

test(global_definition_typedef_struct, [true((Sym == MyStruct, Kind == typedef))]) :-
    phrase(global_definition(Sym, Kind), `typedef struct MyStruct {`).

:- end_tests(c_grammar).
```

## Pattern: Property-Based Testing

```prolog
:- begin_tests(parser_properties).

% Roundtrip: parse then unparse should give original
test(roundtrip_tsv_line, [forall(gen_tsv_line(Input))]) :-
    phrase(tsv_line(Fields), Input),
    phrase(unparse_tsv_line(Fields), Output),
    Input == Output.

% Helper to generate test inputs
gen_tsv_line(`a\tb\tc\n`).
gen_tsv_line(`field1\tfield2\n`).
gen_tsv_line(`x\ty\tz\tw\n`).

:- end_tests(parser_properties).
```

## Running Tests

```prolog
% Run all tests
?- run_tests.

% Run specific test suite
?- run_tests(parser).

% Run with verbose output
?- run_tests(parser, [verbose(true)]).

% Run and show only failures
?- run_tests([silent(true)]).
```

## Pattern: Test Helpers

```prolog
% test/prolog/test_helpers.pl
:- module(test_helpers, [
    codes_from_string/2,
    must_parse/3,
    must_fail_parse/2
]).

% Convert string to codes for testing
codes_from_string(String, StringCodes) :-
    string_codes(String, StringCodes).

% Assert that parsing succeeds
must_parse(Grammar, Input, Expected) :-
    phrase(Grammar, Input, Rest),
    assertion(Rest == []),
    assertion(Expected).

% Assert that parsing fails
must_fail_parse(Grammar, Input) :-
    \+ phrase(Grammar, Input).
```

## Test Coverage Goals

For each DCG rule, ensure tests cover:
- ✅ **Happy path**: Valid input parsing correctly
- ✅ **Edge cases**: Empty input, single element, boundary conditions
- ✅ **Invalid input**: Graceful failure or error reporting
- ✅ **Position tracking**: Correct offset and line number reporting
- ✅ **Whitespace handling**: Different whitespace combinations
- ✅ **Line endings**: `\n`, `\r\n`, `\r`, no ending
- ✅ **Large inputs**: Performance with realistic data sizes

## Best Practices

1. **Separate test files** from grammar rules
2. **Test each DCG rule** individually with unit tests
3. **Cover edge cases** thoroughly (empty, single, boundary)
4. **Verify position tracking** when implemented
5. **Test different line endings** (\n, \r\n, \r, no ending)
6. **Include property-based tests** for roundtrip guarantees
7. **Use test helpers** to reduce boilerplate
8. **Run tests frequently** during development

## Example Test Session

```prolog
?- consult('test/prolog/test_parser.pl').
true.

?- run_tests.
% PL-Unit: parser ............... passed 0.001 sec
% PL-Unit: parser_edge_cases .... passed 0.002 sec
% PL-Unit: parser_positions ..... passed 0.001 sec
% PL-Unit: c_grammar ............ passed 0.003 sec
% All 28 tests passed
true.
```
