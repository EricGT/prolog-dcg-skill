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

## Pattern: Testing Non-Exported DCG Rules

### The Solution: Module Qualification

**Don't export internal predicates for testing.** Instead, use explicit module qualification to access them directly in tests while maintaining proper encapsulation.

**Module definition (keep internals private):**

```prolog
:- module(cscope_extract, [
    % Public API only
    generate_cscope_data/2,
    extract_definitions/3,
    extract_function_calls/3
]).

% Internal DCG rules (not exported)
global_definition(Symbol, Kind) --> ...
c_identifier(Id) --> ...
c_identifier_rest(Rest) --> ...
```

**Test file with module qualification:**

```prolog
% test/prolog/test_cscope_extract.pl
:- begin_tests(cscope_dcg_internal).

% Test internal DCG directly using module:predicate qualification
test(macro_simple, [true((Symbol == 'MAX_SIZE', Kind == macro))]) :-
    string_codes("#define MAX_SIZE", Codes),
    phrase(cscope_extract:global_definition(Symbol, Kind), Codes, _Rest).

test(identifier_underscore, [true(Id == '_private')]) :-
    string_codes("_private", Codes),
    phrase(cscope_extract:c_identifier(Id), Codes, _Rest).

test(identifier_rest_digits, [true(Rest == 'bar123')]) :-
    string_codes("bar123", Codes),
    phrase(cscope_extract:c_identifier_rest(Rest), Codes, _Rest).

:- end_tests(cscope_dcg_internal).
```

### Proper Test Execution Workflow

Following [PlUnit documentation best practices](https://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/plunit.html%27)):

```prolog
% 1. Load the source module
?- [src/prolog/cscope_extract].
true.

% 2. Load test files using load_test_files/1
?- load_test_files([]).
true.

% 3. Run tests
?- run_tests.
% PL-Unit: cscope_dcg_internal ... done
% All 3 tests passed
true.
```

**Critical:** Use `load_test_files([])` instead of directly consulting `.plt` files. This ensures the module system maintains proper context, allowing module-qualified calls to work correctly.

### Why This Approach Works

- ✅ **Maintains encapsulation** - internal predicates never exposed in public API
- ✅ **Direct testing** - test DCG rules directly without indirection
- ✅ **Comprehensive edge case coverage** - easy to test all scenarios
- ✅ **Fast execution** - no API layer overhead
- ✅ **Explicit intent** - `module:predicate` syntax makes it obvious you're testing internals
- ✅ **Clean module interface** - only true public API exported
- ✅ **Standard workflow** - follows PlUnit documentation best practices

### Real-World Results

Testing experience with cscope_extract.pl (77-test suite):

```prolog
% Clean module interface - only public API
:- module(cscope_extract, [
    generate_cscope_data/2
]).

% Test internals with module qualification
test(identifier_test) :-
    phrase(cscope_extract:c_identifier(Id), Codes, _).

test(definition_test) :-
    phrase(cscope_extract:global_definition(Sym, Kind), Codes, _).
```

**Results:**
- ✅ 77 comprehensive tests (26 for `global_definition//2`, 23 for `c_identifier//1`)
- ✅ Discovered anti-pattern in `has_function_call//2` through direct testing
- ✅ Found Unicode issue with `code_type/2` through edge case coverage
- ✅ Maintained clean module interface with only public API exported
- ✅ Easy test organization by DCG rule

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

## Pattern: Documenting Known Issues with Test Annotations

### Overview

plunit provides test options for documenting known problems without failing the test suite. This allows you to track technical debt, document limitations, and maintain clean CI/CD pipelines while being transparent about issues.

### Test Annotations

#### [blocked('reason')]

Use for known bugs or missing features that are documented but not yet fixed:

```prolog
:- begin_tests(known_issues).

% Functionality doesn't work due to architectural issue
test(feature_not_implemented, [blocked('waiting for DCG library update')]) :-
    phrase(future_dcg_feature, Input).

% Broken due to anti-pattern (manual difference lists)
test(detect_paren_simple, [blocked('test_has_paren/1 is broken')]) :-
    test_has_paren("contains ( paren").

test(detect_paren_nested, [blocked('test_has_paren/1 is broken')]) :-
    test_has_paren("func(nested(call))").

:- end_tests(known_issues).
```

**When to use:**
- External dependencies not yet available
- Features planned but not implemented
- Bugs in external code you depend on
- Architectural issues requiring major refactoring
- Breaking changes that need coordination

#### [fixme('reason')]

Use for bugs in your code that should be fixed:

```prolog
:- begin_tests(needs_fixing).

% Code behavior is incorrect and should be fixed
test(should_reject_unicode, [fixme('code_type/2 accepts Unicode')]) :-
    % This SHOULD fail but doesn't due to code_type/2 behavior
    string_codes("über", Codes),
    \+ phrase(c_identifier(_), Codes, _).

test(boundary_condition_bug, [fixme('off-by-one in line counting')]) :-
    phrase(parse_with_position(Result), Input),
    Result = pos(LineNum, _),
    LineNum == 10.  % Currently returns 9

:- end_tests(needs_fixing).
```

**When to use:**
- Bugs you've identified but haven't fixed yet
- Incorrect behavior that violates specifications
- Edge cases that fail but shouldn't
- Behavior that needs to change

#### [fail] or [condition(fail)]

Use for proper negative tests (these should fail and that's correct):

```prolog
:- begin_tests(negative_tests).

% Valid negative test - this SHOULD fail
test(invalid_input_rejected, [fail]) :-
    phrase(c_identifier(_), "123invalid", _).

test(empty_identifier_rejected, [fail]) :-
    phrase(c_identifier(_), "", _).

:- end_tests(negative_tests).
```

**When to use:**
- Testing that invalid input is properly rejected
- Verifying error conditions
- Ensuring constraints are enforced

### Benefits of Test Annotations

1. **Track Technical Debt** - Known issues are visible in test suite
2. **Document Limitations** - Clear what doesn't work and why
3. **Clean Test Runs** - Issues don't block CI/CD pipelines
4. **Easy Discovery** - Grep for `blocked` or `fixme` to find issues
5. **Version Tracking** - Git history shows when issues were introduced
6. **Professional Communication** - Transparent about known problems

### Test Output Examples

**Standard output:**

```
% PL-Unit: cscope_extract_dcg
% 10 tests are blocked (use run_tests/2 with show_blocked(true) for details)
% all 1 tests flagged FIXME failed
% All 67 (+-1 sub-tests) tests passed in 0.035 seconds (0.031 cpu)
```

**Show blocked test details:**

```prolog
?- run_tests([show_blocked(true)]).

% PL-Unit: cscope_extract_dcg
% Blocked tests:
%   test_has_paren:detect_paren_simple (blocked: test_has_paren/1 is broken)
%   test_has_paren:detect_paren_in_middle (blocked: test_has_paren/1 is broken)
%   test_has_paren:detect_paren_nested (blocked: test_has_paren/1 is broken)
%   ...
% all 1 tests flagged FIXME failed
% All 67 (+-1 sub-tests) tests passed in 0.035 seconds
```

### Pattern: Document and Track

Use this pattern for discovered issues:

```prolog
% 1. Create test that exposes the issue
test(unicode_in_identifier, [fixme('code_type/2 accepts non-ASCII letters')]) :-
    % C identifiers must be ASCII-only, but code_type/2 accepts Unicode
    string_codes("über", Codes),
    \+ phrase(c_identifier(_), Codes, _).

% 2. Add detailed comment explaining the issue
/**
 * FIXME: code_type/2 Unicode Acceptance Issue
 *
 * Problem: code_type(C, alpha) accepts Unicode letters (ü, é, ñ, etc.)
 *          but C identifiers (ANSI C, C89, C99) must be ASCII-only.
 *
 * Impact: Parser incorrectly accepts invalid C identifiers like "über",
 *         "naïve", "café" which are not valid in C source code.
 *
 * Workaround: Use explicit ASCII range checks:
 *             C >= 0'a, C =< 0'z ; C >= 0'A, C =< 0'Z
 *
 * See: ascii_letter_check/1 for correct implementation
 *
 * Tracking: GitHub issue #42
 * Priority: Medium (affects correctness but not common in practice)
 */

% 3. Create issue in your tracker (GitHub, etc.)

% 4. When fixed, remove annotation and update test
test(unicode_in_identifier) :-
    % Now correctly rejects non-ASCII
    string_codes("über", Codes),
    \+ phrase(c_identifier(_), Codes, _).
```

### Best Practices

✅ **Use `blocked`** for missing features or external dependencies

✅ **Use `fixme`** for bugs in your code that need fixing

✅ **Use `[fail]`** for proper negative tests (expected failures)

✅ **Add detailed comments** explaining the issue, impact, and workaround

✅ **Reference issue tracker** if available (GitHub issue number, etc.)

✅ **Include priority/severity** in comments for triage

✅ **Remove annotations** when issues are fixed

✅ **Review periodically** to prevent stale annotations

❌ **Don't use blocked/fixme as excuse** to avoid fixing bugs indefinitely

❌ **Don't leave annotations without explanation** - always add comments

### Real-World Example

From the cscope_extract.pl test suite:

```prolog
/**
 * Tests for test_has_paren/1 predicate
 *
 * BLOCKED: All tests blocked due to anti-pattern in has_function_call//2
 *
 * Issue: has_function_call//2 is declared with DCG notation (//2) but
 *        implemented with manual difference list parameters. This breaks
 *        phrase/2 usage in test_has_paren/1.
 *
 * Resolution: Rewrite has_function_call//2 as proper DCG or remove
 *             DCG notation and fix test_has_paren/1 accordingly.
 */
:- begin_tests(test_has_paren).

test(detect_paren_simple, [blocked('test_has_paren/1 is broken')]) :-
    test_has_paren("(").

test(detect_paren_in_middle, [blocked('test_has_paren/1 is broken')]) :-
    test_has_paren("text ( more").

test(no_paren, [blocked('test_has_paren/1 is broken')]) :-
    \+ test_has_paren("no paren here").

% ... 7 more blocked tests ...

:- end_tests(test_has_paren).
```

**Results:**
- ✅ 10 blocked tests clearly documented
- ✅ Issue explanation in comments
- ✅ Test suite still passes (67 passing tests)
- ✅ Technical debt is tracked and visible
- ✅ Tests ready to enable when issue is fixed

### Integration with Development Workflow

**During development:**

```bash
# Find all blocked tests
$ grep -r "blocked(" test/

# Find all fixme tests
$ grep -r "fixme(" test/

# Count technical debt
$ grep -c "blocked\|fixme" test/**/*.pl
```

**In CI/CD:**

```prolog
% ci_tests.pl - Run tests but allow blocked/fixme
:- initialization(main, main).

main :-
    run_tests([silent(true)]),
    halt.

main :-
    format('Tests failed!~n'),
    halt(1).
```

### Summary Statistics Example

After comprehensive testing with annotations:

```
Test Suite: cscope_extract_dcg
Total Tests: 77
├─ Passing: 67
├─ Blocked: 10 (test_has_paren/1 anti-pattern)
└─ Fixme: 1 (code_type/2 Unicode issue)

Technical Debt Tracked: 2 issues
- Issue #42: code_type/2 Unicode acceptance (Priority: Medium)
- Issue #43: has_function_call//2 anti-pattern (Priority: High)

Test Coverage:
- global_definition//2: 26 tests
- c_identifier//1: 23 tests
- Integration: 7 tests
- Edge cases: 11 tests
- Known issues: 10 tests (blocked)
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
