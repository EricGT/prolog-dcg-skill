%% SQL Dialect Translation - Pure Difference List Pattern (Version 2)
%% Demonstrates OPTIMAL difference list output accumulation for reordering
%%
%% Key Innovation: ALL rules have _dl versions that emit via difference list
%% - Buffered elements are stored AS difference lists
%% - Emitting buffered elements is O(1) unification (not O(n) traversal!)
%% - Uniform pattern throughout - maximum composability
%%
%% Result: TRUE O(n) processing for both input parsing AND output emission
%%
%% Converts SQL Server SELECT TOP syntax to PostgreSQL SELECT LIMIT syntax
%% Input:  "SELECT TOP 10 name, age FROM users WHERE active = 1"
%% Output: "SELECT name, age FROM users WHERE active = 1 LIMIT 10"

:- module(sql_reordering_v2, [
    convert_sql/2,
    test_sql_conversion/0
]).

%% Main conversion entry point
convert_sql(InputSQL, OutputSQL) :-
    string_codes(InputSQL, InputCodes),
    phrase(convert_statement_dl(OutputCodes-[]), InputCodes),
    string_codes(OutputSQL, OutputCodes).

%% convert_statement_dl(-Hole0-Hole)//
%% Main conversion - all output emission through difference lists
%% Pattern: Buffered elements are ALREADY difference lists - O(1) to emit!
convert_statement_dl(Hole0-Hole) -->
    `SELECT`,
    'whites+',
    top_clause_dl(TopDL),           % Parse TOP, return as difference list!
    'whites*',
    convert_fields_dl(Hole0-Hole1), % 1: Emit "SELECT fields"
    'whites+',
    convert_from_dl(Hole1-Hole2),   % 2: Emit " FROM table"
    (
        'whites+',
        convert_where_dl(Hole2-Hole3), % 3: Emit " WHERE condition"
        !
    ;
        { Hole3 = Hole2 }            % Optional WHERE
    ),
    add_limit_dl(TopDL, Hole3-Hole). % 4: Emit " LIMIT n" - O(1) unification!

%% top_clause_dl(-DigitsDLOrNone)//
%% Parse TOP clause and return digits AS difference list
%% Key: Returns difference list [0'1,0'0|Hole]-Hole, not plain list!
top_clause_dl(DigitsDL) -->
    `TOP`,
    'whites+',
    'digits_dl+'(DigitsDL),         % DigitsDL is already in DL form!
    !.
top_clause_dl(none) --> [].

%% convert_fields_dl(-Hole0-Hole)//
%% Emit SELECT keyword and field list
convert_fields_dl(Hole0-Hole) -->
    keyword_select_dl(Hole0-Hole1),
    literal_space_dl(Hole1-Hole2),
    field_list_dl(Hole2-Hole).

%% field_list_dl(-Hole0-Hole)//
%% Parse and emit comma-separated field list
field_list_dl(Hole0-Hole) -->
    identifier_dl(Hole0-Hole1),
    (
        'whites*', `,`, 'whites*',
        !,
        literal_comma_space_dl(Hole1-Hole2),
        field_list_dl(Hole2-Hole)
    ;
        { Hole1 = Hole }
    ).

%% convert_from_dl(-Hole0-Hole)//
%% Emit FROM clause
convert_from_dl(Hole0-Hole) -->
    `FROM`,
    'whites+',
    literal_space_dl(Hole0-Hole1),
    keyword_from_dl(Hole1-Hole2),
    literal_space_dl(Hole2-Hole3),
    identifier_dl(Hole3-Hole).

%% convert_where_dl(-Hole0-Hole)//
%% Emit WHERE clause
convert_where_dl(Hole0-Hole) -->
    `WHERE`,
    'whites+',
    literal_space_dl(Hole0-Hole1),
    keyword_where_dl(Hole1-Hole2),
    literal_space_dl(Hole2-Hole3),
    condition_dl(Hole3-Hole).

%% add_limit_dl(+DigitsDLOrNone, -Hole0-Hole)//
%% Emit LIMIT clause if TOP was present
%% KEY INSIGHT: DigitsDL is already a difference list!
%% Emitting it is O(1) unification, not O(n) traversal!
add_limit_dl(none, Hole-Hole) --> [].
add_limit_dl(DigitsDL, Hole0-Hole) -->
    {
        DigitsDL \= none  % Ensure we have digits
    },
    literal_space_dl(Hole0-Hole1),
    keyword_limit_dl(Hole1-Hole2),
    literal_space_dl(Hole2-Hole3),
    { DigitsDL = Hole3-Hole }.    % O(1) unification! Beautiful!

%% condition_dl(-Hole0-Hole)//
%% Emit simple equality condition
condition_dl(Hole0-Hole) -->
    identifier_dl(Hole0-Hole1),
    'whites*',
    [0'=],
    'whites*',
    literal_space_equal_space_dl(Hole1-Hole2),
    value_dl(Hole2-Hole).

%% =============================================================================
%% Keyword and Literal Emission DCGs
%% These emit specific keywords/literals to output difference list
%% =============================================================================

keyword_select_dl([0'S,0'E,0'L,0'E,0'C,0'T|Hole]-Hole) --> [].
keyword_from_dl([0'F,0'R,0'O,0'M|Hole]-Hole) --> [].
keyword_where_dl([0'W,0'H,0'E,0'R,0'E|Hole]-Hole) --> [].
keyword_limit_dl([0'L,0'I,0'M,0'I,0'T|Hole]-Hole) --> [].

literal_space_dl([0' |Hole]-Hole) --> [].
literal_comma_space_dl([0',,0' |Hole]-Hole) --> [].
literal_space_equal_space_dl([0' ,0'=,0' |Hole]-Hole) --> [].

%% =============================================================================
%% Core Parsing DCGs with Difference List Output
%% These parse input AND emit to output difference list simultaneously
%% =============================================================================

%% identifier_dl(-Hole0-Hole)//
%% Parse identifier from input, emit to output difference list
identifier_dl(Hole0-Hole) -->
    [First],
    {
        code_type(First, alpha)
    ;
        First = 0'_
    },
    { Hole0 = [First|Hole1] },
    'identifier_rest_dl*'(Hole1-Hole).

'identifier_rest_dl*'(Hole0-Hole) -->
    [C],
    {
        code_type(C, alnum)
    ;
        C = 0'_
    },
    !,
    { Hole0 = [C|Hole1] },
    'identifier_rest_dl*'(Hole1-Hole).
'identifier_rest_dl*'(Hole-Hole) --> [].

%% value_dl(-Hole0-Hole)//
%% Parse value (number or identifier), emit to output
%% Pattern: Let digits_dl+ self-determine applicability, cut on success
value_dl(Hole0-Hole) -->
    'digits_dl+'(Hole0-Hole), !.
value_dl(Hole0-Hole) -->
    identifier_dl(Hole0-Hole).

%% 'digits_dl+'(-Hole0-Hole)//
%% Parse one or more digits, emit to output difference list
%% This is KEY for buffering TOP value efficiently!
'digits_dl+'(Hole0-Hole) -->
    [D],
    { code_type(D, digit) },
    !,
    { Hole0 = [D|Hole1] },
    'digits_dl*'(Hole1-Hole).

'digits_dl*'(Hole0-Hole) -->
    [D],
    { code_type(D, digit) },
    !,
    { Hole0 = [D|Hole1] },
    'digits_dl*'(Hole1-Hole).
'digits_dl*'(Hole-Hole) --> [].

%% =============================================================================
%% Helper DCGs (Input parsing only, no output emission)
%% =============================================================================

%% peek(-Code)//
%% Peek at next character without consuming
peek(C), [C] --> [C].

%% whites+//
%% Required whitespace
'whites+' -->
    [C],
    { code_type(C, space) },
    'whites*'.

%% whites*//
%% Optional whitespace
'whites*' -->
    [C],
    { code_type(C, space) },
    !,
    'whites*'.
'whites*' --> [].

%% =============================================================================
%% Test Cases
%% =============================================================================

test_sql_conversion :-
    writeln('Testing SQL Conversion (Pure Difference List Pattern v2)...'),
    writeln(''),

    % Test 1: Basic TOP to LIMIT conversion
    convert_sql(
        "SELECT TOP 10 name FROM users",
        Out1
    ),
    format('Test 1: ~w~n', [Out1]),
    (Out1 == "SELECT name FROM users LIMIT 10" ->
        writeln('  ✓ PASS') ; writeln('  ✗ FAIL')),
    writeln(''),

    % Test 2: With WHERE clause
    convert_sql(
        "SELECT TOP 5 name FROM users WHERE active = 1",
        Out2
    ),
    format('Test 2: ~w~n', [Out2]),
    (Out2 == "SELECT name FROM users WHERE active = 1 LIMIT 5" ->
        writeln('  ✓ PASS') ; writeln('  ✗ FAIL')),
    writeln(''),

    % Test 3: Without TOP clause
    convert_sql(
        "SELECT name FROM users WHERE id = 42",
        Out3
    ),
    format('Test 3: ~w~n', [Out3]),
    (Out3 == "SELECT name FROM users WHERE id = 42" ->
        writeln('  ✓ PASS') ; writeln('  ✗ FAIL')),
    writeln(''),

    % Test 4: Multiple fields
    convert_sql(
        "SELECT TOP 20 id, name, email FROM customers",
        Out4
    ),
    format('Test 4: ~w~n', [Out4]),
    (Out4 == "SELECT id, name, email FROM customers LIMIT 20" ->
        writeln('  ✓ PASS') ; writeln('  ✗ FAIL')),
    writeln(''),

    % Test 5: Multiple fields with WHERE
    convert_sql(
        "SELECT TOP 100 id, name, status FROM orders WHERE customer = 999",
        Out5
    ),
    format('Test 5: ~w~n', [Out5]),
    (Out5 == "SELECT id, name, status FROM orders WHERE customer = 999 LIMIT 100" ->
        writeln('  ✓ PASS') ; writeln('  ✗ FAIL')),
    writeln(''),

    writeln('All tests completed!').
