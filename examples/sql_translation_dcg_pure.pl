%% SQL Dialect Translation Example - Pure Difference Lists
%% Demonstrates DCG parsing with difference list output accumulation
%% WITHOUT using append/3 - Pure O(n) difference list threading
%%
%% Converts SQL Server SELECT TOP syntax to PostgreSQL SELECT LIMIT syntax
%% Input:  "SELECT TOP 10 name, age FROM users WHERE active = 1"
%% Output: "SELECT name, age FROM users WHERE active = 1 LIMIT 10"

:- module(sql_translation_pure, [
    convert_sql/2,
    test_sql_conversion/0
]).

%% Main conversion entry point
convert_sql(InputSQL, OutputSQL) :-
    string_codes(InputSQL, InputCodes),
    phrase(convert_statement(OutputCodes-[]), InputCodes),
    string_codes(OutputSQL, OutputCodes).

%% convert_statement(-Output-OutputRest)//
%% Main conversion - difference list threaded through multiple parts
%% NO append/3 calls - pure difference list threading
convert_statement(Out-OutRest) -->
    `SELECT`,
    'whites+',
    top_clause(TopN),           % Parse TOP but don't output yet (reordering)
    'whites*',
    convert_fields(Out-Out1),   % 1st accumulation: "SELECT fields"
    'whites+',
    convert_from(Out1-Out2),    % 2nd accumulation: " FROM table"
    (
        'whites+',
        convert_where(Out2-Out3), % 3rd accumulation: " WHERE condition"
        !
    ;
        { Out3 = Out2 }           % Optional WHERE
    ),
    add_limit(TopN, Out3-OutRest). % 4th accumulation: " LIMIT n"

%% top_clause(-NumberOrNone)//
%% Parse TOP clause but extract the number for later use
top_clause(N) -->
    `TOP`,
    'whites+',
    'digits+'(Digits),
    { number_codes(N, Digits) },
    !.
top_clause(none) --> [].

%% convert_fields(-Output-OutputRest)//
%% Convert SELECT field list - builds output using difference list
%% Pattern: Build prefix, thread remainder through field_list
convert_fields(Out-OutRest) -->
    {
        % Build "SELECT " prefix directly in difference list
        Out = [0'S, 0'E, 0'L, 0'E, 0'C, 0'T, 0' |FieldsStart]
    },
    field_list(FieldsStart-OutRest).

%% field_list(-Output-OutputRest)//
%% Parse comma-separated field list and build output
%% Pattern: Pure difference list - NO append/3
field_list(Out-OutRest) -->
    identifier(FieldCodes),
    {
        % Add field codes to difference list without append
        add_codes_dl(FieldCodes, Out-Rest)
    },
    (
        'whites*', `,`, 'whites*',
        !,
        {
            % Add ", " separator directly
            Rest = [0',, 0' |NextField]
        },
        field_list(NextField-OutRest)
    ;
        {
            % No more fields, close the difference list
            Rest = OutRest
        }
    ).

%% convert_from(-Output-OutputRest)//
%% Convert FROM clause - pure difference list, no append
convert_from(Out-OutRest) -->
    `FROM`,
    'whites+',
    identifier(TableCodes),
    {
        % Build " FROM " prefix and add table name
        Out = [0' , 0'F, 0'R, 0'O, 0'M, 0' |TableStart],
        add_codes_dl(TableCodes, TableStart-OutRest)
    }.

%% convert_where(-Output-OutputRest)//
%% Convert WHERE clause - pure difference list, no append
convert_where(Out-OutRest) -->
    `WHERE`,
    'whites+',
    condition(CondCodes),
    {
        % Build " WHERE " prefix and add condition
        Out = [0' , 0'W, 0'H, 0'E, 0'R, 0'E, 0' |CondStart],
        add_codes_dl(CondCodes, CondStart-OutRest)
    }.

%% add_limit(+TopN, -Output-OutputRest)//
%% Add LIMIT clause if TOP was present - pure difference list, no append
add_limit(none, Rest-Rest) --> [].
add_limit(N, Out-OutRest) -->
    {
        % Build " LIMIT " prefix and add number
        Out = [0' , 0'L, 0'I, 0'M, 0'I, 0'T, 0' |LimitStart],
        number_codes(N, NCodes),
        add_codes_dl(NCodes, LimitStart-OutRest)
    }.

%% condition(-ConditionCodes)//
%% Parse simple equality condition
%% Returns code list (not difference list) - simple enough
condition(CondCodes) -->
    identifier(FieldCodes),
    'whites*',
    [0'=],  % Equal sign
    'whites*',
    value(ValueCodes),
    {
        % Build condition with difference list helper
        add_codes_dl(FieldCodes, CondCodes-Rest1),
        Rest1 = [0' , 0'=, 0' |Rest2],
        add_codes_dl(ValueCodes, Rest2-[])
    }.

%% add_codes_dl(+Codes, -DiffList)
%% Convert a code list to difference list representation
%% This is the KEY helper that replaces append/3
%% Pattern: Pure difference list construction - O(n) in length of Codes only
add_codes_dl([], Rest-Rest).
add_codes_dl([C|Cs], [C|More]-Rest) :-
    add_codes_dl(Cs, More-Rest).

%% identifier(-Codes)//
%% Parse SQL identifier (table name, field name, etc.)
%% Returns regular code list
identifier([First|Rest]) -->
    [First],
    {
        code_type(First, alpha)
    ;
        First = 0'_
    },
    'identifier_rest*'(Rest).

'identifier_rest*'([C|Cs]) -->
    [C],
    {
        code_type(C, alnum)
    ;
        C = 0'_
    },
    !,
    'identifier_rest*'(Cs).
'identifier_rest*'([]) --> [].

%% value(-Codes)//
%% Parse value (number or identifier)
value(Codes) -->
    peek(C),
    { code_type(C, digit) },
    !,
    'digits+'(Codes).
value(Codes) -->
    identifier(Codes).

%% peek(-Code)//
%% Peek at next character without consuming
peek(C), [C] --> [C].

%% digits+(​-Digits)//
%% Parse one or more digits
'digits+'([D|Ds]) -->
    [D],
    { code_type(D, digit) },
    !,
    'digits*'(Ds).

'digits*'([D|Ds]) -->
    [D],
    { code_type(D, digit) },
    !,
    'digits*'(Ds).
'digits*'([]) --> [].

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

%% Test cases
test_sql_conversion :-
    writeln('Testing SQL conversion (Pure Difference Lists)...'),
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
        "SELECT TOP 3 id, name, status FROM orders WHERE customer = 100",
        Out5
    ),
    format('Test 5: ~w~n', [Out5]),
    (Out5 == "SELECT id, name, status FROM orders WHERE customer = 100 LIMIT 3" ->
        writeln('  ✓ PASS') ; writeln('  ✗ FAIL')),
    writeln(''),

    writeln('All tests completed!').
