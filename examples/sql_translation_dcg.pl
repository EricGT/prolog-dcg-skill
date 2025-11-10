%% SQL Dialect Translation Example
%% Demonstrates DCG parsing patterns with difference list output accumulation
%%
%% Converts SQL Server SELECT TOP syntax to PostgreSQL SELECT LIMIT syntax
%% Input:  "SELECT TOP 10 name, age FROM users WHERE active = 1"
%% Output: "SELECT name, age FROM users WHERE active = 1 LIMIT 10"

:- module(sql_translation, [
    convert_sql/2,
    test_sql_conversion/0
]).

%% Main conversion entry point
%% convert_sql(+InputSQL, -OutputSQL)
%% Converts SQL Server syntax to PostgreSQL syntax
convert_sql(InputSQL, OutputSQL) :-
    string_codes(InputSQL, InputCodes),
    phrase(convert_statement(OutputCodes-[]), InputCodes),
    string_codes(OutputSQL, OutputCodes).

%% convert_statement(-Output-OutputRest)//
%% Main conversion - difference list threaded through multiple parts
%% Pattern: Difference list for output accumulation
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
%% The difference list Out-OutRest gets unified with "SELECT <fields>" where
%% Out starts with "SELECT " and OutRest is where the next part will attach
convert_fields(Out-OutRest) -->
    {
        % Build "SELECT " prefix as code list
        Out = [0'S, 0'E, 0'L, 0'E, 0'C, 0'T, 0' |FieldsStart]
    },
    field_list(FieldsStart-OutRest).

%% field_list(-Output-OutputRest)//
%% Parse comma-separated field list and build output
%% Pattern: DCG forward accumulation with difference lists
field_list(Out-OutRest) -->
    identifier(FieldCodes),
    {
        % Attach field codes to output, Rest is where next part goes
        append(FieldCodes, Rest, Out)
    },
    (
        'whites*', `,`, 'whites*',
        !,
        {
            % Add ", " separator
            Rest = [0',, 0' |NextField]
        },
        field_list(NextField-OutRest)
    ;
        {
            % No more fields, Rest becomes the final tail
            Rest = OutRest
        }
    ).

%% convert_from(-Output-OutputRest)//
%% Convert FROM clause
convert_from(Out-OutRest) -->
    `FROM`,
    'whites+',
    identifier(TableCodes),
    {
        % Build " FROM <table>"
        Out = [0' , 0'F, 0'R, 0'O, 0'M, 0' |TableStart],
        append(TableCodes, OutRest, TableStart)
    }.

%% convert_where(-Output-OutputRest)//
%% Convert WHERE clause with condition
convert_where(Out-OutRest) -->
    `WHERE`,
    'whites+',
    condition(CondCodes),
    {
        % Build " WHERE <condition>"
        Out = [0' , 0'W, 0'H, 0'E, 0'R, 0'E, 0' |CondStart],
        append(CondCodes, OutRest, CondStart)
    }.

%% add_limit(+TopN, -Output-OutputRest)//
%% Add LIMIT clause if TOP was present
add_limit(none, Rest-Rest) --> [].
add_limit(N, Out-OutRest) -->
    {
        % Build " LIMIT <n>"
        Out = [0' , 0'L, 0'I, 0'M, 0'I, 0'T, 0' |LimitStart],
        number_codes(N, NCodes),
        append(NCodes, OutRest, LimitStart)
    }.

%% condition(-ConditionCodes)//
%% Parse simple equality condition (simplified for demonstration)
%% Pattern: Parse as codes, work with codes, return codes
condition(CondCodes) -->
    identifier(FieldCodes),
    'whites*',
    [0'=],  % Equal sign
    'whites*',
    value(ValueCodes),
    {
        % Build "field = value" as code list
        append(FieldCodes, [0' , 0'=, 0' |ValueCodes], CondCodes)
    }.

%% identifier(-Codes)//
%% Parse SQL identifier (table name, field name, etc.)
%% Pattern: C-style identifiers, parse as codes
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
%% Pattern: Peek for choosing parse path
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
%% Pattern: Quantifier naming convention with +
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
%% Required whitespace (one or more whitespace chars)
%% Pattern: Using code_type/2 for character classification
'whites+' -->
    [C],
    { code_type(C, space) },
    'whites*'.

%% whites*//
%% Optional whitespace (zero or more whitespace chars)
'whites*' -->
    [C],
    { code_type(C, space) },
    !,
    'whites*'.
'whites*' --> [].

%% Test cases
test_sql_conversion :-
    writeln('Testing SQL conversion...'),

    % Test 1: Basic TOP to LIMIT conversion
    convert_sql(
        "SELECT TOP 10 name FROM users",
        Out1
    ),
    format('Test 1: ~w~n', [Out1]),
    (Out1 == "SELECT name FROM users LIMIT 10" -> writeln('  PASS') ; writeln('  FAIL')),

    % Test 2: With WHERE clause
    convert_sql(
        "SELECT TOP 5 name FROM users WHERE active = 1",
        Out2
    ),
    format('Test 2: ~w~n', [Out2]),
    (Out2 == "SELECT name FROM users WHERE active = 1 LIMIT 5" -> writeln('  PASS') ; writeln('  FAIL')),

    % Test 3: Without TOP clause
    convert_sql(
        "SELECT name FROM users WHERE id = 42",
        Out3
    ),
    format('Test 3: ~w~n', [Out3]),
    (Out3 == "SELECT name FROM users WHERE id = 42" -> writeln('  PASS') ; writeln('  FAIL')),

    % Test 4: Multiple fields
    convert_sql(
        "SELECT TOP 20 id, name, email FROM customers",
        Out4
    ),
    format('Test 4: ~w~n', [Out4]),
    (Out4 == "SELECT id, name, email FROM customers LIMIT 20" -> writeln('  PASS') ; writeln('  FAIL')),

    writeln('All tests completed!').
