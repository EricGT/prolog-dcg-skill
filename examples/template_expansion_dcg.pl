%% Template Variable Expansion - Pure Streaming Example
%% Demonstrates PURE difference list streaming for output accumulation
%%
%% This is a TRUE O(n) streaming transformation where:
%% - Each input character is processed exactly once
%% - Output flows continuously through difference list
%% - No buffering except minimal variable name accumulation
%% - No reordering of input structure
%%
%% Expands template variables: "Hello ${name}, you are ${age} years old"
%% With bindings: [name-"John", age-"25"]
%% Produces: "Hello John, you are 25 years old"
%%
%% Unknown variables are left as-is: ${unknown} stays ${unknown}

:- module(template_expansion, [
    expand_template/3,
    test_template_expansion/0
]).

%% expand_template(+TemplateString, +Bindings, -OutputString)
%% Main entry point - convert template with variables to expanded output
expand_template(TemplateString, Bindings, OutputString) :-
    string_codes(TemplateString, TemplateCodes),
    phrase(expand_stream(Bindings, OutputCodes-[]), TemplateCodes),
    string_codes(OutputString, OutputCodes).

%% expand_stream(+Bindings, -Output-OutputRest)//
%% Main streaming loop - processes input character by character
%% Pattern: Pure streaming - each character flows through exactly once
expand_stream(Bindings, Out-OutRest) -->
    (
        `${`,
        !,
        % Found variable start - parse and expand
        expand_variable(Bindings, Out-Out1),
        expand_stream(Bindings, Out1-OutRest)
    ;
        [C],
        !,
        % Regular character - stream directly to output
        {
            Out = [C|Out1]
        },
        expand_stream(Bindings, Out1-OutRest)
    ;
        % End of input - close difference list
        {
            Out = OutRest
        }
    ).

%% expand_variable(+Bindings, -Output-OutputRest)//
%% Parse variable name and expand it
%% Pattern: Minimal buffering - only variable name is accumulated
expand_variable(Bindings, Out-OutRest) -->
    variable_name(VarNameCodes),
    `}`,
    !,
    {
        % Look up variable in bindings
        atom_codes(VarName, VarNameCodes),
        (
            memberchk(VarName-Value, Bindings)
        ->
            % Found - stream replacement value to output
            string_codes(Value, ValueCodes),
            add_codes_dl(ValueCodes, Out-OutRest)
        ;
            % Not found - stream original ${varname} to output
            % This shows how to emit literal text when needed
            Out = [0'$, 0'{|Rest1],
            add_codes_dl(VarNameCodes, Rest1-Rest2),
            Rest2 = [0'}|OutRest]
        )
    }.
expand_variable(_Bindings, Out-OutRest) -->
    % No closing } found - treat as literal ${ and continue
    {
        Out = [0'$, 0'{|OutRest]
    }.

%% variable_name(-Codes)//
%% Parse variable name (alphanumeric + underscore)
%% Returns regular code list (minimal buffering for lookup)
variable_name([C|Cs]) -->
    [C],
    {
        code_type(C, alnum)
    ;
        C = 0'_
    },
    !,
    variable_name(Cs).
variable_name([]) --> [].

%% add_codes_dl(+Codes, -DiffList)
%% Convert a code list to difference list representation
%% Pattern: Pure difference list construction - O(n) in length of Codes only
add_codes_dl([], Rest-Rest).
add_codes_dl([C|Cs], [C|More]-Rest) :-
    add_codes_dl(Cs, More-Rest).

%% Test cases
test_template_expansion :-
    writeln('Testing Template Variable Expansion (Pure Streaming)...'),
    writeln(''),

    % Test 1: Basic variable expansion
    expand_template(
        "Hello ${name}!",
        [name-"John"],
        Out1
    ),
    format('Test 1: ~w~n', [Out1]),
    (Out1 == "Hello John!" ->
        writeln('  ✓ PASS') ; writeln('  ✗ FAIL')),
    writeln(''),

    % Test 2: Multiple variables
    expand_template(
        "Hello ${name}, you are ${age} years old",
        [name-"Alice", age-"30"],
        Out2
    ),
    format('Test 2: ~w~n', [Out2]),
    (Out2 == "Hello Alice, you are 30 years old" ->
        writeln('  ✓ PASS') ; writeln('  ✗ FAIL')),
    writeln(''),

    % Test 3: Unknown variable (stays as-is)
    expand_template(
        "Hello ${name}, your ${unknown} is here",
        [name-"Bob"],
        Out3
    ),
    format('Test 3: ~w~n', [Out3]),
    (Out3 == "Hello Bob, your ${unknown} is here" ->
        writeln('  ✓ PASS') ; writeln('  ✗ FAIL')),
    writeln(''),

    % Test 4: No variables
    expand_template(
        "Just plain text",
        [],
        Out4
    ),
    format('Test 4: ~w~n', [Out4]),
    (Out4 == "Just plain text" ->
        writeln('  ✓ PASS') ; writeln('  ✗ FAIL')),
    writeln(''),

    % Test 5: Variable with underscore and numbers
    expand_template(
        "User ${user_id_123} logged in",
        [user_id_123-"admin"],
        Out5
    ),
    format('Test 5: ~w~n', [Out5]),
    (Out5 == "User admin logged in" ->
        writeln('  ✓ PASS') ; writeln('  ✗ FAIL')),
    writeln(''),

    % Test 6: Adjacent variables
    expand_template(
        "${first}${last}",
        [first-"John", last-"Doe"],
        Out6
    ),
    format('Test 6: ~w~n', [Out6]),
    (Out6 == "JohnDoe" ->
        writeln('  ✓ PASS') ; writeln('  ✗ FAIL')),
    writeln(''),

    % Test 7: Incomplete variable (no closing brace)
    expand_template(
        "Hello ${incomplete",
        [incomplete-"value"],
        Out7
    ),
    format('Test 7: ~w~n', [Out7]),
    (Out7 == "Hello ${incomplete" ->
        writeln('  ✓ PASS') ; writeln('  ✗ FAIL')),
    writeln(''),

    writeln('All tests completed!').
