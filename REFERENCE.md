# DCG Reference

Advanced topics including performance optimization, common mistakes, and debugging. See [SKILL.md](SKILL.md) for core concepts.

## Performance Considerations

### ✅ Efficient Patterns

```prolog
% Tail recursive - good
parse_many([X|Xs]) -->
    parse_one(X), !,
    parse_many(Xs).
parse_many([]) --> [].

% Cut after successful parse - good
parse_field(F) -->
    identifier(F), !.
parse_field(default) --> [].

% Character class checking - good
is_digit(C) :- code_type(C, digit).
```

### ❌ Inefficient Patterns

```prolog
% Non-tail recursive - avoid
parse_many([X|Xs]) -->
    parse_one(X),
    parse_many(Xs), !.  % ! too late

% No cut, excessive backtracking - avoid
parse_field(F) -->
    (
      identifier(F)
    ;
      default_field(F)
    ).

% Repeated conversions - avoid
parse_field(F) -->
    codes(Cs),
    { atom_codes(Temp, Cs),
      atom_string(Temp, F) }.  % Why convert twice?
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

## Common Mistakes to Avoid

### 1. Converting to String and Back

```prolog
% ❌ AVOID
field(F) -->
    codes(FieldCodes),
    { atom_codes(A, FieldCodes),
      atom_string(A, S),
      string_codes(S, NewFieldCodes),
      ... }.

% ✅ PREFER
field(F) -->
    codes(FieldCodes),
    { atom_codes(F, FieldCodes) }.
```

### 2. Using split_string Before DCG

```prolog
% ❌ AVOID
parse_input(Input, Result) :-
    split_string(Input, "\n", "", Lines),
    phrase(parse_lines(Result), Lines).  % Lines are strings, not codes!

% ✅ PREFER
parse_input(Input, Result) :-
    string_codes(Input, Codes),
    phrase(parse_lines(Result), Codes).
```

### 3. Mixing Paradigms

```prolog
% ❌ AVOID - mixing string ops with DCG
parse_line(Line, Result) -->
    { split_string(Line, "\t", "", [F1, F2]) },  % String op in DCG
    parse_field(F2, Result).

% ✅ PREFER - pure DCG
parse_line(Result) -->
    field(F1), `\t`,
    parse_field(Result).
```

## Debugging Support for Generated Code

When generating DCG parsers, include debugging support to help users diagnose issues.

### Pattern: Inserting gtrace for Interactive Debugging

**IMPORTANT**: When users report parse errors, suggest inserting `gtrace/0` at strategic points. Note that GUI debugger requires launching SWI-Prolog with GUI support.

```prolog
% Insert gtrace before suspected error location
parse_problematic_line(Result, Offset, Line) -->
    field(F1),
    { gtrace },  % Drop into debugger here
    `\t`,
    field(F2),
    { Result = data(F1, F2) }.

% Conditional debugging based on position
parse_line(Result, Offset, Line) -->
    { (Line == 42 -> gtrace ; true) },  % Debug only line 42
    field(F1), `\t`,
    field(F2),
    { Result = data(F1, F2) }.

% Conditional debugging on specific values
parse_field(Field, Offset, Line) -->
    field_codes(FieldCodes),
    { atom_codes(Field, FieldCodes),
      (Field == problematic_symbol -> gtrace ; true) },  % Debug specific symbol
    ...
```

**To suggest to users:**
- On Windows: Launch with `"C:\Program Files\swipl\bin\swipl-win.exe" --win_app`
- Insert `gtrace` before suspected error location in code
- Use conditional gtrace for specific lines/values
- Remove gtrace once issue is identified

### Pattern: Understanding DCG Expansion

**IMPORTANT**: When helping users debug DCGs, show the expanded Prolog code to clarify what the DCG actually does.

```prolog
% DCG notation - what user writes
field(Field) -->
    field_codes(FieldCodes),
    { atom_codes(Field, FieldCodes) }.

field_codes([C|Cs]) -->
    [C], { C \= 9 }, !,  % Not tab
    field_codes(Cs).
field_codes([]) --> [].

% Expanded Prolog - what actually runs
field(Field, S0, S) :-
    field_codes(FieldCodes, S0, S1),
    atom_codes(Field, FieldCodes),
    S1 = S.

field_codes([C|Cs], [C|S0], S) :-
    C \= 9, !,  % Not tab
    field_codes(Cs, S0, S).
field_codes([], S, S).
```

**To show users:**
- DCG rule `-->` becomes regular predicate with 2 extra arguments
- Input/output difference list threaded through: `S0` (input), `S` (output)
- Terminal `[C]` matches and consumes from input list
- Goals in `{...}` are regular Prolog, don't touch difference list
- Understanding expansion helps debug unexpected behavior

**Use `listing/1` to see actual expanded code:**
```prolog
?- listing(field/3).
?- listing(field_codes/3).
```

### Common DCG Pitfalls to Watch For

**1. Forgetting Cut in Recursive Rules**
```prolog
% ❌ BAD: No cut, creates choice points
parse_many([X|Xs]) -->
    parse_one(X),
    parse_many(Xs).
parse_many([]) --> [].

% ✅ GOOD: Cut after successful parse
parse_many([X|Xs]) -->
    parse_one(X), !,
    parse_many(Xs).
parse_many([]) --> [].
```

**2. Using Regular Prolog Unification on Difference List**
```prolog
% ❌ BAD: Tries to unify S0 = [C|S1] incorrectly
bad_consume(C, [C|S1], S1).  % Not a DCG!

% ✅ GOOD: Use DCG notation
good_consume(C) --> [C].
```

**3. Not Consuming Input in Escape Hatches**
```prolog
% ❌ BAD: Escapes DCG but needs to consume
parse_item(Item) -->
    { parse_complex(Item) }.  % Doesn't consume input!

% ✅ GOOD: Explicitly manage difference list
parse_item(Item, S0, S) :-
    parse_complex(Item, S0, S).  % Or use DCG inside
```

**4. Mixing String Operations with DCG**
```prolog
% ❌ BAD: Splitting string then trying to parse
parse_line(Result, Line) -->
    { split_string(Line, "\t", "", Parts) },  % Wrong!
    ...

% ✅ GOOD: Parse delimiters in DCG
parse_line(Result) -->
    field(F1), `\t`,
    field(F2).
```

**5. Accumulator Position Wrong**
```prolog
% ❌ BAD: Accumulator not at end
parse_line(SeenIn, SeenOut, Result) -->  % Wrong position
    ...

% ✅ GOOD: Accumulators at end, in/out pairs
parse_line(Result, SeenIn, SeenOut) -->  % Correct position
    ...
```

**6. Not Threading Position Through All Rules**
```prolog
% ❌ BAD: Position tracking lost
parse_line(Record, LineIn, LineOut) -->
    parse_fields(Fields),  % Lost offset tracking!
    { Record = record(Fields, LineIn) }.

% ✅ GOOD: Thread position through
parse_line(Record, LineIn, LineOut, OffsetIn, OffsetOut) -->
    parse_fields(Fields, LineIn, LineIn, OffsetIn, OffsetOut),
    { Record = record(Fields, pos(LineIn, OffsetIn)),
      succ(LineIn, LineOut) }.
```

**7. Building Lists Backward Without Reverse**
```prolog
% ❌ BAD: Build list backward, then need reverse/2 at higher level
parse_file(Items) -->
    parse_items_backward([], ItemsBackward),
    { reverse(ItemsBackward, Items) }.  % Need reverse!

parse_items_backward(Acc, Result) -->
    parse_item(Item), !,
    parse_items_backward([Item|Acc], Result).  % Builds backward
parse_items_backward(Items, Items) --> [].

% ✅ GOOD: Build list forward with [Item|Items] - no reverse needed
parse_file(Items) -->
    parse_items(Items).

parse_items([Item|Items]) -->
    parse_item(Item), !,
    parse_items(Items).
parse_items([]) --> [].
```

**8. EOS Check Not at End**
```prolog
% ❌ BAD: Checks eos too early
parse_file([L|Ls]) -->
    eos, !,  % Wrong! Checks before attempting parse
    line(L),
    parse_file(Ls).

% ✅ GOOD: EOS check at base case
parse_file([L|Ls]) -->
    line(L), !,
    parse_file(Ls).
parse_file([]) --> eos.
```

### Debugging Strategy When Helping Users

1. **Ask for error message and position**: What line/offset failed?
2. **Show expanded DCG code**: Help user understand what DCG does
3. **Suggest gtrace insertion**: Add gtrace before error position
4. **Check common pitfalls**: Review list above
5. **Verify position threading**: Ensure offsets/lines thread correctly
6. **Test with phrase/3**: Use remainder to see where parse stopped
7. **Add debug/3 calls**: Strategic debug output for parse trace

## Additional Resources

For comprehensive documentation including:
- All DCG patterns (30+ patterns)
- Position tracking details
- File I/O integration
- Error handling strategies

See [PATTERNS.md](PATTERNS.md) and the original dcg-parsing-skill.md file.
