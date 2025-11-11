# Optimization Patterns

Patterns for performance and efficiency in DCG parsing. See [../SKILL.md](../SKILL.md) for core philosophy and [index.md](index.md) for all pattern categories.

## Pattern: DCG Forward Accumulation Instead of append/3

**IMPORTANT**: Use DCG forward accumulation for list building. Avoid `append/3` which is O(n) per call.

```prolog
% ❌ AVOID: append/3 in recursive building (O(n²))
build_list([X|Xs], Acc0, Result) :-
    process(X, Item),
    append(Acc0, [Item], Acc1),  % O(n) each time!
    build_list(Xs, Acc1, Result).
build_list([], Result, Result).

% ✅ PREFER: DCG with forward accumulation
parse_items([Item|Items]) -->
    parse_item(Item), !,  % Natural O(1) cons with [Item|Items]
    parse_items(Items).
parse_items([]) --> [].

% Or just use append once if unavoidable
combine_once(Part1, Part2, Result) :-
    append(Part1, Part2, Result).  % OK for single use
```

**Guidelines:**
- Use DCG forward accumulation with `[Item|Items]` pattern for incremental building
- DCGs naturally build lists forward - prefer DCG notation
- Avoid multiple `append/3` calls in loops/recursion
- Single `append/3` at end is acceptable if needed

## Pattern: DCG Forward Accumulation to Avoid reverse/2

**IMPORTANT**: Use DCG forward accumulation to build lists in correct order instead of using reverse/2.

```prolog
% ❌ AVOID: Building list backwards, then reversing
parse_lines(Lines, Result) :-
    parse_lines_acc(Lines, [], Rev),
    reverse(Rev, Result).

parse_lines_acc([], Acc, Acc).
parse_lines_acc([L|Ls], Acc, Result) :-
    parse_line(L, Item),
    parse_lines_acc(Ls, [Item|Acc], Result).

% ✅ PREFER: DCG with forward accumulation using unification
parse_lines([Item|Items]) -->
    parse_line(Item), !,
    parse_lines(Items).
parse_lines([]) --> [].
```

**Benefits:**
- O(n) instead of O(n) + O(n) for reverse
- More efficient memory usage
- Natural DCG style builds lists forward automatically
- Cleaner code without explicit reverse/2

## Pattern: Just-in-Time Clause Indexing

**IMPORTANT**: Prefer clause ordering that enables just-in-time indexing when it makes sense.

SWI-Prolog automatically indexes on the first argument. Order clauses to take advantage of this:

```prolog
% ✅ PREFER: Most specific cases first, indexed on first argument
parse_line([], Seen, Seen) --> [].
parse_line([Item|Items], SeenIn, SeenOut) -->
    parse_item(Item, SeenIn, Seen1), !,
    parse_line(Items, Seen1, SeenOut).

% Pattern matching on structure
handle_def(def(File, Symbol, Line, Context)) -->
    % Handle definition
    ...
handle_def(call(File, Caller, Line, Context, Callee)) -->
    % Handle call
    ...
handle_def(include(File, Header)) -->
    % Handle include
    ...

% DCG alternatives indexed by lookahead
parse_declaration -->
    `typedef`, !, parse_typedef.
parse_declaration -->
    `struct`, !, parse_struct.
parse_declaration -->
    `enum`, !, parse_enum.
parse_declaration -->
    `#define`, !, parse_macro.
```

**Benefits:**
- Prolog indexes on first argument automatically
- Deterministic choice points with cuts after specific patterns
- Faster lookup for specific cases
- Clearer pattern matching intent

## Efficient DCG Patterns

### 1. Tail Recursion

```prolog
% ✅ GOOD: Tail recursive
parse_many([X|Xs]) -->
    parse_one(X), !,
    parse_many(Xs).
parse_many([]) --> [].

% ❌ BAD: Non-tail recursive
parse_many([X|Xs]) -->
    parse_one(X),
    parse_many(Xs), !.  % Cut too late, builds stack
```

### 2. Strategic Cuts

```prolog
% ✅ GOOD: Cut after successful parse
parse_field(F) -->
    identifier(F), !.
parse_field(default) --> [].

% ❌ BAD: No cut, excessive backtracking
parse_field(F) -->
    (
      identifier(F)
    ;
      default_field(F)
    ).
```

### 3. Character Class Checking

```prolog
% ✅ GOOD: Use code_type/2
is_digit(C) :- code_type(C, digit).

digit(D) --> [D], { code_type(D, digit) }.

% ❌ AVOID: Manual range checking (less readable)
digit(D) --> [D], { D >= 48, D =< 57 }.
```

### 4. Avoid Repeated Conversions

```prolog
% ❌ BAD: Multiple conversions
parse_field(F) -->
    codes(Cs),
    { atom_codes(Temp, Cs),
      atom_string(Temp, F) }.  % Why convert twice?

% ✅ GOOD: Single conversion
parse_field(F) -->
    codes(Cs),
    { string_codes(F, Cs) }.  % Direct conversion
```

## Performance Anti-Patterns to Avoid

### 1. Multiple Passes Over Data

```prolog
% ❌ AVOID: Multiple passes
parse_file(File, Result) :-
    read_file_to_string(File, Content, []),          % Pass 1: read
    split_string(Content, "\n", "\r", Lines),        % Pass 2: split lines
    maplist(process_line, Lines, Results).           % Pass 3: process

process_line(Line, Result) :-
    split_string(Line, "\t", "", [F1, F2, F3]),      % Pass 4: split fields
    string_codes(F3, Codes),                          % Pass 5: convert to codes
    phrase(parse_field(Data), Codes),                 % Pass 6: finally parse with DCG
    Result = data(F1, F2, Data).

% ✅ PREFER: Single pass with DCG
parse_file(File, Result) :-
    phrase_from_file(file_content(Result), File).

file_content([Line|Lines]) -->
    file_line(Line), !,
    file_content(Lines).
file_content([]) --> eos.

file_line(data(F1, F2, Data)) -->
    field(F1), `\t`,
    field(F2), `\t`,
    parse_field(Data),
    line_end.
```

### 2. Building Lists Backwards

```prolog
% ❌ BAD: Build backwards, then reverse
parse_file(Items) -->
    parse_items_backward([], ItemsBackward),
    { reverse(ItemsBackward, Items) }.

parse_items_backward(Acc, Result) -->
    parse_item(Item), !,
    parse_items_backward([Item|Acc], Result).
parse_items_backward(Items, Items) --> [].

% ✅ GOOD: Build forward with DCG
parse_file(Items) -->
    parse_items(Items).

parse_items([Item|Items]) -->
    parse_item(Item), !,
    parse_items(Items).
parse_items([]) --> [].
```

### 3. Non-Deterministic Membership Testing

```prolog
% ❌ BAD: Creates choice points
parse_line(Item, Seen0, Seen) -->
    field(Name),
    { \+ member(Name, Seen0) },  % Creates choice points
    rest_of_line(Data),
    { Item = item(Name, Data),
      Seen = [Name|Seen0] }.

% ✅ GOOD: Deterministic
parse_line(Item, Seen0, Seen) -->
    field(Name),
    { \+ memberchk(Name, Seen0) },  % Deterministic, no choice points
    rest_of_line(Data),
    { Item = item(Name, Data),
      Seen = [Name|Seen0] }.
```

### 4. Mixing String Operations with DCG

```prolog
% ❌ BAD: Mixes paradigms
parse_line(Result, Line) -->
    { split_string(Line, "\t", "", Parts) },  % String op in DCG
    parse_field(F2, Result).

% ✅ GOOD: Pure DCG
parse_line(Result) -->
    field(F1), `\t`,
    parse_field(Result).
```

## Performance Guidelines Summary

### Do These (Fast):
- ✅ Use tail recursion for lists
- ✅ Place cuts strategically after successful parses
- ✅ Use `memberchk/2` for deterministic membership testing
- ✅ Use `code_type/2` for character classification
- ✅ Build lists forward with DCG patterns
- ✅ Single-pass parsing with DCGs
- ✅ Parse as codes, convert once at the end
- ✅ Index on first argument with specific patterns
- ✅ Use `succ/2` for simple +1 operations

### Avoid These (Slow):
- ❌ Non-tail recursion (builds stack)
- ❌ No cuts (excessive backtracking)
- ❌ `member/2` when don't need backtracking
- ❌ Manual character range checking
- ❌ Building lists backwards then reversing
- ❌ Multiple passes over data
- ❌ String splits before DCG parsing
- ❌ Multiple string/atom/code conversions
- ❌ `append/3` in recursive list building
- ❌ Using `is/2` for simple +1 (use `succ/2`)

## Optimization Checklist

When optimizing DCG parsers:

1. **Profile first**: Identify actual bottlenecks, don't guess
2. **Single pass**: Parse data once with DCGs, not multiple string operations
3. **Forward accumulation**: Use DCG `[Item|Items]` pattern, avoid reverse/2 and append/3
4. **Strategic cuts**: Cut after successful parse to prevent backtracking
5. **Deterministic predicates**: Use `memberchk/2`, not `member/2`
6. **Tail recursion**: Ensure recursive DCG rules are tail-recursive
7. **Character codes**: Parse as codes, convert once at end
8. **Clause indexing**: Order clauses to benefit from first-argument indexing
9. **Character primitives**: Reusable single-character DCGs for position tracking
10. **Use succ/2**: For simple +1/-1 operations instead of `is/2`

## Measuring Performance

```prolog
% Time a parse operation
test_performance(File) :-
    time(phrase_from_file(parse_file(Result), File)),
    length(Result, Count),
    format('Parsed ~w items~n', [Count]).

% Compare two approaches
compare_approaches(File) :-
    format('Approach 1 (string splitting):~n'),
    time(parse_with_splitting(File, R1)),

    format('~nApproach 2 (pure DCG):~n'),
    time(parse_with_dcg(File, R2)),

    format('~nResults equal: ~w~n', [R1 == R2]).
```

## Real-World Performance Example

Before optimization (multiple passes):
```prolog
% 6 passes through data, ~15 seconds for 100MB file
process_cscope_output(Stream, Results) :-
    read_string(Stream, _, Content),              % Pass 1
    split_string(Content, "\n", "\r", Lines),     % Pass 2
    maplist(parse_cscope_line, Lines, Results).   % Pass 3

parse_cscope_line(Line, result(File, Sym, Num, Context)) :-
    split_string(Line, " ", "", Parts),           % Pass 4
    Parts = [File, Sym, Num | ContextParts],
    atomics_to_string(ContextParts, " ", Context). % Pass 5
```

After optimization (single pass):
```prolog
% Single pass, ~2 seconds for 100MB file
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
```

**Result**: 7.5x speedup by eliminating intermediate conversions and multiple passes.

## See Also

- [accumulators.md](accumulators.md) - Efficient state management
- [difference-lists.md](difference-lists.md) - O(1) append with difference lists
- [position-tracking.md](position-tracking.md) - Efficient position tracking patterns
- [basic.md](basic.md) - Fundamental efficient patterns