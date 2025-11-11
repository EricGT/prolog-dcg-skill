# Accumulator Patterns

Patterns for stateful parsing with accumulators. See [../SKILL.md](../SKILL.md) for core philosophy and [index.md](index.md) for all pattern categories.

## Pattern: Accumulator-Based Parsing

**IMPORTANT Accumulator Rules:**
1. **Always use in/out pairs**: `parse(..., Acc0, Acc)`
2. **Place at end of argument list**: Regular args first, then accumulator pairs
3. **Pair accumulators together**: `Line0, Line, Offset0, Offset` (not mixed)
4. **In first, Out second**: Consistent ordering

**IMPORTANT Accumulator Naming Convention:**
- **In variable**: Name ending in `0` (e.g., `Seen0`, `Line0`, `Offset0`)
- **Out variable**: Name without number (e.g., `Seen`, `Line`, `Offset`)
- **Intermediate variables**: Increment number, except final has no number (e.g., `Seen0 → Seen1 → Seen`)
- **Multiple accumulators**: Use consistent numbering (e.g., `Line0, Line, Offset0, Offset`)

**IMPORTANT Head|Tail List Naming Convention:**
- **For descriptive names**: Use singular/plural pairs: `[Item|Items]`, `[Record|Records]`, `[Line|Lines]`
- **For single letters**: Use letter with 's' suffix: `[X|Xs]`, `[C|Cs]`, `[D|Ds]`
- **Avoid**: Generic words like `[Head|Tail]`, `[H|T]` (unless using single letter convention)
- **Benefit**: Self-documenting code that clarifies what the list contains

```prolog
% ❌ AVOID: Wrong accumulator naming and position
parse_lines(Seen, [Item|Items]) -->
    parse_line(Seen, Item), !,
    { update_seen(Seen, Item, Seen1) },
    parse_lines(Seen1, Items).

% ✅ PREFER: Correct naming (Seen0/Seen) and position (at end)
parse_lines([Item|Items], Seen0, Seen) -->
    parse_line(Item, Seen0, Seen1), !,
    parse_lines(Items, Seen1, Seen).
parse_lines([], Seen, Seen) --> [].

% Parse single line with accumulator pair at end
parse_line(Item, Seen0, Seen) -->
    field(Name),
    { \+ memberchk(Name, Seen0) },  % Use memberchk/2, not member/2
    `\t`,
    rest_of_line(Data),
    { Item = item(Name, Data),
      Seen = [Name|Seen0] }. % Build list (DCG forward accumulation is better)
```

## Pattern: Multiple Accumulators

When tracking multiple values (line, offset), keep pairs together at end with consistent numbering:

```prolog
% ✅ CORRECT: Accumulator pairs at end, 0-suffix for input
parse_file(Result, Line0, Line, Offset0, Offset) -->
    parse_line(Item, Line0, Line1, Offset0, Offset1), !,
    parse_file(Rest, Line1, Line, Offset1, Offset),
    { Result = [Item|Rest] }.
parse_file([], Line, Line, Offset, Offset) --> [].

% Parse line: result first, then accumulator pairs
parse_line(Item, Line0, Line, Offset0, Offset) -->
    field(File), `\t`,
    rest_of_line(Content),
    line_end,
    { Item = data(File, pos(Line0, Offset0), Content),
      succ(Line0, Line),
      string_length(Content, Len),
      Offset is Offset0 + Len + 1 }.  % +1 for newline
```

**See [position-tracking.md](position-tracking.md) for comprehensive examples** demonstrating multiple accumulator pairs in real-world parsing scenarios.

## Pattern: memberchk/2 vs member/2

**IMPORTANT**: Prefer `memberchk/2` over `member/2` when you only need to check membership once (don't need backtracking).

```prolog
% ❌ AVOID: member/2 creates unnecessary choice points
parse_line(Item, Seen0, Seen) -->
    field(Name),
    { \+ member(Name, Seen0) },  % Creates choice points
    rest_of_line(Data),
    { Item = item(Name, Data),
      Seen = [Name|Seen0] }.

% ✅ PREFER: memberchk/2 is deterministic
parse_line(Item, Seen0, Seen) -->
    field(Name),
    { \+ memberchk(Name, Seen0) },  % Deterministic, no choice points
    rest_of_line(Data),
    { Item = item(Name, Data),
      Seen = [Name|Seen0] }.

% Check for valid keyword
keyword(Kw) -->
    identifier(Id),
    { memberchk(Id, [if, then, else, while, for]) },  % Deterministic check
    { Kw = Id }.
```

**Guidelines:**
- Use `memberchk/2` for simple membership testing
- Use `member/2` only when you need backtracking over alternatives
- `memberchk/2` stops at first match, more efficient

## Accumulator Placement and Organization

### ✅ CORRECT Pattern

```prolog
% Parse with result first, then accumulator pairs at end
parse_data(Result, Acc0, Acc) -->
    ...

% Multiple accumulators: keep pairs together
parse_data(Result, Line0, Line, Offset0, Offset) -->
    ...

% Ordering: In (0-suffix) before Out (no suffix)
parse_item(Item, Seen0, Seen, Count0, Count) -->
    ...
```

### ❌ INCORRECT Patterns

```prolog
% ❌ WRONG: Accumulators not at end
parse_data(Acc0, Acc, Result) -->
    ...

% ❌ WRONG: Accumulator pairs separated
parse_data(Result, Line0, Offset0, Line, Offset) -->
    ...

% ❌ WRONG: Inconsistent ordering (out before in)
parse_item(Item, Seen, Seen0) -->
    ...
```

## Accumulator Use Cases

### 1. Tracking Seen Items

```prolog
% Track seen identifiers to detect duplicates
parse_unique_items(Items) -->
    parse_unique_items_acc(Items, [], _).

parse_unique_items_acc([Item|Items], Seen0, Seen) -->
    parse_item(Item, Seen0, Seen1), !,
    parse_unique_items_acc(Items, Seen1, Seen).
parse_unique_items_acc([], Seen, Seen) --> [].

parse_item(Item, Seen0, Seen) -->
    identifier(Id),
    {
      memberchk(Id, Seen0)
    ->
      throw(error(duplicate_identifier(Id), context(...)))
    ;
      Item = item(Id),
      Seen = [Id|Seen0]
    }.
```

### 2. Building Context During Parsing

```prolog
% Accumulate context information
parse_with_context(Result, Context0, Context) -->
    parse_header(Header, Context0, Context1),
    parse_body(Body, Context1, Context2),
    parse_footer(Footer, Context2, Context),
    { Result = document(Header, Body, Footer) }.

parse_header(Header, Context0, Context) -->
    field(Title),
    `\n`,
    field(Author),
    { Header = header(Title, Author),
      Context = Context0.put(title, Title).put(author, Author) }.
```

### 3. Counting During Parsing

```prolog
% Count items while parsing
parse_counted_items(Items, Count) -->
    parse_counted_items_acc(Items, 0, Count).

parse_counted_items_acc([Item|Items], Count0, Count) -->
    parse_item(Item), !,
    { succ(Count0, Count1) },
    parse_counted_items_acc(Items, Count1, Count).
parse_counted_items_acc([], Count, Count) --> [].
```

## Common Accumulator Pitfalls

### 1. Forgetting to Thread Through

```prolog
% ❌ BAD: Accumulator not threaded
parse_items([Item|Items], Acc0, Acc) -->
    parse_item(Item, Acc0, Acc1), !,
    parse_items(Items, Acc0, Acc).  % ❌ Should be Acc1

% ✅ GOOD: Properly threaded
parse_items([Item|Items], Acc0, Acc) -->
    parse_item(Item, Acc0, Acc1), !,
    parse_items(Items, Acc1, Acc).  % ✅ Correct
```

### 2. Wrong Variable Naming

```prolog
% ❌ BAD: No 0-suffix for input
parse_line(Item, Seen, SeenOut) -->  % ❌ Should be Seen0
    ...

% ✅ GOOD: Consistent naming
parse_line(Item, Seen0, Seen) -->  % ✅ Correct
    ...
```

### 3. Accumulator in Wrong Position

```prolog
% ❌ BAD: Accumulator not at end
parse_items(Acc0, Acc, [Item|Items]) -->  % ❌ Wrong order
    ...

% ✅ GOOD: Accumulator at end
parse_items([Item|Items], Acc0, Acc) -->  % ✅ Correct
    ...
```

## Pattern: Three-Clause Error Handling for Line Parsers

### Overview

Robust file parsing requires graceful handling of empty lines, valid lines, and malformed lines. This pattern ensures parsing doesn't crash on unexpected input.

### The Three-Clause Structure

```prolog
% Clause 1: Empty/Skip Case
line_parser(none, Acc, Acc) -->
    'whites*',
    newline,
    !.

% Clause 2: Success Case
line_parser(Result, Acc0, Acc) -->
    parse_fields(...),
    newline,
    !,
    { process_and_validate(..., Result, Acc0, Acc) }.

% Clause 3: Fallback Case (catches errors)
line_parser(none, Acc, Acc) -->
    skip_until_newline,
    newline.
```

### Why This Works

1. **Clause 1** handles empty lines explicitly (common case)
2. **Clause 2** processes valid input and updates state
3. **Clause 3** catches malformed input without crashing

The cuts (`!`) prevent backtracking after successful parses, ensuring deterministic behavior.

### Complete Example

```prolog
% Parse file with robust error handling
symbol_file_lines(Defs, Seen0) -->
    symbol_line(Def, Seen0, Seen1),
    !,
    {
        (   Def = def(_, _, _, _)        % Valid definition found
        ->  Defs = [Def|RestDefs]
        ;   Defs = RestDefs,              % Skip invalid/empty
            Seen1 = Seen0                 % No state change
        )
    },
    symbol_file_lines(RestDefs, Seen1).
symbol_file_lines([], _) -->
    [].

% Clause 1: Empty line
symbol_line(none, Seen, Seen) -->
    'whites*',
    newline,
    !.

% Clause 2: Valid line
symbol_line(Def, Seen0, Seen) -->
    field_until_tab(FileCodes), `\t`,
    field_until_tab(ScopeCodes), `\t`,
    field_until_tab(LineNumCodes), `\t`,
    field_until_newline(ContextCodes),
    newline,
    !,
    {
        atom_codes(File, FileCodes),
        atom_codes(Scope, ScopeCodes),
        number_codes(LineNum, LineNumCodes),
        string_codes(Context, ContextCodes),
        process_line(File, Scope, LineNum, Context, Def, Seen0, Seen)
    }.

% Clause 3: Malformed line - skip gracefully
symbol_line(none, Seen, Seen) -->
    skip_until_newline,
    newline.
```

### Benefits

✅ **Robustness**: Handles unexpected input without crashing
✅ **Clarity**: Clear separation of empty/valid/invalid cases
✅ **Maintainability**: Easy to add logging or error collection
✅ **Determinism**: Cuts prevent unwanted backtracking
✅ **Accumulator Safety**: Invalid lines don't corrupt state

### Pattern Variations

**With Error Collection:**

```prolog
% Collect errors instead of silently skipping
symbol_line(error(LineNum, Reason), Seen, Seen) -->
    { get_current_line(LineNum) },
    skip_until_newline,
    newline,
    { Reason = 'Malformed line' }.
```

**With Debug Logging:**

```prolog
% Clause 3 with debug output
symbol_line(none, Seen, Seen) -->
    skip_until_newline,
    newline,
    { debug(parser, 'Skipped malformed line', []) }.
```

### Testing Error Handling

```prolog
:- begin_tests(robust_parsing).

test(empty_line_skipped) :-
    phrase(symbol_line(Result, [], Seen), `\n`),
    Result == none,
    Seen == [].

test(valid_line_parsed) :-
    Input = `file.c\tmain\t10\tint main() {\n`,
    phrase(symbol_line(Result, [], Seen), Input),
    Result = def(_, _, _, _).

test(malformed_line_skipped) :-
    Input = `incomplete\tdata\n`,
    phrase(symbol_line(Result, [], Seen), Input),
    Result == none,
    Seen == [].

:- end_tests(robust_parsing).
```

## Integration with Other Patterns

Accumulators work well with:
- **Position tracking** - Track line/offset alongside other state
- **DCG forward accumulation** - Build result lists while maintaining state
- **Error handling** - Include position and context in errors
- **Difference lists** - Can combine with difference list patterns

See also:
- [position-tracking.md](position-tracking.md) - Position accumulators
- [optimization.md](optimization.md) - Forward accumulation patterns
- [basic.md](basic.md#pattern-equality-operators-in-conditionals-and-guards) - Testing accumulator values

## Summary

**Key Rules:**
1. Use in/out pairs: `Acc0, Acc`
2. Place at end of argument list
3. Input ends in `0`, output has no number
4. Intermediate variables increment: `Acc0 → Acc1 → Acc2 → Acc`
5. Multiple accumulators: keep pairs together in consistent order
6. Use `memberchk/2` for deterministic membership testing
7. Thread accumulators through all DCG rules

**Common Uses:**
- Tracking seen items
- Building context
- Counting during parsing
- Maintaining parse state

**Watch Out For:**
- Forgetting to thread accumulator
- Wrong naming (no 0-suffix for input)
- Accumulators not at end
- Pairs separated or out of order