# Difference List Patterns

Advanced patterns for efficient list building and transformation using difference lists. See [../SKILL.md](../SKILL.md) for core philosophy and [index.md](index.md) for all pattern categories.

## Overview

Difference lists enable O(1) append operations and efficient streaming transformations. This document covers two main patterns:

1. **Pure Streaming Transformation** - Continuous flow with minimal buffering
2. **Output Accumulation with Reordering** - Efficient reordering with O(1) buffered emission

## Pattern: Difference Lists for Pure Streaming Transformation

**IMPORTANT**: Use explicit difference lists for TRUE streaming transformations where input flows continuously to output with minimal buffering.

This is the PUREST form of difference list output accumulation where:
- **Each input character is processed exactly once**
- **Output flows continuously through difference list**
- **No reordering** - input order ≈ output order
- **Minimal buffering** - only what's necessary for lookahead
- **True O(n)** - single pass, no backtracking

### The Core Pattern

```prolog
% Stream input directly to output through difference list

% Handle special pattern
expand_stream(Hole0-Hole) -->
    special_pattern,
    !,
    process_special(Hole0-Hole1),
    expand_stream(Hole1-Hole).

% Stream character directly
expand_stream(Hole0-Hole) -->
    [C],
    !,
    { Hole0 = [C|Hole1] },
    expand_stream(Hole1-Hole).

% End of input
expand_stream(Hole-Hole) --> [].
```

### Complete Working Example

Template Variable Expansion ([examples/template_expansion_dcg.pl](../examples/template_expansion_dcg.pl)):

**Input:**  `"Hello ${name}, you are ${age} years old"`
**Bindings:** `[name-"John", age-"25"]`
**Output:** `"Hello John, you are 25 years old"`

```prolog
expand_template(TemplateString, Bindings, OutputString) :-
    string_codes(TemplateString, TemplateCodes),
    phrase(expand_stream(Bindings, OutputCodes-[]), TemplateCodes),
    string_codes(OutputString, OutputCodes).

% Main streaming loop - pure continuous flow

% Found variable - parse and expand
expand_stream(Bindings, Hole0-Hole) -->
    `${`,
    !,
    expand_variable(Bindings, Hole0-Hole1),
    expand_stream(Bindings, Hole1-Hole).

% Regular character - stream directly to output
expand_stream(Bindings, Hole0-Hole) -->
    [C],
    !,
    { Hole0 = [C|Hole1] },
    expand_stream(Bindings, Hole1-Hole).

% End of input
expand_stream(_, Hole-Hole) --> [].

% Parse variable name and expand it
expand_variable(Bindings, Hole0-Hole) -->
    variable_name(VarNameCodes),
    `}`,
    !,
    {
        atom_codes(VarName, VarNameCodes),
        expand_variable_value(VarName, Bindings, VarNameCodes, Hole0-Hole)
    }.

% Helper: Found in bindings - stream replacement to output
expand_variable_value(VarName, Bindings, _, Hole0-Hole) :-
    memberchk(VarName-Value, Bindings),
    !,
    string_codes(Value, ValueCodes),
    add_codes_dl(ValueCodes, Hole0-Hole).

% Helper: Not found - stream original ${varname} to output
expand_variable_value(_, _, VarNameCodes, Hole0-Hole) :-
    Hole0 = [0'$, 0'{|Hole1],
    add_codes_dl(VarNameCodes, Hole1-Hole2),
    Hole2 = [0'}|Hole].

% Helper: Convert code list to difference list (NO append/3)
add_codes_dl([], Hole-Hole).
add_codes_dl([C|Cs], [C|More]-Hole) :-
    add_codes_dl(Cs, More-Hole).

% Parse variable name (minimal buffering for lookup)
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
```

**Usage:**
```prolog
?- expand_template("Hello ${name}, you are ${age} years old",
                   [name-"John", age-"25"], Out).
Out = "Hello John, you are 25 years old".
```

### Why This Is Pure Streaming

1. **Character flow**: `H → e → l → l → o →  → $ → {...var...} → , ...`
2. **Direct streaming**: Regular characters go straight to output: `Out = [C|Out1]`
3. **Minimal buffering**: Only variable name buffered (bounded, necessary for lookup)
4. **No reordering**: Output order matches input order
5. **Single pass**: Each character touched exactly once

### Key Benefits

1. **True O(n)**: Each character processed exactly once
2. **Continuous flow**: Characters stream through without accumulation
3. **Memory efficient**: No intermediate structures
4. **Predictable performance**: Linear in input size
5. **Elegant composition**: Clear streaming pattern

### When to Use Pure Streaming

- ✅ Format conversions that preserve structure (comment style changes)
- ✅ Text transformations (escaping, case conversion)
- ✅ Template expansion with in-place replacement
- ✅ Any transformation where output order ≈ input order
- ❌ Transformations requiring reordering (use next pattern)
- ❌ Complex restructuring (use parsing + transformation phases)

## Pattern: Difference Lists for Output Accumulation with Reordering

**IMPORTANT**: Use explicit difference lists when building output that requires **reordering** or **buffering** parsed elements. Use the **`_dl` suffix convention** for all rules that emit via difference lists, and **buffer elements AS difference lists** for O(1) emission.

This pattern is for transformations where:
- **Input structure is reordered** in output
- **Parsed elements are buffered AS difference lists** for later O(1) emission
- **Multiple segments combined** efficiently with uniform `_dl` rules
- **ALL emission rules** use `_dl` suffix for composability

**⚠️ NOTE**: This is NOT a pure streaming pattern! Characters may be buffered and reordered.

### The Core Pattern with `_dl` Suffix

```prolog
% Parse and reorder - buffer AS difference list for O(1) emission!
convert_dl(Hole0-Hole) -->
    parse_part_A_dl(BufferedDL),    % Parse and buffer AS difference list!
    parse_part_B_dl(Hole0-Hole1),   % Output part B first
    parse_part_C_dl(Hole1-Hole2),   % Then part C
    { BufferedDL = Hole2-Hole }.    % Emit buffered - O(1) unification!
```

### Complete Working Example

SQL Dialect Translation ([examples/sql_reordering_dcg_v2.pl](../examples/sql_reordering_dcg_v2.pl)):

**Input:**  `"SELECT TOP 10 name FROM users WHERE active = 1"`
**Output:** `"SELECT name FROM users WHERE active = 1 LIMIT 10"`

**KEY INNOVATION**: All rules use `_dl` suffix, buffered elements stored AS difference lists for O(1) emission!

```prolog
convert_sql(InputSQL, OutputSQL) :-
    string_codes(InputSQL, InputCodes),
    phrase(convert_statement_dl(OutputCodes-[]), InputCodes),
    string_codes(OutputSQL, OutputCodes).

% Main conversion - all _dl rules for uniform composition
convert_statement_dl(Hole0-Hole) -->
    `SELECT`, 'whites+',
    top_clause_dl(TopDL),           % Buffer TOP AS difference list!
    'whites*',
    convert_fields_dl(Hole0-Hole1), % 1: Emit "SELECT fields"
    'whites+',
    convert_from_dl(Hole1-Hole2),   % 2: Emit " FROM table"
    (
      'whites+',
      convert_where_dl(Hole2-Hole3), !
    ;
      { Hole3 = Hole2 }
    ),
    add_limit_dl(TopDL, Hole3-Hole). % 4: Emit buffered - O(1)!

% Parse TOP - return AS difference list (not closed list!)
top_clause_dl(DigitsDL) -->
    `TOP`, 'whites+',
    'digits_dl+'(DigitsDL),         % DigitsDL = [0'1,0'0|Hole]-Hole
    !.
top_clause_dl(none) --> [].

% Emit LIMIT clause - O(1) unification!
add_limit_dl(none, Hole-Hole) --> [].
add_limit_dl(TopDL, Hole0-Hole) -->
    { TopDL \= none },
    literal_space_dl(Hole0-Hole1),
    keyword_limit_dl(Hole1-Hole2),
    literal_space_dl(Hole2-Hole3),
    { TopDL = Hole3-Hole }.         % O(1) unification! No traversal!

% Keyword emission DCGs (no input parsing, just emit)
keyword_select_dl([0'S,0'E,0'L,0'E,0'C,0'T|Hole]-Hole) --> [].
keyword_limit_dl([0'L,0'I,0'M,0'I,0'T|Hole]-Hole) --> [].
literal_space_dl([0' |Hole]-Hole) --> [].

% Parse identifier from input, emit to output difference list
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

% Parse digits, emit via difference list (for buffering!)
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
```

**Usage:**
```prolog
?- convert_sql("SELECT TOP 10 name FROM users WHERE active = 1", Out).
Out = "SELECT name FROM users WHERE active = 1 LIMIT 10".
```

### The `_dl` Suffix Pattern Innovation

**KEY INSIGHT**: Use the `_dl` suffix for ALL rules that emit via difference list. This creates a uniform, composable pattern where buffered elements are stored AS difference lists for O(1) emission.

**IMPORTANT NAMING CONVENTION**: The `_dl` suffix comes **before** quantifiers:
- ✅ `'digits_dl+'` - "difference list version of digits, one or more"
- ✅ `'digits_dl*'` - "difference list version of digits, zero or more"
- ✅ `'identifier_rest_dl*'` - "difference list version of identifier_rest, zero or more"
- ❌ `'digits+_dl'` - WRONG: confusing, suggests "digits plus something called dl"

**Pattern**: `base_dl` + quantifier (`+`, `*`, `?`)

### Traditional vs Improved Approach

**Traditional Approach (requires O(n) traversal):**
```prolog
% Parse and return closed list
top_clause(Digits) -->
    'digits+'(Digits).              % Digits = [0'1, 0'0] (closed list)

% Later: Must traverse to emit
add_limit(Digits, Hole0-Hole) -->
    {
        Hole0 = [0' ,0'L,0'I,0'M,0'I,0'T,0' |LimitStart],
        add_codes_dl(Digits, LimitStart-Hole)  % O(n) traversal!
    }.
```

**Improved Approach with `_dl` Suffix (O(1) emission):**
```prolog
% Parse and return AS difference list
top_clause_dl(DigitsDL) -->
    'digits_dl+'(DigitsDL).         % DigitsDL = [0'1,0'0|Hole]-Hole (DL!)

% Later: Just unify - O(1)!
add_limit_dl(TopDL, Hole0-Hole) -->
    literal_space_dl(Hole0-Hole1),
    keyword_limit_dl(Hole1-Hole2),
    literal_space_dl(Hole2-Hole3),
    { TopDL = Hole3-Hole }.         % O(1) unification! Just connect holes!
```

**Why This Works:**
```prolog
% TopDL is stored as: [0'1, 0'0 | Hole]-Hole
% When emitting at Hole3-Hole:
%   Unify: [0'1, 0'0 | Hole]-Hole = Hole3-Hole
%   Result: Hole3 = [0'1, 0'0 | Hole], variables unified
% No list traversal - pure pointer manipulation!
```

### Benefits of `_dl` Suffix Pattern

1. **O(1) Buffered Emission**: Emitting stored elements is unification, not traversal
2. **Uniform Composition**: All `_dl` rules have consistent `Hole0-Hole` signature
3. **Clear Naming**: `_dl` suffix + Hole terminology explicitly marks difference lists
4. **Clause Indexing**: Consistent argument patterns enable just-in-time indexing
5. **No Helper Needed**: No `add_codes_dl/2` helper for buffered elements

### Common `_dl` Rule Patterns

```prolog
% Keyword emission (no input parsing, just emit to output)
keyword_select_dl([0'S,0'E,0'L,0'E,0'C,0'T|Hole]-Hole) --> [].

% Literal emission (no input parsing)
literal_space_dl([0' |Hole]-Hole) --> [].
literal_comma_dl([0',,Hole]-Hole) --> [].

% Parse from input AND emit to output simultaneously
identifier_dl(Hole0-Hole) -->
    [C],                            % Parse from input
    { Hole0 = [C|Hole1] },          % Emit to output DL
    more_dl(Hole1-Hole).            % Recurse

% Base case - close difference list
base_dl(Hole-Hole) --> [].
```

### The "Magic" of Difference List Threading

```prolog
% Hole0-Hole1 represents a "list with a hole":
% Hole0 = [part1_item1, part1_item2, ... | Hole1]
%                                          ^^^^^
%                                          The "hole"

% Hole1 becomes the start of the next segment:
% Hole1 = [part2_item1, part2_item2, ... | Hole2]
%                                          ^^^^^
%                                          New hole

% Creates ONE continuous list:
% Hole0 = [part1_item1, part1_item2, part2_item1, part2_item2, ... | Hole]
```

### Key Characteristics

1. **`_dl` suffix pattern**: All emission rules consistently named
2. **Reordering**: TOP parsed at position 2, output at position 5
3. **Buffered AS difference lists**: TopDL = `[0'1,0'0|Hole]-Hole`, not `[0'1,0'0]`
4. **O(1) emission**: `{ TopDL = Hole3-Hole }` is pure unification
5. **No re-processing**: Digits kept as codes, never converted

### Key Benefits

1. **O(1) buffered emission**: No traversal when emitting stored elements
2. **True O(n) end-to-end**: Input parsing + output emission both O(n)
3. **Uniform composition**: All `_dl` rules have consistent signature
4. **Flexible reordering**: Parse in one order, output in another
5. **Just-in-time indexing**: Consistent patterns enable clause indexing
6. **No helper predicates**: No `add_codes_dl/2` needed for buffered elements

### When to Use Reordering Pattern

- ✅ Converting between formats with structural differences
- ✅ Building output with reordered components
- ✅ Combining many segments efficiently
- ✅ Any situation requiring buffering of parsed elements
- ❌ Pure streaming transformations (use previous pattern)
- ❌ Complex multi-pass transformations (consider separate phases)

### Trade-offs

- ✅ O(1) emission of buffered elements (vs O(n) traditional approach)
- ✅ Uniform composable pattern with `_dl` suffix
- ✅ Bounded buffering (not full document)
- ⚠️ Not pure streaming - reordering breaks continuous flow
- ⚠️ Buffered elements must fit in memory
- ⚠️ Requires discipline to use `_dl` suffix consistently

## Naming Convention: Hole0-Hole Pattern

**IMPORTANT**: Use consistent `Hole0-Hole` naming throughout difference list DCG code for clarity and pedagogy.

### The Pattern

Difference list variables should follow the "Hole" naming convention with numbered threading that matches the traditional accumulator pattern (`Acc0`, `Acc1`, `Acc`):

```prolog
% DCG signature uses Hole0-Hole
rule_dl(Hole0-Hole) -->
    part_A_dl(Hole0-Hole1),    % First: Hole0 → Hole1
    part_B_dl(Hole1-Hole2),    % Second: Hole1 → Hole2
    part_C_dl(Hole2-Hole).     % Final: Hole2 → Hole (closes the chain)

% Base case: Hole-Hole (no numbering, same variable twice)
base_case_dl(Hole-Hole) --> [].

% Single step emission: Hole0-Hole1
single_step_dl(Hole0-Hole1) -->
    [C],
    { Hole0 = [C|Hole1] }.
```

### Why This Convention

1. **Pedagogical Clarity**: "Hole" explicitly represents the unbound tail of a difference list
2. **Matches Accumulator Pattern**: `Hole0`, `Hole1`, `Hole2`, `Hole` mirrors `Acc0`, `Acc1`, `Acc2`, `Acc`
3. **Threading Visibility**: Numbered holes show data flow through the DCG chain
4. **Consistent with `_dl` Suffix**: Both naming conventions reinforce difference list usage

### Hole Numbering Rules

1. **Start with `Hole0`**: First argument in sequence is `Hole0-Hole`
2. **Number intermediate holes**: Each step increments: `Hole0-Hole1`, `Hole1-Hole2`, etc.
3. **End with `Hole`**: Final variable is unnumbered `Hole` (like `Acc` in accumulator pattern)
4. **Base case uses `Hole-Hole`**: When no numbering needed, use same variable name twice

### Complete Example

```prolog
% Top-level: OutputCodes-[] initializes the difference list
convert_sql(InputSQL, OutputSQL) :-
    string_codes(InputSQL, InputCodes),
    phrase(convert_statement_dl(OutputCodes-[]), InputCodes),
    string_codes(OutputSQL, OutputCodes).

% Main conversion: Hole0-Hole with numbered threading
convert_statement_dl(Hole0-Hole) -->
    keyword_select_dl(Hole0-Hole1),     % Emit "SELECT"
    literal_space_dl(Hole1-Hole2),      % Emit " "
    field_list_dl(Hole2-Hole3),         % Emit fields
    from_clause_dl(Hole3-Hole).         % Emit FROM clause

% Keyword emission: Hole-Hole (base case pattern)
keyword_select_dl([0'S,0'E,0'L,0'E,0'C,0'T|Hole]-Hole) --> [].

% Literal emission: Hole-Hole (base case pattern)
literal_space_dl([0' |Hole]-Hole) --> [].
```

### Benefits

- Makes difference list "magic" explicit and learnable
- Distinguishes difference list code from traditional DCG code
- Facilitates code review and maintenance
- Consistent with Prolog community conventions for accumulators

## Pattern: Sentinel Values for Optional Elements

**IMPORTANT**: Use sentinel values (like `none`) for optional parsed elements instead of empty difference lists or unbound variables.

When parsing optional elements that may or may not be present, use a sentinel value to represent absence. This makes code clearer and easier to work with.

### Problem with Empty Difference Lists

```prolog
% Hard to work with - awkward structure matching
top_clause_dl([]-[]) --> [].

% Later: Complex pattern matching and guards
add_limit_dl([]-[], Hole-Hole) --> [].
add_limit_dl(TopDL, Hole0-Hole) -->
    { TopDL \= ([]-[]) },  % Awkward structural guard
    ...
```

### Solution with Sentinel Value

```prolog
% Clean and explicit
top_clause_dl(none) --> [].

% Later: Simple pattern matching
add_limit_dl(none, Hole-Hole) --> [].
add_limit_dl(TopDL, Hole0-Hole) -->
    { TopDL \= none },  % Simple guard
    ...
```

### When to Use Different Sentinels

**1. Simple `none` (most common):**
Use when only one type of absence in the given context.

```prolog
% Context (argument position) distinguishes meaning
top_clause_dl(none) --> [].
where_clause_dl(none) --> [].
order_by_clause_dl(none) --> [].

% Later usage is unambiguous
add_limit_dl(TopDL, ...) -->        % TopDL is none or difference list
    ...
add_where_dl(WhereDL, ...) -->      % WhereDL is none or difference list
    ...
```

**2. Namespaced Sentinels (for clarity/debugging):**
Use when multiple optional elements in same structure or when debugging needs clarity.

```prolog
% Namespace by clause type - explicit in traces
top_clause_dl(top(none)) --> [].
where_clause_dl(where(none)) --> [].
order_by_clause_dl(order_by(none)) --> [].

% Later: Clearer what's absent
add_limit_dl(top(none), Hole-Hole) --> [].  % Explicit: TOP is absent
```

**3. Descriptive Atoms (distinguishing absence reasons):**
Use when need to distinguish different absence scenarios.

```prolog
% Different reasons for absence
top_clause_dl(absent) --> [].           % Not present in input
top_clause_dl(empty) -->                % Present but empty value
    `TOP`, 'whites+', 'whites*'.
top_clause_dl(default(10)) -->          % Use default value
    { DefaultLimit = 10 }.
```

**4. Compound Terms (with metadata):**
Use when absence carries additional information.

```prolog
% Error with details
optional_clause_dl(error(parse_failed, Line, Msg)) --> ...

% Default with provenance
optional_value_dl(default(value(10), from(config))) --> ...
```

### Benefits of Sentinel Pattern

1. **Clearer Semantics**: `none` explicitly means "optional element not present"
2. **Easier Pattern Matching**: Direct match on atom, not complex structure
3. **Simpler Guards**: `TopDL \= none` more readable than structural checks
4. **Better Debugging**: Seeing `none` in trace clearer than unbound variables
5. **Type-Like Checking**: Acts as tagged union (value is `none` OR actual data)
6. **Prevents Errors**: Can't accidentally unify with partial difference list

### Guidelines

- ✅ Use simple `none` when context distinguishes meaning (most cases)
- ✅ Use namespaced `clause(none)` when need clarity in debugging
- ✅ Use descriptive atoms when distinguishing absence reasons
- ✅ Use compound terms when carrying metadata about absence
- ❌ Avoid empty difference lists `[]-[]` for optional elements
- ❌ Avoid leaving variables unbound to represent absence

### Example from SQL Translation

```prolog
% TOP is optional - use none sentinel
top_clause_dl(DigitsDL) -->
    `TOP`, 'whites+',
    'digits_dl+'(DigitsDL),
    !.
top_clause_dl(none) --> [].  % Clean: TOP not present

% Later: Simple to handle
add_limit_dl(none, Hole-Hole) --> [].  % No LIMIT when no TOP
add_limit_dl(TopDL, Hole0-Hole) -->    % Add LIMIT when TOP present
    { TopDL \= none },
    literal_space_dl(Hole0-Hole1),
    keyword_limit_dl(Hole1-Hole2),
    literal_space_dl(Hole2-Hole3),
    { TopDL = Hole3-Hole }.  % O(1) unification
```

## Guidelines for Both Patterns

1. **Use `_dl` suffix** for all rules that emit via difference list
2. **Use `Hole0-Hole` naming** consistently in DCG signatures for pedagogy
3. Initialize with `Output-[]` at top level
4. Thread through operations as `Hole0-Hole1`, `Hole1-Hole2`, etc.
5. Base case unifies tail with remainder: `Hole-Hole`
6. **Buffer elements AS difference lists** for O(1) emission (reordering pattern)
7. Keep parsed elements as codes (no unnecessary conversions)
8. Choose pattern based on whether reordering is needed
9. **Use sentinel values** (like `none`) for optional elements

## Related Patterns

- Combines with **DCG Forward Accumulation** (build result lists)
- Complements **String/Atom/Codes Conversions** (parse as codes, build as codes)
- Works with **Multiple Accumulators** when tracking position + building output
- Uses **Sentinel Values for Optional Elements** for cleaner optional parsing

## See Also

- [accumulators.md](accumulators.md) - Accumulator patterns for state management
- [optimization.md](optimization.md) - Forward accumulation patterns
- [position-tracking.md](position-tracking.md) - Position tracking with accumulators
- [basic.md](basic.md) - String/codes conversions