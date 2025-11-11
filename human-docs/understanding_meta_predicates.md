# Understanding Meta-Predicates and call/N

**This is a supplementary human-readable tutorial. It provides detailed explanations of concepts mentioned in the main skill documentation.**

## Introduction

Meta-predicates and `call/N` are powerful Prolog features that enable higher-order programming - passing predicates as arguments to other predicates. This tutorial breaks down how they work with concrete examples and execution traces.

## What is a Meta-Predicate?

A meta-predicate is a predicate that takes another predicate as an argument. The `:- meta_predicate` directive tells Prolog how to interpret these predicate arguments.

### Meta-Predicate Mode Indicators

```prolog
:- meta_predicate ancestor(2, ?, ?).
```

**Mode indicators**:
- `2` - An argument that is a predicate taking 2 arguments
- `1` - An argument that is a predicate taking 1 argument
- `0` - An argument that is a goal (predicate with 0 arguments)
- `:` - A module-qualified goal
- `?` - A normal argument (not a predicate)
- `+` - An input argument
- `-` - An output argument

**Common examples**:
```prolog
:- meta_predicate findall(?, 0, ?).        % Goal with 0 extra args
:- meta_predicate maplist(1, ?).           % Predicate with 1 arg
:- meta_predicate filter(2, ?, ?).         % Predicate with 2 args
:- meta_predicate transitive_closure(2, ?, ?).  % Predicate with 2 args
```

## Understanding call/N

`call/N` is a built-in that invokes a predicate with additional arguments.

### Basic Examples

```prolog
% call/1 - just invoke a goal
?- call(true).
true.

% call/2 - add 1 argument
?- call(atom, hello).
true.
% Equivalent to: atom(hello)

% call/3 - add 2 arguments
?- call(append, [1,2], [3,4], X).
X = [1, 2, 3, 4].
% Equivalent to: append([1,2], [3,4], X)
```

### How call/2 Works with Predicates

When you write `call(Pred, Arg1)`, Prolog:
1. Takes the predicate name/functor from `Pred`
2. Adds `Arg1` as the first argument
3. Invokes the resulting goal

**Example**:
```prolog
?- Pred = atom, call(Pred, hello).
% Step 1: Pred = atom
% Step 2: Add argument 'hello'
% Step 3: Execute: atom(hello)
true.
```

## Transitive Closure: Complete Walkthrough

Let's understand Jan's elegant parameterized relation solution with a complete execution trace.

### The Code

```prolog
:- meta_predicate transitive_closure(2, ?, ?).

% Base case: direct relation
transitive_closure(Rel, A, B) :-
    call(Rel, A, B).

% Recursive case: indirect relation through intermediate
transitive_closure(Rel, A, C) :-
    call(Rel, A, B),
    transitive_closure(Rel, B, C).
```

### The Facts

```prolog
% Family tree
parent(tom, bob).
parent(bob, ann).
parent(ann, sue).
parent(sue, joe).
```

### Example 1: Direct Relation

**Query**: `transitive_closure(parent, tom, bob)`

```prolog
Step 1: Try first clause
  transitive_closure(parent, tom, bob) :-
      call(parent, tom, bob).

Step 2: Expand call/3
  call(parent, tom, bob)
  → parent(tom, bob)    % Adds tom and bob as arguments to parent

Step 3: Check facts
  parent(tom, bob).      % ✓ This fact exists!

Result: true
```

**Key insight**: `call(parent, tom, bob)` becomes `parent(tom, bob)`

### Example 2: Indirect Relation (One Step)

**Query**: `transitive_closure(parent, tom, ann)`

```prolog
Step 1: Try first clause
  transitive_closure(parent, tom, ann) :-
      call(parent, tom, ann).

Step 2: Expand call/3
  call(parent, tom, ann)
  → parent(tom, ann)     % Check if direct parent

Step 3: Check facts
  parent(tom, ann).      % ✗ This fact does NOT exist

Step 4: Backtrack, try second clause
  transitive_closure(parent, tom, ann) :-
      call(parent, tom, B),        % Find someone tom is parent of
      transitive_closure(parent, B, ann).  % Then check if B reaches ann

Step 5: Expand first call/3
  call(parent, tom, B)
  → parent(tom, B)       % Find B where tom is parent of B

Step 6: Unify with facts
  parent(tom, bob).      % ✓ B = bob

Step 7: Now solve the recursive goal
  transitive_closure(parent, bob, ann)

Step 8: Try first clause of transitive_closure
  transitive_closure(parent, bob, ann) :-
      call(parent, bob, ann).

Step 9: Expand call/3
  call(parent, bob, ann)
  → parent(bob, ann)     % Check if direct parent

Step 10: Check facts
  parent(bob, ann).      % ✓ This fact exists!

Result: true
Path: tom → bob → ann
```

### Example 3: Deep Transitive Relation

**Query**: `transitive_closure(parent, tom, joe)`

```prolog
Step 1: Try first clause
  call(parent, tom, joe)
  → parent(tom, joe).    % ✗ Does not exist

Step 2: Try second clause
  call(parent, tom, B1),
  transitive_closure(parent, B1, joe).

Step 3: Find B1
  parent(tom, B1).       % B1 = bob

Step 4: Recursive call
  transitive_closure(parent, bob, joe)

Step 5: Try direct
  call(parent, bob, joe)
  → parent(bob, joe).    # ✗ Does not exist

Step 6: Try indirect
  call(parent, bob, B2),
  transitive_closure(parent, B2, joe).

Step 7: Find B2
  parent(bob, B2).       % B2 = ann

Step 8: Recursive call
  transitive_closure(parent, ann, joe)

Step 9: Try direct
  call(parent, ann, joe)
  → parent(ann, joe).    % ✗ Does not exist

Step 10: Try indirect
  call(parent, ann, B3),
  transitive_closure(parent, B3, joe).

Step 11: Find B3
  parent(ann, B3).       % B3 = sue

Step 12: Recursive call
  transitive_closure(parent, sue, joe)

Step 13: Try direct
  call(parent, sue, joe)
  → parent(sue, joe).    % ✓ Exists!

Result: true
Path: tom → bob → ann → sue → joe
```

## Side-by-Side Comparison: Direct Call vs call/2

### Direct Call Version

```prolog
% Hardcoded to use parent/2
ancestor(A, B) :-
    parent(A, B).

ancestor(A, C) :-
    parent(A, B),
    ancestor(B, C).

% Query
?- ancestor(tom, joe).
```

**Execution trace**:
```prolog
ancestor(tom, joe)
→ Try parent(tom, joe)         % ✗ Fails
→ Try parent(tom, B), ancestor(B, joe)
  → parent(tom, bob)            % ✓ B = bob
  → ancestor(bob, joe)
    → parent(bob, joe)          % ✗ Fails
    → parent(bob, ann), ancestor(ann, joe)
      → parent(bob, ann)        % ✓
      → ancestor(ann, joe)
        → parent(ann, joe)      % ✗ Fails
        → parent(ann, sue), ancestor(sue, joe)
          → parent(ann, sue)    % ✓
          → ancestor(sue, joe)
            → parent(sue, joe)  # ✓ SUCCESS!
```

### Parameterized Version with call/2

```prolog
% Generic - works with ANY binary relation
transitive_closure(Rel, A, B) :-
    call(Rel, A, B).

transitive_closure(Rel, A, C) :-
    call(Rel, A, B),
    transitive_closure(Rel, B, C).

% Query
?- transitive_closure(parent, tom, joe).
```

**Execution trace** (showing call expansion):
```prolog
transitive_closure(parent, tom, joe)
→ Try call(parent, tom, joe)         % Expands to parent(tom, joe) - ✗ Fails
→ Try call(parent, tom, B), transitive_closure(parent, B, joe)
  → call(parent, tom, B)              % Expands to parent(tom, B)
    → parent(tom, bob)                % ✓ B = bob
  → transitive_closure(parent, bob, joe)
    → call(parent, bob, joe)          % Expands to parent(bob, joe) - ✗
    → call(parent, bob, B), transitive_closure(parent, B, joe)
      → call(parent, bob, B)          % Expands to parent(bob, B)
        → parent(bob, ann)            % ✓ B = ann
      → transitive_closure(parent, ann, joe)
        → call(parent, ann, joe)      % ✗
        → call(parent, ann, B), transitive_closure(parent, B, joe)
          → call(parent, ann, B)      % Expands to parent(ann, B)
            → parent(ann, sue)        % ✓ B = sue
          → transitive_closure(parent, sue, joe)
            → call(parent, sue, joe)  # Expands to parent(sue, joe) - ✓ SUCCESS!
```

**Key difference**: The parameterized version carries `Rel` (the relation predicate) through the recursion and uses `call/3` to invoke it with the actual arguments.

## Why This is Powerful

### Reusability

```prolog
% Same code works for multiple relations!

% Ancestor relationships
?- transitive_closure(parent, tom, X).
X = bob ;
X = ann ;
X = sue ;
X = joe.

% Road network reachability
road(city_a, city_b).
road(city_b, city_c).
road(city_c, city_d).

?- transitive_closure(road, city_a, city_d).
true.

% Manager hierarchy
manages(alice, bob).
manages(bob, charlie).
manages(charlie, diana).

?- transitive_closure(manages, alice, diana).
true.
```

### Testing Benefits

```prolog
% In tests, define local predicates
:- begin_tests(assembly).

% Test-specific facts
parent(bike, frame).
parent(bike, wheel).
parent(wheel, spokes).

% Use the generic transitive_closure
test(bike_contains_spokes) :-
    transitive_closure(parent, bike, spokes).

test(bike_contains_frame) :-
    transitive_closure(parent, bike, frame).

:- end_tests(assembly).
```

**Why this works**: `call(parent, bike, spokes)` will find the `parent/2` predicate in the test module's context, so test-local facts are naturally accessible!

## Meta-Predicate Declaration Explained

```prolog
:- meta_predicate transitive_closure(2, ?, ?).
```

This tells Prolog:
- **First argument** (`2`): A binary relation (predicate taking 2 arguments)
- **Second argument** (`?`): A normal term
- **Third argument** (`?`): A normal term

### Why This Matters for Modules

When you call a meta-predicate from another module, Prolog needs to know which arguments are predicates so it can resolve them in the correct module context.

**Without meta_predicate declaration**:
```prolog
% In module A
?- transitive_closure(parent, X, Y).
% Error! Where is 'parent' defined? Module A? Module B?
```

**With meta_predicate declaration**:
```prolog
:- meta_predicate transitive_closure(2, ?, ?).

% In module A
?- transitive_closure(parent, X, Y).
% Success! Prolog knows to look for parent/2 in module A
```

## Common Patterns

### Pattern 1: Filtering with call/2

```prolog
:- meta_predicate filter(1, ?, ?).

filter(_, [], []).
filter(Pred, [H|T], [H|Filtered]) :-
    call(Pred, H),
    !,
    filter(Pred, T, Filtered).
filter(Pred, [_|T], Filtered) :-
    filter(Pred, T, Filtered).

% Usage
is_positive(X) :- X > 0.

?- filter(is_positive, [1, -2, 3, -4, 5], Result).
Result = [1, 3, 5].
```

**How it works**:
- `call(Pred, H)` expands to `is_positive(1)`, `is_positive(-2)`, etc.
- Each element is tested with the provided predicate

### Pattern 2: Mapping with call/3

```prolog
:- meta_predicate my_maplist(2, ?, ?).

my_maplist(_, [], []).
my_maplist(Pred, [H1|T1], [H2|T2]) :-
    call(Pred, H1, H2),
    my_maplist(Pred, T1, T2).

% Usage
double(X, Y) :- Y is X * 2.

?- my_maplist(double, [1, 2, 3], Result).
Result = [2, 4, 6].
```

**How it works**:
- `call(Pred, H1, H2)` expands to `double(1, H2)`, `double(2, H2)`, etc.
- First list element becomes first arg, second list element becomes second arg

### Pattern 3: Folding with call/4

```prolog
:- meta_predicate foldl(3, ?, ?, ?).

foldl(_, [], Acc, Acc).
foldl(Pred, [H|T], Acc0, Acc) :-
    call(Pred, H, Acc0, Acc1),
    foldl(Pred, T, Acc1, Acc).

% Usage
sum(X, Acc, NewAcc) :- NewAcc is Acc + X.

?- foldl(sum, [1, 2, 3, 4], 0, Total).
Total = 10.
```

**How it works**:
- `call(Pred, H, Acc0, Acc1)` expands to `sum(1, 0, Acc1)`, then `sum(2, 1, Acc1)`, etc.
- Accumulator is threaded through each call

## Module Context and call/N

A critical aspect: `call/N` resolves predicates in the **caller's module context**, not the callee's.

```prolog
% In module 'main'
:- module(main, [test_closure/0]).

:- meta_predicate transitive_closure(2, ?, ?).

transitive_closure(Rel, A, B) :-
    call(Rel, A, B).
transitive_closure(Rel, A, C) :-
    call(Rel, A, B),
    transitive_closure(Rel, B, C).

% Test module
:- begin_tests(ancestry).

% Local facts - only visible in this test module
parent(a, b).
parent(b, c).

test(local_facts) :-
    % When we call transitive_closure from here,
    % call(parent, A, B) resolves 'parent' in THIS module context!
    transitive_closure(parent, a, c).

:- end_tests(ancestry).
```

**Why this works**: The meta_predicate declaration tells Prolog to resolve `parent` in the calling context (the test module), not in the `main` module where `transitive_closure` is defined.

## Summary: Direct Call vs call/2

| Aspect | Direct Call | call/2 with Meta-Predicate |
|--------|-------------|----------------------------|
| **Flexibility** | Hardcoded to one relation | Works with any relation |
| **Reusability** | Must write new predicate for each relation | One predicate handles all relations |
| **Testing** | Hard to test with different data | Easy - pass test-local predicates |
| **Module Context** | Fixed to defining module | Resolves in caller's module |
| **Clarity** | More explicit | Requires understanding call/N |
| **Performance** | Slightly faster (no indirection) | Minimal overhead |

## When to Use Each

**Use direct calls when**:
- ✅ You only need to work with one specific relation
- ✅ Performance is absolutely critical
- ✅ Code clarity is more important than flexibility

**Use parameterized relations with call/N when**:
- ✅ You need to work with multiple different relations
- ✅ You want highly reusable generic predicates
- ✅ You need to test with different fact sets
- ✅ You're building library predicates

## Practice Exercises

### Exercise 1: Reflexive Transitive Closure

Modify transitive_closure to include reflexive property (A relates to itself):

```prolog
:- meta_predicate reflexive_transitive_closure(2, ?, ?).

reflexive_transitive_closure(_Rel, A, A).  % Add this clause!
reflexive_transitive_closure(Rel, A, B) :-
    call(Rel, A, B).
reflexive_transitive_closure(Rel, A, C) :-
    call(Rel, A, B),
    reflexive_transitive_closure(Rel, B, C).
```

**Test it**:
```prolog
?- reflexive_transitive_closure(parent, tom, tom).
true.  % Now succeeds!
```

### Exercise 2: Symmetric Closure

Create a symmetric closure (if A→B then B→A):

```prolog
:- meta_predicate symmetric(2, ?, ?).

symmetric(Rel, A, B) :-
    call(Rel, A, B).
symmetric(Rel, A, B) :-
    call(Rel, B, A).  % Also check reverse direction
```

**Test it**:
```prolog
sibling(tom, bob).

?- symmetric(sibling, bob, tom).
true.  % Now works both ways!
```

### Exercise 3: Path Finding

Extend transitive_closure to collect the path:

```prolog
:- meta_predicate path(2, ?, ?, ?).

path(Rel, A, B, [A, B]) :-
    call(Rel, A, B).
path(Rel, A, C, [A|Path]) :-
    call(Rel, A, B),
    path(Rel, B, C, Path).
```

**Test it**:
```prolog
?- path(parent, tom, joe, Path).
Path = [tom, bob, ann, sue, joe].
```

## References

- SWI-Prolog meta_predicate documentation: https://www.swi-prolog.org/pldoc/man?section=metapred
- call/1..8 documentation: https://www.swi-prolog.org/pldoc/doc_for?object=call/2
- Higher-order predicates: https://www.swi-prolog.org/pldoc/man?section=builtin-higher-order

## Conclusion

Meta-predicates and `call/N` enable powerful generic programming in Prolog. While they add a layer of abstraction, the benefits of reusability, testability, and flexibility often outweigh the slight learning curve. Jan Wielemaker's recommendation to use parameterized relations reflects decades of Prolog best practices!
