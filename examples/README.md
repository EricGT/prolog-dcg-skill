# DCG Parsing Examples

Complete working examples demonstrating DCG patterns from the skill documentation. Each file is self-contained and runnable in SWI-Prolog.

## Examples Overview

### 1. Template Expansion - Pure Streaming (⭐ Recommended First Example)

**File**: [template_expansion_dcg.pl](template_expansion_dcg.pl)

**Pattern**: Pure streaming transformation with difference lists

**What it demonstrates**:
- TRUE O(n) streaming where each character flows through exactly once
- Minimal buffering (only variable names for lookup)
- No reordering - output order matches input order
- Pure difference list output accumulation

**Input**: `"Hello ${name}, you are ${age} years old"`
**Bindings**: `[name-"John", age-"25"]`
**Output**: `"Hello John, you are 25 years old"`

**Usage**:
```prolog
?- [template_expansion_dcg].
?- test_template_expansion.
?- expand_template("Hello ${name}!", [name-"World"], Out).
Out = "Hello World!".
```

**Referenced in**: [PATTERNS.md](../PATTERNS.md) - "Pattern: Difference Lists for Pure Streaming Transformation"

---

### 2. SQL Translation v2 - Optimal Reordering (⭐ Recommended for Reordering)

**File**: [sql_reordering_dcg_v2.pl](sql_reordering_dcg_v2.pl)

**Pattern**: Output accumulation with reordering using `_dl` suffix convention

**What it demonstrates**:
- ALL rules use `_dl` suffix for uniform composition
- Buffered elements stored AS difference lists for O(1) emission
- Reordering: TOP parsed at position 2, output at position 5
- True O(n) for both input parsing AND output emission

**Input**: `"SELECT TOP 10 name FROM users WHERE active = 1"`
**Output**: `"SELECT name FROM users WHERE active = 1 LIMIT 10"`

**Usage**:
```prolog
?- [sql_reordering_dcg_v2].
?- test_sql_conversion.
?- convert_sql("SELECT TOP 5 id FROM products", Out).
Out = "SELECT id FROM products LIMIT 5".
```

**Referenced in**: [PATTERNS.md](../PATTERNS.md) - "Pattern: Difference Lists for Output Accumulation with Reordering"

---

### 3. SQL Translation v1 - Reordering Introduction

**File**: [sql_reordering_dcg.pl](sql_reordering_dcg.pl)

**Pattern**: Output accumulation with reordering (earlier version)

**What it demonstrates**:
- Output reordering using difference lists
- Buffering parsed elements for later use
- NOT pure streaming (reordering breaks continuous flow)
- Comparison point for understanding v2 improvements

**Input/Output**: Same as v2
**Difference**: Less optimal - buffered elements as closed lists, not difference lists

**Usage**:
```prolog
?- [sql_reordering_dcg].
?- test_sql_conversion.
```

**Referenced in**: [PATTERNS.md](../PATTERNS.md) - Shows evolution to v2 pattern

---

### 4. SQL Translation - Basic Difference Lists

**File**: [sql_translation_dcg.pl](sql_translation_dcg.pl)

**Pattern**: Basic difference list output accumulation

**What it demonstrates**:
- Fundamental difference list threading
- Output accumulation through multiple parsing stages
- Reordering with difference lists

**Usage**:
```prolog
?- [sql_translation_dcg].
?- test_sql_conversion.
```

---

### 5. SQL Translation Pure - No append/3

**File**: [sql_translation_dcg_pure.pl](sql_translation_dcg_pure.pl)

**Pattern**: Pure difference list threading without append/3

**What it demonstrates**:
- Pure O(n) difference list threading
- Avoiding append/3 for better performance
- Clean separation of parsing and output

**Usage**:
```prolog
?- [sql_translation_dcg_pure].
?- test_sql_conversion.
```

---

## Learning Path

### Beginner
1. Start with **template_expansion_dcg.pl** - clearest example of streaming
2. Understand pure difference list pattern

### Intermediate
3. Study **sql_reordering_dcg.pl** - see how reordering works
4. Compare with **sql_translation_dcg.pl** - different approaches

### Advanced
5. Master **sql_reordering_dcg_v2.pl** - optimal pattern with `_dl` suffix
6. Compare v1 and v2 to understand O(1) emission improvement

---

## Running Examples

All examples include test predicates:

```prolog
% Load any example
?- [template_expansion_dcg].

% Run its tests
?- test_template_expansion.

% Try your own inputs
?- expand_template("Your ${template} here", [template-"example"], Out).
```

---

## Key Concepts Demonstrated

### Difference Lists
- Thread output through multiple operations
- O(1) concatenation vs O(n) for append/3
- Uniform `Hole0-Hole` naming convention

### Streaming vs Reordering
- **Streaming**: Input order ≈ output order, minimal buffering
- **Reordering**: Buffer parsed elements for later output

### Performance Patterns
- **O(1) emission**: Store buffered elements AS difference lists (v2 pattern)
- **O(n) traversal**: Store as closed lists, need helper to emit (v1 pattern)

### Naming Conventions
- `_dl` suffix for all difference list emission rules
- `Hole0-Hole` for difference list variables
- `Rule0, Rule1, Rule2...` for accumulator threading

---

## Related Documentation

- **[PATTERNS.md](../PATTERNS.md)** - Complete pattern library with detailed explanations
- **[EXAMPLES.md](../EXAMPLES.md)** - Additional integrated examples
- **[SKILL.md](../SKILL.md)** - Core DCG philosophy and best practices
- **[TESTING.md](../TESTING.md)** - How to test your DCG parsers

---

## Contributing

These examples are extracted from real parsing projects and demonstrate production-ready patterns. They follow the conventions documented in the skill files.

To add a new example:
1. Follow the established naming conventions
2. Include comprehensive comments
3. Provide a `test_*` predicate
4. Document which pattern it demonstrates
5. Update this README
