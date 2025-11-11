# DCG Patterns Index

Comprehensive DCG patterns for parsing in SWI-Prolog, organized by category. See [SKILL.md](../SKILL.md) for core philosophy and quick reference.

## Pattern Categories

### [Basic Patterns](basic.md)
Core DCG parsing patterns for everyday use:
- Tab-Separated Fields and delimiters
- C Identifiers and token parsing
- **Partial Consumption with phrase/3** (NEW) - Testing prefix matches and boundaries
- Quantifier naming conventions (?, *, +)
- DCG string syntax and code handling
- Disjunction formatting for readability
- Conditional control flow (multiple clauses vs if-then-else)
- Equality operators in guards (== vs =)
- Rest of line parsing
- String/Atom/Codes conversions
- Lookahead and backtracking control
- Bounds checking for peek operations
- Avoiding redundant peek

### [Accumulator Patterns](accumulators.md)
Patterns for stateful parsing with accumulators:
- Accumulator-based parsing fundamentals
- Multiple accumulator threading
- Accumulator naming conventions (Acc0/Acc)
- memberchk/2 vs member/2 for deterministic checking
- Accumulator placement at end of argument list
- **Three-Clause Error Handling for Line Parsers** (NEW) - Robust handling of empty/valid/malformed lines

### [Difference List Patterns](difference-lists.md)
Advanced patterns for efficient list building and transformation:
- Pure streaming transformation (continuous flow)
- Output accumulation with reordering (O(1) buffered emission)
- Hole0-Hole naming convention
- _dl suffix convention for composability
- Sentinel values for optional elements
- Template expansion and SQL translation examples

### [Position Tracking Patterns](position-tracking.md)
Patterns for tracking parse positions for debugging and error reporting:
- Byte-oriented tracking (absolute offset)
- Line-oriented tracking (line + column)
- Character-level DCG composition
- succ/2 for incrementing positions
- Position accumulator placement and naming
- Incremental variable usage (Line0, Line1, Line2...)

### [Optimization Patterns](optimization.md)
Patterns for performance and efficiency:
- DCG forward accumulation instead of append/3
- DCG forward accumulation to avoid reverse/2
- Just-in-time clause indexing
- Tail recursion and cuts

### [File I/O Integration](file-io.md)
Patterns for integrating DCG parsers with file operations:
- Using phrase_from_file/2
- **Direct File Parsing with phrase_from_file/2** (NEW) - Single-pass parsing with accumulator threading
- Using phrase/2 with read_string
- Using catch/3 for error handling
- Stream management with call_cleanup/2
- **Single-Pass Stream Processing** (NEW) - Efficient stream parsing from external sources

## Quick Reference by Use Case

**Parsing delimited data (TSV, CSV):**
- [Tab-Separated Fields](basic.md#pattern-tab-separated-fields) - Basic
- [Position Tracking](position-tracking.md) - Track line/column

**Building complex data structures:**
- [Accumulator Patterns](accumulators.md) - State during parsing
- [Difference Lists](difference-lists.md) - Efficient list building

**Format conversion and transformation:**
- [Difference Lists - Streaming](difference-lists.md#pattern-difference-lists-for-pure-streaming-transformation) - Template expansion
- [Difference Lists - Reordering](difference-lists.md#pattern-difference-lists-for-output-accumulation-with-reordering) - SQL translation

**Error reporting and debugging:**
- [Position Tracking](position-tracking.md) - Line and offset tracking
- [Partial Parsing](basic.md#pattern-partial-consumption-with-phrase3) - Find parse failures
- [Three-Clause Error Handling](accumulators.md#pattern-three-clause-error-handling-for-line-parsers) - Robust line parsing

**Performance optimization:**
- [Optimization Patterns](optimization.md) - Forward accumulation, indexing
- [Accumulator Patterns](accumulators.md) - Efficient state management
- [Single-Pass Stream Processing](file-io.md#pattern-single-pass-stream-processing) - Efficient stream parsing

**File parsing:**
- [Direct File Parsing](file-io.md#pattern-direct-file-parsing-with-phrase_from_file2) - Single-pass with accumulators
- [Stream Processing](file-io.md#pattern-single-pass-stream-processing) - Parse external command output
- [Three-Clause Error Handling](accumulators.md#pattern-three-clause-error-handling-for-line-parsers) - Robust line parsing

## See Also

- [SKILL.md](../SKILL.md) - Core philosophy and essential patterns
- [EXAMPLES.md](../EXAMPLES.md) - Complete working examples
- [ERROR_HANDLING.md](../ERROR_HANDLING.md) - Error handling strategies
- [TESTING.md](../TESTING.md) - Testing and debugging techniques
- [REFERENCE.md](../REFERENCE.md) - Performance and common mistakes