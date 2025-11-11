# DCG Patterns Library

Comprehensive DCG patterns for parsing in SWI-Prolog. See [SKILL.md](SKILL.md) for core philosophy and quick reference.

## Pattern Organization

The patterns have been organized into focused, manageable files for better progressive disclosure. See the [patterns directory](patterns/) for all pattern files.

### Quick Links

- **[Pattern Index](patterns/index.md)** - Complete catalog of all patterns with navigation
- **[Basic Patterns](patterns/basic.md)** - Core DCG parsing patterns
- **[Accumulator Patterns](patterns/accumulators.md)** - Stateful parsing with accumulators
- **[Difference List Patterns](patterns/difference-lists.md)** - Efficient list building and transformation
- **[Position Tracking Patterns](patterns/position-tracking.md)** - Position tracking for debugging
- **[Optimization Patterns](patterns/optimization.md)** - Performance and efficiency
- **[File I/O Integration](patterns/file-io.md)** - File operations and error handling

## Quick Reference by Use Case

**Parsing delimited data (TSV, CSV):**
- [Tab-Separated Fields](patterns/basic.md#pattern-tab-separated-fields)
- [Position Tracking](patterns/position-tracking.md)

**Building complex data structures:**
- [Accumulator Patterns](patterns/accumulators.md)
- [Difference Lists](patterns/difference-lists.md)

**Format conversion and transformation:**
- [Difference Lists - Streaming](patterns/difference-lists.md#pattern-difference-lists-for-pure-streaming-transformation)
- [Difference Lists - Reordering](patterns/difference-lists.md#pattern-difference-lists-for-output-accumulation-with-reordering)

**Error reporting and debugging:**
- [Position Tracking](patterns/position-tracking.md)
- [Partial Parsing](patterns/basic.md#pattern-partial-parsing-with-phrase3)

**Performance optimization:**
- [Optimization Patterns](patterns/optimization.md)
- [Accumulator Patterns](patterns/accumulators.md)

## See Also

- [SKILL.md](SKILL.md) - Core philosophy and essential patterns
- [EXAMPLES.md](EXAMPLES.md) - Complete working examples
- [ERROR_HANDLING.md](ERROR_HANDLING.md) - Error handling strategies
- [TESTING.md](TESTING.md) - Testing and debugging techniques
- [REFERENCE.md](REFERENCE.md) - Performance and common mistakes