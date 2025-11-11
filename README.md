# DCG Parsing Skill for Claude Code

A specialized Claude Code skill that guides Claude in writing efficient, idiomatic SWI-Prolog DCGs (Definite Clause Grammars) following best practices for single-pass parsing, character codes, and pure declarative style.

## Features

- Single-pass parsing patterns
- Character code processing
- Pure declarative style
- Accumulator patterns
- Tab-separated value parsing
- C identifier parsing
- Position tracking

## Installation

### Personal (User-Level)

```bash
cd ~/.claude/skills
git clone https://github.com/EricGT/prolog-dcg-skill.git dcg-parsing
```

### Project-Level (Git Submodule)

```bash
git submodule add https://github.com/EricGT/prolog-dcg-skill.git .claude/skills/dcg-parsing
```

## Usage

Once installed, Claude will automatically invoke this skill when you work with Prolog DCG parsing tasks.

Example prompts:
- "Help me write a DCG to parse tab-separated values"
- "Create a DCG parser for C-style identifiers"
- "Parse this file format using Prolog DCGs"

## Documentation

- **[SKILL.md](SKILL.md)** - Main skill guide with core philosophy and patterns
- **[PATTERNS.md](PATTERNS.md)** - Comprehensive pattern library (30+ patterns)
- **[ERROR_HANDLING.md](ERROR_HANDLING.md)** - Error handling and debugging strategies
- **[EXAMPLES.md](EXAMPLES.md)** - Complete integrated examples
- **[TESTING.md](TESTING.md)** - Testing strategies and debugging techniques
- **[REFERENCE.md](REFERENCE.md)** - Performance considerations, common mistakes, and references

## License

BSD 2-Clause License - See [LICENSE](LICENSE) for details.
