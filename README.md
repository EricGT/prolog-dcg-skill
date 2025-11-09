# DCG Parsing Skill for Claude Code

A Claude Code skill for writing efficient, idiomatic SWI-Prolog DCGs (Definite Clause Grammars).

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
git clone https://github.com/YOUR-USERNAME/dcg-parsing-skill.git dcg-parsing
```

### Project-Level (Git Submodule)

```bash
git submodule add https://github.com/YOUR-USERNAME/dcg-parsing-skill.git .claude/skills/dcg-parsing
```

## Usage

Once installed, Claude will automatically invoke this skill when you work with Prolog DCG parsing tasks.

Example prompts:
- "Help me write a DCG to parse tab-separated values"
- "Create a DCG parser for C-style identifiers"
- "Parse this file format using Prolog DCGs"

## Documentation

- **SKILL.md** - Main skill guide with core philosophy and patterns
- **PATTERNS.md** - Comprehensive pattern library
- **EXAMPLES.md** - Complete integrated examples
- **TESTING.md** - Testing strategies and debugging
- **REFERENCE.md** - Performance considerations and references

## License

[Choose your license - MIT, Apache 2.0, etc.]
