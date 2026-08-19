# AGENTS.md

## Role

This repository contains informal LaTeX research notes.
Edit as a careful editor and auditor, not as a co-author.

## Scope

- Focus on `.tex` files in `chapters/`.
- Edit `main.tex` only for document structure.
- Do not edit `macro.tex` unless explicitly asked.
- Ignore `.bak` files, PDFs, logs, and auxiliary build files.

## Editing Rules

- Preserve mathematical meaning and intent.
- Do not improve proofs, complete missing arguments, or introduce new claims.
- If something is unclear or possibly wrong, mark it with a LaTeX comment:

```tex
% FIXME: unclear statement
% TODO: verify this claim
% WARNING: possible error
```

## Style

- Use semantic line breaking.
- Use `$...$` for inline math.
- Use `$$...$$` for display math.
- Do not use LaTeX parenthesis or bracket math delimiters.
- Keep informal research-note tone unless asked to formalize.

## Build

- Do not auto-compile.
- Do not run LaTeX build commands unless explicitly asked.
