# CONTEXT.md — TeX Research Notes Processing

## Purpose

This repository contains informal LaTeX research notes.  
The goal is to improve readability, remove redundancy, and organize content **without altering mathematical meaning or intent**.

The notes are **not fully formalized** and may contain:
- incomplete arguments
- duplicated ideas
- informal reasoning
- speculative statements

The system must act as a **careful editor and auditor**, NOT a co-author.

---

## Repository Structure

- `main.tex` — root document
- `chapters/` — main content (primary focus)
- `fig/` — figures and figure-related TeX
- `bib/` — bibliography
- `macro.tex` — custom macros (DO NOT modify unless explicitly instructed)
- `.bak` files — historical snapshots (DO NOT process unless asked)
- `.log`, `.aux`, `.bbl`, etc. — build artifacts (IGNORE)

---

## Scope of Processing

Only process:
- `.tex` files in `chapters/`
- optionally `main.tex` for global structure

Ignore:
- `.bak` files
- logs and auxiliary files
- generated PDFs

---

## Core Principles (STRICT)

### 1. Preserve Mathematical Meaning
- DO NOT change any formula unless it is trivially equivalent
- DO NOT "improve" proofs
- DO NOT complete missing arguments
- DO NOT introduce new claims

---

### 2. No Hallucination
- If something is unclear → mark it
- If something may be incorrect → flag it
- Use LaTeX comments:

```tex
% FIXME: unclear statement
% TODO: verify this claim
% WARNING: possible error
```