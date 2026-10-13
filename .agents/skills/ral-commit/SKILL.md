---
name: ral-commit
description: >
  Writes a PascalRAL commit message: an English subject that reads as a release note,
  and the keyword rule that decides its CHANGELOG.md section. Use before any commit or
  pull request in this repository, or when a commit landed in the wrong changelog
  section.
---

# Commit messages

`CHANGELOG.md` is generated from the commit messages by
`.github/workflows/changelog.yml`, so **the subject is the release note a user reads**.

- **English**, saying what changes for whoever uses RAL: "Fix the fpHTTP client writing
  requests into closed connections", not "fixes in RALfpHTTPClient".
- **No AI or assistant attribution**: no `Co-Authored-By`, no session trailer, no
  "generated with". This is a project rule and overrides any default.
- One subject line; a body only when the why is not obvious from the subject.

## How the section is chosen

The workflow lowercases **subject and body together** and takes the **first** section
whose keyword appears anywhere, as a plain substring:

| order | keywords | section |
|---|---|---|
| 1 | `security`, `vulnerability`, `cve`, `exploit` | Security |
| 2 | `breaking change`, `breaking:`, `break:` | Breaking Changes |
| 3 | `deprecat`, `obsolete`, `phase out` | Deprecated |
| 4 | `remove`, `delete`, `drop`, `eliminate` | Removed |
| 5 | `add`, `new`, `create`, `implement`, `feat`, `feature` | Added |
| 6 | `fix`, `resolve`, `correct`, `patch`, `bug`, `issue` | Fixed |
| 7 | `chore`, `chr` | left out of the changelog |
| - | none | Changed |

Messages holding `[skip ci]`, `docs: update changelog` or `changelog.yml` are left out
entirely.

The traps that follow from it:

- **`remove` is tested before `add`**: a feature whose body says "removes the old
  check" lands under Removed. Pick the section first, then keep every keyword of an
  earlier section out of the whole message.
- **Substrings count**: "address" holds `add`, "renew" holds `new`, "prefix" holds
  `fix`, "chromium" holds `chr`.
- **English only**: "Adicionado ..." has no `add` and falls into Changed.

## Check before committing

```bat
.agents\local\bin\commitsection.exe "Add PBKDF2 key derivation for the AES body cipher"
git log -1 --format=%B > msg.txt
.agents\local\bin\commitsection.exe --file=msg.txt
```

It prints the section, the keyword that decided it and the word it was found in. Build
the tool once: see `.agents/tools/` in `AGENTS.md`.

## Examples

```
Add PBKDF2 key derivation for the AES body cipher
Fix the fpHTTP client writing requests into closed connections
Breaking change: CORS lets no origin in by default
chore: document RALClient
```
