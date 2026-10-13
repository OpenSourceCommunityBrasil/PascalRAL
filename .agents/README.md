# .agents

Material for AI coding agents working with PascalRAL. The entry point is `AGENTS.md`
at the repository root.

| path | what it is |
|---|---|
| `skills/<name>/SKILL.md` | one skill per task: using RAL in an application, contributing to it |
| `tools/<name>/` | Pascal console programs that check what instructions cannot (`bplunits`, `commitsection`, `checkskills`) |
| `PROJECT_MAP.md` | every folder, package and unit of `src/` |
| `ENVIRONMENT.example.md` | template of the machine paths file |
| `local/` | never committed: plans, notes, caches, compiled tools, `ENVIRONMENT.md` |

## Changing a skill

- One folder per skill, directly under `skills/`, with `SKILL.md` starting with a YAML
  frontmatter: `name` (equal to the folder, lowercase and hyphens, prefix `ral-`) and
  `description` (what it does **and when to use it**: the words, errors and symptoms a
  request carries; up to 1024 characters).
- Keep `SKILL.md` short (about 150 lines); move reference material to a `reference.md`
  next to it.
- English only. No machine paths (they come from `ENVIRONMENT.md`), no scripts, no
  reference to tools outside this repository.
- Point to `CLAUDE.md` sections and wiki pages instead of copying them.
- List the skill in the table of `AGENTS.md`.
- Run `checkskills` before committing:

```bat
.agents\local\bin\checkskills.exe --repo=.
```

It checks the frontmatter, the names, the `AGENTS.md` table, every `src/` and `pkg/`
path cited in the agent files, and that no committed agent file cites local material,
a machine path, a script or the test orchestrator, or carries Portuguese text.
