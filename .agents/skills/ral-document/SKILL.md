---
name: ral-document
description: >
  Writes and checks PascalRAL's doc comments (the /// and { } that the IDE hint and
  pasdoc show), the order of class members, and runs pasdoc over pasdoc.pds. Use when
  adding or changing any declaration in src/, when reviewing documentation, or when
  generating the API docs.
---

# Documentation in the code

The description is what Delphi's and Lazarus' autocomplete and hint show, and what
pasdoc publishes. **Everything is documented**: unit, class, record, enum, event type,
constant, routine and every class member, private ones included.

## Form

- **One line** (90 columns with the indent): `/// Text.`
- **More than one line**: braces. The text starts after `{ `, continuation lines align
  with it, and `}` closes on the last line. Two or three lines at most:

  ```pascal
    { Accepted server certificates (SHA-256), one per line: "fp" for any host,
      "host=fp" or "host:port=fp" for one server. }
    property Pins: TStrings read GetPins write SetPins;
    /// Refuses to send anything over plain http, whatever the URL says.
    property Required: boolean read FRequired write FRequired default False;
  ```

- Directly above the declaration, no blank line between.
- Inside `{ }`: never `}` or `{`, and never start with `$` (it becomes a directive).
- `@` literal is `@@`. Tags only when they help: `@code(...)`, `@link(...)`, `@param`,
  `@returns`, `@raises`, `@seealso`, `@deprecated`. No backticks and no "- item" lists:
  pasdoc shows them literally (`Markdown=0`).
- A plain identifier (`TRALServer.SSL`) becomes a link by itself.
- **A compiler directive is code, not a comment**: never create, move or split a
  `{$...}`.
- Enums, sets and initialized constants get a description **before** the declaration,
  which itself does not change (order, line breaks, values: the ordinal travels on the
  wire).
- A unit starts with a `///` before `unit` saying its scope.
- New comments are ASCII: `-` for a dash, straight quotes.

## Content

- US English, present tense, third person ("Returns...", "Sends..."). The first sentence
  is the summary and ends with a period: pasdoc's tables show it.
- Say **what it is or does** and what a user needs: unit and default ("milliseconds;
  0 = no limit"), what `nil` or empty means, when it raises, which thread an event runs
  on, platform or engine limits ("Windows only").
- **Never**: history ("used to", "since 1.3", dates), plans or rounds, measurements,
  discarded alternatives, comparisons with another engine, emphasis in capitals.
- Each member has its own description; one comment above a group does not count.
- Left without a description on purpose: `Create`/`Destroy` without a parameter worth
  explaining, an `override` that keeps the ancestor's contract, and the private field,
  getter and setter of a documented property.
- The same declaration in two `{$IFDEF}` branches or parallel includes
  (`RALJSON_*.inc`) gets the same text in each.
- A long "why" does not go in the code: it goes to the wiki pages `internos-*`
  ([guia-contribuicao](https://github.com/OpenSourceCommunityBrasil/PascalRAL/wiki/guia-contribuicao)).
  In the code stays one sentence with the constraint.

Comments inside methods (`//` or `{ }`) only when the code does not explain itself: an
invariant ("the caller holds Lock"), a required order, a protocol reference (RFC and
section). One or two lines, present tense. No commented-out code.

## Order of class members

Sections in the order `private`, `protected`, `public`, `published` (`strict private`
before `private`; an empty section is not written). Inside each section, in this order
and each group alphabetical:

1. fields (and `class var`);
2. `constructor`;
3. `destructor`;
4. methods (`procedure` and `function` mixed; the keyword does not count);
5. event methods (names starting with `On`);
6. properties.

- Directives (`{$IFDEF}`...), section keywords and nested `type`/`const` blocks are
  fixed points: members are ordered only between two of them, and nothing moves into
  or out of an `{$IFDEF}`.
- Overloads stay together; the description moves with its member.
- **Not reordered**: records (C layouts), enums, sets, initialized constants,
  interfaces (the order is the vtable), form classes (the designer's section), and
  third-party code (`src/others`, `src/bindings/libsagui.pas`).
- Reordering published properties changes the order a saved `.dfm`/`.lfm` writes them;
  a setter that depends on another property already read must be checked.

The full standard, in Portuguese: wiki
[padrao-codigo](https://github.com/OpenSourceCommunityBrasil/PascalRAL/wiki/padrao-codigo).

## pasdoc

`pasdoc.pds` is the pasdoc_gui project: open it in `pasdoc_gui` and generate. On the
command line, write the `[Files]` values one per line to a list file and pass each
`[IncludeDirectories]` value with `-I`:

```bat
pasdoc --format html --output <out> -D FPC -D MSWINDOWS ^
  --visible-members published,public,protected,automated --write-uses-list ^
  --auto-link --auto-abstract --title PascalRAL -I src\core -I src\common ... ^
  --source <list of files>
```

Exit code 3 with warnings about `Declared(...)` and `CompilerVersion` is expected:
pasdoc cannot evaluate them and assumes true. Any other warning is a finding. A new
unit is missing from the docs until it is in `pasdoc.pds` (`ral-packages`).
