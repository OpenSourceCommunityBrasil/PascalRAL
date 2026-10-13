---
name: ral-rad-component
description: >
  Designs a PascalRAL feature the RAD way: a component dropped on a form and configured
  in the Object Inspector, with events for the code, before any code-only API. Use when
  adding a property, a component, a plugin, a module or any user-facing feature, or
  when choosing between "a new component" and "an extension mechanism".
---

# RAD first

RAL users drop a component, set it in the Object Inspector and write only the event.
Code-only setup is for console programs and is not how most users work. Every feature
needs a no-code path, and that path is tested.

## Shape

- **Prefer a component of its own, with properties one level deep**, over extension
  mechanisms with sub-levels (collections of extensions, class helpers, registries the
  user has to fill). A descendant component (`TRALSOAPClient` is a `TRALClient`) beats
  a plug-in system inside an existing one.
- Settings are **published properties**; behaviour the user writes is an **event**
  (`On...`, `of object`, so the handler can be a method of the form).
- Give a component everything needed to work without code where possible: component
  verbs (an "Import WSDL..." menu), actions for a `TActionList`, a link to a dataset
  (`ResultDataSet`) instead of an API to call.
- Palettes: `RAL - Server`, `RAL - Client`, `RAL - Modules`, `RAL - Plugins`,
  `RAL - Storage`, `RAL - DAO`; registration in the package's `RAL<Item>Reg` unit
  (`ral-packages`).

## Properties that stream right

- **`default` equals what the constructor sets**, both from one constant in
  `RALConsts`. Streaming skips a value equal to `default`: a mismatch makes a form
  silently run with another value.
- **Defaults keep the old behaviour**: a new limit or protection starts off.
- An owned object property (a `TPersistent` sub-object) has a setter that **copies**
  (`RALAssignOwned`), never `write FField`.
- A `TStrings`/collection property that is empty by default is written only when it
  has items (`stored`), so a form saved by a newer version still opens in an older one.
- A property that matters only for some configurations is hidden from the Object
  Inspector by overriding `TRALComponent.IsPropertyRelevant` (see `TRALClient`, which
  hides `HTTPVersion` unless the engine supports HTTP/2); the hidden value is kept and
  ignored.
- `Active := True` read from a form is applied in `Loaded`, after the other properties
  were read.
- A reference to another component takes a `FreeNotification` and clears itself in
  `Notification`.

## Design time

- Nothing that needs the network, a DLL or the server runs at design time
  (`csDesigning`); libraries load on activation, never in a constructor or an
  `initialization`.
- Design-time editors and verbs live in a `...Dsgn` package, in a `design/` subfolder.

## Check

Drop the component on a form in Delphi and in Lazarus, set it in the Object Inspector,
save, close and reopen the form, and run it without writing setup code.
