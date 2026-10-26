---
name: ral-dbware
description: >
  Exposes a database over HTTP with PascalRAL DBWare and consumes it from client
  datasets: TRALDBModule on the server (FireDAC, Zeos or SQLDB), TRALDBConnection plus a
  storage link and a memtable on the client, the connection pool, OnValidateSQL, the
  FireDAC DAO, stored procedures by name (proc/:name), AutoCommit and the client
  transaction queue. Use for "remote dataset", "open a SELECT through the server",
  "ApplyUpdates over REST", "DBWare", "stored procedure", "all or nothing", or a
  429/500 from a database route.
---

# DBWare: a database through the server

The client opens a dataset with a `SELECT` as if the database were local; the RAL
server talks to the database.

```
client: memtable -> TRALDBConnection -> TRALClient --- HTTP --- TRALServer -> TRALDBModule -> FireDAC/Zeos/SQLDB -> database
```

## Packages (2.0)

`RALDBPackage` (+ `RALDBDsgn` for the IDE) and `RALStoragePackage`, then the link of the
data access library: `RALDBFireDACPackage` (+ `RALDBFireDACDsgn`), `RALDBZeosPackage`
(+ `RALDBZeosDsgn`), or on Lazarus `raldbsqldbpackage` (+ `raldbsqldbdsgn`). DBWare is
optional: RAL works without it.

## Server

A `TRALDBModule` (palette `RAL - Modules`) with `Server` set, and:

| property | example | meaning |
|---|---|---|
| `DatabaseLink` | FireDAC, Zeos, SQLDB | the data access library the server uses (its driver unit linked: `RALDBFireDAC`, `RALDBZeos`, `RALDBSQLDB`) |
| `DatabaseType` | `dtFirebird`, `dtSQLite`, `dtMySQL`, `dtPostgreSQL` | the database |
| `Database`, `Hostname`, `Port`, `Username`, `Password` | | the connection |
| `LibLocation` | `fbclient.dll` | client library, when not on the default path |
| `CharacterSet` | empty | empty = the driver's default (UTF8 on Firebird) |
| `ConnectionParams` | `busytimeout=10000` | driver settings, one per line; win over RAL's |
| `Domain` | `db` | route prefix: `/db/opensql` |
| `OnValidateSQL` | | **approve or refuse every statement** |
| `AutoCommit` | `True` | `True`: each statement commits on its own; `False`: a request is one transaction, all or nothing |
| `ProcOptions` | `Enabled`, `Filter = API_*` | the stored procedure route, off by default (below) |
| `OnValidateProc` | | approve, refuse or fill the params of every procedure call |

It creates `opensql`, `execsql`, `applyupdates`, `gettables`, `getfields`,
`getsqlfields`.

**Without `OnValidateSQL`, any client (any logged-in client, with authentication) runs
any SQL, `DROP TABLE` included.** Put authentication on the server (`ral-auth`) and a
rule in `OnValidateSQL`.

Pool (off by default): `PoolOptions.Enabled`, `MinSize`/`MaxSize`, `WaitTimeout` (past
it the client gets **429**), `IdleTimeout`, `MaxLifetime`, `MaxUses`,
`ValidateOnAcquire`, `PrepareOnActivate`. A route of your own takes a connection with
`AcquireDatabase(ARequest, AResponse)` and gives it back in a `finally` with
`ReleaseDatabase` - never create a `TRALDBBase` per request.

## Client

1. A `TRALClient` with `BaseURL` and an engine (`ral-client`).
2. A `TRALDBConnection`: `Client` = that client, `ModuleRoute` = the module's `Domain`.
3. A storage link (palette `RAL - Storage`): `TRALStorageBINLink` (fastest),
   `TRALStorageJSONLink`, `TRALStorageCSVLink`, `TRALStorageBSONLink`,
   `TRALStorageXMLLink`.
4. The dataset of the client's data access library: `TRALDBFDMemTable` (Delphi,
   FireDAC), `TRALDBZMemTable` (Zeos), `TRALDBBufDataset` (Lazarus, SQLDB).

```pascal
RALDBFDMemTable1.RALConnection := RALDBConnection1;
RALDBFDMemTable1.Storage := RALStorageBINLink1;
RALDBFDMemTable1.SQL.Text := 'select id, name from clients where city = :city';
RALDBFDMemTable1.Params.ParamByName('city').AsString := 'Recife';
RALDBFDMemTable1.Open;

RALDBFDMemTable1.UpdateTable := 'clients';
RALDBFDMemTable1.Edit;
RALDBFDMemTable1.FieldByName('name').AsString := 'Ana Maria';
RALDBFDMemTable1.Post;
RALDBFDMemTable1.ApplyUpdates;   // raises on failure
```

- `ApplyUpdates` and `ExecSQL` raise on failure, and an UPDATE/DELETE that does not
  affect exactly one row is an error (someone changed the row since the `Open`). For an
  `UpdateSQL` that touches several rows on purpose, turn the count off:
  `UpdateOptions.CountUpdatedRecords` on the FireDAC memtable, `CountUpdatedRecords` on
  the Zeos and SQLDB ones.
- Persistent fields made in the Fields Editor must match what the server sends; recreate
  them after changing the server's driver.

## Stored procedures

`ProcOptions.Enabled` creates **one** route, `<Route>/:name` (POST; `Route` defaults to
`proc`): the procedure is looked up in the database's catalog (nothing is kept in the
module), with its kind and params, and called with the right syntax - Firebird `select
* from` / `execute procedure`, PostgreSQL `call` / `select * from`, MySQL `call` /
`select f()`. SQLite has none (`emDBNoStoredProc`).

- The real limit is the database: give the module's user `GRANT EXECUTE` only on what
  the API exposes. `Filter` (masks, one per line), `Schemas` and `OnValidateProc`
  narrow it further; a value the event writes to `AParams` replaces the client's.
- `CatalogLife` (minutes, default 5) caches what the catalog said; `RefreshProcedures`
  forgets it. On PostgreSQL the param names sent choose between overloads.
- A plain HTTP client posts a JSON object (or query/form fields) and gets the rows in
  the storage its `Accept` names, else `ProcOptions.Storage`, else JSON. Outputs come
  as a one-row dataset; more than one dataset (outputs + rows, several result sets) is
  `multipart/mixed`, parts `params`, `data`, `data2`... (each with its Content-Type and
  `Content-Disposition: inline; name="..."`). Nothing to answer: 204.
- A RAL client: `StoredProcName` + `ExecProc` on the three memtables (outputs back in
  `Params`; `Open` when the procedure answers rows), `NextRecordSet`, and `ResultSets`
  to send the 2nd, 3rd... result set to other datasets (any `TDataSet`, through the
  storage). Without a memtable: `RALDBConnection.ExecProcRemote('NAME', Params,
  [ds1, ds2], StorageLink)`. `TRALDBConnection.ProcRoute` must match the server's
  `ProcOptions.Route`.

```pascal
RALDBFDMemTable1.StoredProcName := 'API_BAIXA_ESTOQUE';
RALDBFDMemTable1.Params.CreateParam(ftInteger, 'PRODUTO', ptInput).AsInteger := 10;
RALDBFDMemTable1.ExecProc;
Saldo := RALDBFDMemTable1.ParamByName('SALDO').AsInteger;
```

## All or nothing

Server-side, `AutoCommit := False` makes every request one transaction. Client-side,
the queue of `TRALDBConnection` sends several operations as **one** request, run as one
transaction whatever `AutoCommit` says:

```pascal
RALDBConnection1.StartTransaction;   // from here nothing goes out
mtClientes.ApplyUpdates;
mtPedidos.ExecSQL;
RALDBConnection1.ExecProcRemote('BAIXA_ESTOQUE', vParams, []);
RALDBConnection1.Commit;             // one request; each caller gets its answer; raises on failure
```

`Rollback` drops the queue (nothing was sent); `Open` is refused while it is open. The
server stays stateless: no connection waits between requests, so a queued statement
cannot read the result of an earlier one - put that logic in a procedure.

## FireDAC DAO (alternative, Delphi only)

`TRALFDConnection` on the server and `TRALFDQuery` on the client (package
`RALDBFireDACDAOPackage`, palette `RAL - DAO`): the client uses a real `TFDQuery` and
FireDAC's own ApplyUpdates. It has `OnValidateSQL` and, for the rows FireDAC generates,
`OnValidateApplyUpdates`.

## Mixed versions

A 2.0 side sends NUMERIC/DECIMAL exactly and dates with milliseconds, which a RAL before
1.2 cannot read: set `RALLegacyWire := True` (`RALStorageTypes`) on the newer side.
The DBWare envelope is version 2 (param directions, outputs, procedure calls, several
result sets, the transaction flag); a 2.0 server reads version 1 and answers an old
client in it, and under `RALLegacyWire` a 2.0 client writes version 1 (a procedure call
or an output param then raises `emDBLegacyWireProc`). A server older than 2.0 refuses
version 2 with `emQueryVersionError`.

## Common errors

| symptom | cause |
|---|---|
| `Malformed string` with accents (Firebird) | a forced `CharacterSet`; leave it empty |
| 429 waiting for a connection | pool exhausted: `MaxSize` or `WaitTimeout` |
| 500 `SQL rejected by OnValidateSQL` | your rule refused it |
| `a statement affected 0 records instead of 1` | the row changed or vanished since `Open` |
| client library not found | `LibLocation` |
| 404 `Stored procedure X not found, or not allowed` | not in the catalog, `Filter`, `Schemas`, or no `GRANT EXECUTE` |
| 403 on `proc/` | `OnValidateProc` refused, or `ProcOptions.Enabled` is off (RAL datasets) |
| `Rolled back: another statement of the same request failed` | `AutoCommit` off or the client queue: the failing one carries the real error |

Guide (Portuguese): wiki
[dbware](https://github.com/OpenSourceCommunityBrasil/PascalRAL/wiki/dbware).
