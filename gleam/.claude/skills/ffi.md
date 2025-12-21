---
name: ffi
description: Validate and scaffold Erlang FFI code with ETS patterns
---

# /ffi - Erlang FFI Helper

Validate and scaffold Erlang FFI code with critical ETS patterns.

## Critical Pattern

**ALL ETS FFI functions MUST call init() first!**

```erlang
% CORRECT
get_data() ->
    init(),  % <-- ALWAYS FIRST
    case ets:lookup(?TABLE, key) of
        [{key, Val}] -> {some, Val};
        [] -> none
    end.

% WRONG - will crash if table doesn't exist
get_data() ->
    case ets:lookup(?TABLE, key) of  % <-- CRASH!
        [{key, Val}] -> {some, Val};
        [] -> none
    end.
```

## Usage

```bash
/ffi validate             # Check all FFI files for init() calls
/ffi validate <file>      # Check specific file
/ffi scaffold <name>      # Generate new FFI module
/ffi list                 # List all FFI modules
```

## FFI Modules

| Module | Purpose |
|--------|---------|
| vibee_p2p_ffi.erl | P2P order storage |
| vibee_payment_ffi.erl | Payment/balance storage |
| vibee_session_ffi.erl | Session ETS operations |
| vibee_db_pool_ffi.erl | Database pool caching |
| vibee_agent_ffi.erl | Agent state persistence |

## Implementation

When user runs `/ffi validate`:

1. **Find FFI Files**
```bash
find src -name "*_ffi.erl"
```

2. **Check Pattern**
```bash
# For each function that uses ets:lookup or ets:insert
# Verify init() is called first
grep -n "ets:lookup\|ets:insert" src/vibee_*_ffi.erl
```

3. **Report Issues**
```
## FFI Validation Report

vibee_p2p_ffi.erl: OK (5 functions, all call init())
vibee_session_ffi.erl: WARNING
  - get_session/1 (line 45): missing init() call

Fix: Add init() as first line of get_session/1
```

## Scaffold Template

When user runs `/ffi scaffold <name>`:

**Create `src/vibee_<name>_ffi.erl`:**
```erlang
-module(vibee_<name>_ffi).
-export([init/0, get/1, set/2, delete/1]).

-define(TABLE, vibee_<name>_table).

init() ->
    case ets:info(?TABLE) of
        undefined -> ets:new(?TABLE, [named_table, public, set]);
        _ -> ok
    end,
    nil.

get(Key) ->
    init(),
    case ets:lookup(?TABLE, Key) of
        [{Key, Value}] -> {some, Value};
        [] -> none
    end.

set(Key, Value) ->
    init(),
    ets:insert(?TABLE, {Key, Value}),
    nil.

delete(Key) ->
    init(),
    ets:delete(?TABLE, Key),
    nil.
```

**Create `src/vibee/<name>_ffi.gleam`:**
```gleam
@external(erlang, "vibee_<name>_ffi", "init")
pub fn init() -> Nil

@external(erlang, "vibee_<name>_ffi", "get")
pub fn get(key: String) -> option.Option(Dynamic)

@external(erlang, "vibee_<name>_ffi", "set")
pub fn set(key: String, value: a) -> Nil

@external(erlang, "vibee_<name>_ffi", "delete")
pub fn delete(key: String) -> Nil
```

## Common Errors

| Error | Cause | Fix |
|-------|-------|-----|
| `badarg` on ets:lookup | Table doesn't exist | Add init() call |
| `already_registered` | init() called after table exists | Check with ets:info first |
| `access_denied` | Table not public | Use `public` option in ets:new |
