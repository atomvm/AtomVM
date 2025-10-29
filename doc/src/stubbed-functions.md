<!--
 Copyright 2025 AtomVM Contributors

 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# Stubbed Functions

AtomVM implements stub functions for certain BEAM operations that are either not applicable to
embedded environments, not yet implemented, or intentionally left as no-ops for compatibility
reasons. These functions allow BEAM code that references them to load and execute without errors,
even though the actual functionality may not be present.

## Purpose of Stubbed Functions

Stubbed functions serve several purposes in AtomVM:

1. **Compatibility**: Allow BEAM modules that use these functions to work anyway
2. **Environment Differences**: Functions that make no sense in embedded contexts (or in AtomVM)
but are required for code compatibility
3. **Performance**: Operations that would be too expensive on microcontrollers and are safely
ignored

## List of Stubbed Functions

The following functions are currently stubbed in AtomVM and always return a fixed value:

### I/O Functions

| Module | Function | Return Value | Notes |
|--------|----------|--------------|-------|
| `io` | `set_ops/1,2` | `ok` | Standard IO options are currently ignored |

### Other functions

| Module | Function | Return Value | Notes |
|--------|----------|--------------|-------|
| `string` | `jaro_similarity/2` | `0.0` |  |

## Important Considerations

When using AtomVM, be aware that stubbed functions will not provide the functionality you might
expect from BEAM. Code that relies on these functions for critical behavior will need to be adapted
for the AtomVM environment.

## Detecting Stubbed Functions

To write portable code that works on both BEAM and AtomVM, you can detect the runtime environment:

```erlang
case erlang:system_info(machine) of
    "BEAM" ->
        %% Use full functionality
        full_implementation();
    "ATOM" ->
        %% Use alternative approach or skip
        alternative_implementation()
end
```

## Future Implementations

Some stubbed functions may be implemented in future versions of AtomVM. Use
[GitHub issues](https://github.com/atomvm/AtomVM/issues) for providing any kind of feedback.
