# Zephyr Platform Customization Rules & Guidelines

Guidelines and rules for coding agents working on the AtomVM Zephyr port.

## Target Version
- The Zephyr port targets Zephyr version **v4.4.2** (defined in [west.yml](file:///Users/petermm/OSScontrib/AtomVM/src/platforms/zephyr/west.yml#L13)). Do not change this version unless explicitly requested.

## JIT (Just-in-Time) Compilation
- JIT is disabled under the Zephyr platform (`AVM_NO_JIT` compile definition in [AtomVMZephyrApp.cmake](file:///Users/petermm/OSScontrib/AtomVM/src/platforms/zephyr/cmake/AtomVMZephyrApp.cmake#L46)). Ensure all code compiles cleanly without JIT support.

## Testing and Simulation
- Do not run tests directly on the host using standard `west` commands unless `ZEPHYR_BASE` is configured.
- Instead, use the helper script [docker-test.sh](file:///Users/petermm/OSScontrib/AtomVM/src/platforms/zephyr/docker-test.sh) to run simulator tests inside the local Docker environment:
  - **QEMU x86_64**: `./src/platforms/zephyr/docker-test.sh`
  - **Native Simulation (ASan/UBSan)**: `./src/platforms/zephyr/docker-test.sh -b native_sim`

## Memory Protection and Safety
- Do not write platform-specific code to directly access or configure the Xtensa Memory Protection Unit (MPU) registers.
- Use Zephyr's standard stack protection (`CONFIG_HW_STACK_PROTECTION=y` in Kconfig) for stack overrun checks.
