# STM32F4Discovery example

This example will blink the 4 LEDs present on the STM32F4Discovery with
different frequencies, using one process per LED.

# Building
- Run `./build.sh`
- If you haven't done so, build PackBEAM (in the `tools` directory)
- Use PackBEAM to pack all Beam module: `../../../tools/packbeam/build/PackBEAM blink-stm32.avm *.beam`

# Flashing
- Follow the STM32 README (`src/platforms/stm32`) to build and flash AtomVM
- Flash the packed AVM with `st-flash --reset write blink-stm32.avm 0x8080000`
