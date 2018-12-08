# AtomVM for STM32

## Prerequisites
To flash both AtomVM and your packed AVM you must use st-flash (https://github.com/texane/stlink). Make sure to
follow its [installation procedure](https://github.com/texane/stlink#installation) before proceeding further.

## Building
- `git clone libopencm3`
- `cd libopencm3 && make -j4 && cd ..`
- `mkdir build`
- `cd build`
- `cmake -DCMAKE_TOOLCHAIN_FILE=../cmake/arm-toolchain.cmake -DLIBOPENCM3_DIR=../libopencm3 ..`
- `make`

You can put libopencm3 wherever you want on your PC as long as you update LIBOPENCM3_DIR to point to it.

## Flashing
To flash AtomVM, use
```
st-flash --reset write atom-vm.bin 0x8000000
```

To flash your packed AVM, use
```
st-flash --reset write /path/to/your/packed.avm 0x8080000
```

Right now AtomVM expects to find the AVM at the address 0x808000. On a STM32 Discovery board this means that
the 1MB of flash will be split in 512KB available for the program and 512KB available for the packed AVM.
If for any reason you want to modify this, you can change `AVM_ADDRESS` and `AVM_FLASH_MAX_SIZE` defines in `main.c`.

# Printing
By default, stdout and stderr are printed on USART2. On the STM32F4Discovery board, you can see them
using a TTL-USB with the TX pin connected to board's pin PA2 (USART2 RX). Baudrate is 115200 and serial transmission
is 8N1 with no flow control.

## Changing device

The default build is based on the STM32F4Discovery board chip (`stm32f407vgt6`). If you want to target a different
chip, pass the `-DDEVICE` flag when invoking cmake. For example, to use the STM32F429Discovery, pass
`-DDEVICE=stm32f429zit6`
