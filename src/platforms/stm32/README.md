# AtomVM for STM32
- `git clone libopencm3`
- `cd libopencm3 && make -j4 && cd ..`
- `mkdir build`
- `cd build`
- `cmake -DCMAKE_TOOLCHAIN_FILE=../cmake/arm-toolchain.cmake -DLIBOPENCM3_DIR=../libopencm3 ..`
- `make`

The default build is based on the STM32F4Discovery board chip. If you want to target a different chip,
pass the `-DDEVICE` flag when invoking cmake. For example, to use the STM32F429Discovery,
pass `-DDEVICE=stm32f429zit6`
