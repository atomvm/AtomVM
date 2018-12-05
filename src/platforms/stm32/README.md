# AtomVM for STM32
- `git clone libopencm3`
- `cd libopencm3 && make -j4 && cd ..`
- `mkdir build`
- `cd build`
- `cmake -DCMAKE_TOOLCHAIN_FILE=../cmake/arm-toolchain.cmake -DLIBOPENCM3_DIR=../libopencm3 ..`
- `make`
