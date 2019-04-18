#
# Copyright (c) 2019 Riccardo Binetti <rbino@gmx.com>
#
# This builds a docker image that is able to build ESP32 and STM32 AtomVM platforms
#

FROM ubuntu:bionic

# Common setup

# Prepare directories
RUN mkdir -p /tools/esp mkdir -p /tools/stm32

# Install common deps
RUN apt-get -qq update && apt-get -qq install git cmake

# Install esp-idf deps
RUN apt-get -qq install wget make libncurses-dev flex bison gperf \
    python python-pip python-setuptools python-serial python-cryptography \
    python-future python-pyparsing python-pyelftools

# Add gcc-arm-embedded ppa and install gcc-arm-embedded
RUN echo "deb http://ppa.launchpad.net/team-gcc-arm-embedded/ppa/ubuntu bionic main" >> /etc/apt/sources.list && \
    apt-key adv --keyserver keyserver.ubuntu.com --recv-keys D1FAA6ECF64D33B0 && \
    apt-get -qq update && apt-get -qq install gcc-arm-embedded

# ESP 32

# Work in esp directory
WORKDIR /tools/esp

# Get xtensa toolchain
RUN wget https://dl.espressif.com/dl/xtensa-esp32-elf-linux64-1.22.0-80-g6c4433a-5.2.0.tar.gz -O xtensa-toolchain.tar.gz && \
    tar xf xtensa-toolchain.tar.gz && \
    rm xtensa-toolchain.tar.gz

# Add xtensa toolchain to path
ENV PATH $PATH:/tools/esp/xtensa-esp32-elf/bin

# Export IDF_PATH
ENV IDF_PATH /tools/esp/esp-idf

# Clone and build esp-idf v3.2-rc
RUN git clone -b v3.2 --recursive https://github.com/espressif/esp-idf.git esp-idf && \
    python -m pip install --user -r $IDF_PATH/requirements.txt

# STM32 stuff

# Work in stm32 directory
WORKDIR /tools/stm32

# Clone and build libopencm3
RUN git clone https://github.com/libopencm3/libopencm3.git && \
    cd libopencm3 && \
    make -j4

# Export LIBOPENCM3_DIR
ENV LIBOPENCM3_DIR /tools/stm32/libopencm3

# Get back to root directory
WORKDIR /root

CMD /bin/bash
