# Copyright 2019 Riccardo Binetti <rbino@gmx.com>
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
#
# This builds a docker image that is able to build STM32 AtomVM platform
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
