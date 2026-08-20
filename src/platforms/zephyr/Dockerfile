#
#  Copyright 2026 Peter M <petermm@gmail.com>
#
#  SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
#

# Use the official Zephyr CI image matching our GitHub Actions workflow
FROM ghcr.io/zephyrproject-rtos/ci:v0.29.3

USER root

# Create workspace directory
WORKDIR /workspace/zephyr-workspace

# Erlang-driven simulator tests compile BEAM files and build packbeam inside
# CMake, following the ESP32/RP2 platform-test pattern.
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        erlang-base \
        erlang-crypto \
        erlang-dev \
        erlang-inets \
        erlang-parsetools \
        erlang-public-key \
        erlang-ssl \
        erlang-syntax-tools \
        erlang-tools \
        rebar3 && \
    rm -rf /var/lib/apt/lists/*

# Copy the west manifest from the host to the temporary location inside the image
COPY west.yml AtomVM/src/platforms/zephyr/west.yml

# Initialize the West workspace and clone the dependencies
RUN mkdir -p .west && \
    printf "[manifest]\npath = AtomVM/src/platforms/zephyr\nfile = west.yml\n" > .west/config && \
    west update --narrow && \
    west blobs fetch hal_espressif && \
    west blobs fetch hal_infineon

# Set default working directory to the Zephyr platform port directory
WORKDIR /workspace/zephyr-workspace/AtomVM/src/platforms/zephyr

# Set the Zephyr base environment variable
ENV ZEPHYR_BASE=/workspace/zephyr-workspace/zephyr
