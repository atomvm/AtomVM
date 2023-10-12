#!/usr/bin/env bash

sudo apt-get install -y cmake gperf erlang elixir zlib1g-dev
mkdir build
cd build
cmake ..
make
