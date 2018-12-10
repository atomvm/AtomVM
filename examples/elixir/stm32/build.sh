#!/bin/bash
elixirc *.ex
erlc ../../../libs/estdlib/*.erl
elixirc ../../../libs/exavmlib/*.ex
