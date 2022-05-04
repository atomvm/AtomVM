# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.5.1] - Unreleased
 ### Fixed
 - Fix `gen_statem`: Cancel outstanding timers during state transitions in
   order to prevent spurious timeout messages from being sent to `gen_statem`
   process.

## [0.5.0] - 2022-03-22
