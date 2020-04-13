# AtomVM ESP Programming Manual

AtomVM provides ESP-specific APIs for programming on the ESP platform.  This document provides a description of these APIs.

For more detailed information about these APIs, please see the [AtomVM Reference API documentation](ref/eavmlib/index.html).

## ESP reset functions

The `esp` module provides functions for restarting the ESP device, as well as retrieving the reason
for the last restart, as a symbolic constant.

* `esp:restart/0`
* `esp:reset_reason/0`

## ESP NVS

The `esp` module provides functions for manipulating enties in non-volatile storage (NVS) on an ESP device.  Entries in NVS survive reboots of the ESP device, and can be used a limited "persistent store" for key-value data.

> Note.  NVS storage is limited in size, and NVS keys are restricted to 15 characters.

The ESP API provides functionality for setting, retrieving, and deleting key-value data in binary form, as well as functionality for reformatting the NVS partition.  Reformatting the NVS partition should be exercised with caution, as all data in the NVS partition is unrecoverably deleted.

> Note. AtomVM currently defines one partition for NVS data.

NVS entries are stored under a namespace and key, both of which are expressed as atoms.  AtomVM uses the namespace `atomvm` for entries under its control.  Applications may write to the `atomvm` namespace, but they are strongly discouraged from doing so.

The following functions are available:

* `esp:get_binary/1`
* `esp:get_binary/2`
* `esp:get_binary/3`
* `esp:set_binary/2`
* `esp:set_binary/3`
* `esp:erase_key/1`
* `esp:erase_key/2`
* `esp:erase_all/0`
* `esp:erase_all/1`
* `esp:reformat/0`

> Note.  NVS entries are stored in plaintext and are not encrypted.  Applications should exercise caution if sensitive security information, such as account passwords, are stored in NVS storage.

## Miscellaneous

The `freq_hz` function can be used to retrieve the clock frequency of the chip.

* `esp:freq_hz/0`
