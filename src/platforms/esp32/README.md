<!---
  Copyright 2020 Fred Dushin <fred@dushin.net>

  SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

This document provides internal implementation details about the build system for AtomVM on the ESP32.  You should read this document if you are doing maintenance or adding features (such as support for peripherals or protocols) to the ESP32 port of AtomVM.

The ESP32 build currently makes use of the IDF SDK [Build System](https://docs.espressif.com/projects/esp-idf/en/v3.3.3/api-guides/build-system.html) to build the IDF SDK libraries (i.e., components) and AtomVM virtual machine.

> Note.  Future versions of AtomVM will likely make use of the IDF SDK [CMake-based Build System](https://docs.espressif.com/projects/esp-idf/en/v4.1/api-guides/build-system.html).

As such, the AtomVM build includes a set of main entrypoints for the ESP application (defined in the `main` directory), as well as system-specific code that is used by the application.

The platform-independent core AtomVM VM library is built as an IDF SDK component, and make targets can be found under the `component.mk` file in the `components/libatomvm` directory.  This make file directs the make system to build the AtomVM library using CMake, first to generate the make files, and then to run make against those generated make files.

## ESP32 integrations

The ESP32 port of AtomVM contains numerous integrations with IDF SDK peripherals and protocols, including the following ESP32 features:

* GPIO interface
* UART controller
* I2C 2-wire interface
* SPI interface
* LEDC control
* Non-volatile storage
* Wifi networking
* UDP/TCP/IP

> Note.  Additional periperhal interfaces and protocols are planned for the future.

Some of the above integrations are implemented as AtomVM Nifs (stateless functions implemented in C), while others are implemented as AtomVM ports, which are essentially stateful Erlang processes implemented in C.

Many applications do not have a need for the AtomVM image to support integration with all peripherals and protocols, and in fact, each integration does add linked code into the image, increasing its size.  For example, an application may have no need for the UART interface, or an application that only uses I2C may have no need for SPI integration, and a smaller image size can result if these integrations are excluded.  Smaller images improve flash time (reducing the time spent in the compile/flash/depbug process) and in some cases can result in decreased RAM usage at runtime.

AtomVM does not support dynamic linkage of components.  Instead, the linker will resolve all required symbols when the image is linked into a single binary.  In order to selectively pick and choose specific integrations, and perhaps disregard others, the build system makes use of some python scripting to generate code that is compiled and linked into the AtomVM image.  The presence of the symbols in this generated code will result in linkage of any dependent IDF SDK components.

There are two python scripts that are used to generate code:

* `component_nifs.py`  Used to generate entry points for AtomVM Nif code
* `component_ports.py`  Used to generate entry points for AtomVM port code

These python scripts are used to generate the needed code, and take as inputs corresponding text files (`component_nifs.txt` and `component_ports.txt`, respectively), which declare the names of the nifs or ports, respectively.  If the name of a nif or port in contained in these files, then the generated code will contain references to the entrypoints for these integrations, and, if properly defined, will get linked into the AtomVM image.

The source tree does not contain the input text files.  However, if these files are not present during a build, defaults will be generated which include all Nifs and ports supported by AtomVM.

### Writing a new Nif

To write support for a new peripheral or protocol using an AtomVM Nif, you need to do the following:

* Choose a name for your nif (e.g, "gpio").  Call this `<moniker>`.
* In your source code, implement the following two functions:
    * `void <moniker>_nif_init(GlobalContext *global);`
        * This function will be called once, when the application is started.
    * `const struct Nif *<moniker>_nif_get_nif(const char *nifname);`
        * This function will be called to locate the Nif during a function call.

Example:

    void gpio_nif_init(GlobalContext *global);
    const struct Nif *gpio_nif_get_nif(const char *nifname);

> Note. Instructions for implementing Nifs is outside of the scope of this document.

* Add your `<moniker>` to the `DEFAULT_COMPONENT_NIFS` variable in the `main/components.mk` file.

From a clean build, your Nif should be linked into the AtomVM image.

### Writing a new Port

To write support for a new peripheral or protocol using an AtomVM port, you need to do the following:

* Choose a name for your port (e.g, "i2c_driver").  Call this `<moniker>`.
* In your source code, implement the following two functions:
    * `void <moniker>_init(GlobalContext *global);`
        * This function will be called once, when the application is started.
    * `Context *<moniker>_create_port(GlobalContext *global, term opts);`
        * This function will be called to locate the Nif during a function call.

Example:

    void i2c_driver_init(GlobalContext *global);
    Context *i2c_driver_create_port(GlobalContext *global, term opts);

> Note. Instructions for implementing Ports is outside of the scope of this document.

* Add your `<moniker>` to the `DEFAULT_COMPONENT_PORTS` variable in the `main/components.mk` file.

From a clean build, your Nif should be linked into the AtomVM image.
