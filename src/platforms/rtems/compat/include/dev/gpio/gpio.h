/* SPDX-License-Identifier: BSD-2-Clause */

/**
 * @file
 *
 * @ingroup RTEMSGenericGPIOAPI
 *
 * @brief This header file provides the interfaces of the generic GPIO
 *   device driver.
 */

/*
 * Copyright (C) 2026 Samuel Price <thesamprice@gmail.com>
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef _DEV_GPIO_GPIO_H
#define _DEV_GPIO_GPIO_H

#include <rtems.h>
#include <rtems/thread.h>

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <sys/ioccom.h>

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

typedef struct rtems_gpio_drv_ctrl rtems_gpio_drv_ctrl;

/**
 * @defgroup RTEMSGenericGPIOAPI Generic GPIO API
 *
 * @ingroup RTEMSDeviceDrivers
 *
 * @brief This group contains the generic GPIO API, which presents a GPIO
 *   controller as a device node driven by ioctl() calls.
 *
 * A controller is a driver in the BSP, which owns the pins: how a pin is
 * muxed and whether the board permits it to be used at all are the BSP's to
 * decide.  Pins are numbered logically, 0 to rtems_gpio_info::pin_count - 1
 * with no holes, and the driver maps them to whatever the silicon calls them.
 *
 * A driver need not implement everything.  A handler left NULL is reported as
 * ENOTSUP, so a controller that can only read and write pins is legal.
 *
 * A pin is not owned by the descriptor that configured it.  Any descriptor
 * for a controller may act on any configured pin of it, and coordinating the
 * users of one controller is the application's.  This is deliberate: RTEMS
 * is one address space, so a descriptor is not a trust boundary, and the
 * same calls are made from BSP and driver code that holds no descriptor at
 * all.  What the API does enforce is the board's decision rather than the
 * application's -- a pin the BSP reports #RTEMS_GPIO_PIN_RESERVED is refused
 * with EACCES before the driver is asked anything, and
 * rtems_gpio_pin_info::owner reports what holds a pin so that a refusal can
 * be acted on.
 *
 * The rtems_gpio_*() functions below are the supported interface.  Each is
 * one ioctl() on the node, and the command numbers are published so that a
 * caller which already holds a descriptor is not forced to use them.
 *
 * @{
 */

/**
 * @brief This constant represents the maximum length of a pin or controller
 *   name, including the terminating NUL.
 */
#define RTEMS_GPIO_NAME_MAX 32

/**
 * @brief This constant represents the number of pins one word of a pin
 *   bitmap carries.
 */
#define RTEMS_GPIO_BITMAP_WORD_BITS 32

/**
 * @brief Returns the number of bitmap words a controller of @a pin_count
 *   pins needs.
 *
 * @param pin_count is the number of pins the controller publishes.
 *
 * @return Returns the number of uint32_t words a bitmap for that many pins
 *   occupies.
 */
#define RTEMS_GPIO_BITMAP_WORDS(pin_count)                                     \
  (((size_t)(pin_count) + RTEMS_GPIO_BITMAP_WORD_BITS - 1) /                   \
   RTEMS_GPIO_BITMAP_WORD_BITS)

/**
 * @brief This enumeration represents the direction of a pin.
 */
typedef enum {
  /**
   * @brief This enumerator indicates that the pin is not configured.
   *
   * What a pin reads as before rtems_gpio_pin_configure() is called on it,
   * and what it returns to after rtems_gpio_pin_release().
   */
  RTEMS_GPIO_DIRECTION_NONE = 0,

  /**
   * @brief This enumerator indicates that the pin is an input.
   */
  RTEMS_GPIO_DIRECTION_INPUT,

  /**
   * @brief This enumerator indicates that the pin is an output.
   */
  RTEMS_GPIO_DIRECTION_OUTPUT
} rtems_gpio_direction;

/**
 * @brief This enumeration represents the bias applied to a pin.
 */
typedef enum {
  /**
   * @brief This enumerator indicates that no pull resistor is applied.
   */
  RTEMS_GPIO_BIAS_NONE = 0,

  /**
   * @brief This enumerator indicates that a pull-up resistor is applied.
   */
  RTEMS_GPIO_BIAS_PULL_UP,

  /**
   * @brief This enumerator indicates that a pull-down resistor is applied.
   */
  RTEMS_GPIO_BIAS_PULL_DOWN
} rtems_gpio_bias;

/**
 * @brief This enumeration represents how an output pin drives.
 */
typedef enum {
  /**
   * @brief This enumerator indicates that the pin drives both levels.
   */
  RTEMS_GPIO_DRIVE_PUSH_PULL = 0,

  /**
   * @brief This enumerator indicates that the pin drives low and floats high.
   */
  RTEMS_GPIO_DRIVE_OPEN_DRAIN,

  /**
   * @brief This enumerator indicates that the pin drives high and floats low.
   */
  RTEMS_GPIO_DRIVE_OPEN_SOURCE
} rtems_gpio_drive;

/**
 * @brief This enumeration represents what makes a pin raise an interrupt.
 */
typedef enum {
  /**
   * @brief This enumerator indicates that the pin raises no interrupt.
   */
  RTEMS_GPIO_TRIGGER_NONE = 0,

  /**
   * @brief This enumerator indicates a low to high transition.
   */
  RTEMS_GPIO_TRIGGER_EDGE_RISING,

  /**
   * @brief This enumerator indicates a high to low transition.
   */
  RTEMS_GPIO_TRIGGER_EDGE_FALLING,

  /**
   * @brief This enumerator indicates a transition in either direction.
   */
  RTEMS_GPIO_TRIGGER_EDGE_BOTH,

  /**
   * @brief This enumerator indicates that the pin is held high.
   */
  RTEMS_GPIO_TRIGGER_LEVEL_HIGH,

  /**
   * @brief This enumerator indicates that the pin is held low.
   */
  RTEMS_GPIO_TRIGGER_LEVEL_LOW
} rtems_gpio_trigger;

/**
 * @brief This enumeration represents whether a pin is a pad on the part or
 *   something the driver provides.
 */
typedef enum {
  /**
   * @brief This enumerator indicates a pad on the part the controller is part
   *   of.
   */
  RTEMS_GPIO_PIN_PHYSICAL = 0,

  /**
   * @brief This enumerator indicates a pin the driver implements rather than
   *   one the part has.
   *
   * An expander behind I2C or SPI, a bit in a shift register, a line on an
   * FPGA fabric, or a pin that exists only in software.  A virtual pin
   * configures, reads, writes and reports through the same calls as any
   * other.  Whether access may block is rtems_gpio_drv_ctrl::can_block and not
   * this: a pad reached over a slow bus may block and a software pin may
   * not.
   */
  RTEMS_GPIO_PIN_VIRTUAL
} rtems_gpio_pin_kind;

/**
 * @name Pin Capabilities
 *
 * What a pin is able to do, as reported in rtems_gpio_pin_info::capabilities
 * by rtems_gpio_pin_get_info().  A caller can ask before it configures
 * anything.
 *
 * @{
 */

/**
 * @brief This constant indicates that the pin can be an input.
 */
#define RTEMS_GPIO_CAP_INPUT (1u << 0)

/**
 * @brief This constant indicates that the pin can be an output.
 */
#define RTEMS_GPIO_CAP_OUTPUT (1u << 1)

/**
 * @brief This constant indicates that reading an output pin returns the level
 *   of the pad rather than the level last written to it.
 */
#define RTEMS_GPIO_CAP_OUTPUT_READBACK (1u << 2)

/**
 * @brief This constant indicates that the pin has a pull-up resistor.
 */
#define RTEMS_GPIO_CAP_PULL_UP (1u << 3)

/**
 * @brief This constant indicates that the pin has a pull-down resistor.
 */
#define RTEMS_GPIO_CAP_PULL_DOWN (1u << 4)

/**
 * @brief This constant indicates that the pin can drive open-drain.
 */
#define RTEMS_GPIO_CAP_OPEN_DRAIN (1u << 5)

/**
 * @brief This constant indicates that the pin can drive open-source.
 */
#define RTEMS_GPIO_CAP_OPEN_SOURCE (1u << 6)

/**
 * @brief This constant indicates that the pin has a selectable drive
 *   strength.
 */
#define RTEMS_GPIO_CAP_DRIVE_STRENGTH (1u << 7)

/**
 * @brief This constant indicates that the pin has a hardware debounce filter.
 */
#define RTEMS_GPIO_CAP_DEBOUNCE (1u << 8)

/**
 * @brief This constant indicates that the pin can interrupt on a low to high
 *   transition.
 */
#define RTEMS_GPIO_CAP_EDGE_RISING (1u << 9)

/**
 * @brief This constant indicates that the pin can interrupt on a high to low
 *   transition.
 */
#define RTEMS_GPIO_CAP_EDGE_FALLING (1u << 10)

/**
 * @brief This constant indicates that the pin can interrupt on a transition
 *   in either direction.
 */
#define RTEMS_GPIO_CAP_EDGE_BOTH (1u << 11)

/**
 * @brief This constant indicates that the pin can interrupt while held high.
 */
#define RTEMS_GPIO_CAP_LEVEL_HIGH (1u << 12)

/**
 * @brief This constant indicates that the pin can interrupt while held low.
 */
#define RTEMS_GPIO_CAP_LEVEL_LOW (1u << 13)

/**
 * @brief This constant indicates that the pin can wake the system from a low
 *   power state.
 */
#define RTEMS_GPIO_CAP_WAKEUP (1u << 14)

/** @} */

/**
 * @name Pin Availability
 *
 * Whether the board permits a pin to be used, as reported in
 * rtems_gpio_pin_info::flags.  A different question from whether the silicon
 * can; both are the BSP's to answer.
 *
 * @{
 */

/**
 * @brief This constant indicates that the pin exists and is free to use.
 */
#define RTEMS_GPIO_PIN_AVAILABLE 0u

/**
 * @brief This constant indicates that the pin must never be handed out.
 *
 * It belongs to something the board cannot do without: a flash pad, a power
 * rail select, the console UART, a USB differential pair.  Configuring it
 * breaks the board rather than failing visibly, so rtems_gpio_pin_configure()
 * refuses it with EACCES.
 */
#define RTEMS_GPIO_PIN_RESERVED (1u << 0)

/**
 * @brief This constant indicates that the pin is in use by another driver or
 *   by an earlier caller.
 */
#define RTEMS_GPIO_PIN_IN_USE (1u << 1)

/**
 * @brief This constant indicates that the pin is usable, but the level held
 *   at reset changes how the board starts.
 *
 * A strapping or boot-mode pin.  Not refused, since using one is sometimes
 * unavoidable on a small part, but a caller that has a choice should be able
 * to make it knowingly.
 */
#define RTEMS_GPIO_PIN_STRAPPING (1u << 2)

/**
 * @brief This constant indicates that the pin is not brought out to a pad in
 *   this package.
 *
 * The controller has it, this board cannot reach it.
 */
#define RTEMS_GPIO_PIN_NO_PAD (1u << 3)

/** @} */

/**
 * @name Configuration Flags
 *
 * Options applied to a pin through rtems_gpio_config::flags.
 *
 * @{
 */

/**
 * @brief This constant indicates that the pin is active low.
 *
 * rtems_gpio_pin_get() and rtems_gpio_pin_set() then speak in logical levels
 * and the driver inverts, so that every consumer of the pin does not have to
 * agree about it separately.
 */
#define RTEMS_GPIO_FLAG_ACTIVE_LOW (1u << 0)

/**
 * @brief This constant indicates that the pin may wake the system from a low
 *   power state.
 */
#define RTEMS_GPIO_FLAG_WAKEUP (1u << 1)

/** @} */

/**
 * @brief This structure provides the configuration of a pin.
 *
 * The same structure sets a configuration and reports one, so that what
 * rtems_gpio_pin_get_configuration() returns can be compared against what was
 * asked for.  A driver is allowed to round a drive strength or a debounce
 * interval to what the hardware has, and reporting the rounded value is how a
 * caller finds out.
 */
typedef struct {
  /**
   * @brief This member contains the direction of the pin.
   */
  rtems_gpio_direction direction;

  /**
   * @brief This member contains the pull resistor applied to the pin, if any.
   */
  rtems_gpio_bias bias;

  /**
   * @brief This member contains how an output drives.  It is ignored for an
   *   input.
   */
  rtems_gpio_drive drive;

  /**
   * @brief This member contains what raises an interrupt on the pin, if
   *   anything.
   */
  rtems_gpio_trigger trigger;

  /**
   * @brief This member contains the #RTEMS_GPIO_FLAG_ACTIVE_LOW and
   *   #RTEMS_GPIO_FLAG_WAKEUP flags applied to the pin.
   */
  uint32_t flags;

  /**
   * @brief This member contains the drive strength in microamperes, or 0 for
   *   the driver's default.
   *
   * Microamperes rather than an index, which means nothing without the part's
   * table and cannot be compared across two parts.  A driver rounds to what
   * it has and reports the rounded value back.
   */
  uint32_t drive_strength;

  /**
   * @brief This member contains the debounce interval in microseconds, or 0
   *   for none.
   *
   * Microseconds and not clock ticks, so that the value does not mean
   * something different in an application configured for a different tick
   * rate.
   */
  uint32_t debounce;

  /**
   * @brief This member contains the level an output takes when it is
   *   configured.
   *
   * Logical, so #RTEMS_GPIO_FLAG_ACTIVE_LOW applies to it.  Ignored unless the
   * direction is an output.  Setting the level as part of configuring the pin
   * is what keeps an output from glitching to the wrong level between being
   * made an output and being written.
   */
  int initial_value;
} rtems_gpio_config;

/**
 * @brief This structure provides what a pin is, as opposed to how it is
 *   configured.
 *
 * Static for the life of the controller, apart from
 * rtems_gpio_pin_info::flags, which tracks whether the pin is in use.
 */
typedef struct {
  /**
   * @brief This member contains the logical pin this describes.
   *
   * Filled in by the caller before #RTEMS_GPIO_IOCTL_PIN_GET_INFO and returned
   * unchanged, so that the answer carries its own question.
   */
  uint32_t pin;

  /**
   * @brief This member contains whether the pin is a pad on the part or one
   *   the driver provides.
   */
  rtems_gpio_pin_kind kind;

  /**
   * @brief This member contains the #RTEMS_GPIO_CAP_INPUT and following
   *   capability flags the pin supports.
   */
  uint32_t capabilities;

  /**
   * @brief This member contains #RTEMS_GPIO_PIN_AVAILABLE, or some of
   *   #RTEMS_GPIO_PIN_RESERVED, #RTEMS_GPIO_PIN_IN_USE,
   *   #RTEMS_GPIO_PIN_STRAPPING and #RTEMS_GPIO_PIN_NO_PAD.
   */
  uint32_t flags;

  /**
   * @brief This member contains what the board calls this pin, or an empty
   *   string.
   *
   * A silkscreen label, a net name, or the function a board fixed to the pin:
   * "LED0", "SDA", "BOOT".  A caller must not require it, since most
   * controllers will leave it empty, but where a board has names this is what
   * lets a configuration name a pin rather than count pads.
   */
  char name[RTEMS_GPIO_NAME_MAX];

  /**
   * @brief This member contains what holds the pin, or an empty string.
   *
   * Only meaningful with #RTEMS_GPIO_PIN_RESERVED or #RTEMS_GPIO_PIN_IN_USE,
   * and empty unless the driver knows.  It is the difference between EACCES
   * and EACCES because the I2C driver has the pad, which is the question
   * actually being asked when a configure is refused.
   */
  char owner[RTEMS_GPIO_NAME_MAX];
} rtems_gpio_pin_info;

/**
 * @brief This structure provides what a controller is.
 */
typedef struct {
  /**
   * @brief This member contains the number of logical pins, which are 0 to
   *   pin_count - 1.
   */
  uint32_t pin_count;

  /**
   * @brief This member is true, if an operation on this controller may
   *   block, otherwise false.
   */
  bool can_block;

  /**
   * @brief This member contains what the controller calls itself, for
   *   diagnostics.
   */
  char name[RTEMS_GPIO_NAME_MAX];
} rtems_gpio_info;

/**
 * @brief This structure provides a set of pins and their values, for the
 *   operations that act on more than one pin at a time.
 *
 * Bit @a n is logical pin @a n, so a banked controller finds its banks
 * already separated into words.  The caller owns the storage and says how
 * much of it there is; rtems_gpio_get_info() reports the pin count to size
 * it from.
 *
 * Serialised as one transaction, so no other caller observes an intermediate
 * state.  Simultaneous physical transitions are @b not implied.
 */
typedef struct {
  /**
   * @brief This member contains how many words mask and values point to.
   *
   * At least one and at most RTEMS_GPIO_BITMAP_WORDS() of the controller's
   * pin count.  Pins from word_count * #RTEMS_GPIO_BITMAP_WORD_BITS upwards
   * are not part of the operation.
   */
  size_t word_count;

  /**
   * @brief This member selects the pins to act on, one bit per pin.
   */
  uint32_t* mask;

  /**
   * @brief This member contains one logical level per selected pin.
   *
   * Read for a set and written for a get.  Logical levels, so
   * #RTEMS_GPIO_FLAG_ACTIVE_LOW applies per pin.  Bits not selected by mask
   * are ignored on a set and undefined on a get.
   */
  uint32_t* values;
} rtems_gpio_pin_bitmap;

/**
 * @brief This type represents the interrupt handler a pin calls.
 *
 * Runs in whatever context the driver raises it from.  Where
 * rtems_gpio_drv_ctrl::can_block is false that is usually interrupt context,
 * so the handler is bound by the same rules as any other RTEMS interrupt
 * handler; where it is true the handler usually runs in a task.
 * rtems_gpio_get_info() is how a caller tells which it has.
 *
 * @param pin is the logical pin that raised the interrupt.
 *
 * @param arg is the argument given to rtems_gpio_pin_irq_enable().
 */
typedef void (*rtems_gpio_irq_handler)(uint32_t pin, void* arg);

/**
 * @brief This type represents the options an interrupt is enabled with.
 *
 * A bit field, so that an option added later is another bit and not a
 * reinterpretation of this one.  A fixed width type rather than an
 * enumeration because this crosses an ioctl() boundary, where the width of
 * an enumeration is the compiler's to choose.
 */
typedef uint32_t rtems_gpio_irq_flags;

/**
 * @brief This constant indicates that no option beyond the default is
 *   requested.
 *
 * The only bit defined, and what every caller passes.  It selects nothing:
 * the argument exists so that a bit can be added later without a second
 * entry point, and until one is, this is the whole of it.
 *
 * Deliberately not zero.  Zero in a bit field is the absence of every
 * option rather than the presence of one, so a caller that passed it would
 * not be naming this bit, and a caller that zeroed its arguments would be
 * asking for something the API has never defined.  Both are answered with
 * ENOTSUP instead.
 */
#define RTEMS_GPIO_IRQ_DEFAULT ((rtems_gpio_irq_flags)(1U << 0))

/**
 * @brief This structure provides what a GPIO driver implements.
 *
 * Any handler except pin_get_info may be NULL, and the generic layer then
 * answers ENOTSUP for the operations that need it.  A driver is not obliged
 * to grow stubs for hardware it does not have.
 *
 * Each handler returns 0 on success or a positive errno on failure.
 * Returning ENOTSUP for a request the hardware cannot meet is correct and
 * expected; quietly substituting something else is not.
 */
typedef struct {
  /**
   * @brief This member reports what a pin is.
   *
   * Required.  A driver that does not implement it cannot be registered,
   * because a caller would then have no way to find out anything about a pin
   * before configuring it.
   */
  int (*pin_get_info)(rtems_gpio_drv_ctrl* ctrl, uint32_t pin,
                      rtems_gpio_pin_info* info);

  /**
   * @brief This member applies a configuration to a pin.
   *
   * The generic layer has already checked that the pin is in range and is
   * neither reserved nor in use, and that every capability the configuration
   * asks for is one the pin reported.  A driver that rounds a value it cannot
   * meet exactly should write what it actually applied back into @a config.
   */
  int (*pin_configure)(rtems_gpio_drv_ctrl* ctrl, uint32_t pin,
                       rtems_gpio_config* config);

  /**
   * @brief This member reports how a pin is configured now.
   *
   * Only called for a pin the driver reports as in use, so a driver need not
   * handle the unconfigured case.  What it reports is what it applied,
   * including any value it rounded.
   */
  int (*pin_get_config)(rtems_gpio_drv_ctrl* ctrl, uint32_t pin,
                        rtems_gpio_config* config);

  /**
   * @brief This member returns a pin to its unconfigured state.
   */
  int (*pin_release)(rtems_gpio_drv_ctrl* ctrl, uint32_t pin);

  /**
   * @brief This member reads a pin into @a value as a logical 0 or 1.
   */
  int (*pin_get)(rtems_gpio_drv_ctrl* ctrl, uint32_t pin, int* value);

  /**
   * @brief This member writes a pin from @a value as a logical 0 or 1.
   */
  int (*pin_set)(rtems_gpio_drv_ctrl* ctrl, uint32_t pin, int value);

  /**
   * @brief This member inverts a pin's current level.
   *
   * Separate from a read and a write because on most parts it is one register
   * access, and because a read-modify-write from the caller is not atomic
   * against another caller.
   */
  int (*pin_toggle)(rtems_gpio_drv_ctrl* ctrl, uint32_t pin);

  /**
   * @brief This member reads several pins as one operation.
   *
   * Both bitmaps are @a words words, which the caller allocated and which
   * may be fewer than the controller's pin count needs.  A driver must not
   * touch a word beyond that, so a wide controller reads only the banks the
   * caller asked for.  The bitmaps carry physical levels: the generic layer
   * applies #RTEMS_GPIO_FLAG_ACTIVE_LOW above this call.
   */
  int (*pin_get_multiple)(rtems_gpio_drv_ctrl* ctrl, const uint32_t* mask,
                          uint32_t* values, size_t words);

  /**
   * @brief This member writes several pins as one operation.
   *
   * Both bitmaps are @a words words and carry physical levels, with the same
   * limit as pin_get_multiple().
   */
  int (*pin_set_multiple)(rtems_gpio_drv_ctrl* ctrl, const uint32_t* mask,
                          const uint32_t* values, size_t words);

  /**
   * @brief This member starts delivering a pin's interrupt to a handler.
   *
   * The options reach the driver rather than being consumed above it, so
   * that an option a driver has to implement -- sharing a line between
   * handlers is the one being asked for -- needs no second change to this
   * table.  A driver returns ENOTSUP for an option it does not implement.
   */
  int (*pin_irq_enable)(rtems_gpio_drv_ctrl* ctrl, uint32_t pin,
                        rtems_gpio_irq_flags flags,
                        rtems_gpio_irq_handler handler, void* arg);

  /**
   * @brief This member stops delivering a pin's interrupt.
   */
  int (*pin_irq_disable)(rtems_gpio_drv_ctrl* ctrl, uint32_t pin);

  /**
   * @brief This member releases whatever the driver holds.
   *
   * Called when the node is destroyed.  May be NULL for a controller whose
   * storage is static, which most are.
   */
  void (*destroy)(rtems_gpio_drv_ctrl* ctrl);
} rtems_gpio_drv_handlers;

/**
 * @brief This structure provides a GPIO controller.
 *
 * A driver embeds this in its own structure and recovers that structure with
 * RTEMS_CONTAINER_OF(), so the generic layer needs no allocation and a
 * controller can be a static object in the BSP.
 */
struct rtems_gpio_drv_ctrl {
  /**
   * @brief This member contains what the driver implements.
   */
  const rtems_gpio_drv_handlers* handlers;

  /**
   * @brief This member contains the number of logical pins this controller
   *   publishes.
   */
  uint32_t pin_count;

  /**
   * @brief This member contains what the controller calls itself.
   */
  const char* name;

  /**
   * @brief This member is true, if an operation on this controller may
   *   block, otherwise false.
   *
   * True for a controller reached over a bus, such as an I2C or SPI
   * expander, and false for one that is a few memory mapped registers.  A
   * caller in interrupt context must not use a controller that may block.
   */
  bool can_block;

  /**
   * @brief This member contains one bit per pin, set while that pin is
   *   configured #RTEMS_GPIO_FLAG_ACTIVE_LOW.
   *
   * Supplied by the driver as RTEMS_GPIO_BITMAP_WORDS() of pin_count words
   * and maintained by the generic layer, which applies the polarity above
   * the handlers.  A controller with no inversion register supports active
   * low for free this way.
   */
  uint32_t* active_low;

  /**
   * @brief This member contains scratch space for one bitmap.
   *
   * Supplied by the driver as RTEMS_GPIO_BITMAP_WORDS() of pin_count words
   * and used only by the generic layer, which needs somewhere to build the
   * physical levels for a bulk write without modifying the caller's bitmap.
   * Only touched under the controller lock.
   */
  uint32_t* scratch;

  /**
   * @brief This member serialises access to the controller.
   *
   * Initialised by rtems_gpio_drv_ctrl_init().  Held across every operation,
   * so a driver's handlers do not need locking of their own.
   */
  rtems_mutex mutex;
};

/**
 * @brief This structure provides a pin and its configuration, as one ioctl()
 *   argument.
 */
typedef struct {
  /**
   * @brief This member contains the logical pin to act on.
   */
  uint32_t pin;

  /**
   * @brief This member contains the configuration of the pin.
   */
  rtems_gpio_config config;
} rtems_gpio_pin_config;

/**
 * @brief This structure provides a pin and its level, as one ioctl()
 *   argument.
 */
typedef struct {
  /**
   * @brief This member contains the logical pin to act on.
   */
  uint32_t pin;

  /**
   * @brief This member contains the logical level of the pin, 0 or 1.
   */
  int value;
} rtems_gpio_pin_value;

/**
 * @brief This structure provides a pin and its interrupt handler, as one
 *   ioctl() argument.
 */
typedef struct {
  /**
   * @brief This member contains the logical pin to act on.
   */
  uint32_t pin;

  /**
   * @brief This member contains #RTEMS_GPIO_IRQ_DEFAULT.
   */
  rtems_gpio_irq_flags flags;

  /**
   * @brief This member contains the handler to call.
   */
  rtems_gpio_irq_handler handler;

  /**
   * @brief This member contains the argument passed to the handler.
   */
  void* arg;
} rtems_gpio_pin_irq;

/**
 * @name GPIO IO Control Commands
 *
 * Every operation is one of these on the controller's node.  The
 * rtems_gpio_*() wrappers below are the supported way to issue them.  An
 * unrecognised command sets errno to ENOTTY.
 *
 * @{
 */

/**
 * @brief Reports the controller.
 *
 * The argument type is a pointer to rtems_gpio_info.
 */
#define RTEMS_GPIO_IOCTL_GET_INFO _IOR('G', 0, rtems_gpio_info)

/**
 * @brief Reports a pin.
 *
 * The argument type is a pointer to rtems_gpio_pin_info, with
 * rtems_gpio_pin_info::pin set by the caller.
 */
#define RTEMS_GPIO_IOCTL_PIN_GET_INFO _IOWR('G', 1, rtems_gpio_pin_info)

/**
 * @brief Configures a pin.
 *
 * The argument type is a pointer to rtems_gpio_pin_config.
 */
#define RTEMS_GPIO_IOCTL_PIN_CONFIGURE _IOWR('G', 2, rtems_gpio_pin_config)

/**
 * @brief Reports how a pin is configured.
 *
 * The argument type is a pointer to rtems_gpio_pin_config, with
 * rtems_gpio_pin_config::pin set by the caller.
 */
#define RTEMS_GPIO_IOCTL_PIN_GET_CONFIG _IOWR('G', 3, rtems_gpio_pin_config)

/**
 * @brief Unconfigures a pin.
 *
 * The argument type is a pointer to uint32_t.
 */
#define RTEMS_GPIO_IOCTL_PIN_RELEASE _IOW('G', 4, uint32_t)

/**
 * @brief Reads a pin.
 *
 * The argument type is a pointer to rtems_gpio_pin_value.
 */
#define RTEMS_GPIO_IOCTL_PIN_GET _IOWR('G', 5, rtems_gpio_pin_value)

/**
 * @brief Writes a pin.
 *
 * The argument type is a pointer to rtems_gpio_pin_value.
 */
#define RTEMS_GPIO_IOCTL_PIN_SET _IOW('G', 6, rtems_gpio_pin_value)

/**
 * @brief Inverts a pin.
 *
 * The argument type is a pointer to uint32_t.
 */
#define RTEMS_GPIO_IOCTL_PIN_TOGGLE _IOW('G', 7, uint32_t)

/**
 * @brief Reads several pins.
 *
 * The argument type is a pointer to rtems_gpio_pin_bitmap.
 */
#define RTEMS_GPIO_IOCTL_PIN_GET_MULTIPLE _IOWR('G', 8, rtems_gpio_pin_bitmap)

/**
 * @brief Writes several pins.
 *
 * The argument type is a pointer to rtems_gpio_pin_bitmap.
 */
#define RTEMS_GPIO_IOCTL_PIN_SET_MULTIPLE _IOW('G', 9, rtems_gpio_pin_bitmap)

/**
 * @brief Enables a pin's interrupt.
 *
 * The argument type is a pointer to rtems_gpio_pin_irq.
 */
#define RTEMS_GPIO_IOCTL_PIN_IRQ_ENABLE _IOW('G', 10, rtems_gpio_pin_irq)

/**
 * @brief Disables a pin's interrupt.
 *
 * The argument type is a pointer to uint32_t.
 */
#define RTEMS_GPIO_IOCTL_PIN_IRQ_DISABLE _IOW('G', 11, uint32_t)

/** @} */

/**
 * @name Driver Side
 *
 * Everything here is prefixed rtems_gpio_drv_ and belongs to the driver.  An
 * application calls none of it: a handler runs only underneath one of the
 * file operations above, and rtems_gpio_drv_ctrl is reachable only from
 * there, so the split in the names is also the split in what may touch what.
 *
 * @{
 */

/**
 * @brief Prepares a controller for registration.
 *
 * @param[in, out] ctrl is the controller, whose handlers, pin_count and name
 *   the driver has already set.  Its mutex is initialised here.
 *
 * @retval 0 Successful operation.
 * @retval EINVAL @a ctrl is NULL, has no handlers, has no pin_get_info
 *   handler, or publishes no pins.
 */
int rtems_gpio_drv_ctrl_init(rtems_gpio_drv_ctrl* ctrl);

/**
 * @brief Publishes a controller as a device node.
 *
 * @param[in, out] ctrl is a controller that rtems_gpio_drv_ctrl_init() has
 *   returned 0 for.
 *
 * @param path is where the node goes, conventionally "/dev/gpio", or
 *   "/dev/gpio0" on a board with more than one controller.
 *
 * @retval 0 Successful operation.
 * @retval -1 An error occurred.  The errno is set to indicate the error and
 *   the controller's destroy handler has been called.
 */
int rtems_gpio_drv_ctrl_register(rtems_gpio_drv_ctrl* ctrl, const char* path);

/** @} */

/**
 * @name Application Side
 *
 * Each of these is one ioctl() on a descriptor for a controller's node.  Each
 * returns 0 on success, or -1 with errno set: ENOTSUP where the driver does
 * not implement the operation, ENODEV where the pin is out of range, EBUSY
 * where the pin is already configured by someone else, EACCES where the board
 * reserves the pin, and ENOTTY where the descriptor is not a GPIO controller.
 *
 * @{
 */

/**
 * @brief Reports the controller behind a descriptor.
 *
 * @param fd is the descriptor for the controller's device node.
 *
 * @param[out] info is the pointer to an rtems_gpio_info object.  When the
 *   call is successful, the controller's name and pin count will be stored in
 *   this object.
 *
 * @retval 0 Successful operation.
 * @retval -1 An error occurred.  The errno is set to indicate the error.
 */
int rtems_gpio_get_info(int fd, rtems_gpio_info* info);

/**
 * @brief Reports what a pin is, before anything is done to it.
 *
 * @param fd is the descriptor for the controller's device node.
 *
 * @param pin is the logical pin to report on.
 *
 * @param[out] info is the pointer to an rtems_gpio_pin_info object.  When the
 *   call is successful, the pin's kind, capabilities, availability and name
 *   will be stored in this object.
 *
 * @retval 0 Successful operation.
 * @retval -1 An error occurred.  The errno is set to indicate the error.
 */
int rtems_gpio_pin_get_info(int fd, uint32_t pin, rtems_gpio_pin_info* info);

/**
 * @brief Finds the pin the board calls @a name.
 *
 * Walks the controller's pins and returns the first whose
 * rtems_gpio_pin_info::name matches.  Linear, and meant for start-up rather
 * than for a loop.
 *
 * @param fd is the descriptor for the controller's device node.
 *
 * @param name is the name to look for.
 *
 * @param[out] pin is the pointer to a uint32_t object.  When the call is
 *   successful, the logical pin will be stored in this object.
 *
 * @retval 0 Successful operation.
 * @retval -1 An error occurred.  The errno is set to indicate the error, and
 *   is ENOENT if no pin has that name.
 */
int rtems_gpio_pin_by_name(int fd, const char* name, uint32_t* pin);

/**
 * @brief Configures a pin.
 *
 * Nothing in the hardware changes unless the call succeeds.
 *
 * @param fd is the descriptor for the controller's device node.
 *
 * @param pin is the logical pin to configure.
 *
 * @param[in, out] config is what to apply.  On success it holds what was
 *   actually applied, which may differ where the driver rounded a value.
 *
 * @retval 0 Successful operation.
 * @retval -1 An error occurred.  The errno is set to indicate the error:
 *   EACCES if the board reserves the pin, EBUSY if it is already configured,
 *   and ENOTSUP if the configuration asks for something the pin does not
 *   report in rtems_gpio_pin_info::capabilities.
 */
int rtems_gpio_pin_configure(int fd, uint32_t pin, rtems_gpio_config* config);

/**
 * @brief Reports how a pin is configured now.
 *
 * Where the driver implements it, asking about a pin nothing has configured
 * is not an error: the direction is #RTEMS_GPIO_DIRECTION_NONE and the rest of
 * the structure is zero.  A driver with no pin_get_config handler answers
 * ENOTSUP for every pin, configured or not.
 *
 * @param fd is the descriptor for the controller's device node.
 *
 * @param pin is the logical pin to report on.
 *
 * @param[out] config is the pointer to an rtems_gpio_config object.  When the
 *   call is successful, the configuration of the pin will be stored in this
 *   object.
 *
 * @retval 0 Successful operation.
 * @retval -1 An error occurred.  The errno is set to indicate the error.
 */
int rtems_gpio_pin_get_configuration(int fd, uint32_t pin,
                                     rtems_gpio_config* config);

/**
 * @brief Returns a pin to its unconfigured state.
 *
 * @param fd is the descriptor for the controller's device node.
 *
 * @param pin is the logical pin to release.
 *
 * @retval 0 Successful operation.
 * @retval -1 An error occurred.  The errno is set to indicate the error.
 */
int rtems_gpio_pin_release(int fd, uint32_t pin);

/**
 * @brief Reads a pin.
 *
 * @param fd is the descriptor for the controller's device node.
 *
 * @param pin is the logical pin to read.
 *
 * @param[out] value is the pointer to an int object.  When the call is
 *   successful, 0 or 1 will be stored in this object.  The level is logical:
 *   a pin configured #RTEMS_GPIO_FLAG_ACTIVE_LOW reads 1 when the pad is low.
 *
 * @retval 0 Successful operation.
 * @retval -1 An error occurred.  The errno is set to indicate the error.
 */
int rtems_gpio_pin_get(int fd, uint32_t pin, int* value);

/**
 * @brief Writes a pin.
 *
 * @param fd is the descriptor for the controller's device node.
 *
 * @param pin is the logical pin to write.
 *
 * @param value is the logical level to write, 0 or 1.
 *
 * @retval 0 Successful operation.
 * @retval -1 An error occurred.  The errno is set to indicate the error.
 */
int rtems_gpio_pin_set(int fd, uint32_t pin, int value);

/**
 * @brief Inverts a pin.
 *
 * @param fd is the descriptor for the controller's device node.
 *
 * @param pin is the logical pin to invert.
 *
 * @retval 0 Successful operation.
 * @retval -1 An error occurred.  The errno is set to indicate the error.
 */
int rtems_gpio_pin_toggle(int fd, uint32_t pin);

/**
 * @brief Reads several pins as one operation.
 *
 * @param fd is the descriptor for the controller's device node.
 *
 * @param[in, out] list is the pins to read on the way in, and their levels in
 *   rtems_gpio_pin_bitmap::values on the way out.
 *
 * @retval 0 Successful operation.
 * @retval -1 An error occurred.  The errno is set to indicate the error.
 */
int rtems_gpio_pin_get_multiple(int fd, rtems_gpio_pin_bitmap* list);

/**
 * @brief Writes several pins as one operation.
 *
 * @param fd is the descriptor for the controller's device node.
 *
 * @param list is the pins to write and the levels to write to them.
 *
 * @retval 0 Successful operation.
 * @retval -1 An error occurred.  The errno is set to indicate the error.
 */
int rtems_gpio_pin_set_multiple(int fd, const rtems_gpio_pin_bitmap* list);

/**
 * @brief Starts delivering a pin's interrupt to a handler.
 *
 * The pin must already be configured with a trigger other than
 * #RTEMS_GPIO_TRIGGER_NONE.
 *
 * @param fd is the descriptor for the controller's device node.
 *
 * @param pin is the logical pin whose interrupt to deliver.
 *
 * @param flags is #RTEMS_GPIO_IRQ_DEFAULT, which is the only bit defined.
 *
 * A handler already on the pin is replaced.  That is a property of this
 * call rather than something @a flags selects, so nothing has to be passed
 * to get it and no option name claims it.
 *
 * @param handler is the handler to call.
 *
 * @param arg is the argument to pass to the handler.
 *
 * @retval 0 Successful operation.
 * @retval -1 An error occurred.  The errno is set to indicate the error.
 *   ENOTSUP where @a flags is anything other than #RTEMS_GPIO_IRQ_DEFAULT,
 *   which is a caller built against a later header than this
 *   implementation.
 */
int rtems_gpio_pin_irq_enable(int fd, uint32_t pin, rtems_gpio_irq_flags flags,
                              rtems_gpio_irq_handler handler, void* arg);

/**
 * @brief Stops delivering a pin's interrupt.
 *
 * @param fd is the descriptor for the controller's device node.
 *
 * @param pin is the logical pin whose interrupt to stop.
 *
 * @retval 0 Successful operation.
 * @retval -1 An error occurred.  The errno is set to indicate the error.
 */
int rtems_gpio_pin_irq_disable(int fd, uint32_t pin);

/** @} */

/** @} */

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* _DEV_GPIO_GPIO_H */
