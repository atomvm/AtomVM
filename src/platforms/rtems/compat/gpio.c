/* SPDX-License-Identifier: BSD-2-Clause */

/**
 * @file
 *
 * @ingroup RTEMSGenericGPIOAPI
 *
 * @brief This source file contains the implementation of the generic GPIO
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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <dev/gpio/gpio.h>

#include <rtems/imfs.h>
#include <rtems/seterr.h>

#include <errno.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/stat.h>

/*
 * The generic layer answers three questions before a driver sees anything:
 * is the pin one this controller has, does the board allow it, and can the
 * pin do what is being asked of it.  Each is answered from what
 * pin_get_info() reports, which is why that handler is the one a driver
 * cannot leave out.
 *
 * Doing it here rather than in each driver is the point of the exercise.  A
 * check that lives in a driver is a check the next driver will not have, and
 * the failure it is there to prevent -- pointing a pad that carries the
 * flash clock at a GPIO output -- does not produce an error, it produces a
 * board that no longer boots.
 */

/*
 * Logical polarity lives here rather than in the driver.  A controller with
 * an inversion register is the exception, not the rule, and a pin that is
 * active low is a property of how the board wired it, not of what the
 * silicon can do.  So the handlers below always see physical levels and
 * this layer flips the ones the caller configured RTEMS_GPIO_FLAG_ACTIVE_LOW.
 */
static bool rtems_gpio_bit_get(const uint32_t* map, uint32_t pin) {
  return (map[pin / RTEMS_GPIO_BITMAP_WORD_BITS] &
          (1u << (pin % RTEMS_GPIO_BITMAP_WORD_BITS))) != 0;
}

static void rtems_gpio_bit_put(uint32_t* map, uint32_t pin, bool value) {
  uint32_t word = pin / RTEMS_GPIO_BITMAP_WORD_BITS;
  uint32_t bit = 1u << (pin % RTEMS_GPIO_BITMAP_WORD_BITS);

  if (value) {
    map[word] |= bit;
  } else {
    map[word] &= ~bit;
  }
}

static bool rtems_gpio_is_active_low(const rtems_gpio_drv_ctrl* ctrl,
                                     uint32_t pin) {
  return rtems_gpio_bit_get(ctrl->active_low, pin);
}

static int rtems_gpio_to_physical(const rtems_gpio_drv_ctrl* ctrl, uint32_t pin,
                                  int value) {
  if (rtems_gpio_is_active_low(ctrl, pin)) {
    return value == 0;
  }

  return value != 0;
}

static rtems_gpio_drv_ctrl* rtems_gpio_get_ctrl(const rtems_libio_t* iop) {
  return IMFS_generic_get_context_by_iop(iop);
}

static void rtems_gpio_drv_ctrl_obtain(rtems_gpio_drv_ctrl* ctrl) {
  rtems_mutex_lock(&ctrl->mutex);
}

static void rtems_gpio_drv_ctrl_unlock(rtems_gpio_drv_ctrl* ctrl) {
  rtems_mutex_unlock(&ctrl->mutex);
}

/*
 * Every operation needs the pin's information, so fetching it is also where
 * the range check happens: a driver is asked about a pin it publishes or it
 * is not asked at all.
 */
static int rtems_gpio_pin_info_locked(rtems_gpio_drv_ctrl* ctrl, uint32_t pin,
                                      rtems_gpio_pin_info* info) {
  if (pin >= ctrl->pin_count) {
    return ENODEV;
  }

  memset(info, 0, sizeof(*info));
  info->pin = pin;

  return (*ctrl->handlers->pin_get_info)(ctrl, pin, info);
}

/*
 * Which capability each part of a configuration needs.  Kept as a function
 * rather than a table so that a driver reporting a capability it does not
 * have is the only way to get past it; there is no path that forgets to
 * ask.
 */
static uint32_t rtems_gpio_required_caps(const rtems_gpio_config* config) {
  uint32_t caps = 0;

  switch (config->direction) {
  case RTEMS_GPIO_DIRECTION_INPUT:
    caps |= RTEMS_GPIO_CAP_INPUT;
    break;
  case RTEMS_GPIO_DIRECTION_OUTPUT:
    caps |= RTEMS_GPIO_CAP_OUTPUT;
    break;
  case RTEMS_GPIO_DIRECTION_NONE:
    break;
  }

  switch (config->bias) {
  case RTEMS_GPIO_BIAS_PULL_UP:
    caps |= RTEMS_GPIO_CAP_PULL_UP;
    break;
  case RTEMS_GPIO_BIAS_PULL_DOWN:
    caps |= RTEMS_GPIO_CAP_PULL_DOWN;
    break;
  case RTEMS_GPIO_BIAS_NONE:
    break;
  }

  /*
   * Only for an output.  An input's drive field is meaningless, and a
   * caller that leaves a configuration structure zeroed and asks for an
   * input should not be refused for the push-pull it did not ask for.
   */
  if (config->direction == RTEMS_GPIO_DIRECTION_OUTPUT) {
    switch (config->drive) {
    case RTEMS_GPIO_DRIVE_OPEN_DRAIN:
      caps |= RTEMS_GPIO_CAP_OPEN_DRAIN;
      break;
    case RTEMS_GPIO_DRIVE_OPEN_SOURCE:
      caps |= RTEMS_GPIO_CAP_OPEN_SOURCE;
      break;
    case RTEMS_GPIO_DRIVE_PUSH_PULL:
      break;
    }
  }

  switch (config->trigger) {
  case RTEMS_GPIO_TRIGGER_EDGE_RISING:
    caps |= RTEMS_GPIO_CAP_EDGE_RISING;
    break;
  case RTEMS_GPIO_TRIGGER_EDGE_FALLING:
    caps |= RTEMS_GPIO_CAP_EDGE_FALLING;
    break;
  case RTEMS_GPIO_TRIGGER_EDGE_BOTH:
    caps |= RTEMS_GPIO_CAP_EDGE_BOTH;
    break;
  case RTEMS_GPIO_TRIGGER_LEVEL_HIGH:
    caps |= RTEMS_GPIO_CAP_LEVEL_HIGH;
    break;
  case RTEMS_GPIO_TRIGGER_LEVEL_LOW:
    caps |= RTEMS_GPIO_CAP_LEVEL_LOW;
    break;
  case RTEMS_GPIO_TRIGGER_NONE:
    break;
  }

  if (config->drive_strength != 0) {
    caps |= RTEMS_GPIO_CAP_DRIVE_STRENGTH;
  }

  if (config->debounce != 0) {
    caps |= RTEMS_GPIO_CAP_DEBOUNCE;
  }

  if ((config->flags & RTEMS_GPIO_FLAG_WAKEUP) != 0) {
    caps |= RTEMS_GPIO_CAP_WAKEUP;
  }

  return caps;
}

static int rtems_gpio_do_configure(rtems_gpio_drv_ctrl* ctrl, uint32_t pin,
                                   rtems_gpio_config* config) {
  rtems_gpio_pin_info info;
  uint32_t needed;
  int logical;
  int err;

  if (ctrl->handlers->pin_configure == NULL) {
    return ENOTSUP;
  }

  err = rtems_gpio_pin_info_locked(ctrl, pin, &info);
  if (err != 0) {
    return err;
  }

  /*
   * EACCES rather than EBUSY, and before anything else: a reserved pin is
   * not a pin that happens to be taken, it is one the board says must not
   * be driven.  The two are worth telling apart in a log, because waiting
   * for the second to free up is reasonable and waiting for the first is
   * not.
   */
  if ((info.flags & RTEMS_GPIO_PIN_RESERVED) != 0) {
    return EACCES;
  }

  if ((info.flags & RTEMS_GPIO_PIN_NO_PAD) != 0) {
    return ENODEV;
  }

  if ((info.flags & RTEMS_GPIO_PIN_IN_USE) != 0) {
    return EBUSY;
  }

  needed = rtems_gpio_required_caps(config);
  if ((needed & ~info.capabilities) != 0) {
    return ENOTSUP;
  }

  /*
   * Record the polarity before the call, because initial_value is a logical
   * level like any other and the handler is owed a physical one.  The
   * caller's structure is put back afterwards: it asked in logical levels
   * and reading its own request back changed underneath it would be a
   * surprise.
   */
  rtems_gpio_bit_put(ctrl->active_low, pin,
                     (config->flags & RTEMS_GPIO_FLAG_ACTIVE_LOW) != 0);

  logical = config->initial_value;
  config->initial_value = rtems_gpio_to_physical(ctrl, pin, logical);

  err = (*ctrl->handlers->pin_configure)(ctrl, pin, config);

  config->initial_value = logical;

  if (err != 0) {
    rtems_gpio_bit_put(ctrl->active_low, pin, false);
  }

  return err;
}

/*
 * The operations that act on an already configured pin share a check: the
 * pin is one of ours and something has configured it.  Driving a pin nobody
 * configured is how a caller with an off-by-one writes to a pad that belongs
 * to another driver.
 */
static int rtems_gpio_check_configured(rtems_gpio_drv_ctrl* ctrl,
                                       uint32_t pin) {
  rtems_gpio_pin_info info;
  int err;

  err = rtems_gpio_pin_info_locked(ctrl, pin, &info);
  if (err != 0) {
    return err;
  }

  if ((info.flags & RTEMS_GPIO_PIN_IN_USE) == 0) {
    return EBADF;
  }

  return 0;
}

/*
 * Every selected pin is checked before any of it is applied.  A partly
 * applied set is worse than a refused one: the caller is told it failed and
 * the hardware is in a state it did not ask for and cannot infer.
 */
/*
 * How many pins a given bitmap actually reaches: the caller's words, capped
 * by the pins the controller has.
 */
static uint32_t rtems_gpio_bitmap_pins(const rtems_gpio_drv_ctrl* ctrl,
                                       const rtems_gpio_pin_bitmap* map) {
  uint32_t pins = (uint32_t)(map->word_count * RTEMS_GPIO_BITMAP_WORD_BITS);

  return pins < ctrl->pin_count ? pins : ctrl->pin_count;
}

static int rtems_gpio_check_bitmap(rtems_gpio_drv_ctrl* ctrl,
                                   const rtems_gpio_pin_bitmap* map) {
  uint32_t pin;
  int err;

  if (map->mask == NULL || map->values == NULL) {
    return EINVAL;
  }

  /*
   * Fewer words than the controller is wide is a caller that only cares
   * about the low pins, not a caller that got it wrong, so the operation is
   * limited to what was provided rather than refused.  More words than the
   * controller has pins is refused: it means the caller thinks this is a
   * bigger controller than it is, and quietly ignoring the extra would hide
   * that.
   */
  if (map->word_count == 0 ||
      map->word_count > RTEMS_GPIO_BITMAP_WORDS(ctrl->pin_count)) {
    return EINVAL;
  }

  for (pin = 0; pin < rtems_gpio_bitmap_pins(ctrl, map); ++pin) {
    if (!rtems_gpio_bit_get(map->mask, pin)) {
      continue;
    }

    err = rtems_gpio_check_configured(ctrl, pin);
    if (err != 0) {
      return err;
    }
  }

  return 0;
}

static int rtems_gpio_do_pin_set_multiple(rtems_gpio_drv_ctrl* ctrl,
                                          const rtems_gpio_pin_bitmap* map) {
  uint32_t pin;
  int err;

  if (ctrl->handlers->pin_set_multiple == NULL) {
    return ENOTSUP;
  }

  err = rtems_gpio_check_bitmap(ctrl, map);
  if (err != 0) {
    return err;
  }

  /*
   * The caller's bitmap holds logical levels and the handler takes physical
   * ones, so the active low pins are flipped on the way down.  Done into the
   * scratch word rather than in place because the argument is const and is
   * the caller's.
   */
  for (pin = 0; pin < rtems_gpio_bitmap_pins(ctrl, map); ++pin) {
    bool value;

    if (!rtems_gpio_bit_get(map->mask, pin)) {
      continue;
    }

    value = rtems_gpio_bit_get(map->values, pin);
    rtems_gpio_bit_put(ctrl->scratch, pin,
                       rtems_gpio_to_physical(ctrl, pin, value ? 1 : 0) != 0);
  }

  return (*ctrl->handlers->pin_set_multiple)(ctrl, map->mask, ctrl->scratch,
                                             map->word_count);
}

static int rtems_gpio_do_pin_get_multiple(rtems_gpio_drv_ctrl* ctrl,
                                          rtems_gpio_pin_bitmap* map) {
  uint32_t pin;
  int err;

  if (ctrl->handlers->pin_get_multiple == NULL) {
    return ENOTSUP;
  }

  err = rtems_gpio_check_bitmap(ctrl, map);
  if (err != 0) {
    return err;
  }

  err = (*ctrl->handlers->pin_get_multiple)(ctrl, map->mask, map->values,
                                            map->word_count);
  if (err != 0) {
    return err;
  }

  /* Physical on the way up, logical to the caller. */
  for (pin = 0; pin < rtems_gpio_bitmap_pins(ctrl, map); ++pin) {
    if (!rtems_gpio_bit_get(map->mask, pin)) {
      continue;
    }

    if (rtems_gpio_is_active_low(ctrl, pin)) {
      rtems_gpio_bit_put(map->values, pin,
                         !rtems_gpio_bit_get(map->values, pin));
    }
  }

  return 0;
}

static int rtems_gpio_ioctl(rtems_libio_t* iop, ioctl_command_t command,
                            void* arg) {
  rtems_gpio_drv_ctrl* ctrl = rtems_gpio_get_ctrl(iop);
  int err;

  if (arg == NULL) {
    rtems_set_errno_and_return_minus_one(EINVAL);
  }

  rtems_gpio_drv_ctrl_obtain(ctrl);

  switch (command) {
  case RTEMS_GPIO_IOCTL_GET_INFO: {
    rtems_gpio_info* info = arg;

    memset(info, 0, sizeof(*info));
    info->pin_count = ctrl->pin_count;
    info->can_block = ctrl->can_block;
    if (ctrl->name != NULL) {
      strncpy(info->name, ctrl->name, sizeof(info->name) - 1);
    }
    err = 0;
    break;
  }

  case RTEMS_GPIO_IOCTL_PIN_GET_INFO: {
    rtems_gpio_pin_info* info = arg;

    err = rtems_gpio_pin_info_locked(ctrl, info->pin, info);
    break;
  }

  case RTEMS_GPIO_IOCTL_PIN_CONFIGURE: {
    rtems_gpio_pin_config* pc = arg;

    err = rtems_gpio_do_configure(ctrl, pc->pin, &pc->config);
    break;
  }

  case RTEMS_GPIO_IOCTL_PIN_GET_CONFIG: {
    rtems_gpio_pin_config* pc = arg;
    rtems_gpio_pin_info info;

    if (ctrl->handlers->pin_get_config == NULL) {
      err = ENOTSUP;
      break;
    }

    err = rtems_gpio_pin_info_locked(ctrl, pc->pin, &info);
    if (err != 0) {
      break;
    }

    memset(&pc->config, 0, sizeof(pc->config));

    /*
     * A pin nobody has configured is not an error to ask about.  The
     * answer is RTEMS_GPIO_DIRECTION_NONE, which is what the zeroed
     * structure already says, and it is the answer a caller deciding
     * whether a pin is free wants.
     */
    if ((info.flags & RTEMS_GPIO_PIN_IN_USE) != 0) {
      err = (*ctrl->handlers->pin_get_config)(ctrl, pc->pin, &pc->config);

      /* Back into the logical levels the caller configured in. */
      if (err == 0) {
        pc->config.initial_value =
            rtems_gpio_to_physical(ctrl, pc->pin, pc->config.initial_value);
      }
    }
    break;
  }

  case RTEMS_GPIO_IOCTL_PIN_RELEASE: {
    uint32_t pin = *(const uint32_t*)arg;

    if (ctrl->handlers->pin_release == NULL) {
      err = ENOTSUP;
      break;
    }

    err = rtems_gpio_check_configured(ctrl, pin);
    if (err == 0) {
      err = (*ctrl->handlers->pin_release)(ctrl, pin);
    }

    if (err == 0) {
      rtems_gpio_bit_put(ctrl->active_low, pin, false);
    }
    break;
  }

  case RTEMS_GPIO_IOCTL_PIN_GET: {
    rtems_gpio_pin_value* pv = arg;

    if (ctrl->handlers->pin_get == NULL) {
      err = ENOTSUP;
      break;
    }

    err = rtems_gpio_check_configured(ctrl, pv->pin);
    if (err == 0) {
      err = (*ctrl->handlers->pin_get)(ctrl, pv->pin, &pv->value);
    }

    if (err == 0 && rtems_gpio_is_active_low(ctrl, pv->pin)) {
      pv->value = (pv->value == 0);
    }
    break;
  }

  case RTEMS_GPIO_IOCTL_PIN_SET: {
    const rtems_gpio_pin_value* pv = arg;

    if (ctrl->handlers->pin_set == NULL) {
      err = ENOTSUP;
      break;
    }

    err = rtems_gpio_check_configured(ctrl, pv->pin);
    if (err == 0) {
      err = (*ctrl->handlers->pin_set)(
          ctrl, pv->pin, rtems_gpio_to_physical(ctrl, pv->pin, pv->value));
    }
    break;
  }

  case RTEMS_GPIO_IOCTL_PIN_TOGGLE: {
    uint32_t pin = *(const uint32_t*)arg;

    if (ctrl->handlers->pin_toggle == NULL) {
      err = ENOTSUP;
      break;
    }

    err = rtems_gpio_check_configured(ctrl, pin);
    if (err == 0) {
      err = (*ctrl->handlers->pin_toggle)(ctrl, pin);
    }
    break;
  }

  case RTEMS_GPIO_IOCTL_PIN_GET_MULTIPLE:
    err = rtems_gpio_do_pin_get_multiple(ctrl, arg);
    break;

  case RTEMS_GPIO_IOCTL_PIN_SET_MULTIPLE:
    err = rtems_gpio_do_pin_set_multiple(ctrl, arg);
    break;

  case RTEMS_GPIO_IOCTL_PIN_IRQ_ENABLE: {
    const rtems_gpio_pin_irq* pi = arg;

    if (ctrl->handlers->pin_irq_enable == NULL) {
      err = ENOTSUP;
      break;
    }

    if (pi->handler == NULL) {
      err = EINVAL;
      break;
    }

    /*
     * One bit is defined, so anything else is either a caller built against a
     * later header than this implementation or one that left the field alone.
     * Refused here rather than passed down, because a driver that ignored a
     * bit it did not recognise would give the caller behaviour it did not ask
     * for and report success.
     */
    if (pi->flags != RTEMS_GPIO_IRQ_DEFAULT) {
      err = ENOTSUP;
      break;
    }

    err = rtems_gpio_check_configured(ctrl, pi->pin);
    if (err == 0) {
      err = (*ctrl->handlers->pin_irq_enable)(ctrl, pi->pin, pi->flags,
                                              pi->handler, pi->arg);
    }
    break;
  }

  case RTEMS_GPIO_IOCTL_PIN_IRQ_DISABLE: {
    uint32_t pin = *(const uint32_t*)arg;

    if (ctrl->handlers->pin_irq_disable == NULL) {
      err = ENOTSUP;
      break;
    }

    err = rtems_gpio_check_configured(ctrl, pin);
    if (err == 0) {
      err = (*ctrl->handlers->pin_irq_disable)(ctrl, pin);
    }
    break;
  }

  default:
    err = ENOTTY;
    break;
  }

  rtems_gpio_drv_ctrl_unlock(ctrl);

  if (err == 0) {
    return 0;
  }

  rtems_set_errno_and_return_minus_one(err);
}

static const rtems_filesystem_file_handlers_r rtems_gpio_handler = {
    .open_h = rtems_filesystem_default_open,
    .close_h = rtems_filesystem_default_close,
    .read_h = rtems_filesystem_default_read,
    .write_h = rtems_filesystem_default_write,
    .ioctl_h = rtems_gpio_ioctl,
    .lseek_h = rtems_filesystem_default_lseek,
    .fstat_h = IMFS_stat,
    .ftruncate_h = rtems_filesystem_default_ftruncate,
    .fsync_h = rtems_filesystem_default_fsync_or_fdatasync,
    .fdatasync_h = rtems_filesystem_default_fsync_or_fdatasync,
    .fcntl_h = rtems_filesystem_default_fcntl,
    .kqfilter_h = rtems_filesystem_default_kqfilter,
    .mmap_h = rtems_filesystem_default_mmap,
    .poll_h = rtems_filesystem_default_poll,
    .readv_h = rtems_filesystem_default_readv,
    .writev_h = rtems_filesystem_default_writev};

static void rtems_gpio_node_destroy(IMFS_jnode_t* node) {
  rtems_gpio_drv_ctrl* ctrl;

  ctrl = IMFS_generic_get_context_by_node(node);

  if (ctrl->handlers->destroy != NULL) {
    (*ctrl->handlers->destroy)(ctrl);
  }

  rtems_mutex_destroy(&ctrl->mutex);

  IMFS_node_destroy_default(node);
}

static const IMFS_node_control rtems_gpio_node_control =
    IMFS_GENERIC_INITIALIZER(&rtems_gpio_handler, IMFS_node_initialize_generic,
                             rtems_gpio_node_destroy);

int rtems_gpio_drv_ctrl_init(rtems_gpio_drv_ctrl* ctrl) {
  if (ctrl == NULL || ctrl->handlers == NULL) {
    return EINVAL;
  }

  /*
   * Without pin_get_info() nothing above the driver can find out what a pin
   * is before it configures it, and every check in this file is built on
   * the answer.  A controller that cannot answer it is not one this layer
   * can make any promises about, so it is refused rather than registered
   * with the checks quietly skipped.
   */
  if (ctrl->handlers->pin_get_info == NULL) {
    return EINVAL;
  }

  if (ctrl->pin_count == 0) {
    return EINVAL;
  }

  /*
   * The generic layer owns logical polarity, so it needs somewhere to keep
   * it.  Required rather than optional: a driver that forgot the storage
   * would otherwise get a controller whose active low pins silently read
   * and drive the wrong way round.
   */
  if (ctrl->active_low == NULL || ctrl->scratch == NULL) {
    return EINVAL;
  }

  memset(ctrl->active_low, 0,
         RTEMS_GPIO_BITMAP_WORDS(ctrl->pin_count) * sizeof(*ctrl->active_low));

  rtems_mutex_init(&ctrl->mutex, ctrl->name != NULL ? ctrl->name : "GPIO");

  return 0;
}

int rtems_gpio_drv_ctrl_register(rtems_gpio_drv_ctrl* ctrl, const char* path) {
  int rv;

  rv = IMFS_make_generic_node(path, S_IFCHR | S_IRWXU | S_IRWXG | S_IRWXO,
                              &rtems_gpio_node_control, ctrl);

  if (rv != 0) {
    if (ctrl->handlers->destroy != NULL) {
      (*ctrl->handlers->destroy)(ctrl);
    }

    rtems_mutex_destroy(&ctrl->mutex);
  }

  return rv;
}

/*
 * The application side.  Each is the ioctl() its documentation says it is,
 * and exists so that a caller does not assemble the argument structures by
 * hand.  No state and no caching: one ioctl() each, so that what a caller
 * observes is what the driver was asked and nothing that happened earlier.
 */

int rtems_gpio_get_info(int fd, rtems_gpio_info* info) {
  return ioctl(fd, RTEMS_GPIO_IOCTL_GET_INFO, info);
}

int rtems_gpio_pin_get_info(int fd, uint32_t pin, rtems_gpio_pin_info* info) {
  if (info == NULL) {
    rtems_set_errno_and_return_minus_one(EINVAL);
  }

  memset(info, 0, sizeof(*info));
  info->pin = pin;

  return ioctl(fd, RTEMS_GPIO_IOCTL_PIN_GET_INFO, info);
}

int rtems_gpio_pin_by_name(int fd, const char* name, uint32_t* pin) {
  rtems_gpio_info info;
  uint32_t i;

  if (name == NULL || pin == NULL) {
    rtems_set_errno_and_return_minus_one(EINVAL);
  }

  if (rtems_gpio_get_info(fd, &info) != 0) {
    return -1;
  }

  for (i = 0; i < info.pin_count; ++i) {
    rtems_gpio_pin_info pin_info;

    if (rtems_gpio_pin_get_info(fd, i, &pin_info) != 0) {
      return -1;
    }

    if (strncmp(pin_info.name, name, sizeof(pin_info.name)) == 0) {
      *pin = i;
      return 0;
    }
  }

  rtems_set_errno_and_return_minus_one(ENOENT);
}

int rtems_gpio_pin_configure(int fd, uint32_t pin, rtems_gpio_config* config) {
  rtems_gpio_pin_config pc;
  int rv;

  if (config == NULL) {
    rtems_set_errno_and_return_minus_one(EINVAL);
  }

  pc.pin = pin;
  pc.config = *config;

  rv = ioctl(fd, RTEMS_GPIO_IOCTL_PIN_CONFIGURE, &pc);
  if (rv == 0) {
    /* What the driver applied, which is not always what was asked for. */
    *config = pc.config;
  }

  return rv;
}

int rtems_gpio_pin_get_configuration(int fd, uint32_t pin,
                                     rtems_gpio_config* config) {
  rtems_gpio_pin_config pc;
  int rv;

  if (config == NULL) {
    rtems_set_errno_and_return_minus_one(EINVAL);
  }

  memset(&pc, 0, sizeof(pc));
  pc.pin = pin;

  rv = ioctl(fd, RTEMS_GPIO_IOCTL_PIN_GET_CONFIG, &pc);
  if (rv == 0) {
    *config = pc.config;
  }

  return rv;
}

int rtems_gpio_pin_release(int fd, uint32_t pin) {
  return ioctl(fd, RTEMS_GPIO_IOCTL_PIN_RELEASE, &pin);
}

int rtems_gpio_pin_get(int fd, uint32_t pin, int* value) {
  rtems_gpio_pin_value pv;
  int rv;

  if (value == NULL) {
    rtems_set_errno_and_return_minus_one(EINVAL);
  }

  pv.pin = pin;
  pv.value = 0;

  rv = ioctl(fd, RTEMS_GPIO_IOCTL_PIN_GET, &pv);
  if (rv == 0) {
    *value = pv.value;
  }

  return rv;
}

int rtems_gpio_pin_set(int fd, uint32_t pin, int value) {
  rtems_gpio_pin_value pv;

  pv.pin = pin;
  pv.value = value;

  return ioctl(fd, RTEMS_GPIO_IOCTL_PIN_SET, &pv);
}

int rtems_gpio_pin_toggle(int fd, uint32_t pin) {
  return ioctl(fd, RTEMS_GPIO_IOCTL_PIN_TOGGLE, &pin);
}

int rtems_gpio_pin_get_multiple(int fd, rtems_gpio_pin_bitmap* list) {
  return ioctl(fd, RTEMS_GPIO_IOCTL_PIN_GET_MULTIPLE, list);
}

int rtems_gpio_pin_set_multiple(int fd, const rtems_gpio_pin_bitmap* list) {
  return ioctl(fd, RTEMS_GPIO_IOCTL_PIN_SET_MULTIPLE, list);
}

int rtems_gpio_pin_irq_enable(int fd, uint32_t pin, rtems_gpio_irq_flags flags,
                              rtems_gpio_irq_handler handler, void* arg) {
  rtems_gpio_pin_irq pi;

  pi.pin = pin;
  pi.flags = flags;
  pi.handler = handler;
  pi.arg = arg;

  return ioctl(fd, RTEMS_GPIO_IOCTL_PIN_IRQ_ENABLE, &pi);
}

int rtems_gpio_pin_irq_disable(int fd, uint32_t pin) {
  return ioctl(fd, RTEMS_GPIO_IOCTL_PIN_IRQ_DISABLE, &pin);
}
