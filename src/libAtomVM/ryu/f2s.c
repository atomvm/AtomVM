// Copyright 2018 Ulf Adams
//
// The contents of this file may be used under the terms of the Apache License,
// Version 2.0.
//
//    (See accompanying file LICENSE-Apache or copy at
//     http://www.apache.org/licenses/LICENSE-2.0)
//
// Alternatively, the contents of this file may be used under the terms of
// the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE-Boost or copy at
//     https://www.boost.org/LICENSE_1_0.txt)
//
// Unless required by applicable law or agreed to in writing, this software
// is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied.

// Runtime compiler options:
// -DRYU_DEBUG Generate verbose debugging output to stdout.

// CHANGE_FOR_ERLANG: "ryu/ryu.h" -> "ryu.h"
#include "ryu.h"
// END CHANGE_FOR_ERLANG

#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#ifdef RYU_DEBUG
#include <stdio.h>
#endif

// CHANGE_FOR_ERLANG: "ryu/*.h" -> "*.h"
#include "common.h"
#include "f2s_intrinsics.h"
#include "digit_table.h"
// END CHANGE_FOR_ERLANG

#define FLOAT_MANTISSA_BITS 23
#define FLOAT_EXPONENT_BITS 8
#define FLOAT_BIAS 127

// A floating decimal representing m * 10^e.
typedef struct floating_decimal_32 {
  uint32_t mantissa;
  // Decimal exponent's range is -45 to 38
  // inclusive, and can fit in a short if needed.
  int32_t exponent;
} floating_decimal_32;

// CHANGE_FOR_ERLANG include STL-inspired to_chars function
#include "to_chars32.h"
// END CHANGE_FOR_ERLANG

static inline floating_decimal_32 f2d(const uint32_t ieeeMantissa, const uint32_t ieeeExponent) {
  int32_t e2;
  uint32_t m2;
  if (ieeeExponent == 0) {
    // We subtract 2 so that the bounds computation has 2 additional bits.
    e2 = 1 - FLOAT_BIAS - FLOAT_MANTISSA_BITS - 2;
    m2 = ieeeMantissa;
  } else {
    e2 = (int32_t) ieeeExponent - FLOAT_BIAS - FLOAT_MANTISSA_BITS - 2;
    m2 = (1u << FLOAT_MANTISSA_BITS) | ieeeMantissa;
  }
  const bool even = (m2 & 1) == 0;
  const bool acceptBounds = even;

#ifdef RYU_DEBUG
  printf("-> %u * 2^%d\n", m2, e2 + 2);
#endif

  // Step 2: Determine the interval of valid decimal representations.
  const uint32_t mv = 4 * m2;
  const uint32_t mp = 4 * m2 + 2;
  // Implicit bool -> int conversion. True is 1, false is 0.
  const uint32_t mmShift = ieeeMantissa != 0 || ieeeExponent <= 1;
  const uint32_t mm = 4 * m2 - 1 - mmShift;

  // Step 3: Convert to a decimal power base using 64-bit arithmetic.
  uint32_t vr, vp, vm;
  int32_t e10;
  bool vmIsTrailingZeros = false;
  bool vrIsTrailingZeros = false;
  uint8_t lastRemovedDigit = 0;
  if (e2 >= 0) {
    const uint32_t q = log10Pow2(e2);
    e10 = (int32_t) q;
    const int32_t k = FLOAT_POW5_INV_BITCOUNT + pow5bits((int32_t) q) - 1;
    const int32_t i = -e2 + (int32_t) q + k;
    vr = mulPow5InvDivPow2(mv, q, i);
    vp = mulPow5InvDivPow2(mp, q, i);
    vm = mulPow5InvDivPow2(mm, q, i);
#ifdef RYU_DEBUG
    printf("%u * 2^%d / 10^%u\n", mv, e2, q);
    printf("V+=%u\nV =%u\nV-=%u\n", vp, vr, vm);
#endif
    if (q != 0 && (vp - 1) / 10 <= vm / 10) {
      // We need to know one removed digit even if we are not going to loop below. We could use
      // q = X - 1 above, except that would require 33 bits for the result, and we've found that
      // 32-bit arithmetic is faster even on 64-bit machines.
      const int32_t l = FLOAT_POW5_INV_BITCOUNT + pow5bits((int32_t) (q - 1)) - 1;
      lastRemovedDigit = (uint8_t) (mulPow5InvDivPow2(mv, q - 1, -e2 + (int32_t) q - 1 + l) % 10);
    }
    if (q <= 9) {
      // The largest power of 5 that fits in 24 bits is 5^10, but q <= 9 seems to be safe as well.
      // Only one of mp, mv, and mm can be a multiple of 5, if any.
      if (mv % 5 == 0) {
        vrIsTrailingZeros = multipleOfPowerOf5_32(mv, q);
      } else if (acceptBounds) {
        vmIsTrailingZeros = multipleOfPowerOf5_32(mm, q);
      } else {
        vp -= multipleOfPowerOf5_32(mp, q);
      }
    }
  } else {
    const uint32_t q = log10Pow5(-e2);
    e10 = (int32_t) q + e2;
    const int32_t i = -e2 - (int32_t) q;
    const int32_t k = pow5bits(i) - FLOAT_POW5_BITCOUNT;
    int32_t j = (int32_t) q - k;
    vr = mulPow5divPow2(mv, (uint32_t) i, j);
    vp = mulPow5divPow2(mp, (uint32_t) i, j);
    vm = mulPow5divPow2(mm, (uint32_t) i, j);
#ifdef RYU_DEBUG
    printf("%u * 5^%d / 10^%u\n", mv, -e2, q);
    printf("%u %d %d %d\n", q, i, k, j);
    printf("V+=%u\nV =%u\nV-=%u\n", vp, vr, vm);
#endif
    if (q != 0 && (vp - 1) / 10 <= vm / 10) {
      j = (int32_t) q - 1 - (pow5bits(i + 1) - FLOAT_POW5_BITCOUNT);
      lastRemovedDigit = (uint8_t) (mulPow5divPow2(mv, (uint32_t) (i + 1), j) % 10);
    }
    if (q <= 1) {
      // {vr,vp,vm} is trailing zeros if {mv,mp,mm} has at least q trailing 0 bits.
      // mv = 4 * m2, so it always has at least two trailing 0 bits.
      vrIsTrailingZeros = true;
      if (acceptBounds) {
        // mm = mv - 1 - mmShift, so it has 1 trailing 0 bit iff mmShift == 1.
        vmIsTrailingZeros = mmShift == 1;
      } else {
        // mp = mv + 2, so it always has at least one trailing 0 bit.
        --vp;
      }
    } else if (q < 31) { // TODO(ulfjack): Use a tighter bound here.
      vrIsTrailingZeros = multipleOfPowerOf2_32(mv, q - 1);
#ifdef RYU_DEBUG
      printf("vr is trailing zeros=%s\n", vrIsTrailingZeros ? "true" : "false");
#endif
    }
  }
#ifdef RYU_DEBUG
  printf("e10=%d\n", e10);
  printf("V+=%u\nV =%u\nV-=%u\n", vp, vr, vm);
  printf("vm is trailing zeros=%s\n", vmIsTrailingZeros ? "true" : "false");
  printf("vr is trailing zeros=%s\n", vrIsTrailingZeros ? "true" : "false");
#endif

  // Step 4: Find the shortest decimal representation in the interval of valid representations.
  int32_t removed = 0;
  uint32_t output;
  if (vmIsTrailingZeros || vrIsTrailingZeros) {
    // General case, which happens rarely (~4.0%).
    while (vp / 10 > vm / 10) {
#ifdef __clang__ // https://bugs.llvm.org/show_bug.cgi?id=23106
      // The compiler does not realize that vm % 10 can be computed from vm / 10
      // as vm - (vm / 10) * 10.
      vmIsTrailingZeros &= vm - (vm / 10) * 10 == 0;
#else
      vmIsTrailingZeros &= vm % 10 == 0;
#endif
      vrIsTrailingZeros &= lastRemovedDigit == 0;
      lastRemovedDigit = (uint8_t) (vr % 10);
      vr /= 10;
      vp /= 10;
      vm /= 10;
      ++removed;
    }
#ifdef RYU_DEBUG
    printf("V+=%u\nV =%u\nV-=%u\n", vp, vr, vm);
    printf("d-10=%s\n", vmIsTrailingZeros ? "true" : "false");
#endif
    if (vmIsTrailingZeros) {
      while (vm % 10 == 0) {
        vrIsTrailingZeros &= lastRemovedDigit == 0;
        lastRemovedDigit = (uint8_t) (vr % 10);
        vr /= 10;
        vp /= 10;
        vm /= 10;
        ++removed;
      }
    }
#ifdef RYU_DEBUG
    printf("%u %d\n", vr, lastRemovedDigit);
    printf("vr is trailing zeros=%s\n", vrIsTrailingZeros ? "true" : "false");
#endif
    if (vrIsTrailingZeros && lastRemovedDigit == 5 && vr % 2 == 0) {
      // Round even if the exact number is .....50..0.
      lastRemovedDigit = 4;
    }
    // We need to take vr + 1 if vr is outside bounds or we need to round up.
    output = vr + ((vr == vm && (!acceptBounds || !vmIsTrailingZeros)) || lastRemovedDigit >= 5);
  } else {
    // Specialized for the common case (~96.0%). Percentages below are relative to this.
    // Loop iterations below (approximately):
    // 0: 13.6%, 1: 70.7%, 2: 14.1%, 3: 1.39%, 4: 0.14%, 5+: 0.01%
    while (vp / 10 > vm / 10) {
      lastRemovedDigit = (uint8_t) (vr % 10);
      vr /= 10;
      vp /= 10;
      vm /= 10;
      ++removed;
    }
#ifdef RYU_DEBUG
    printf("%u %d\n", vr, lastRemovedDigit);
    printf("vr is trailing zeros=%s\n", vrIsTrailingZeros ? "true" : "false");
#endif
    // We need to take vr + 1 if vr is outside bounds or we need to round up.
    output = vr + (vr == vm || lastRemovedDigit >= 5);
  }
  const int32_t exp = e10 + removed;

#ifdef RYU_DEBUG
  printf("V+=%u\nV =%u\nV-=%u\n", vp, vr, vm);
  printf("O=%u\n", output);
  printf("EXP=%d\n", exp);
#endif

  floating_decimal_32 fd;
  fd.exponent = exp;
  fd.mantissa = output;
  return fd;
}

int f2s_buffered_n(float f, char* result) {
  // Step 1: Decode the floating-point number, and unify normalized and subnormal cases.
  const uint32_t bits = float_to_bits(f);

#ifdef RYU_DEBUG
  printf("IN=");
  for (int32_t bit = 31; bit >= 0; --bit) {
    printf("%u", (bits >> bit) & 1);
  }
  printf("\n");
#endif

  // Decode bits into sign, mantissa, and exponent.
  const bool ieeeSign = ((bits >> (FLOAT_MANTISSA_BITS + FLOAT_EXPONENT_BITS)) & 1) != 0;
  const uint32_t ieeeMantissa = bits & ((1u << FLOAT_MANTISSA_BITS) - 1);
  const uint32_t ieeeExponent = (bits >> FLOAT_MANTISSA_BITS) & ((1u << FLOAT_EXPONENT_BITS) - 1);

  // Case distinction; exit early for the easy cases.
  if (ieeeExponent == ((1u << FLOAT_EXPONENT_BITS) - 1u) || (ieeeExponent == 0 && ieeeMantissa == 0)) {
    return copy_special_str(result, ieeeSign, ieeeExponent, ieeeMantissa);
  }

  const floating_decimal_32 v = f2d(ieeeMantissa, ieeeExponent);
  return to_chars32(v, ieeeSign, result);
}

void f2s_buffered(float f, char* result) {
  const int index = f2s_buffered_n(f, result);

  // Terminate the string.
  result[index] = '\0';
}

char* f2s(float f) {
  char* const result = (char*) malloc(16);
  f2s_buffered(f, result);
  return result;
}
