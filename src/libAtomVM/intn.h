/*
 * This file is part of AtomVM.
 *
 * Copyright 2025 Davide Bettio <davide@uninstall.it>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
 */

/**
 * @file intn.h
 * @brief Multi-precision integer arithmetic for up to 256-bit integers (IntN)
 *
 * The module name "intn" stands for "Integer N-bits" where N can be up to 256
 * in the current implementation. While these are often called "big integers"
 * (bigints) in higher-level contexts, we use the term "multi-precision integer"
 * in this header to emphasize the implementation using arrays of fixed-precision
 * digits.
 *
 * This module provides multi-precision integer arithmetic operations on arrays
 * of digits (also called limbs in other libraries). Integers are represented in
 * sign-magnitude form (not two's complement) with the sign stored separately.
 * The magnitude is the absolute value of the integer, stored as an array of
 * unsigned digits.
 *
 * ## Integer Representation
 *
 * Multi-precision integers are stored as arrays of \c intn_digit_t digits in
 * little-endian digit order (least significant digit first):
 * - digit[0] contains the least significant bits
 * - digit[n-1] contains the most significant bits
 *
 * Example representations:
 * @code
 * // 0xCAFEFACEDEADBEEF parsed from hex string:
 * // bigint[0] = 0xDEADBEEF, bigint[1] = 0xCAFEFACE, sign = IntNPositiveInteger
 *
 * // -0xCAFEFACEDEADBEEF1234:
 * // {0xBEEF1234, 0xFACEDEAD, 0xCAFE}, sign = IntNNegativeInteger
 *
 * // 2^127 (0x80000000000000000000000000000000):
 * // {0x0, 0x0, 0x0, 0x80000000}, sign = IntNPositiveInteger
 * @endcode
 *
 * @note On little-endian systems, the memory layout matches native \c uint64_t
 *       for 2-digit values. On big-endian systems, digit order remains the same
 *       but byte order within each digit follows system endianness.
 * @warning Multi-precision integers cannot be compared using \c memcmp
 *
 * ## Normalized (Canonical) Form
 *
 * An integer is in normalized form when it has no leading zero digits.
 * Leading zeros are zero-valued digits at the end of the array (highest indices).
 *
 * Examples:
 * @code
 * // Normalized form (no leading zeros):
 * // {0xDEADBEEF, 0xCAFEFACE} - 2 digits, both non-zero
 *
 * // Not normalized (has leading zero):
 * // {0xDEADBEEF, 0xCAFEFACE, 0x0} - digit[2] is a leading zero
 *
 * // Function example:
 * intn_count_digits({0x0, 0x0, 0x0, 0x80000000}, 4) -> 4 (normalized)
 * intn_count_digits({0x0, 0x0, 0x0, 0x80000000, 0x0}, 5) -> 4 (not normalized)
 * @endcode
 *
 * ## Functions Requiring Normalized Input
 *
 * The following functions MUST receive normalized input for correct operation:
 * - \c intn_to_int64() - for correct conversion
 * - \c intn_fits_int64() - for accurate check
 *
 * All other functions accept both normalized and non-normalized inputs.
 *
 * ## Output Buffer Sizing
 *
 * Functions require pre-allocated output buffers. Use the provided macros
 * (e.g., \c INTN_MUL_OUT_LEN) to ensure sufficient space. No output length
 * parameter is passed - callers must ensure buffers are large enough.
 *
 * @note Algorithms for multiplication and division are based on Hacker's Delight
 * @note We use the term "digit" instead of "limb" in this module
 */

#ifndef _INTN_H_
#define _INTN_H_

#include <stdbool.h>
#include <string.h>

#include "utils.h"

// INTN_MAX_RES_LEN is bigger than INTN_MAX_IN_LEN, even the most trivial
// INTN_MUL_OUT_LEN(8, 1) = 9.
//
// Also since we may use INTN_INT64_LEN digits even for small values such as `1` (it will be padded
// with zeros, actually INTN_INT64_LEN + 1 digits, for some reason specific to how AtomVM handles
// boxed values).
//
// Example: { ... 8 digits ... } * { 0x1, 0x0, 0x0}, that will require INTN_MUL_OUT_LEN(8, 3) = 11
// digits.
//
// Also we need some room for any potential overflow, worst case is still INTN_MUL_OUT_LEN(8, 3).

/**
 * @def INTN_INT64_LEN
 * @brief Number of \c intn_digit_t digits needed to represent any \c int64_t value
 */
#define INTN_INT64_LEN 2

/**
 * @def INTN_UINT64_LEN
 * @brief Number of \c intn_digit_t digits needed to represent any \c uint64_t value
 */
#define INTN_UINT64_LEN 2

/**
 * @def INTN_MAX_IN_LEN
 * @brief Maximum input length in digits (256 bits / 32 bits = 8 digits)
 */
#define INTN_MAX_IN_LEN 8 // 256 bit / 32 bit = 8 digits

/**
 * @def INTN_MAX_RES_LEN
 * @brief Maximum result length in digits, provides extra room for intermediate overflow
 *
 * @note Larger than \c INTN_MAX_IN_LEN to accommodate temporary overflow before normalization
 */
#define INTN_MAX_RES_LEN (INTN_MAX_IN_LEN + INTN_INT64_LEN + 1)

/**
 * @def INTN_BSL_MAX_RES_LEN
 * @brief Maximum result length for bit shift left operations
 */
#define INTN_BSL_MAX_RES_LEN 8

/**
 * @def MAX_LEN(m, n)
 * @brief Return the maximum of two values
 */
#define MAX_LEN(m, n) (((m) > (n)) ? (m) : (n))

/**
 * @def INTN_ADD_OUT_LEN(m, n)
 * @brief Calculate output buffer size needed for addition
 *
 * @param m Length of first operand in digits
 * @param n Length of second operand in digits
 * @return Maximum possible output length in digits
 */
#define INTN_ADD_OUT_LEN(m, n) ((MAX_LEN(m, n)) + 1)

/**
 * @def INTN_SUB_OUT_LEN(m, n)
 * @brief Calculate output buffer size needed for subtraction
 *
 * @param m Length of minuend in digits
 * @param n Length of subtrahend in digits
 * @return Maximum possible output length in digits
 */
#define INTN_SUB_OUT_LEN(m, n) ((MAX_LEN(m, n)) + 1)

/**
 * @def INTN_MUL_OUT_LEN(m, n)
 * @brief Calculate output buffer size needed for multiplication
 *
 * @param m Length of first operand in digits
 * @param n Length of second operand in digits
 * @return Maximum possible output length in digits
 *
 * @note Result always has exactly m + n digits (some may be zero)
 */
#define INTN_MUL_OUT_LEN(m, n) ((m) + (n))

/**
 * @def INTN_REM_OUT_LEN(m, n)
 * @brief Calculate output buffer size needed for remainder
 *
 * @param m Length of dividend in digits
 * @param n Length of divisor in digits
 * @return Maximum possible remainder length in digits
 */
#define INTN_REM_OUT_LEN(m, n) (n)

/**
 * @def INTN_DIV_OUT_LEN(m, n)
 * @brief Calculate output buffer size needed for division quotient
 *
 * @param m Length of dividend in digits
 * @param n Length of divisor in digits
 * @return Maximum possible quotient length in digits
 */
#define INTN_DIV_OUT_LEN(m, n) ((m) - (n) + 1 + 1)

/**
 * @def INTN_DIGIT_BITS
 * @brief Number of bits in each digit (32 bits in current implementation)
 */
#define INTN_DIGIT_BITS 32

/**
 * @def INTN_MAX_UNSIGNED_BYTES_SIZE
 * @brief Maximum size in bytes for unsigned integer representation (256 bits / 8)
 */
#define INTN_MAX_UNSIGNED_BYTES_SIZE 32

/**
 * @def INTN_MAX_UNSIGNED_BITS_SIZE
 * @brief Maximum size in bits for unsigned integer representation
 */
#define INTN_MAX_UNSIGNED_BITS_SIZE 256

/**
 * @brief Sign of a multi-precision integer
 *
 * Integers are stored in sign-magnitude form with sign separate from digits
 */
typedef enum
{
    /** @brief Positive integer (including zero) */
    IntNPositiveInteger = 0,
    /** @brief Negative integer */
    IntNNegativeInteger = 4
} intn_integer_sign_t;

/**
 * @brief Options for integer byte conversion
 */
typedef enum
{
    /** @brief Unsigned big-endian format */
    IntnUnsignedBigEndian = 0,
    /** @brief Signed two's complement format */
    IntnSigned = 1,
    /** @brief Little-endian format */
    IntnLittleEndian = 2
} intn_from_integer_options_t;

/**
 * @brief Single digit of a multi-precision integer
 *
 * Currently 32 bits, but this is an implementation detail that may change.
 * Always use \c intn_digit_t type and related macros rather than assuming size.
 */
typedef uint32_t intn_digit_t;

// Uncomment this for debug
// void print_num(const intn_digit_t num[], int len);

/**
 * @brief Compare two unsigned multi-precision integers
 *
 * Compares the magnitude of two multi-precision integers, ignoring sign.
 * Accepts both normalized and non-normalized inputs.
 *
 * @param a First integer array
 * @param a_len Length of first integer in digits
 * @param b Second integer array
 * @param b_len Length of second integer in digits
 * @return -1 if a < b, 0 if a == b, 1 if a > b
 *
 * @note Leading zeros are ignored in comparison
 * @note Accepts both normalized and non-normalized inputs
 */
int intn_cmp(const intn_digit_t a[], size_t a_len, const intn_digit_t b[], size_t b_len);

/**
 * @brief Add two unsigned multi-precision integers
 *
 * Performs addition of magnitudes only, without considering signs
 * (similar to unsigned addition).
 *
 * @param a First addend
 * @param a_len Length of first addend in digits
 * @param b Second addend
 * @param b_len Length of second addend in digits
 * @param[out] out Result buffer (must have at least \c INTN_ADD_OUT_LEN(a_len, b_len) digits)
 * @return Actual length of result in digits (may be less than buffer size)
 *
 * @pre out buffer must be at least \c INTN_ADD_OUT_LEN(a_len, b_len) digits
 * @post Result may have leading zeros (not normalized)
 * @note Accepts both normalized and non-normalized inputs
 */
size_t intn_addu(
    const intn_digit_t a[], size_t a_len, const intn_digit_t b[], size_t b_len, intn_digit_t out[]);

/**
 * @brief Add two signed multi-precision integers
 *
 * Performs signed addition of two multi-precision integers with separate signs.
 *
 * @param m First addend magnitude
 * @param m_len Length of first addend in digits
 * @param m_sign Sign of first addend
 * @param n Second addend magnitude
 * @param n_len Length of second addend in digits
 * @param n_sign Sign of second addend
 * @param[out] out Result buffer (must have at least \c INTN_ADD_OUT_LEN(m_len, n_len) digits)
 * @param[out] out_sign Sign of result
 * @return Actual length of result in digits (may be less than buffer size)
 *
 * @pre out buffer must be at least \c INTN_ADD_OUT_LEN(m_len, n_len) digits
 * @post Result may have leading zeros (not normalized)
 * @note Accepts both normalized and non-normalized inputs
 */
size_t intn_add(const intn_digit_t m[], size_t m_len, intn_integer_sign_t m_sign,
    const intn_digit_t n[], size_t n_len, intn_integer_sign_t n_sign, intn_digit_t out[],
    intn_integer_sign_t *out_sign);

/**
 * @brief Add two 64-bit signed integers (\c int64_t) producing multi-precision result
 *
 * Specialized addition for \c int64_t values that may overflow.
 *
 * @param num1 First 64-bit addend
 * @param num2 Second 64-bit addend
 * @param[out] out Result buffer (must have at least \c INTN_ADD_OUT_LEN(INTN_INT64_LEN,
 * INTN_INT64_LEN) digits)
 * @param[out] out_sign Sign of result
 * @return Actual length of result in digits
 *
 * @pre out buffer must be at least \c INTN_ADD_OUT_LEN(INTN_INT64_LEN, INTN_INT64_LEN) digits
 */
size_t intn_add_int64(int64_t num1, int64_t num2, intn_digit_t *out, intn_integer_sign_t *out_sign);

/**
 * @brief Subtract two unsigned multi-precision integers
 *
 * Performs subtraction of magnitudes only (a - b) where a must be >= b.
 *
 * @param a Minuend (must be >= b)
 * @param a_len Length of minuend in digits
 * @param b Subtrahend
 * @param b_len Length of subtrahend in digits
 * @param[out] out Result buffer (must have at least \c INTN_SUB_OUT_LEN(a_len, b_len) digits)
 * @return Actual length of result in digits (may be less than buffer size)
 *
 * @pre a >= b (use \c intn_cmp to verify if needed)
 * @pre out buffer must be at least \c INTN_SUB_OUT_LEN(a_len, b_len) digits
 * @post Result may have leading zeros (not normalized)
 * @note Accepts both normalized and non-normalized inputs
 */
size_t intn_subu(
    const intn_digit_t a[], size_t a_len, const intn_digit_t b[], size_t b_len, intn_digit_t out[]);

/**
 * @brief Subtract two signed multi-precision integers
 *
 * Performs signed subtraction (m - n) with separate signs.
 *
 * @param m Minuend magnitude
 * @param m_len Length of minuend in digits
 * @param m_sign Sign of minuend
 * @param n Subtrahend magnitude
 * @param n_len Length of subtrahend in digits
 * @param n_sign Sign of subtrahend
 * @param[out] out Result buffer (must have at least \c INTN_SUB_OUT_LEN(m_len, n_len) digits)
 * @param[out] out_sign Sign of result
 * @return Actual length of result in digits (may be less than buffer size)
 *
 * @pre out buffer must be at least \c INTN_SUB_OUT_LEN(m_len, n_len) digits
 * @post Result may have leading zeros (not normalized)
 * @note Accepts both normalized and non-normalized inputs
 */
size_t intn_sub(const intn_digit_t m[], size_t m_len, intn_integer_sign_t m_sign,
    const intn_digit_t n[], size_t n_len, intn_integer_sign_t n_sign, intn_digit_t out[],
    intn_integer_sign_t *out_sign);

/**
 * @brief Subtract two 64-bit signed integers (\c int64_t) producing multi-precision result
 *
 * Specialized subtraction for \c int64_t values that may overflow.
 *
 * @param num1 Minuend
 * @param num2 Subtrahend
 * @param[out] out Result buffer (must have at least \c INTN_SUB_OUT_LEN(INTN_INT64_LEN,
 * INTN_INT64_LEN) digits)
 * @param[out] out_sign Sign of result
 * @return Actual length of result in digits
 *
 * @pre out buffer must be at least \c INTN_SUB_OUT_LEN(INTN_INT64_LEN, INTN_INT64_LEN) digits
 */
size_t intn_sub_int64(int64_t num1, int64_t num2, intn_digit_t *out, intn_integer_sign_t *out_sign);

/**
 * @brief Compute sign of multiplication or division result
 *
 * Applies standard sign rules: same signs give positive, different signs give negative.
 *
 * @param s1 Sign of first operand
 * @param s2 Sign of second operand
 * @return Sign of the result
 */
static inline intn_integer_sign_t intn_muldiv_sign(intn_integer_sign_t s1, intn_integer_sign_t s2)
{
    return (intn_integer_sign_t) ((unsigned int) s1 ^ (unsigned int) s2) & IntNNegativeInteger;
}

/**
 * @brief Multiply two unsigned multi-precision integers
 *
 * Performs multiplication of magnitudes only, without considering signs.
 *
 * @param m First multiplicand
 * @param m_len Length of first multiplicand in digits
 * @param n Second multiplicand
 * @param n_len Length of second multiplicand in digits
 * @param[out] out Result buffer (must have at least \c INTN_MUL_OUT_LEN(m_len, n_len) digits)
 *
 * @pre out buffer must be at least \c INTN_MUL_OUT_LEN(m_len, n_len) digits
 * @post Exactly m_len + n_len digits are written (some may be zero)
 * @post Result may have leading zeros (not normalized)
 * @note Accepts both normalized and non-normalized inputs
 * @note Based on algorithms from Hacker's Delight
 */
void intn_mulu(
    const intn_digit_t m[], size_t m_len, const intn_digit_t n[], size_t n_len, intn_digit_t out[]);

/**
 * @brief Multiply two signed multi-precision integers
 *
 * Performs signed multiplication with separate signs.
 *
 * @param m First multiplicand magnitude
 * @param m_len Length of first multiplicand in digits
 * @param m_sign Sign of first multiplicand
 * @param n Second multiplicand magnitude
 * @param n_len Length of second multiplicand in digits
 * @param n_sign Sign of second multiplicand
 * @param[out] out Result buffer (must have at least \c INTN_MUL_OUT_LEN(m_len, n_len) digits)
 * @param[out] out_sign Sign of result
 *
 * @pre out buffer must be at least \c INTN_MUL_OUT_LEN(m_len, n_len) digits
 * @post Exactly m_len + n_len digits are written (some may be zero)
 * @post Result may have leading zeros (not normalized)
 * @note Accepts both normalized and non-normalized inputs
 */
static inline void intn_mul(const intn_digit_t m[], size_t m_len, intn_integer_sign_t m_sign,
    const intn_digit_t n[], size_t n_len, intn_integer_sign_t n_sign, intn_digit_t out[],
    intn_integer_sign_t *out_sign)
{
    *out_sign = intn_muldiv_sign(m_sign, n_sign);
    intn_mulu(m, m_len, n, n_len, out);
}

/**
 * @brief Multiply two 64-bit signed integers (\c int64_t) producing multi-precision result
 *
 * Specialized multiplication for \c int64_t values that may overflow.
 *
 * @param num1 First multiplicand
 * @param num2 Second multiplicand
 * @param[out] out Result buffer (must have at least \c INTN_MUL_OUT_LEN(INTN_INT64_LEN,
 * INTN_INT64_LEN) digits)
 * @param[out] out_sign Sign of result
 *
 * @pre out buffer must be at least \c INTN_MUL_OUT_LEN(INTN_INT64_LEN, INTN_INT64_LEN) digits
 * @post Exactly INTN_INT64_LEN + INTN_INT64_LEN digits are written
 */
void intn_mul_int64(int64_t num1, int64_t num2, intn_digit_t *out, intn_integer_sign_t *out_sign);

/**
 * @brief Divide two unsigned multi-precision integers with optional remainder
 *
 * Performs division of magnitudes m / n, computing quotient and optionally remainder.
 *
 * @param m Dividend
 * @param m_len Length of dividend in digits
 * @param n Divisor (must not be zero)
 * @param n_len Length of divisor in digits
 * @param[out] q_out Quotient buffer (must have at least \c INTN_DIV_OUT_LEN(m_len, n_len) digits)
 * @param[out] r_out Remainder buffer (may be NULL, else must have at least \c
 * INTN_REM_OUT_LEN(m_len, n_len) digits)
 * @param[out] r_out_len Length of remainder (may be NULL if r_out is NULL)
 * @return Length of quotient in digits
 *
 * @pre n must not be zero
 * @pre q_out buffer must be at least \c INTN_DIV_OUT_LEN(m_len, n_len) digits
 * @pre r_out buffer (if not NULL) must be at least \c INTN_REM_OUT_LEN(m_len, n_len) digits
 * @post Quotient and remainder may have leading zeros (not normalized)
 * @note Accepts both normalized and non-normalized inputs
 * @note Based on algorithms from Hacker's Delight
 */
size_t intn_divu(const intn_digit_t m[], size_t m_len, const intn_digit_t n[], size_t n_len,
    intn_digit_t q_out[], intn_digit_t r_out[], size_t *r_out_len);

/**
 * @brief Divide two signed multi-precision integers with optional remainder
 *
 * Performs signed division m / n with separate signs.
 *
 * @param m Dividend magnitude
 * @param m_len Length of dividend in digits
 * @param m_sign Sign of dividend
 * @param n Divisor magnitude (must not be zero)
 * @param n_len Length of divisor in digits
 * @param n_sign Sign of divisor
 * @param[out] q_out Quotient buffer (must have at least \c INTN_DIV_OUT_LEN(m_len, n_len) digits)
 * @param[out] qout_sign Sign of quotient
 * @param[out] r_out Remainder buffer (may be NULL, else must have at least \c
 * INTN_REM_OUT_LEN(m_len, n_len) digits)
 * @param[out] r_out_len Length of remainder (may be NULL if r_out is NULL)
 * @return Length of quotient in digits
 *
 * @pre n must not be zero
 * @pre q_out buffer must be at least \c INTN_DIV_OUT_LEN(m_len, n_len) digits
 * @pre r_out buffer (if not NULL) must be at least \c INTN_REM_OUT_LEN(m_len, n_len) digits
 * @post Remainder has same sign as dividend (Euclidean division)
 * @note Accepts both normalized and non-normalized inputs
 */
static inline size_t intn_div(const intn_digit_t m[], size_t m_len, intn_integer_sign_t m_sign,
    const intn_digit_t n[], size_t n_len, intn_integer_sign_t n_sign, intn_digit_t q_out[],
    intn_integer_sign_t *qout_sign, intn_digit_t r_out[], size_t *r_out_len)
{
    *qout_sign = intn_muldiv_sign(m_sign, n_sign);
    return intn_divu(m, m_len, n, n_len, q_out, r_out, r_out_len);
}

/**
 * @brief Bitwise OR of two signed multi-precision integers
 *
 * Performs bitwise OR by internally converting to two's complement,
 * applying the operation, then converting back to sign-magnitude form.
 *
 * @param m First operand magnitude
 * @param m_len Length of first operand in digits
 * @param m_sign Sign of first operand
 * @param n Second operand magnitude
 * @param n_len Length of second operand in digits
 * @param n_sign Sign of second operand
 * @param[out] out Result buffer (must have at least \c MAX_LEN(m_len, n_len) + 1 digits)
 * @param[out] out_sign Sign of result
 * @return Length of result in digits
 *
 * @pre out buffer must be at least \c MAX_LEN(m_len, n_len) + 1 digits
 * @post Result may have leading zeros (not normalized)
 * @note Input and output are in sign-magnitude form, not two's complement
 * @note Accepts both normalized and non-normalized inputs
 *
 * @code
 * // Example: 0xFFFFFFFF00000000000012345678 | -1
 * // Input: {0x12345678, 0x0, 0xFFFF0000, 0xFFFF} | {0x1} with negative sign
 * // Internal two's complement: {0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF}
 * // Result: {0x1, 0x0, 0x0, 0x0} with negative sign (equals -1)
 * @endcode
 */
size_t intn_bor(const intn_digit_t m[], size_t m_len, intn_integer_sign_t m_sign,
    const intn_digit_t n[], size_t n_len, intn_integer_sign_t n_sign, intn_digit_t out[],
    intn_integer_sign_t *out_sign);

/**
 * @brief Bitwise AND of two signed multi-precision integers
 *
 * Performs bitwise AND by internally converting to two's complement,
 * applying the operation, then converting back to sign-magnitude form.
 *
 * @param m First operand magnitude
 * @param m_len Length of first operand in digits
 * @param m_sign Sign of first operand
 * @param n Second operand magnitude
 * @param n_len Length of second operand in digits
 * @param n_sign Sign of second operand
 * @param[out] out Result buffer (must have at least \c MAX_LEN(m_len, n_len) + 1 digits)
 * @param[out] out_sign Sign of result
 * @return Length of result in digits
 *
 * @pre out buffer must be at least \c MAX_LEN(m_len, n_len) + 1 digits
 * @post Result may have leading zeros (not normalized)
 * @note Input and output are in sign-magnitude form, not two's complement
 * @note Accepts both normalized and non-normalized inputs
 *
 * @code
 * // Example: 0xFFFFFFFFF123456789ABFFFFFFFF & -0xFFFFFFFF000000000000FFFFFFFF
 * // Input: {0xFFFFFFFF, 0x456789AB, 0xFFFFF123, 0xFFFF} &
 * //        {0xFFFFFFFF, 0x0, 0xFFFF0000, 0xFFFF} with negative sign
 * // Result: {0x1, 0x456789AB, 0xF123} (equals 0xF123456789AB00000001)
 * @endcode
 */
size_t intn_band(const intn_digit_t m[], size_t m_len, intn_integer_sign_t m_sign,
    const intn_digit_t n[], size_t n_len, intn_integer_sign_t n_sign, intn_digit_t out[],
    intn_integer_sign_t *out_sign);

/**
 * @brief Bitwise XOR of two signed multi-precision integers
 *
 * Performs bitwise XOR by internally converting to two's complement,
 * applying the operation, then converting back to sign-magnitude form.
 *
 * @param m First operand magnitude
 * @param m_len Length of first operand in digits
 * @param m_sign Sign of first operand
 * @param n Second operand magnitude
 * @param n_len Length of second operand in digits
 * @param n_sign Sign of second operand
 * @param[out] out Result buffer (must have at least \c MAX_LEN(m_len, n_len) + 1 digits)
 * @param[out] out_sign Sign of result
 * @return Length of result in digits
 *
 * @pre out buffer must be at least \c MAX_LEN(m_len, n_len) + 1 digits
 * @post Result may have leading zeros (not normalized)
 * @note Input and output are in sign-magnitude form, not two's complement
 * @note Accepts both normalized and non-normalized inputs
 *
 * @code
 * // Example: 0xFFFFFFFF00000000000012345678 ^ -1
 * // Input: {0x12345678, 0x0, 0xFFFF0000, 0xFFFF} ^ {0x1} with negative sign
 * // Result: {0x12345679, 0x0, 0xFFFF0000, 0xFFFF} with negative sign
 * @endcode
 */
size_t intn_bxor(const intn_digit_t m[], size_t m_len, intn_integer_sign_t m_sign,
    const intn_digit_t n[], size_t n_len, intn_integer_sign_t n_sign, intn_digit_t out[],
    intn_integer_sign_t *out_sign);

/**
 * @brief Bitwise NOT of a signed multi-precision integer
 *
 * Performs bitwise NOT operation (one's complement).
 *
 * @param m Operand magnitude
 * @param m_len Length of operand in digits
 * @param m_sign Sign of operand
 * @param[out] out Result buffer (must have at least m_len + 1 digits)
 * @param[out] out_sign Sign of result
 * @return Length of result in digits
 *
 * @pre out buffer must be at least m_len + 1 digits
 * @post Result may have leading zeros (not normalized)
 * @note Accepts both normalized and non-normalized inputs
 */
size_t intn_bnot(const intn_digit_t m[], size_t m_len, intn_integer_sign_t m_sign,
    intn_digit_t out[], intn_integer_sign_t *out_sign);

/**
 * @brief Bit shift left of multi-precision integer
 *
 * Shifts integer left by n bit positions.
 *
 * @param num Integer to shift
 * @param len Length of integer in digits
 * @param n Number of bit positions to shift
 * @param[out] out Result buffer (must have sufficient space, see warning)
 * @return Length of result in digits
 *
 * @warning If return value > \c INTN_BSL_MAX_RES_LEN, result overflowed and out buffer
 *          was not written. Caller must check return value before using result.
 * @pre out buffer must be at least \c INTN_BSL_MAX_RES_LEN digits when shift is reasonable
 * @post Result may have leading zeros (not normalized)
 * @note Accepts both normalized and non-normalized inputs
 */
size_t intn_bsl(const intn_digit_t num[], size_t len, size_t n, intn_digit_t *out);

/**
 * @brief Bit shift right of signed multi-precision integer
 *
 * Performs arithmetic right shift (sign-extending) by n bit positions.
 *
 * @param num Integer magnitude to shift
 * @param len Length of integer in digits
 * @param num_sign Sign of integer
 * @param n Number of bit positions to shift
 * @param[out] out Result buffer (must have at least len digits)
 * @return Length of result in digits
 *
 * @pre out buffer must be at least len digits
 * @post Result may have leading zeros (not normalized)
 * @note Follows Erlang semantics: large shifts converge to -1 (negative) or 0 (non-negative)
 * @note Accepts both normalized and non-normalized inputs
 */
size_t intn_bsr(const intn_digit_t num[], size_t len, intn_integer_sign_t num_sign, size_t n,
    intn_digit_t *out);

/**
 * @brief Count non-zero digits in multi-precision integer
 *
 * Returns the number of significant (non-zero) digits, effectively normalizing
 * the length. This is used to determine the actual size of a result after an
 * operation that may produce leading zeros.
 *
 * @param num Integer array to count
 * @param num_len Length of array in digits
 * @return Number of non-zero digits (0 if integer is zero)
 *
 * @note Essential for normalization after operations
 * @code
 * // Examples:
 * intn_count_digits({0x0, 0x0, 0x0, 0x80000000}, 4) -> 4 (no leading zeros)
 * intn_count_digits({0x0, 0x0, 0x0, 0x80000000, 0x0}, 5) -> 4 (one leading zero)
 * @endcode
 */
size_t intn_count_digits(const intn_digit_t *num, size_t num_len);

/**
 * @brief Convert multi-precision integer to string
 *
 * Converts integer to ASCII string representation in specified base.
 * Output uses uppercase letters for digits > 9, with no base prefix (e.g., no "0x").
 *
 * @param num Integer magnitude (must be normalized)
 * @param len Length of integer in digits
 * @param num_sign Sign of integer
 * @param base Number base for conversion (2-36)
 * @param[out] string_len Length of resulting string (not including null terminator)
 * @return Newly allocated null-terminated string (caller must free)
 *
 * @pre base >= 2 && base <= 36
 * @post Returned string must be freed by caller
 * @note Output format: uppercase letters, no base prefix
 * @note Accepts both normalized and non-normalized inputs
 */
char *intn_to_string(const intn_digit_t *num, size_t len, intn_integer_sign_t num_sign, int base,
    size_t *string_len);

/**
 * @brief Parse ASCII string to multi-precision integer
 *
 * Parses integer from ASCII representation in specified base.
 * Supports chunk parsing for arbitrarily large integers.
 *
 * @param buf Buffer containing ASCII digits
 * @param buf_len Length of buffer in bytes
 * @param base Number base for parsing (2-36)
 * @param[out] out Result buffer (must have at least \c INTN_MAX_RES_LEN digits)
 * @param[out] out_sign Sign of parsed integer
 * @return Number of digits in result, or negative on parse error
 *
 * @pre base >= 2 && base <= 36
 * @pre buf != NULL when buf_len > 0 (NULL allowed only for zero-length buffer)
 * @pre out buffer must be at least \c INTN_MAX_RES_LEN digits
 * @post Result may have leading zeros (not normalized)
 *
 * @note No base prefixes (like "0x") are supported
 * @note Leading zeros in input are skipped automatically
 * @note Signs (+/-) accepted unless rejected by caller options
 * @note Case-insensitive for letter digits (a-z, A-Z)
 */
int intn_parse(
    const char buf[], size_t buf_len, int base, intn_digit_t *out, intn_integer_sign_t *out_sign);

/**
 * @brief Convert multi-precision integer to double
 *
 * Converts integer to floating-point representation.
 * May lose precision for large integers.
 *
 * @param num Integer magnitude (must be normalized)
 * @param len Length of integer in digits
 * @param sign Sign of integer
 * @return Double representation
 *
 * @note Precision loss expected for integers > 53 bits
 * @note With current 256-bit limit, result always fits in double range
 * @note Accepts both normalized and non-normalized inputs
 */
double intn_to_double(const intn_digit_t *num, size_t len, intn_integer_sign_t sign);

/**
 * @brief Convert double to multi-precision integer
 *
 * Converts floating-point value to integer, truncating fractional part.
 *
 * @param dnum Double value to convert
 * @param[out] out Result buffer (must have sufficient space)
 * @param[out] out_sign Sign of result
 * @return Number of digits in result, or negative on error
 *
 * @pre dnum must be finite (not NaN or infinity)
 * @post Result may have leading zeros (not normalized)
 */
int intn_from_double(double dnum, intn_digit_t *out, intn_integer_sign_t *out_sign);

/**
 * @brief Convert byte array to multi-precision integer
 *
 * Converts integer from byte representation with specified endianness and signedness.
 *
 * @param in Input byte array
 * @param in_size Size of input in bytes
 * @param opts Conversion options (endianness, signedness)
 * @param[out] out Result buffer (must have at least \c
 * intn_required_digits_for_unsigned_integer(in_size) digits)
 * @param[out] out_sign Sign of result
 * @return Number of digits in result, or negative on error
 *
 * @pre out buffer must have sufficient space based on in_size
 * @post Result may have leading zeros (not normalized)
 */
int intn_from_integer_bytes(const uint8_t in[], size_t in_size, intn_from_integer_options_t opts,
    intn_digit_t out[], intn_integer_sign_t *out_sign);

/**
 * @brief Convert multi-precision integer to byte array
 *
 * Converts integer to byte representation with specified endianness and signedness.
 *
 * @param in Integer magnitude (must be normalized)
 * @param in_len Length of integer in digits
 * @param in_sign Sign of integer
 * @param opts Conversion options (endianness, signedness)
 * @param[out] out Output byte buffer
 * @param out_len Size of output buffer in bytes
 * @return Number of bytes written, or negative on error (buffer too small)
 *
 * @pre Input must be normalized for correct size calculation
 */
int intn_to_integer_bytes(const intn_digit_t in[], size_t in_len, intn_integer_sign_t in_sign,
    intn_from_integer_options_t opts, uint8_t out[], size_t out_len);

/**
 * @brief Calculate bytes needed for unsigned integer representation
 *
 * Returns minimum number of bytes needed to represent the integer
 * as an unsigned value.
 *
 * @param in Integer magnitude (must be normalized)
 * @param in_len Length of integer in digits
 * @return Number of bytes required
 *
 * @pre Input must be normalized for accurate result
 */
size_t intn_required_unsigned_integer_bytes(const intn_digit_t in[], size_t in_len);

/**
 * @brief Calculate digits needed for byte array
 *
 * Returns number of \c intn_digit_t digits needed to store an integer
 * of given byte size.
 *
 * @param size_in_bytes Size of integer in bytes
 * @return Number of digits required
 */
static inline size_t intn_required_digits_for_unsigned_integer(size_t size_in_bytes)
{
    return size_align_up_pow2(size_in_bytes, sizeof(intn_digit_t)) / sizeof(intn_digit_t);
}

/**
 * @brief Negate sign of integer
 *
 * Flips the sign from positive to negative or vice versa.
 *
 * @param sign Original sign
 * @return Negated sign
 */
static inline intn_integer_sign_t intn_negate_sign(intn_integer_sign_t sign)
{
    return (sign == IntNPositiveInteger) ? IntNNegativeInteger : IntNPositiveInteger;
}

/**
 * @brief Copy and optionally zero-extend multi-precision integer
 *
 * Copies integer to output buffer, padding with zeros if output
 * buffer is larger than input.
 *
 * @param num Source integer
 * @param num_len Length of source in digits
 * @param[out] out Destination buffer
 * @param extend_to Size of destination buffer in digits
 *
 * @pre extend_to >= num_len
 * @post Digits from num_len to extend_to are zeroed
 */
static inline void intn_copy(
    const intn_digit_t *num, size_t num_len, intn_digit_t *out, size_t extend_to)
{
    memcpy(out, num, num_len * sizeof(intn_digit_t));
    memset(out + num_len, 0, (extend_to - num_len) * sizeof(intn_digit_t));
}

/**
 * @brief Convert \c uint64_t to multi-precision integer
 *
 * Converts unsigned 64-bit value to 2-digit representation.
 *
 * @param absu64 Unsigned 64-bit value
 * @param[out] out Output buffer (must have at least \c INTN_UINT64_LEN digits)
 *
 * @pre out buffer must have at least \c INTN_UINT64_LEN digits
 * @post Exactly \c INTN_UINT64_LEN digits are written
 */
static inline void intn_from_uint64(uint64_t absu64, intn_digit_t out[])
{
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
    memcpy(out, &absu64, sizeof(absu64));
#elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
    const uint32_t *i32 = (const uint32_t *) &absu64;
    out[0] = i32[1];
    out[1] = i32[0];
#else
#error "Unsupported endianness"
#endif
}

/**
 * @brief Convert \c int64_t to multi-precision integer
 *
 * Converts signed 64-bit value to magnitude-sign representation.
 *
 * @param i64 Signed 64-bit value
 * @param[out] out Output buffer (must have at least \c INTN_INT64_LEN digits)
 * @param[out] out_sign Sign of result
 *
 * @pre out buffer must have at least \c INTN_INT64_LEN digits
 * @post Exactly \c INTN_INT64_LEN digits are written
 */
static inline void intn_from_int64(int64_t i64, intn_digit_t out[], intn_integer_sign_t *out_sign)
{
    bool is_negative;
    uint64_t absu64 = int64_safe_unsigned_abs_set_flag(i64, &is_negative);
    *out_sign = is_negative ? IntNNegativeInteger : IntNPositiveInteger;
    intn_from_uint64(absu64, out);
}

/**
 * @brief Convert 2-digit multi-precision integer to \c uint64_t
 *
 * Extracts unsigned 64-bit value from 2-digit representation.
 *
 * @param num Integer array (must have exactly 2 digits)
 * @return Unsigned 64-bit value
 *
 * @pre num must have exactly 2 digits
 */
static inline uint64_t intn_to_uint64(const intn_digit_t num[])
{
    uint64_t utmp;
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
    memcpy(&utmp, num, sizeof(uint64_t));
#elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
    utmp = (((uint64_t) num[1] << 32) | (uint64_t) num[0]);
#else
#error "Unsupported endianness"
#endif

    return utmp;
}

/**
 * @brief Convert multi-precision integer to \c int64_t
 *
 * Converts magnitude-sign representation to signed 64-bit value.
 *
 * @param num Integer magnitude (must be normalized)
 * @param len Length of integer in digits
 * @param sign Sign of integer
 * @return Signed 64-bit value
 *
 * @pre Integer must fit in \c int64_t range (use \c intn_fits_int64 to verify)
 * @pre Input must be normalized
 * @warning Undefined behavior if value doesn't fit in \c int64_t
 */
static inline int64_t intn_to_int64(const intn_digit_t num[], size_t len, intn_integer_sign_t sign)
{
    switch (len) {
        case 0:
            return 0;
        case 1:
            return int64_cond_neg_unsigned(sign == IntNNegativeInteger, num[0]);
        case 2: {
            uint64_t utmp = intn_to_uint64(num);
            return int64_cond_neg_unsigned(sign == IntNNegativeInteger, utmp);
        }
        default:
            UNREACHABLE();
    }
}

/**
 * @brief Check if multi-precision integer fits in \c int64_t
 *
 * Tests whether integer can be represented as signed 64-bit value.
 *
 * @param num Integer magnitude (must be normalized)
 * @param len Length of integer in digits
 * @param sign Sign of integer
 * @return true if fits in \c int64_t, false otherwise
 *
 * @pre Input must be normalized (no leading zeros)
 */
static inline bool intn_fits_int64(const intn_digit_t num[], size_t len, intn_integer_sign_t sign)
{
    if (len < INTN_INT64_LEN) {
        return true;
    } else if (len == INTN_INT64_LEN) {
        uint64_t u64 = intn_to_uint64(num);
        return !uint64_does_overflow_int64(u64, sign == IntNNegativeInteger);
    }
    return false;
}

#endif
