#ifndef MBEDTLS_DECLARE_PRIVATE_IDENTIFIERS
#define MBEDTLS_DECLARE_PRIVATE_IDENTIFIERS
#endif
#include <mbedtls/private/ecdsa.h>

#include <string.h>

#ifdef __cplusplus
extern "C" {
#endif

// Helper to write an ASN.1 integer (backward-writing)
static int asn1_write_integer(unsigned char **p, unsigned char *start, const unsigned char *d, size_t len)
{
    // Skip leading zeros
    while (len > 1 && *d == 0) {
        d++;
        len--;
    }
    // Check if we need a leading 0x00 to make it positive in two's complement
    int lead_zero = (d[0] & 0x80) ? 1 : 0;
    size_t total_len = len + lead_zero;

    if (*p - start < (int)(2 + total_len)) {
        return -1;
    }
    *p -= len;
    memcpy(*p, d, len);
    if (lead_zero) {
        *p -= 1;
        (*p)[0] = 0x00;
    }
    *p -= 1;
    (*p)[0] = (unsigned char)total_len;
    *p -= 1;
    (*p)[0] = 0x02; // Tag: INTEGER

    return 0;
}

// Convert raw (R || S) to DER signature
/*
static inline int mbedtls_ecdsa_raw_to_der(
    size_t bits, const unsigned char *raw, size_t raw_len,
    unsigned char *der, size_t der_size, size_t *der_len)
{
    (void)bits;
    if (raw_len % 2 != 0) {
        return -1;
    }
    size_t key_len = raw_len / 2;
    const unsigned char *r_raw = raw;
    const unsigned char *s_raw = raw + key_len;

    unsigned char *p = der + der_size;
    unsigned char *end = p;

    if (asn1_write_integer(&p, der, s_raw, key_len) != 0) {
        return -1;
    }
    if (asn1_write_integer(&p, der, r_raw, key_len) != 0) {
        return -1;
    }

    size_t seq_len = end - p;
    if (seq_len < 128) {
        if (p - der < 2) {
            return -1;
        }
        p -= 1;
        p[0] = (unsigned char)seq_len;
        p -= 1;
        p[0] = 0x30;
    } else {
        if (p - der < 3) {
            return -1;
        }
        p -= 1;
        p[0] = (unsigned char)(seq_len & 0xFF);
        p -= 1;
        p[0] = 0x81;
        p -= 1;
        p[0] = 0x30;
    }

    *der_len = end - p;
    memmove(der, p, *der_len);
    return 0;
}
*/

// Helper to read an ASN.1 integer
static int asn1_read_integer(const unsigned char **p, const unsigned char *end, unsigned char *d, size_t len)
{
    if (end - *p < 3) {
        return -1;
    }
    if ((*p)[0] != 0x02) { // Tag: INTEGER
        return -1;
    }
    size_t int_len = (*p)[1];
    *p += 2;
    if (end - *p < (int)int_len) {
        return -1;
    }
    const unsigned char *src = *p;
    *p += int_len;

    // Strip leading zero if it was added for padding
    if (int_len > 1 && src[0] == 0x00 && (src[1] & 0x80)) {
        src++;
        int_len--;
    }
    if (int_len > len) {
        return -1; // too big
    }
    // Pad destination with leading zeros
    size_t pad = len - int_len;
    memset(d, 0, pad);
    memcpy(d + pad, src, int_len);
    return 0;
}

/*
static inline int mbedtls_ecdsa_der_to_raw(
    size_t bits, const unsigned char *der, size_t der_len,
    unsigned char *raw, size_t raw_size, size_t *raw_len)
{
    (void)bits;
    const unsigned char *p = der;
    const unsigned char *end = der + der_len;

    if (end - p < 2) {
        return -1;
    }
    if (p[0] != 0x30) { // Tag: SEQUENCE
        return -1;
    }
    size_t seq_len = 0;
    if (p[1] < 128) {
        seq_len = p[1];
        p += 2;
    } else if (p[1] == 0x81) {
        if (end - p < 3) {
            return -1;
        }
        seq_len = p[2];
        p += 3;
    } else {
        return -1;
    }

    if (end - p != (int)seq_len) {
        return -1;
    }

    if (raw_size % 2 != 0) {
        return -1;
    }
    size_t key_len = raw_size / 2;
    unsigned char *r_raw = raw;
    unsigned char *s_raw = raw + key_len;

    if (asn1_read_integer(&p, end, r_raw, key_len) != 0) {
        return -1;
    }
    if (asn1_read_integer(&p, end, s_raw, key_len) != 0) {
        return -1;
    }

    *raw_len = raw_size;
    return 0;
}
*/

#ifdef __cplusplus
}
#endif
