%
% This file is part of AtomVM.
%
% Copyright 2023 Fred Dushin <fred@dushin.net>
% Copyright 2026 Davide Bettio <davide@uninstall.it>
%
% Licensed under the Apache License, Version 2.0 (the "License")
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%    http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%
-module(crypto).

-export([
    hash/2,
    hash_init/1,
    hash_update/2,
    hash_final/1,
    crypto_one_time/4,
    crypto_one_time/5,
    crypto_one_time_aead/6,
    crypto_one_time_aead/7,
    crypto_init/3,
    crypto_init/4,
    crypto_update/2,
    crypto_final/1,
    generate_key/2,
    compute_key/4,
    sign/4,
    verify/5,
    mac/4,
    mac_init/3,
    mac_update/2,
    mac_final/1,
    mac_finalN/2,
    pbkdf2_hmac/5,
    hash_equals/2,
    strong_rand_bytes/1,
    info_lib/0
]).

-type hash_algorithm() :: md5 | sha | sha224 | sha256 | sha384 | sha512.
-type digest() :: binary().

-export_type([hash_state/0]).
-opaque hash_state() :: reference().

-export_type([crypto_state/0]).
%% Opaque mutable state for streaming cipher operations.
%% The state is mutated in place by crypto_update/2 and crypto_final/1.
-opaque crypto_state() :: reference().

%% Note: PKCS7 padding (`pkcs_padding') is **not** supported for ECB ciphers
%% in AtomVM.  Use a CBC cipher if you need PKCS7 padding.
-type cipher_no_iv() ::
    aes_128_ecb
    | aes_192_ecb
    | aes_256_ecb.

-type cipher_iv() ::
    aes_128_cbc
    | aes_192_cbc
    | aes_256_cbc
    | aes_128_cfb128
    | aes_192_cfb128
    | aes_256_cfb128
    | aes_128_ctr
    | aes_192_ctr
    | aes_256_ctr
    | aes_128_ofb
    | aes_192_ofb
    | aes_256_ofb.

-type cipher_aead() ::
    aes_128_gcm
    | aes_192_gcm
    | aes_256_gcm
    | aes_128_ccm
    | aes_192_ccm
    | aes_256_ccm
    | chacha20_poly1305.

-type padding() :: none | pkcs_padding.

-type crypto_opt() :: {encrypt, boolean()} | {padding, padding()}.
-type crypto_opts() :: boolean() | [crypto_opt()].

-type pk_type() :: eddh | eddsa | ecdh.

%% Curves/params accepted by the current AtomVM implementation.
%% Note: not all combinations are supported by every function (see docs below).
-type pk_param() ::
    x25519
    | x448
    | ed25519
    | ed448
    | secp256r1
    | secp384r1
    | secp521r1
    | secp256k1
    | brainpoolP256r1
    | brainpoolP384r1
    | brainpoolP512r1.

%% ECDSA is currently supported only on short Weierstrass secp* and brainpool curves.
-type ecdsa_curve() ::
    secp256k1
    | secp256r1
    | secp384r1
    | secp521r1
    | brainpoolP256r1
    | brainpoolP384r1
    | brainpoolP512r1.

-type ecdsa_private_key() :: [binary() | ecdsa_curve()].
-type ecdsa_public_key() :: [binary() | ecdsa_curve()].

-type mac_type() :: cmac | hmac.

-type cmac_subtype() ::
    aes_128_cbc
    | aes_128_ecb
    | aes_192_cbc
    | aes_192_ecb
    | aes_256_cbc
    | aes_256_ecb.

-type mac_subtype() :: cmac_subtype() | hash_algorithm() | ripemd160.

-export_type([mac_state/0]).
-opaque mac_state() :: reference().

%%-----------------------------------------------------------------------------
%% @param   Type the hash algorithm
%% @param   Data the data to hash
%% @returns Returns the result of hashing the supplied data using the supplied
%%          hash algorithm.
%% @doc     Hash data using a specified hash algorithm.
%% @end
%%-----------------------------------------------------------------------------
-spec hash(Type :: hash_algorithm(), Data :: iolist()) -> digest().
hash(_Type, _Data) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Type the hash algorithm
%% @returns Returns an opaque hash state that can be passed to `hash_update/2'
%%          and `hash_final/1'.
%% @doc     Initialize a streaming hash computation.
%% @end
%%-----------------------------------------------------------------------------
-spec hash_init(Type :: hash_algorithm()) -> hash_state().
hash_init(_Type) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   State an opaque hash state returned from `hash_init/1' or a
%%          previous call to `hash_update/2'
%% @param   Data the data to feed into the hash computation
%% @returns Returns a new opaque hash state with the supplied data folded in.
%% @doc     Update a streaming hash computation with new data.
%%
%%          The original `State' is not modified; a new state is returned so
%%          the same state can be forked into independent hash computations.
%% @end
%%-----------------------------------------------------------------------------
-spec hash_update(State :: hash_state(), Data :: iolist()) -> hash_state().
hash_update(_State, _Data) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   State an opaque hash state returned from `hash_init/1' or
%%          `hash_update/2'
%% @returns Returns the final hash digest as a binary.
%% @doc     Finalize a streaming hash computation and return the digest.
%%
%%          The state is not consumed; calling `hash_final/1' again on the
%%          same state will produce the same digest.
%% @end
%%-----------------------------------------------------------------------------
-spec hash_final(State :: hash_state()) -> digest().
hash_final(_State) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Cipher a supported cipher
%% @param   Key the encryption / decryption key
%% @param   Data to be crypted or encrypted
%% @param   FlagOrOptions either just true for encryption (or false for decryption), or a proplist
%%          for any additional option
%% @returns Returns crypted or encrypted data.
%% @doc     Encrypted/decrypt data using given cipher and key
%% @end
%%-----------------------------------------------------------------------------
-spec crypto_one_time(
    Cipher :: cipher_no_iv(),
    Key :: iodata(),
    Data :: iodata(),
    FlagOrOptions :: crypto_opts()
) -> binary().
crypto_one_time(_Cipher, _Key, _Data, _FlagOrOptions) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Cipher a supported cipher that makes use of IV
%% @param   Key the encryption / decryption key
%% @param   IV an initialization vector
%% @param   Data to be crypted or encrypted
%% @param   FlagOrOptions either just true for encryption (or false for decryption), or a proplist
%%          for any additional option such as padding.
%% @returns Returns crypted or encrypted data.
%% @doc     Encrypted/decrypt data using given cipher, key, IV.
%% @end
%%-----------------------------------------------------------------------------
-spec crypto_one_time(
    Cipher :: cipher_iv(),
    Key :: iodata(),
    IV :: iodata(),
    Data :: iodata(),
    FlagOrOptions :: crypto_opts()
) -> binary().
crypto_one_time(_Cipher, _Key, _IV, _Data, _FlagOrOptions) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Cipher an AEAD cipher
%% @param   Key the encryption key
%% @param   IV nonce / initialization vector
%% @param   InText plaintext to encrypt (iodata)
%% @param   AAD additional authenticated data (iodata)
%% @param   EncFlag `true' for encryption
%% @returns Returns `{CipherText, Tag}' on success.
%% @doc     Encrypt data using an AEAD cipher with the default tag length.
%%
%%          Supported ciphers: `aes_128_gcm', `aes_192_gcm', `aes_256_gcm',
%%          `aes_128_ccm', `aes_192_ccm', `aes_256_ccm', `chacha20_poly1305'.
%% @end
%%-----------------------------------------------------------------------------
-spec crypto_one_time_aead(
    Cipher :: cipher_aead(),
    Key :: iodata(),
    IV :: iodata(),
    InText :: iodata(),
    AAD :: iodata(),
    EncFlag :: true
) -> {binary(), binary()}.
crypto_one_time_aead(_Cipher, _Key, _IV, _InText, _AAD, _EncFlag) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Cipher an AEAD cipher
%% @param   Key the encryption / decryption key
%% @param   IV nonce / initialization vector
%% @param   InText plaintext (encrypt) or ciphertext (decrypt) (iodata)
%% @param   AAD additional authenticated data (iodata)
%% @param   TagOrTagLength integer tag length when encrypting, tag binary when decrypting
%% @param   EncFlag `true' for encryption, `false' for decryption
%% @returns Encryption: `{CipherText, Tag}'. Decryption: plaintext binary, or `error' on
%%          authentication failure.
%% @doc     Encrypt or decrypt data using an AEAD cipher.
%%
%%          When decrypting, authentication failure returns the atom `error'
%%          rather than raising an exception, matching OTP behaviour.
%% @end
%%-----------------------------------------------------------------------------
-spec crypto_one_time_aead(
    Cipher :: cipher_aead(),
    Key :: iodata(),
    IV :: iodata(),
    InText :: iodata(),
    AAD :: iodata(),
    TagOrTagLength :: non_neg_integer() | binary(),
    EncFlag :: boolean()
) -> {binary(), binary()} | binary() | error.
crypto_one_time_aead(_Cipher, _Key, _IV, _InText, _AAD, _TagOrTagLength, _EncFlag) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Cipher a supported cipher (no IV required, e.g. ECB modes)
%% @param   Key the encryption / decryption key
%% @param   FlagOrOptions either `true' for encryption, `false' for decryption,
%%          or a proplist with options such as `{encrypt, boolean()}' and
%%          `{padding, padding()}'.
%% @returns Returns an opaque `crypto_state()' reference for use with
%%          `crypto_update/2' and `crypto_final/1'.
%% @doc     Initialize a streaming cipher operation for ciphers that do not use an IV.
%%
%%          This is equivalent to `crypto_init(Cipher, Key, <<>>, FlagOrOptions)'.
%% @end
%%-----------------------------------------------------------------------------
-spec crypto_init(
    Cipher :: cipher_no_iv(),
    Key :: iodata(),
    FlagOrOptions :: crypto_opts()
) -> crypto_state().
crypto_init(Cipher, Key, FlagOrOptions) ->
    crypto_init(Cipher, Key, <<>>, FlagOrOptions).

%%-----------------------------------------------------------------------------
%% @param   Cipher a supported cipher
%% @param   Key the encryption / decryption key
%% @param   IV an initialization vector (use `<<>>' for ciphers that do not require one)
%% @param   FlagOrOptions either `true' for encryption, `false' for decryption,
%%          or a proplist with options such as `{encrypt, boolean()}' and
%%          `{padding, padding()}'.
%% @returns Returns an opaque `crypto_state()' reference for use with
%%          `crypto_update/2' and `crypto_final/1'.
%% @doc     Initialize a streaming cipher operation.
%%
%%          The returned state is **mutable**: `crypto_update/2' and `crypto_final/1'
%%          mutate it in place.  After `crypto_final/1' the state must not be reused.
%%
%%          **AtomVM padding support:**
%%          `{padding, pkcs_padding}' is supported **only with CBC ciphers**
%%          (e.g. `aes_128_cbc', `aes_192_cbc', `aes_256_cbc').  Requesting
%%          PKCS7 padding with ECB or any other cipher mode raises a `badarg'
%%          error.  This differs from OTP, which silently accepts the option
%%          for non-CBC modes.
%% @end
%%-----------------------------------------------------------------------------
-spec crypto_init(
    Cipher :: cipher_no_iv() | cipher_iv(),
    Key :: iodata(),
    IV :: iodata(),
    FlagOrOptions :: crypto_opts()
) -> crypto_state().
crypto_init(_Cipher, _Key, _IV, _FlagOrOptions) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   State an opaque cipher state returned from `crypto_init/3' or
%%          `crypto_init/4'
%% @param   Data the data to encrypt or decrypt (iodata)
%% @returns Returns the encrypted/decrypted bytes produced so far as a binary.
%% @doc     Feed data into a streaming cipher operation.
%%
%%          For block ciphers the output may be shorter than the input if an
%%          incomplete block is buffered internally.
%% @end
%%-----------------------------------------------------------------------------
-spec crypto_update(State :: crypto_state(), Data :: iodata()) -> binary().
crypto_update(_State, _Data) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   State an opaque cipher state returned from `crypto_init/3' or
%%          `crypto_init/4'
%% @returns Returns any remaining bytes (e.g. a padded final block) as a binary,
%%          or `<<>>' if there are none.
%% @doc     Finalize a streaming cipher operation.
%%
%%          For block ciphers with `{padding, none}' an error is raised if the
%%          total data fed was not a multiple of the block size.
%%
%%          **AtomVM limitation:** after `crypto_final/1' returns, the state is
%%          permanently finalized.  Calling `crypto_final/1' again, or calling
%%          `crypto_update/2' on the same state, raises a `badarg' error.
%%          OTP allows reuse of the state after `crypto_final/1'.
%% @end
%%-----------------------------------------------------------------------------
-spec crypto_final(State :: crypto_state()) -> binary().
crypto_final(_State) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Type the key algorithm family
%% @param   Param curve/parameter selection
%% @returns Returns a tuple `{PublicKey, PrivateKey}'.
%% @doc     Generate a public/private key pair.
%%
%%          Supported forms:
%%          * `eddh' with `x25519 | x448'
%%          * `ecdh' with `x25519 | x448 | secp256k1 | secp256r1 | secp384r1 | secp521r1 |
%%            brainpoolP256r1 | brainpoolP384r1 | brainpoolP512r1`
%%          * `eddsa' with `ed25519 | ed448' (availability depends on the underlying mbedTLS build)
%%
%%          Keys are returned as **raw exported key material**, not PEM, DER, or `public_key'
%%          records.
%%
%%          Key generation draws from the platform entropy source.  Consult
%%          your platform documentation to ensure the hardware RNG is properly
%%          seeded before generating keys.
%% @end
%%-----------------------------------------------------------------------------
-spec generate_key(Type :: pk_type(), Param :: pk_param()) -> {binary(), binary()}.
generate_key(_Type, _Param) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Type the key agreement type
%% @param   OtherPublicKey peer public key (binary)
%% @param   MyPrivateKey local private key (binary)
%% @param   Param curve/parameter selection
%% @returns Returns the shared secret as a binary.
%% @doc     Compute a shared secret.
%%
%%          Supported forms:
%%          * `eddh' with `x25519 | x448'
%%          * `ecdh' with `x25519 | x448 | secp256k1 | secp256r1 | secp384r1 | secp521r1 |
%%            brainpoolP256r1 | brainpoolP384r1 | brainpoolP512r1`
%%
%%          The public/private key binaries must be in the same format as returned by
%%          `generate_key/2'.
%% @end
%%-----------------------------------------------------------------------------
-spec compute_key(
    Type :: pk_type(),
    OtherPublicKey :: binary(),
    MyPrivateKey :: binary(),
    Param :: pk_param()
) -> binary().
compute_key(_Type, _OtherPublicKey, _MyPrivateKey, _Param) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Algorithm signing algorithm (AtomVM supports `ecdsa')
%% @param   DigestType hash algorithm identifier
%% @param   Data message bytes (iodata)
%% @param   Key signing key material
%% @returns Returns a DER-encoded signature as a binary.
%% @doc     Create a digital signature.
%%
%%          AtomVM currently supports:
%%          * `Algorithm = ecdsa'
%%          * `Key = [PrivateKeyBin, Curve]' where `Curve' is one of
%%            `secp256k1 | secp256r1 | secp384r1 | secp521r1 |
%%            brainpoolP256r1 | brainpoolP384r1 | brainpoolP512r1'
%%
%%          The signature is returned in **DER** form.
%%
%%          ECDSA signing requires a random nonce internally.  The quality of
%%          this nonce depends on the platform entropy source.  Consult your
%%          platform documentation to ensure the hardware RNG is properly
%%          seeded; a predictable nonce can lead to private key recovery.
%% @end
%%-----------------------------------------------------------------------------
-spec sign(
    Algorithm :: ecdsa,
    DigestType :: hash_algorithm(),
    Data :: iodata(),
    Key :: ecdsa_private_key()
) -> binary().
sign(_Algorithm, _DigestType, _Data, _Key) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Algorithm verification algorithm (AtomVM supports `ecdsa')
%% @param   DigestType hash algorithm identifier
%% @param   Data message bytes (iodata)
%% @param   Signature DER-encoded signature
%% @param   Key verification key material
%% @returns Returns `true' if the signature is valid, otherwise `false'.
%% @doc     Verify a digital signature.
%%
%%          AtomVM currently supports:
%%          * `Algorithm = ecdsa'
%%          * `Key = [PublicKeyBin, Curve]' where `Curve' is one of
%%            `secp256k1 | secp256r1 | secp384r1 | secp521r1 |
%%            brainpoolP256r1 | brainpoolP384r1 | brainpoolP512r1`
%%
%%          Invalid DER signatures yield `false' (not an exception).
%% @end
%%-----------------------------------------------------------------------------
-spec verify(
    Algorithm :: ecdsa,
    DigestType :: hash_algorithm(),
    Data :: iodata(),
    Signature :: binary(),
    Key :: ecdsa_public_key()
) -> boolean().
verify(_Algorithm, _DigestType, _Data, _Signature, _Key) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Type MAC algorithm family (`cmac' or `hmac')
%% @param   SubType MAC subtype (cipher for CMAC, digest for HMAC)
%% @param   Key MAC key bytes
%% @param   Data message bytes (iodata)
%% @returns Returns the computed MAC as a binary.
%% @doc     Compute a MAC.
%%
%%          Supported forms:
%%          * `mac(cmac, Cipher, Key, Data)' where `Cipher' selects AES key size:
%%            `aes_128_(cbc|ecb) | aes_192_(cbc|ecb) | aes_256_(cbc|ecb)'
%%            (key length must match the selected size)
%%          * `mac(hmac, Digest, Key, Data)' where `Digest' is a supported hash atom
%%            (AtomVM accepts `ripemd160' in addition to the `hash/2' digests)
%% @end
%%-----------------------------------------------------------------------------
-spec mac(
    Type :: mac_type(),
    SubType :: mac_subtype(),
    Key :: binary(),
    Data :: iodata()
) -> binary().
mac(_Type, _SubType, _Key, _Data) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Type    MAC algorithm family (`cmac' or `hmac')
%% @param   SubType MAC subtype (cipher for CMAC, digest for HMAC)
%% @param   Key     MAC key bytes (iodata)
%% @returns Returns an opaque MAC state for use with mac_update/2 and mac_final/1.
%% @doc     Initialize a streaming MAC operation.
%% @end
%%-----------------------------------------------------------------------------
-spec mac_init(Type :: mac_type(), SubType :: mac_subtype(), Key :: iodata()) -> mac_state().
mac_init(_Type, _SubType, _Key) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   State MAC state from mac_init/3 or a previous mac_update/2
%% @param   Data  data to add (iodata)
%% @returns Returns an updated MAC state.
%% @doc     Add data to a streaming MAC calculation.
%% @end
%%-----------------------------------------------------------------------------
-spec mac_update(State :: mac_state(), Data :: iodata()) -> mac_state().
mac_update(_State, _Data) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   State MAC state from mac_init/3 or mac_update/2
%% @returns Returns the computed MAC as a binary.
%% @doc     Finalize a streaming MAC operation and return the MAC value.
%% @end
%%-----------------------------------------------------------------------------
-spec mac_final(State :: mac_state()) -> binary().
mac_final(_State) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   State     MAC state from mac_init/3 or mac_update/2
%% @param   MacLength desired output length in bytes
%% @returns Returns the computed MAC truncated to MacLength bytes.
%% @doc     Finalize a streaming MAC operation with a custom output length.
%% @end
%%-----------------------------------------------------------------------------
-spec mac_finalN(State :: mac_state(), MacLength :: pos_integer()) -> binary().
mac_finalN(_State, _MacLength) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   DigestType hash algorithm (`sha', `sha256', etc.)
%% @param   Password   password bytes (iodata)
%% @param   Salt       salt bytes (iodata)
%% @param   Iterations iteration count (positive integer)
%% @param   KeyLen     desired output length in bytes (positive integer)
%% @returns Returns the derived key as a binary of `KeyLen' bytes.
%% @doc     Derive a key using PBKDF2-HMAC (RFC 8018 §5.2).
%%
%%          Supported digest types are the same as for `hash/2'.
%% @end
%%-----------------------------------------------------------------------------
-spec pbkdf2_hmac(
    DigestType :: hash_algorithm(),
    Password :: iodata(),
    Salt :: iodata(),
    Iterations :: pos_integer(),
    KeyLen :: pos_integer()
) -> binary().
pbkdf2_hmac(_DigestType, _Password, _Salt, _Iterations, _KeyLen) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Mac1 first MAC or hash binary
%% @param   Mac2 second MAC or hash binary
%% @returns Returns `true' if the two binaries are equal, `false' otherwise.
%% @doc     Compare two MACs or hashes for equality in constant time.
%%
%%          Both arguments must be binaries of the same length; otherwise
%%          an error is raised. The comparison is performed in constant time
%%          to prevent timing side-channel attacks.
%% @end
%%-----------------------------------------------------------------------------
-spec hash_equals(Mac1 :: binary(), Mac2 :: binary()) -> boolean().
hash_equals(_Mac1, _Mac2) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   N desired length of cryptographically secure random data
%% @returns Returns Cryptographically secure random data of length `N'
%% @doc     Generate N cryptographically secure random octets
%%          and return the result in a binary.
%%
%%          The quality of the output depends on the platform entropy source.
%%          Consult your platform documentation to ensure the hardware RNG is
%%          properly seeded before using this function for key material or
%%          other security-sensitive purposes.
%% @end
%%-----------------------------------------------------------------------------
-spec strong_rand_bytes(N :: non_neg_integer()) -> binary().
strong_rand_bytes(_N) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @returns Returns a list of tuples describing the crypto library name, version number,
%%          and version string.
%% @doc     Get the name and version of the libraries used by crypto.
%%
%%          Returns a list containing a single tuple `{Name, VerNum, VerStr}' where:
%%          * `Name' is the library name as a binary (e.g., `<<"mbedtls">>')
%%          * `VerNum' is the numeric version according to the library's versioning scheme
%%          * `VerStr' is the version string as a binary
%%
%%          Example: `[{<<"mbedtls">>, 50790144, <<"Mbed TLS 3.6.1">>}]'
%% @end
%%-----------------------------------------------------------------------------
-spec info_lib() -> [{binary(), integer(), binary()}].
info_lib() ->
    erlang:nif_error(undefined).
