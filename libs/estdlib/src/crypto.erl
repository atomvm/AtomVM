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
    crypto_one_time/4,
    crypto_one_time/5,
    generate_key/2,
    compute_key/4,
    strong_rand_bytes/1,
    info_lib/0
]).

-type hash_algorithm() :: md5 | sha | sha224 | sha256 | sha384 | sha512.
-type digest() :: binary().

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
    | aes_256_ctr.

-type padding() :: none | pkcs_padding.

-type crypto_opt() :: {encrypt, boolean()} | {padding, padding()}.
-type crypto_opts() :: [crypto_opt()].

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
%% @param   N desired length of cryptographically secure random data
%% @returns Returns Cryptographically secure random data of length `N'
%% @doc     Generate N cryptographically secure random octets
%%          and return the result in a binary.
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
