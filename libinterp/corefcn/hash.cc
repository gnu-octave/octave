/*

Copyright (C) 2007-2015 David Bateman
Copyright (C) 2015 Kai T. Ohlhus

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

/*
Implementation note (Dec, 2015): All supported cryptographic hash
functions are calling "low-level" inplementations of the GNULIB.

The GNULIB, contains even more HMAC based algorithms, c.f.
https://www.gnu.org/software/gnulib/MODULES.html#module=crypto/hmac-md5
so a future project might be including these algorithms as well, adding
a third key input parameter.  There is also a GNULIB "high-level"
interface to Libcrypt. It might be easier to use, but it introduces
a new build dependency, so better stick to the "low-level" functions
for now.
*/

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include <algorithm>
#include <iomanip>
#include <sstream>

#include "defun.h"
#include "md2.h"
#include "md4.h"
#include "md5.h"
#include "sha1.h"
#include "sha256.h"
#include "sha512.h"

DEFUN (hash, args, ,
       "-*- texinfo -*-\n\
@deftypefn {} {} hash (@var{hfun}, @var{str})\n\
Calculate the hash value of the string @var{str} using the hash function\n\
@var{hfun}.\n\
\n\
The available hash functions are given in the table below.\n\
\n\
@table @samp\n\
@item MD2\n\
Message-Digest Algorithm 2 (RFC 1319).\n\
\n\
@item MD4\n\
Message-Digest Algorithm 4 (RFC 1320).\n\
\n\
@item MD5\n\
Message-Digest Algorithm 5 (RFC 1321).\n\
\n\
@item SHA1\n\
Secure Hash Algorithm 1 (RFC 3174)\n\
\n\
@item SHA224\n\
Secure Hash Algorithm 2 (224 Bits, RFC 3874)\n\
\n\
@item SHA256\n\
Secure Hash Algorithm 2 (256 Bits, RFC 6234)\n\
\n\
@item SHA384\n\
Secure Hash Algorithm 2 (384 Bits, RFC 6234)\n\
\n\
@item SHA512\n\
Secure Hash Algorithm 2 (512 Bits, RFC 6234)\n\
@end table\n\
\n\
To calculate for example the MD5 hash value of the string\n\
@nospell{@qcode{\"abc\"}} the @code{hash} function is called as follows:\n\
\n\
@example\n\
@group\n\
hash (\"md5\", \"abc\")\n\
     @print{} ans = 900150983cd24fb0d6963f7d28e17f72\n\
@end group\n\
@end example\n\
\n\
For the same string, the SHA-1 hash value is calculated with:\n\
\n\
@example\n\
@group\n\
hash (\"sha1\", \"abc\")\n\
     @print{} ans = a9993e364706816aba3e25717850c26c9cd0d89d\n\
@end group\n\
@end example\n\
\n\
And to compute the hash value of a file, e.g., @code{file = \"file.txt\"},\n\
call @code{hash} in combination with the @code{fileread}:\n\
\n\
@example\n\
@group\n\
hash (\"md5\", fileread (file));\n\
@end group\n\
@end example\n\
\n\
@end deftypefn")
{
  if (args.length () != 2)
    print_usage ();

  std::string hfun = args(0).string_value ();
  std::string str = args(1).string_value ();

  // Determine hash function used
  std::transform(hfun.begin(), hfun.end(), hfun.begin(), ::toupper);

  // Buffer with maximum size for all hash function
  char result_buffer[SHA512_DIGEST_SIZE];

  int result_buffer_len = 0;
  if (hfun == "MD2")
    {
      md2_buffer (str.data (), str.length (), result_buffer);
      result_buffer_len = MD2_DIGEST_SIZE;
    }
  else if (hfun == "MD4")
    {
      md4_buffer (str.data (), str.length (), result_buffer);
      result_buffer_len = MD4_DIGEST_SIZE;
    }
  else if (hfun == "MD5")
    {
      md5_buffer (str.data (), str.length (), result_buffer);
      result_buffer_len = MD5_DIGEST_SIZE;
    }
  else if (hfun == "SHA1")
    {
      sha1_buffer (str.data (), str.length (), result_buffer);
      result_buffer_len = SHA1_DIGEST_SIZE;
    }
  else if (hfun == "SHA224")
    {
      sha224_buffer (str.data (), str.length (), result_buffer);
      result_buffer_len = SHA224_DIGEST_SIZE;
    }
  else if (hfun == "SHA256")
    {
      sha256_buffer (str.data (), str.length (), result_buffer);
      result_buffer_len = SHA256_DIGEST_SIZE;
    }
  else if (hfun == "SHA384")
    {
      sha384_buffer (str.data (), str.length (), result_buffer);
      result_buffer_len = SHA384_DIGEST_SIZE;
    }
  else if (hfun == "SHA512")
    {
      sha512_buffer (str.data (), str.length (), result_buffer);
      result_buffer_len = SHA512_DIGEST_SIZE;
    }
  else
    error ("hash function not supported");

  // Everything went fine, return formatted hash string
  std::ostringstream os;
  for (int i = 0; i < result_buffer_len; i++)
    {
      // Assure hex representation of one byte is written
      os << std::hex << std::setw (2) << std::setfill ('0')
         << (static_cast<int> (result_buffer[i]) & 0xFF);
    }

  return ovl (os.str ());
}

/*
## MD2 test suite (RFC 1319)
%!assert (hash ("md2", ""), "8350e5a3e24c153df2275c9f80692773");
%!assert (hash ("md2", "a"), "32ec01ec4a6dac72c0ab96fb34c0b5d1");
%!assert (hash ("md2", "abc"), "da853b0d3f88d99b30283a69e6ded6bb");
%!assert (hash ("md2", "message digest"), "ab4f496bfb2a530b219ff33031fe06b0");
%!assert (hash ("md2", "abcdefghijklmnopqrstuvwxyz"),
%!        "4e8ddff3650292ab5a4108c3aa47940b");
%!assert (hash ("md2", ["ABCDEFGHIJKLMNOPQRSTUVWXYZ", ...
%!              "abcdefghijklmnopqrstuvwxyz0123456789"]),
%!        "da33def2a42df13975352846c30338cd");
%!assert (hash ("md2", ["123456789012345678901234567890123456789", ...
%!              "01234567890123456789012345678901234567890"]),
%!        "d5976f79d83d3a0dc9806c3c66f3efd8");

## MD4 test suite (RFC 1320)
%!assert (hash ("md4", ""), "31d6cfe0d16ae931b73c59d7e0c089c0");
%!assert (hash ("md4", "a"), "bde52cb31de33e46245e05fbdbd6fb24");
%!assert (hash ("md4", "abc"), "a448017aaf21d8525fc10ae87aa6729d");
%!assert (hash ("md4", "message digest"), "d9130a8164549fe818874806e1c7014b");
%!assert (hash ("md4", "abcdefghijklmnopqrstuvwxyz"),
%!        "d79e1c308aa5bbcdeea8ed63df412da9");
%!assert (hash ("md4", ["ABCDEFGHIJKLMNOPQRSTUVWXYZ", ...
%!              "abcdefghijklmnopqrstuvwxyz0123456789"]),
%!        "043f8582f241db351ce627e153e7f0e4");
%!assert (hash ("md4", ["123456789012345678901234567890123456789", ...
%!              "01234567890123456789012345678901234567890"]),
%!        "e33b4ddc9c38f2199c3e7b164fcc0536");

## MD5 test suite (RFC 1321)
%!assert (hash ("md5", ""), "d41d8cd98f00b204e9800998ecf8427e");
%!assert (hash ("md5", "a"), "0cc175b9c0f1b6a831c399e269772661");
%!assert (hash ("md5", "abc"), "900150983cd24fb0d6963f7d28e17f72");
%!assert (hash ("md5", "message digest"), "f96b697d7cb7938d525a2f31aaf161d0");
%!assert (hash ("md5", "abcdefghijklmnopqrstuvwxyz"),
%!        "c3fcd3d76192e4007dfb496cca67e13b");
%!assert (hash ("md5", ["ABCDEFGHIJKLMNOPQRSTUVWXYZ", ...
%!              "abcdefghijklmnopqrstuvwxyz0123456789"]),
%!        "d174ab98d277d9f5a5611c2c9f419d9f");
%!assert (hash ("md5", ["123456789012345678901234567890123456789", ...
%!              "01234567890123456789012345678901234567890"]),
%!        "57edf4a22be3c955ac49da2e2107b67a");

## SHA1 test suite (RFC 3174) and more
%!assert (hash ("sha1", ""), "da39a3ee5e6b4b0d3255bfef95601890afd80709");
%!assert (hash ("sha1", "a"), "86f7e437faa5a7fce15d1ddcb9eaeaea377667b8");
%!assert (hash ("sha1", "abc"), "a9993e364706816aba3e25717850c26c9cd0d89d");
%!assert (hash ("sha1", ["abcdbcdecdefdefgefghfghighijhi", ...
%!              "jkijkljklmklmnlmnomnopnopq"]),
%!        "84983e441c3bd26ebaae4aa1f95129e5e54670f1");
%!assert (hash ("sha1", ["01234567012345670123456701234567", ...
%!              "01234567012345670123456701234567"]),
%!        "e0c094e867ef46c350ef54a7f59dd60bed92ae83");
%!assert (hash ("sha1", "The quick brown fox jumps over the lazy dog"),
%!        "2fd4e1c67a2d28fced849ee1bb76e7391b93eb12");

## SHA224 test suite (RFC 3874) and more
%!assert (hash ("sha224", ""),
%!        "d14a028c2a3a2bc9476102bb288234c415a2b01f828ea62ac5b3e42f");
%!assert (hash ("sha224", "a"),
%!        "abd37534c7d9a2efb9465de931cd7055ffdb8879563ae98078d6d6d5");
%!assert (hash ("sha224", "abc"),
%!        "23097d223405d8228642a477bda255b32aadbce4bda0b3f7e36c9da7");
%!assert (hash ("sha224", ["abcdbcdecdefdefgefghfghighijh", ...
%!              "ijkijkljklmklmnlmnomnopnopq"]),
%!        "75388b16512776cc5dba5da1fd890150b0c6455cb4f58b1952522525");

## SHA256/384/512 tests (RFC 6234) and more
%!assert (hash ("sha256", ""),
%!        "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855");
%!assert (hash ("sha384", ""),
%!        ["38b060a751ac96384cd9327eb1b1e36a21fdb71114be07434c0cc", ...
%!         "7bf63f6e1da274edebfe76f65fbd51ad2f14898b95b"]);
%!assert (hash ("sha512", ""),
%!        ["cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a", ...
%!         "921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47", ...
%!         "417a81a538327af927da3e"]);

## Test special character behavior (bug #31689)
%!assert (hash ("md2", "abc\0"), "5a636d615002a7874ac1c9e9a43361f7");
%!assert (hash ("md4", "abc\0"), "0ee5201897ecb206c4eaba1d2da5224d");
%!assert (hash ("md5", "abc\0"), "147a664a2ca9410911e61986d3f0d52a");
%!assert (hash ("sha1", "abc\0"), "686483805ac47ca14e03514f7481a7973b401762");
%!assert (hash ("sha224", "abc\0"),
%!        "fbc8e47920e108bb1d0b631d18b36ae9b1549d28362aa15ebe960cfb");
%!assert (hash ("sha256", "abc\0"),
%!        "dc1114cd074914bd872cc1f9a23ec910ea2203bc79779ab2e17da25782a624fc");
%!assert (hash ("sha384", "abc\0"),
%!        ["eba81f2dfba4ec60d3f786c89d91b08e6c0b63d55986874378e385", ...
%!         "e6fac587cce7a520ca9437290fe626cbf75c855e17"]);
%!assert (hash ("sha512", "abc\0"),
%!        ["7ce05eda233e545a2d5c626862a5ddaafb09b9d8ec3bec08aa458b", ...
%!         "7c9e7d939d84a57d5a20d8a9002983aabae2457b19c50ba326bf5b", ...
%!         "081f75b41342f42c3383"]);

## Test equivalence to deprecated md5sum offering file hashing
%!test
%! tfile = tempname ();
%! fid = fopen (tfile, "wb");
%! fwrite (fid, "abc\0");
%! fclose (fid);
%! assert (hash ("md5", fileread (tfile)), "147a664a2ca9410911e61986d3f0d52a");
%! unlink (tfile);

## Test bad function calls
%!error hash ();
%!error hash ("");
%!error hash ("", "");
%!error hash ("", "", "");
%!error hash (1, "");
%!error hash ([1, 0; 0, 1], "");
%!error hash ("unknown", "");
%!error hash ("md5");
%!error hash ("sha1");
%!error hash ("sha512");
*/
