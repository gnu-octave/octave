/*

Copyright (C) 1996-2017 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if ! defined (octave_file_ops_h)
#define octave_file_ops_h 1

#include "octave-config.h"

#include <string>

#include <sys/types.h>

#include "str-vec.h"

namespace octave
{
  namespace sys
  {
    namespace file_ops
    {
      typedef std::string (*tilde_expansion_hook) (const std::string&);

      // If non-null, this contains the address of a function that the
      // application wants called before trying the standard tilde
      // expansions.  The function is called with the text sans tilde, and
      // returns a malloc()'ed string which is the expansion, or a NULL
      // pointer if the expansion fails.

      extern tilde_expansion_hook tilde_expansion_preexpansion_hook;

      // If non-null, this contains the address of a function to call if the
      // standard meaning for expanding a tilde fails.  The function is
      // called with the text (sans tilde, as in "foo"), and returns a
      // malloc()'ed string which is the expansion, or a NULL pointer if
      // there is no expansion.

      extern tilde_expansion_hook tilde_expansion_failure_hook;

      // When non-null, this is a NULL terminated array of strings which are
      // duplicates for a tilde prefix.  Bash uses this to expand '=~' and
      // ':~'.

      extern string_vector tilde_additional_prefixes;

      // When non-null, this is a NULL terminated array of strings which
      // match the end of a username, instead of just "/".  Bash sets this
      // to ':' and '=~'.

      extern string_vector tilde_additional_suffixes;

      // Find the start of a tilde expansion in S, and return the index
      // of the tilde which starts the expansion.  Place the length of the
      // text which identified this tilde starter in LEN, excluding the
      // tilde itself.

      char dev_sep_char (void);

      bool is_dev_sep (char c);

      char dir_sep_char (void);

      std::string dir_sep_str (void);

      std::string dir_sep_chars (void);

      bool is_dir_sep (char c);

      // If NAME has a leading ~ or ~user, Unix-style, expand it to the
      // user's home directory.  If no ~, or no <pwd.h>, just return NAME.

      std::string tilde_expand (const std::string&);

      // A vector version of the above.

      string_vector tilde_expand (const string_vector&);

      std::string concat (const std::string&, const std::string&);

      // Return the directory part of a filename or an empty string if
      // there is no directory component.  Does not check to see
      // whether the file exists or is a directory.

      std::string dirname (const std::string& path);

      // Return the tail member of a filename.

      std::string tail (const std::string& path);

      // convert path from UNIX type separators to whatever is the system separators

      std::string native_separator_path (const std::string& path);
    }

    extern OCTAVE_API int
    mkdir (const std::string&, mode_t);

    extern OCTAVE_API int
    mkdir (const std::string&, mode_t, std::string&);

    extern OCTAVE_API int
    mkfifo (const std::string&, mode_t);

    extern OCTAVE_API int
    mkfifo (const std::string&, mode_t, std::string&);

    extern OCTAVE_API int
    link (const std::string&, const std::string&);

    extern OCTAVE_API int
    link (const std::string&, const std::string&, std::string&);

    extern OCTAVE_API int
    symlink (const std::string&, const std::string&);

    extern OCTAVE_API int
    symlink (const std::string&, const std::string&, std::string&);

    extern OCTAVE_API int
    readlink (const std::string&, std::string&);

    extern OCTAVE_API int
    readlink (const std::string&, std::string&, std::string&);

    extern OCTAVE_API int
    rename (const std::string&, const std::string&);

    extern OCTAVE_API int
    rename (const std::string&, const std::string&, std::string&);

    extern OCTAVE_API int
    rmdir (const std::string&);

    extern OCTAVE_API int
    rmdir (const std::string&, std::string&);

    extern OCTAVE_API int
    recursive_rmdir (const std::string&);

    extern OCTAVE_API int
    recursive_rmdir (const std::string&, std::string&);

    extern OCTAVE_API int
    umask (mode_t);

    extern OCTAVE_API int
    unlink (const std::string&);

    extern OCTAVE_API int
    unlink (const std::string&, std::string&);

    extern OCTAVE_API std::string
    tempnam (const std::string&, const std::string&);

    extern OCTAVE_API std::string
    tempnam (const std::string&, const std::string&, std::string&);

    extern OCTAVE_API std::string
    canonicalize_file_name (const std::string&);

    extern OCTAVE_API std::string
    canonicalize_file_name (const std::string&, std::string&);
  }
}

#if defined (OCTAVE_USE_DEPRECATED_FUNCTIONS)

OCTAVE_DEPRECATED (4.2, "use 'octave::sys::file_ops' instead")
  typedef octave::sys::file_ops file_ops;

OCTAVE_DEPRECATED (4.2, "use 'octave::sys::mkdir' instead")
inline int
octave_mkdir (const std::string& nm, mode_t md)
{
  return octave::sys::mkdir (nm, md);
}

OCTAVE_DEPRECATED (4.2, "use 'octave::sys::mkdir' instead")
inline int
octave_mkdir (const std::string& nm, mode_t md, std::string& msg)
{
  return octave::sys::mkdir (nm, md, msg);
}

OCTAVE_DEPRECATED (4.2, "use 'octave::sys::mkfifo' instead")
inline int
octave_mkfifo (const std::string& nm, mode_t md)
{
  return octave::sys::mkfifo (nm, md);
}

OCTAVE_DEPRECATED (4.2, "use 'octave::sys::mkfifo' instead")
inline int
octave_mkfifo (const std::string& nm, mode_t md, std::string& msg)
{
  return octave::sys::mkfifo (nm, md, msg);
}

OCTAVE_DEPRECATED (4.2, "use 'octave::sys::link' instead")
inline int
octave_link (const std::string& old_name, const std::string& new_name)
{
  return octave::sys::link (old_name, new_name);
}

OCTAVE_DEPRECATED (4.2, "use 'octave::sys::link' instead")
inline int
octave_link (const std::string& old_name, const std::string& new_name,
             std::string& msg)
{
  return octave::sys::link (old_name, new_name, msg);
}

OCTAVE_DEPRECATED (4.2, "use 'octave::sys::symlink' instead")
inline int
octave_symlink (const std::string& old_name, const std::string& new_name)
{
  return octave::sys::symlink (old_name, new_name);
}

OCTAVE_DEPRECATED (4.2, "use 'octave::sys::symlink' instead")
inline int
octave_symlink (const std::string& old_name, const std::string& new_name,
                std::string& msg)
{
  return octave::sys::symlink (old_name, new_name, msg);
}

OCTAVE_DEPRECATED (4.2, "use 'octave::sys::readlink' instead")
inline int
octave_readlink (const std::string& path, std::string& result)
{
  return octave::sys::readlink (path, result);
}

OCTAVE_DEPRECATED (4.2, "use 'octave::sys::readlink' instead")
inline int
octave_readlink (const std::string& path, std::string& result, std::string& msg)
{
  return octave::sys::readlink (path, result, msg);
}

OCTAVE_DEPRECATED (4.2, "use 'octave::sys::rename' instead")
inline int
octave_rename (const std::string& from, const std::string& to)
{
  return octave::sys::rename (from, to);
}

OCTAVE_DEPRECATED (4.2, "use 'octave::sys::rename' instead")
inline int
octave_rename (const std::string& from, const std::string& to, std::string& msg)
{
  return octave::sys::rename (from, to, msg);
}

OCTAVE_DEPRECATED (4.2, "use 'octave::sys::rmdir' instead")
inline int
octave_rmdir (const std::string& nm)
{
  return octave::sys::rmdir (nm);
}

OCTAVE_DEPRECATED (4.2, "use 'octave::sys::rmdir' instead")
inline int
octave_rmdir (const std::string& nm, std::string& msg)
{
  return octave::sys::rmdir (nm, msg);
}

OCTAVE_DEPRECATED (4.2, "use 'octave::sys::recursive_rmdir' instead")
inline int
octave_recursive_rmdir (const std::string& nm)
{
  return octave::sys::recursive_rmdir (nm);
}

OCTAVE_DEPRECATED (4.2, "use 'octave::sys::recursive_rmdir' instead")
inline int
octave_recursive_rmdir (const std::string& nm, std::string& msg)
{
  return octave::sys::recursive_rmdir (nm, msg);
}

OCTAVE_DEPRECATED (4.2, "use 'octave::sys::umask' instead")
inline int
octave_umask (mode_t md)
{
  return octave::sys::umask (md);
}

OCTAVE_DEPRECATED (4.2, "use 'octave::sys::unlink' instead")
inline int
octave_unlink (const std::string& nm)
{
  return octave::sys::unlink (nm);
}

OCTAVE_DEPRECATED (4.2, "use 'octave::sys::unlink' instead")
inline int
octave_unlink (const std::string& nm, std::string& msg)
{
  return octave::sys::unlink (nm, msg);
}

OCTAVE_DEPRECATED (4.2, "use 'octave::sys::tempnam' instead")
inline std::string
octave_tempnam (const std::string& dir, const std::string& pfx)
{
  return octave::sys::tempnam (dir, pfx);
}

OCTAVE_DEPRECATED (4.2, "use 'octave::sys::tempnam' instead")
inline std::string
octave_tempnam (const std::string& dir, const std::string& pfx,
                std::string& msg)
{
  return octave::sys::tempnam (dir, pfx, msg);
}

OCTAVE_DEPRECATED (4.2, "use 'octave::sys::canonicalize_file_name' instead")
inline std::string
octave_canonicalize_file_name (const std::string& nm)
{
  return octave::sys::canonicalize_file_name (nm);
}

OCTAVE_DEPRECATED (4.2, "use 'octave::sys::canonicalize_file_name' instead")
inline std::string
octave_canonicalize_file_name (const std::string& nm, std::string& msg)
{
  return octave::sys::canonicalize_file_name (nm, msg);
}

#endif

#endif
