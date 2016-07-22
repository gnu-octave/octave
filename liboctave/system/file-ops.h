/*

Copyright (C) 1996-2015 John W. Eaton

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
    struct
    OCTAVE_API
    file_ops
    {
    protected:

      // Use a singleton class for dir_sep data members instead of just
      // making them static members of the file_ops class so that we
      // can ensure proper initialization.

      file_ops (char dev_sep_char_arg = 0, char dir_sep_char_arg = 0,
                const std::string& dir_sep_str_arg = std::string ("/"),
                const std::string& dir_sep_chars_arg = std::string ("/"))
        : m_dev_sep_char (dev_sep_char_arg),
          m_dir_sep_char (dir_sep_char_arg),
          m_dir_sep_str (dir_sep_str_arg),
          m_dir_sep_chars (dir_sep_chars_arg) { }

    public:

      typedef std::string (*tilde_expansion_hook) (const std::string&);

      static tilde_expansion_hook tilde_expansion_preexpansion_hook;

      static tilde_expansion_hook tilde_expansion_failure_hook;

      static string_vector tilde_additional_prefixes;

      static string_vector tilde_additional_suffixes;

      static char dev_sep_char (void)
      {
        return instance_ok () ? instance->m_dev_sep_char : 0;
      }

      static bool is_dev_sep (char c);

      static char dir_sep_char (void)
      {
        return instance_ok () ? instance->m_dir_sep_char : 0;
      }

      static std::string dir_sep_str (void)
      {
        return instance_ok () ? instance->m_dir_sep_str : "";
      }

      static std::string dir_sep_chars (void)
      {
        return instance_ok () ? instance->m_dir_sep_chars : "";
      }

      static bool is_dir_sep (char c)
      {
        std::string tmp = dir_sep_chars ();
        return tmp.find (c) != std::string::npos;
      }

      static std::string tilde_expand (const std::string&);

      static string_vector tilde_expand (const string_vector&);

      static std::string concat (const std::string&, const std::string&);

      // Return the directory part of a filename or an empty string if
      // there is no directory component.  Does not check to see
      // whether the file exists or is a directory.
      static std::string dirname (const std::string& path)
      {
        size_t ipos = path.find_last_of (dir_sep_chars ());

        return (ipos != std::string::npos) ? path.substr (0, ipos) : "";
      }

      // Return the tail member of a filename.
      static std::string tail (const std::string& path)
      {
        size_t ipos = path.find_last_of (dir_sep_chars ());

        if (ipos != std::string::npos)
          ipos++;
        else
          ipos = 0;

        return path.substr (ipos);
      }

      // convert path from UNIX type separators to whatever is the system separators
      static std::string native_separator_path (const std::string& path);

    private:

      static file_ops *instance;

      static void cleanup_instance (void) { delete instance; instance = 0; }

      // No copying!

      file_ops (const file_ops&);

      file_ops& operator = (const file_ops&);

      static bool instance_ok (void);

      char m_dev_sep_char;

      char m_dir_sep_char;
      std::string m_dir_sep_str;
      std::string m_dir_sep_chars;
    };

    // We don't have these in the file_ops class with their simple names
    // (i.e., mkdir instead of octave_mdir) because function names in
    // standard headers may be #defined.

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

OCTAVE_DEPRECATED ("use 'octave::sys::file_ops' instead")
  typedef octave::sys::file_ops file_ops;

OCTAVE_DEPRECATED ("use 'octave::sys::mkdir' instead")
inline int
octave_mkdir (const std::string& nm, mode_t md)
{
  return octave::sys::mkdir (nm, md);
}

OCTAVE_DEPRECATED ("use 'octave::sys::mkdir' instead")
inline int
octave_mkdir (const std::string& nm, mode_t md, std::string& msg)
{
  return octave::sys::mkdir (nm, md, msg);
}

OCTAVE_DEPRECATED ("use 'octave::sys::mkfifo' instead")
inline int
octave_mkfifo (const std::string& nm, mode_t md)
{
  return octave::sys::mkfifo (nm, md);
}

OCTAVE_DEPRECATED ("use 'octave::sys::mkfifo' instead")
inline int
octave_mkfifo (const std::string& nm, mode_t md, std::string& msg)
{
  return octave::sys::mkfifo (nm, md, msg);
}

OCTAVE_DEPRECATED ("use 'octave::sys::link' instead")
inline int
octave_link (const std::string& old_name, const std::string& new_name)
{
  return octave::sys::link (old_name, new_name);
}

OCTAVE_DEPRECATED ("use 'octave::sys::link' instead")
inline int
octave_link (const std::string& old_name, const std::string& new_name,
             std::string& msg)
{
  return octave::sys::link (old_name, new_name, msg);
}

OCTAVE_DEPRECATED ("use 'octave::sys::symlink' instead")
inline int
octave_symlink (const std::string& old_name, const std::string& new_name)
{
  return octave::sys::symlink (old_name, new_name);
}

OCTAVE_DEPRECATED ("use 'octave::sys::symlink' instead")
inline int
octave_symlink (const std::string& old_name, const std::string& new_name,
                std::string& msg)
{
  return octave::sys::symlink (old_name, new_name, msg);
}

OCTAVE_DEPRECATED ("use 'octave::sys::readlink' instead")
inline int
octave_readlink (const std::string& path, std::string& result)
{
  return octave::sys::readlink (path, result);
}

OCTAVE_DEPRECATED ("use 'octave::sys::readlink' instead")
inline int
octave_readlink (const std::string& path, std::string& result, std::string& msg)
{
  return octave::sys::readlink (path, result, msg);
}

OCTAVE_DEPRECATED ("use 'octave::sys::rename' instead")
inline int
octave_rename (const std::string& from, const std::string& to)
{
  return octave::sys::rename (from, to);
}

OCTAVE_DEPRECATED ("use 'octave::sys::rename' instead")
inline int
octave_rename (const std::string& from, const std::string& to, std::string& msg)
{
  return octave::sys::rename (from, to, msg);
}

OCTAVE_DEPRECATED ("use 'octave::sys::rmdir' instead")
inline int
octave_rmdir (const std::string& nm)
{
  return octave::sys::rmdir (nm);
}

OCTAVE_DEPRECATED ("use 'octave::sys::rmdir' instead")
inline int
octave_rmdir (const std::string& nm, std::string& msg)
{
  return octave::sys::rmdir (nm, msg);
}

OCTAVE_DEPRECATED ("use 'octave::sys::recursive_rmdir' instead")
inline int
octave_recursive_rmdir (const std::string& nm)
{
  return octave::sys::recursive_rmdir (nm);
}

OCTAVE_DEPRECATED ("use 'octave::sys::recursive_rmdir' instead")
inline int
octave_recursive_rmdir (const std::string& nm, std::string& msg)
{
  return octave::sys::recursive_rmdir (nm, msg);
}

OCTAVE_DEPRECATED ("use 'octave::sys::umask' instead")
inline int
octave_umask (mode_t md)
{
  return octave::sys::umask (md);
}

OCTAVE_DEPRECATED ("use 'octave::sys::unlink' instead")
inline int
octave_unlink (const std::string& nm)
{
  return octave::sys::unlink (nm);
}

OCTAVE_DEPRECATED ("use 'octave::sys::unlink' instead")
inline int
octave_unlink (const std::string& nm, std::string& msg)
{
  return octave::sys::unlink (nm, msg);
}

OCTAVE_DEPRECATED ("use 'octave::sys::tempnam' instead")
inline std::string
octave_tempnam (const std::string& dir, const std::string& pfx)
{
  return octave::sys::tempnam (dir, pfx);
}

OCTAVE_DEPRECATED ("use 'octave::sys::tempnam' instead")
inline std::string
octave_tempnam (const std::string& dir, const std::string& pfx,
                std::string& msg)
{
  return octave::sys::tempnam (dir, pfx, msg);
}

OCTAVE_DEPRECATED ("use 'octave::sys::canonicalize_file_name' instead")
inline std::string
octave_canonicalize_file_name (const std::string& nm)
{
  return octave::sys::canonicalize_file_name (nm);
}

OCTAVE_DEPRECATED ("use 'octave::sys::canonicalize_file_name' instead")
inline std::string
octave_canonicalize_file_name (const std::string& nm, std::string& msg)
{
  return octave::sys::canonicalize_file_name (nm, msg);
}

#endif

#endif
