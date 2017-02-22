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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <string>

#include "file-ops.h"
#include "lo-error.h"
#include "lo-sysdep.h"
#include "unistd-wrappers.h"

namespace octave
{
  namespace sys
  {
    std::string
    getcwd (void)
    {
      std::string retval;

      // Using octave_getcwd_wrapper ensures that we have a getcwd that
      // will allocate a buffer as large as necessary if buf and size are
      // both 0.

      char *tmp = octave_getcwd_wrapper (0, 0);

      if (! tmp)
        (*current_liboctave_error_handler) ("unable to find current directory");

      retval = tmp;
      free (tmp);

      return retval;
    }

    int
    chdir (const std::string& path_arg)
    {
      std::string path = octave::sys::file_ops::tilde_expand (path_arg);

#if defined (OCTAVE_USE_WINDOWS_API)
      if (path.length () == 2 && path[1] == ':')
        path += "\\";
#endif

      return octave_chdir_wrapper (path.c_str ());
    }
  }
}
