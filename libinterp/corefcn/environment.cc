////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2017-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <string>

#include "dir-ops.h"
#include "oct-env.h"
#include "file-stat.h"
#include "pathsearch.h"
#include "str-vec.h"

#include "defaults.h"
#include "defun.h"
#include "environment.h"
#include "interpreter.h"
#include "variables.h"

OCTAVE_BEGIN_NAMESPACE(octave)

static void append_to_shell_path (const std::string& exec_path)
{
  // FIXME: should there be a way to remove a previous setting from
  // PATH?

  if (exec_path.empty ())
    return;

  // FIXME: should we really be modifying PATH in the environment?

  std::string shell_path = sys::env::getenv ("PATH");

  if (shell_path.empty ())
    sys::env::putenv ("PATH", exec_path);
  else
    {
      // If PATH doesn't already have exec_path, append it.
      // FIXME: should we search for the elements individually, and
      // only append those that are missing?

      std::string path_sep = directory_path::path_sep_str ();

      if (shell_path.find (exec_path) == std::string::npos)
        sys::env::putenv ("PATH", shell_path + path_sep + exec_path);
    }
}

octave_value
environment::editor (const octave_value_list& args, int nargout)
{
  return set_internal_variable (m_editor, args, nargout, "EDITOR", false);
}


octave_value
environment::exec_path (const octave_value_list& args, int nargout)
{
  octave_value retval
    = set_internal_variable (m_exec_path, args, nargout, "EXEC_PATH", false);

  append_to_shell_path (m_exec_path);

  return retval;
}

std::string environment::exec_path (const std::string& path)
{
  std::string old_val = set (m_exec_path, path);

  append_to_shell_path (m_exec_path);

  return old_val;
}

octave_value
environment::image_path (const octave_value_list& args, int nargout)
{
  return set_internal_variable (m_image_path, args, nargout, "IMAGE_PATH",
                                false);
}

std::string environment::init_editor (void)
{
  std::string retval = "emacs";

  std::string env_editor = sys::env::getenv ("EDITOR");

  if (! env_editor.empty ())
    retval = env_editor;

  return retval;
}

std::string environment::init_exec_path (void)
{
  std::string exec_path = sys::env::getenv ("OCTAVE_EXEC_PATH");

  std::string path_sep = directory_path::path_sep_str ();

  if (exec_path.empty ())
    exec_path = (config::local_ver_arch_lib_dir () + path_sep
                 + config::local_api_arch_lib_dir () + path_sep
                 + config::local_arch_lib_dir () + path_sep
                 + config::arch_lib_dir () + path_sep
                 + config::bin_dir ());

  append_to_shell_path (exec_path);

  return exec_path;
}

std::string environment::init_image_path (void)
{
  std::string image_path = ".";

  std::string path_sep = directory_path::path_sep_str ();

  std::string env_path = sys::env::getenv ("OCTAVE_IMAGE_PATH");

  if (! env_path.empty ())
    image_path += path_sep + env_path;

  std::string gen_path = genpath (config::image_dir (), "");

  if (! gen_path.empty ())
    image_path += path_sep + gen_path;

  return image_path;
}

DEFMETHOD (EDITOR, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} EDITOR ()
@deftypefnx {} {@var{old_val} =} EDITOR (@var{new_val})
@deftypefnx {} {@var{old_val} =} EDITOR (@var{new_val}, "local")
Query or set the internal variable that specifies the default text editor.

The default value is taken from the environment variable @w{@env{EDITOR}}
when Octave starts.  If the environment variable is not initialized,
@w{@env{EDITOR}} will be set to @qcode{"emacs"}.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.

@seealso{edit, edit_history}
@end deftypefn */)
{
  environment& env = interp.get_environment ();

  return env.editor (args, nargout);
}

/*
%!test
%! orig_val = EDITOR ();
%! old_val = EDITOR ("X");
%! assert (orig_val, old_val);
%! assert (EDITOR (), "X");
%! EDITOR (orig_val);
%! assert (EDITOR (), orig_val);

%!error EDITOR (1, 2)
*/

DEFMETHOD (EXEC_PATH, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} EXEC_PATH ()
@deftypefnx {} {@var{old_val} =} EXEC_PATH (@var{new_val})
@deftypefnx {} {@var{old_val} =} EXEC_PATH (@var{new_val}, "local")
Query or set the internal variable that specifies a colon separated
list of directories to append to the shell PATH when executing external
programs.

The initial value of is taken from the environment variable
@w{@env{OCTAVE_EXEC_PATH}}, but that value can be overridden by the command
line argument @option{--exec-path PATH}.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.

@seealso{IMAGE_PATH, OCTAVE_HOME, OCTAVE_EXEC_HOME}
@end deftypefn */)
{
  environment& env = interp.get_environment ();

  return env.exec_path (args, nargout);
}

/*
%!test
%! orig_val = EXEC_PATH ();
%! old_val = EXEC_PATH ("X");
%! assert (orig_val, old_val);
%! assert (EXEC_PATH (), "X");
%! EXEC_PATH (orig_val);
%! assert (EXEC_PATH (), orig_val);

%!error EXEC_PATH (1, 2)
*/

DEFMETHOD (IMAGE_PATH, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} IMAGE_PATH ()
@deftypefnx {} {@var{old_val} =} IMAGE_PATH (@var{new_val})
@deftypefnx {} {@var{old_val} =} IMAGE_PATH (@var{new_val}, "local")
Query or set the internal variable that specifies a colon separated
list of directories in which to search for image files.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.

@seealso{EXEC_PATH, OCTAVE_HOME, OCTAVE_EXEC_HOME}
@end deftypefn */)
{
  environment& env = interp.get_environment ();

  return env.image_path (args, nargout);
}

/*
%!test
%! orig_val = IMAGE_PATH ();
%! old_val = IMAGE_PATH ("X");
%! assert (orig_val, old_val);
%! assert (IMAGE_PATH (), "X");
%! IMAGE_PATH (orig_val);
%! assert (IMAGE_PATH (), orig_val);

%!error IMAGE_PATH (1, 2)
*/

OCTAVE_END_NAMESPACE(octave)
