/*

Copyright (C) 1996, 1997 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstdlib>

#include <iostream>
#include <string>

#ifdef HAVE_UNISTD_H
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <unistd.h>
#endif

#include "oct-env.h"
#include "file-stat.h"
#include "pathsearch.h"
#include "str-vec.h"

#include <defaults.h>
#include "defun.h"
#include "error.h"
#include "file-ops.h"
#include "gripes.h"
#include "help.h"
#include "input.h"
#include "oct-obj.h"
#include "ov.h"
#include "parse.h"
#include "toplev.h"
#include "unwind-prot.h"
#include "variables.h"
#include <version.h>

std::string Voctave_home;

std::string Vbin_dir;
std::string Vinfo_dir;
std::string Vdata_dir;
std::string Vlibexec_dir;
std::string Varch_lib_dir;
std::string Vlocal_arch_lib_dir;
std::string Vlocal_ver_arch_lib_dir;
std::string Vfcn_file_dir;
std::string Voct_file_dir;

// The default path that will be searched for programs that we
// execute (in addition to the user-specified --exec-path).
static std::string Vdefault_exec_path;

// The path that will be searched for programs that we execute.
// (--exec-path path)
std::string Vexec_path;

// Load path specified on command line.
// (--path path; -p path)
static std::string Vload_path;

// The default load path with OCTAVE_HOME appropriately substituted.
static std::string Vdefault_load_path;

// And the cached directory path corresponding to Vload_path.
dir_path Vload_path_dir_path;

// Name of the editor to be invoked by the edit_history command.
std::string Veditor;

std::string Vimagepath;

std::string Vlocal_site_defaults_file;
std::string Vsite_defaults_file;

// Name of the FFTW wisdom program.
std::string Vfftw_wisdom_prog;

// Each element of A and B should be directory names.  For each
// element of A not in the list B, execute SCRIPT_FILE in that
// directory if it exists.

static void
maybe_add_or_del_packages (const string_vector& a,
			   const string_vector& b,
			   const std::string& script_file)
{
  if (! octave_interpreter_ready)
    return;

  unwind_protect::begin_frame ("maybe_add_or_del_packages");

  unwind_protect_bool (input_from_startup_file);

  input_from_startup_file = true;

  octave_idx_type a_len = a.length ();
  octave_idx_type b_len = b.length ();

  for (octave_idx_type i = 0; i < a_len; i++)
    {
      std::string a_dir = a[i];

      bool found = false;

      for (octave_idx_type j = 0; j < b_len; j++)
	{
	  if (b[j] == a_dir)
	    {
	      found = true;
	      break;
	    }
	}

      if (! found)
	{
	  std::string file = a_dir + file_ops::dir_sep_str + script_file;

	  file_stat fs = file_stat (file);

	  if (fs.exists ())
	    source_file (file);

	  if (error_state)
	    return;
	}
    }

  unwind_protect::run_frame ("maybe_add_or_del_packages");
}

static void
update_load_path_dir_path (void)
{
  string_vector old_dirs = Vload_path_dir_path.all_directories ();

  Vload_path_dir_path = dir_path (Vload_path, Vdefault_load_path);

  string_vector new_dirs = Vload_path_dir_path.all_directories ();

  maybe_add_or_del_packages (old_dirs, new_dirs, "PKG_DEL");

  if (! error_state)
    maybe_add_or_del_packages (new_dirs, old_dirs, "PKG_ADD");
}

void
execute_default_pkg_add_files (void)
{
  string_vector old_dirs;
  string_vector new_dirs = Vload_path_dir_path.all_directories ();
  
  maybe_add_or_del_packages (new_dirs, old_dirs, "PKG_ADD");
}

static std::string
subst_octave_home (const std::string& s)
{
  std::string retval;

  std::string prefix = OCTAVE_PREFIX;

  retval = s;

  if (Voctave_home != prefix)
    {
      octave_idx_type len = prefix.length ();
      size_t start = 0;
      while ((start = retval.find (prefix, start)) != NPOS)
	{
	  retval.replace (start, len, Voctave_home);
	  start += len;
	}
    }

  return retval;
}

static void
set_octave_home (void)
{
  std::string oh = octave_env::getenv ("OCTAVE_HOME");

  Voctave_home = oh.empty () ? std::string (OCTAVE_PREFIX) : oh;
}

static void
set_default_info_dir (void)
{
  Vinfo_dir = subst_octave_home (OCTAVE_INFODIR);
}

static void
set_default_data_dir (void)
{
  Vdata_dir = subst_octave_home (OCTAVE_DATADIR);
}

static void
set_default_libexec_dir (void)
{
  Vlibexec_dir = subst_octave_home (OCTAVE_LIBEXECDIR);
}

static void
set_default_arch_lib_dir (void)
{
  Varch_lib_dir = subst_octave_home (OCTAVE_ARCHLIBDIR);
}

static void
set_default_local_arch_lib_dir (void)
{
  Vlocal_arch_lib_dir = subst_octave_home (OCTAVE_LOCALARCHLIBDIR);
}

static void
set_default_local_ver_arch_lib_dir (void)
{
  Vlocal_ver_arch_lib_dir = subst_octave_home (OCTAVE_LOCALVERARCHLIBDIR);
}

static void
set_default_fcn_file_dir (void)
{
  Vfcn_file_dir = subst_octave_home (OCTAVE_FCNFILEDIR);
}

static void
set_default_oct_file_dir (void)
{
  Voct_file_dir = subst_octave_home (OCTAVE_OCTFILEDIR);
}

static void
set_default_bin_dir (void)
{
  Vbin_dir = subst_octave_home (OCTAVE_BINDIR);
}

static void
set_default_default_exec_path (void)
{
  Vdefault_exec_path
    = Vlocal_ver_arch_lib_dir + std::string (SEPCHAR_STR)
    + Vlocal_arch_lib_dir + std::string (SEPCHAR_STR)
    + Varch_lib_dir + std::string (SEPCHAR_STR)
    + Vbin_dir;
}

static void
set_default_exec_path (void)
{
  std::string octave_exec_path = octave_env::getenv ("OCTAVE_EXEC_PATH");

  if (octave_exec_path.empty ())
    {
      std::string shell_path = octave_env::getenv ("PATH");

      if (! shell_path.empty ())
	{
	  Vexec_path = std::string (SEPCHAR_STR);
	  Vexec_path.append (shell_path);
	}
    }
  else
    Vexec_path = std::string (octave_exec_path);
}

static void
set_default_path (void)
{
  Vdefault_load_path = subst_octave_home (OCTAVE_FCNFILEPATH);

  std::string oct_path = octave_env::getenv ("OCTAVE_PATH");

  Vload_path = oct_path.empty () ? std::string (SEPCHAR_STR) : oct_path;

  update_load_path_dir_path ();
}

static void
set_default_info_file (void)
{
  std::string std_info_file = subst_octave_home (OCTAVE_INFOFILE);

  std::string oct_info_file = octave_env::getenv ("OCTAVE_INFO_FILE");

  Vinfo_file = oct_info_file.empty () ? std_info_file : oct_info_file;
}

static void
set_default_info_prog (void)
{
  std::string oct_info_prog = octave_env::getenv ("OCTAVE_INFO_PROGRAM");

  if (oct_info_prog.empty ())
    Vinfo_prog = "info";
  else
    Vinfo_prog = std::string (oct_info_prog);
}

static void
set_default_fftw_wisdom_prog (void)
{
  std::string oct_wisdom_prog = octave_env::getenv ("OCTAVE_FFTW_WISDOM_PROGRAM");

  if (oct_wisdom_prog.empty ())
    Vfftw_wisdom_prog = "fftw-wisdom";
  else
    Vfftw_wisdom_prog = std::string (oct_wisdom_prog);
}

static void
set_default_editor (void)
{
  Veditor = "emacs";

  std::string env_editor = octave_env::getenv ("EDITOR");

  if (! env_editor.empty ())
    Veditor = env_editor;
}

static void
set_local_site_defaults_file (void)
{
  Vlocal_site_defaults_file = subst_octave_home (OCTAVE_LOCALSTARTUPFILEDIR);
  Vlocal_site_defaults_file.append ("/octaverc");
}

static void
set_site_defaults_file (void)
{
  Vsite_defaults_file = subst_octave_home (OCTAVE_STARTUPFILEDIR);
  Vsite_defaults_file.append ("/octaverc");
}

std::string
maybe_add_default_load_path (const std::string& pathstring)
{
  std::string retval;

  if (! pathstring.empty ())
    {
      if (pathstring[0] == SEPCHAR)
	{
	  retval = Vdefault_load_path;
	  retval.append (pathstring);
	}
      else
	retval = pathstring;

      if (pathstring[pathstring.length () - 1] == SEPCHAR)
	retval.append (Vdefault_load_path);

      size_t pos = 0;
      do
	{
	  pos = retval.find (std::string (SEPCHAR_STR) + 
			     std::string (SEPCHAR_STR));

	  if (pos != NPOS)
	    retval.insert (pos+1, Vdefault_load_path);
	}
      while (pos != NPOS);
    }

  return retval;
}

void
install_defaults (void)
{
  // OCTAVE_HOME must be set first!

  set_octave_home ();

  set_default_info_dir ();

  set_default_data_dir ();

  set_default_libexec_dir ();

  set_default_arch_lib_dir ();

  set_default_local_arch_lib_dir ();

  set_default_local_ver_arch_lib_dir ();

  set_default_fcn_file_dir ();

  set_default_oct_file_dir ();

  set_default_bin_dir ();

  set_default_default_exec_path ();

  set_default_exec_path ();

  set_default_path ();

  set_default_info_file ();

  set_default_info_prog ();

  set_default_fftw_wisdom_prog ();

  set_default_editor ();

  set_local_site_defaults_file ();

  set_site_defaults_file ();
}

DEFUN (rehash, , ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} rehash ()\n\
Reinitialize Octave's @code{LOADPATH} directory cache.\n\
@end deftypefn")
{
  octave_value_list retval;

  Vload_path_dir_path.rehash ();

  return retval;
}

static int
editor (void)
{
  int status = 0;

  std::string s = builtin_string_variable ("EDITOR");

  if (s.empty ())
    {
      gripe_invalid_value_specified ("EDITOR");
      status = -1;
    }
  else
    Veditor = s;

  return status;
}

static int
exec_path (void)
{
  int status = 0;

  std::string s = builtin_string_variable ("EXEC_PATH");

  if (s.empty ())
    {
      gripe_invalid_value_specified ("EXEC_PATH");
      status = -1;
    }
  else
    {
      Vexec_path = s;

      std::string path;

      int eplen = Vexec_path.length ();

      if (eplen > 0)
	{
	  bool prepend = (Vexec_path[0] == ':');
	  bool append = (eplen > 1 && Vexec_path[eplen-1] == ':');

	  if (prepend)
	    {
	      path = Vdefault_exec_path + Vexec_path;
	    }
	  else
	    {
	      path = Vexec_path;

	      if (append)
		path.append (Vdefault_exec_path);
	    }
	}
      else
	path = Vdefault_exec_path;

      octave_env::putenv ("PATH", path);
    }

  return status;
}

static int
fftw_wisdom_prog (void)
{
  int status = 0;

  std::string s = builtin_string_variable ("FFTW_WISDOM_PROGRAM");

  if (s.empty ())
    {
      gripe_invalid_value_specified ("FFTW_WISDOM_PROGRAM");
      status = -1;
    }
  else
    Vfftw_wisdom_prog = s;

  return status;
}

static int
default_exec_path (void)
{
  int status = 0;

  std::string s = builtin_string_variable ("DEFAULT_EXEC_PATH");

  if (s.empty ())
    {
      gripe_invalid_value_specified ("DEFAULT_EXEC_PATH");
      status = -1;
    }
  else
    {
      Vdefault_exec_path = s;

      // Now also update PATH in environment.
      exec_path ();
    }

  return status;
}

static int
imagepath (void)
{
  int status = 0;

  std::string s = builtin_string_variable ("IMAGEPATH");

  if (s.empty ())
    {
      gripe_invalid_value_specified ("IMAGEPATH");
      status = -1;
    }
  else
    Vimagepath = s;

  return status;
}

static int
loadpath (void)
{
  int status = 0;

  std::string s = builtin_string_variable ("LOADPATH");

  if (s.empty ())
    {
      gripe_invalid_value_specified ("LOADPATH");
      status = -1;
    }
  else if (Vload_path != s)
    {
      // I'm not sure whether this causes more problems that it
      // solves...
      //      if (! (s[0] == ':' || s[s.length () - 1] == ':'
      //	     || s.find (std::string (SEPCHAR_STR) + 
      //                        std::string (SEPCHAR_STR)) != NPOS))
      //	warning ("LOADPATH will ignore default load path");

      Vload_path = s;

      // By resetting the last prompt time variable, we will force
      // checks for out of date symbols even if the change to LOADPATH
      // and subsequent function calls happen between prompts.

      // XXX FIXME XXX -- maybe we should rename
      // Vlast_prompt_time_stamp since the new usage doesn't really
      // fit with the current name?

      Vlast_prompt_time.stamp ();

      update_load_path_dir_path ();
    }

  return status;
}

static int
default_load_path (void)
{
  int status = 0;

  std::string s = builtin_string_variable ("DEFAULT_LOADPATH");

  if (s.empty ())
    {
      gripe_invalid_value_specified ("DEFAULT_LOADPATH");
      status = -1;
    }
  else
    {
      Vdefault_load_path = s;

      update_load_path_dir_path ();
    }

  return status;
}

void
symbols_of_defaults (void)
{
  DEFVAR (EDITOR, Veditor, editor,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} EDITOR\n\
A string naming the editor to use with the @code{edit_history} command.\n\
If the environment variable @code{EDITOR} is set when Octave starts, its\n\
value is used as the default.  Otherwise, @code{EDITOR} is set to\n\
@code{\"emacs\"}.\n\
@end defvr");

  DEFVAR (FFTW_WISDOM_PROGRAM, Vfftw_wisdom_prog, fftw_wisdom_prog,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} FFTW_WISDOM_PROGRAM\n\
A string naming the FFTW wisdom program to use to create wisdom data\n\
to accelerate Fourier transforms. If the environment variable\n\
@code{OCTAVE_WISDOM_PROGRAM} is set when Octave starts, its value is used\n\
as the default. Otherwise, @code{WISDOM_PROGRAM} is set to\n\
@code{\"fftw-wisdom\"}.\n\
@end defvr");
  
  DEFVAR (EXEC_PATH, Vexec_path, exec_path,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} EXEC_PATH\n\
The variable @code{EXEC_PATH} is a colon separated list of directories\n\
to search when executing external programs.  Its initial value is taken from\n\
the environment variable @code{OCTAVE_EXEC_PATH} (if it exists) or\n\
@code{PATH}, but that value can be overridden by the command line\n\
argument @code{--exec-path PATH}, or by setting the value of\n\
@code{EXEC_PATH} in a startup script.  If the value of @code{EXEC_PATH}\n\
begins (ends) with a colon, the directories\n\
\n\
@example\n\
@group\n\
@var{octave-home}/libexec/octave/site/exec/@var{arch}\n\
@var{octave-home}/libexec/octave/@var{version}/exec/@var{arch}\n\
@end group\n\
@end example\n\
\n\
@noindent\n\
are prepended (appended) to @code{EXEC_PATH}, where @var{octave-home}\n\
is the top-level directory where all of Octave is installed\n\
(the default value is @file{@value{OCTAVEHOME}}).  If you don't specify\n\
a value for @code{EXEC_PATH} explicitly, these special directories are\n\
prepended to your shell path.\n\
@end defvr");

  DEFVAR (DEFAULT_EXEC_PATH, Vdefault_exec_path, default_exec_path,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} DEFAULT_EXEC_PATH\n\
A colon separated list of directories in which to search when executing\n\
external programs.  The value of this variable is automatically\n\
substituted for leading, trailing, or doubled colons that appear in the\n\
built-in variable @code{EXEC_PATH}.\n\
@end defvr");

  DEFVAR (LOADPATH, Vload_path, loadpath,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} LOADPATH\n\
A colon separated list of directories in which to search for function\n\
files.  @xref{Functions and Scripts}.  The value of @code{LOADPATH}\n\
overrides the environment variable @code{OCTAVE_PATH}.  @xref{Installation}.\n\
\n\
@code{LOADPATH} is now handled in the same way as @TeX{} handles\n\
@code{TEXINPUTS}.  Leading, trailing, or doubled colons that appear in\n\
@code{LOADPATH} are replaced by the value of @code{DEFAULT_LOADPATH}.\n\
The default value of @code{LOADPATH} is @code{\"\n"
SEPCHAR_STR
"\"}, which tells Octave to search in the directories specified by\n\
@code{DEFAULT_LOADPATH}.\n\
\n\
In addition, if any path element ends in @samp{//}, that directory and\n\
all subdirectories it contains are searched recursively for function\n\
files.  This can result in a slight delay as Octave caches the lists of\n\
files found in the @code{LOADPATH} the first time Octave searches for a\n\
function.  After that, searching is usually much faster because Octave\n\
normally only needs to search its internal cache for files.\n\
\n\
To improve performance of recursive directory searching, it is best for\n\
each directory that is to be searched recursively to contain\n\
@emph{either} additional subdirectories @emph{or} function files, but\n\
not a mixture of both.\n\
\n\
@xref{Organization of Functions}, for a description of the function file\n\
directories that are distributed with Octave.\n\
@end defvr");

  DEFVAR (DEFAULT_LOADPATH, Vdefault_load_path, default_load_path,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} DEFAULT_LOADPATH\n\
A colon separated list of directories in which to search for function\n\
files.  The value of this variable is automatically substituted for\n\
leading, trailing, or doubled colons that appear in the built-in\n\
variable @code{LOADPATH}.\n\
@end defvr");
  
  DEFVAR (IMAGEPATH, OCTAVE_IMAGEPATH, imagepath,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} IMAGEPATH\n\
A colon separated list of directories in which to search for image\n\
files.\n\
@end defvr");

  DEFCONST (OCTAVE_HOME, Voctave_home,
    "-*- texinfo -*-\n\
@defvr {Built-in Constant} OCTAVE_HOME\n\
The name of the top-level Octave installation directory.\n\
@end defvr");

}

DEFUNX ("OCTAVE_VERSION", FOCTAVE_VERSION, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} OCTAVE_VERSION ()\n\
Return the version number of Octave, as a string.\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 0)
    retval = OCTAVE_VERSION;
  else
    print_usage ("OCTAVE_VERSION");

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
