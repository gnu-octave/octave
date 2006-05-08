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
static std::string VDEFAULT_EXEC_PATH;

// The path that will be searched for programs that we execute.
// (--exec-path path)
static std::string VEXEC_PATH;

// Load path specified on command line.
// (--path path; -p path)
static std::string VLOADPATH;

// The default load path with OCTAVE_HOME appropriately substituted.
static std::string VDEFAULT_LOADPATH;

// And the cached directory path corresponding to Vload_path.
dir_path Vload_path_dir_path;

// Name of the editor to be invoked by the edit_history command.
std::string VEDITOR;

static std::string VIMAGEPATH;

std::string Vlocal_site_defaults_file;
std::string Vsite_defaults_file;

// Name of the FFTW wisdom program.
std::string Vfftw_wisdom_program;

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

  Vload_path_dir_path = dir_path (VLOADPATH, VDEFAULT_LOADPATH);

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
  VDEFAULT_EXEC_PATH
    = Vlocal_ver_arch_lib_dir + dir_path::path_sep_str
    + Vlocal_arch_lib_dir + dir_path::path_sep_str
    + Varch_lib_dir + dir_path::path_sep_str
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
	  VEXEC_PATH = dir_path::path_sep_str;
	  VEXEC_PATH.append (shell_path);
	}
    }
  else
    VEXEC_PATH = std::string (octave_exec_path);
}

static void
set_default_path (void)
{
  VDEFAULT_LOADPATH = subst_octave_home (OCTAVE_FCNFILEPATH);

  std::string oct_path = octave_env::getenv ("OCTAVE_PATH");

  VLOADPATH = oct_path.empty () ? dir_path::path_sep_str : oct_path;

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
    Vinfo_program = "info";
  else
    Vinfo_program = std::string (oct_info_prog);
}

static void
set_default_fftw_wisdom_prog (void)
{
  std::string oct_wisdom_prog = octave_env::getenv ("OCTAVE_FFTW_WISDOM_PROGRAM");

  if (oct_wisdom_prog.empty ())
    Vfftw_wisdom_program = "fftw-wisdom";
  else
    Vfftw_wisdom_program = std::string (oct_wisdom_prog);
}

static void
set_default_editor (void)
{
  VEDITOR = "emacs";

  std::string env_editor = octave_env::getenv ("EDITOR");

  if (! env_editor.empty ())
    VEDITOR = env_editor;
}

static void
set_local_site_defaults_file (void)
{
  std::string lsf = octave_env::getenv ("OCTAVE_LOCAL_SITE_INITFILE");

  if (lsf.empty ())
    {
      Vlocal_site_defaults_file = subst_octave_home (OCTAVE_LOCALSTARTUPFILEDIR);
      Vlocal_site_defaults_file.append ("/octaverc");
    }
  else
    Vlocal_site_defaults_file = lsf;
}

static void
set_site_defaults_file (void)
{
  std::string sf = octave_env::getenv ("OCTAVE_SITE_INITFILE");

  if (sf.empty ())
    {
      Vsite_defaults_file = subst_octave_home (OCTAVE_STARTUPFILEDIR);
      Vsite_defaults_file.append ("/octaverc");
    }
  else
    Vsite_defaults_file = sf;
}

std::string
maybe_add_default_load_path (const std::string& pathstring)
{
  std::string retval;

  if (! pathstring.empty ())
    {
      if (dir_path::is_path_sep (pathstring[0]))
	{
	  retval = VDEFAULT_LOADPATH;
	  retval.append (pathstring);
	}
      else
	retval = pathstring;

      if (dir_path::is_path_sep (pathstring[pathstring.length () - 1]))
	retval.append (VDEFAULT_LOADPATH);

      size_t pos = 0;
      do
	{
	  pos = retval.find (dir_path::path_sep_str + dir_path::path_sep_str);

	  if (pos != NPOS)
	    retval.insert (pos+1, VDEFAULT_LOADPATH);
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

DEFUN (EDITOR, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{val} =} EDITOR ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} EDITOR (@var{new_val})\n\
Query or set the internal variable that specifies the editor to\n\
use with the @code{edit_history} command.  If the environment\n\
variable @code{EDITOR} is set when Octave starts, its\n\
value is used as the default.  Otherwise, @code{EDITOR} is set to\n\
@code{\"emacs\"}.\n\
@seealso{edit_history}\n\
@end deftypefn")
{
  return SET_NONEMPTY_INTERNAL_STRING_VARIABLE (EDITOR);
}

static void
update_exec_path (void)
{
  std::string path;

  int eplen = VEXEC_PATH.length ();

  if (eplen > 0)
    {
      bool prepend = (VEXEC_PATH[0] == ':');
      bool append = (eplen > 1 && VEXEC_PATH[eplen-1] == ':');

      if (prepend)
	{
	  path = VDEFAULT_EXEC_PATH + VEXEC_PATH;
	}
      else
	{
	  path = VEXEC_PATH;

	  if (append)
	    path.append (VDEFAULT_EXEC_PATH);
	}
    }
  else
    path = VDEFAULT_EXEC_PATH;

  octave_env::putenv ("PATH", path);
}

DEFUN (EXEC_PATH, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{val} =} EXEC_PATH ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} EXEC_PATH (@var{new_val})\n\
Query or set the internal variable that specifies a colon separated\n\
list of directories to search when executing external programs.\n\
Its initial value is taken from the environment variable\n\
@code{OCTAVE_EXEC_PATH} (if it exists) or @code{PATH}, but that\n\
value can be overridden by the command line argument\n\
@code{--exec-path PATH}.  Any leading, trailing, or doubled colon in\n\
the value of @code{EXEC_PATH} are replaced by by the value of\n\
@code{DEFAULT_EXEC_PATH}.\n\
@seealso{DEFAULT_EXEC_PATH}\n\
@end deftypefn")
{
  std::string saved_exec_path = VEXEC_PATH;

  octave_value retval = SET_NONEMPTY_INTERNAL_STRING_VARIABLE (EXEC_PATH);

  if (VEXEC_PATH != saved_exec_path)
    update_exec_path ();

  return retval;
}

DEFUN (fftw_wisdom_program, args, nargout,
    "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{val} =} FFTW_WISDOM_PROGRAM ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} FFTW_WISDOM_PROGRAM (@var{new_val})\n\
Query or set the internal variable that specifies the FFTW wisdom\n\
program to use to create wisdom data to accelerate Fourier transforms.\n\
If the environment variable @code{OCTAVE_WISDOM_PROGRAM} is set when\n\
Octave starts, its value is used as the default. Otherwise,\n\
@code{WISDOM_PROGRAM} is set to @code{\"fftw-wisdom\"}.\n\
@end deftypefn")
{
  return SET_NONEMPTY_INTERNAL_STRING_VARIABLE (fftw_wisdom_program);
}

DEFUN (DEFAULT_EXEC_PATH, args, nargout,
    "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{val} =} DEFAULT_EXEC_PATH ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} DEFAULT_EXEC_PATH (@var{new_val})\n\
Query or set the internal variable that specifies a colon separated\n\
list of directories in which to search when executing\n\
external programs.  The value of this variable is automatically\n\
substituted for leading, trailing, or doubled colons that appear in the\n\
built-in variable @code{EXEC_PATH}.\n\
@seealso{EXEC_PATH}\n\
@end deftypefn")
{
  std::string saved_default_exec_path = VDEFAULT_EXEC_PATH;

  octave_value retval
    = SET_NONEMPTY_INTERNAL_STRING_VARIABLE (DEFAULT_EXEC_PATH);

  if (VDEFAULT_EXEC_PATH != saved_default_exec_path)
    update_exec_path ();

  return retval;
}

DEFUN (IMAGEPATH, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{val} =} IMAGEPATH ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} IMAGEPATH (@var{new_val})\n\
Query or set the internal variable that specifies a colon separated\n\
list of directories in which to search for image files.\n\
@end deftypefn")
{
  return SET_NONEMPTY_INTERNAL_STRING_VARIABLE (IMAGEPATH);
}

DEFUN (LOADPATH, args, nargout,
    "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{val} =} LOADPATH ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} LOADPATH (@var{new_val})\n\
Query or set the internal variable that specifies a colon separated\n\
list of directories in which to search for function\n\
files.  @xref{Functions and Scripts}.  The value of @code{LOADPATH}\n\
overrides the environment variable @code{OCTAVE_PATH}.  @xref{Installation}.\n\
\n\
Leading, trailing, or doubled colons that appear in\n\
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
@seealso{DEFAULT_LOADPATH}\n\
@end deftypefn")
{
  std::string saved_loadpath = VLOADPATH;

  octave_value retval = SET_NONEMPTY_INTERNAL_STRING_VARIABLE (LOADPATH);

  if (VLOADPATH != saved_loadpath)
    {
      // By resetting the last prompt time variable, we will force
      // checks for out of date symbols even if the change to LOADPATH
      // and subsequent function calls happen between prompts.

      // FIXME -- maybe we should rename
      // Vlast_prompt_time_stamp since the new usage doesn't really
      // fit with the current name?

      Vlast_prompt_time.stamp ();

      update_load_path_dir_path ();
    }

  return retval;
}

DEFUN (DEFAULT_LOADPATH, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{val} =} DEFAULT_LOADPATH ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} DEFAULT_LOADPATH (@var{new_val})\n\
Query or set the internal variable that specifies the colon separated\n\
list of directories in which to search for function files.  The value\n\
of this variable is automatically substituted for leading, trailing,\n\
or doubled colons that appear in the internal @code{loadpath} variable.\n\
@seealso{LOADPATH}\n\
@end deftypefn")
{
  std::string saved_default_loadpath = VDEFAULT_LOADPATH;

  octave_value retval
    = SET_NONEMPTY_INTERNAL_STRING_VARIABLE (DEFAULT_LOADPATH);

  if (VDEFAULT_LOADPATH != saved_default_loadpath)
    update_load_path_dir_path ();

  return retval;
}
  
DEFUN (OCTAVE_HOME, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} OCTAVE_HOME ()\n\
Return the name of the top-level Octave installation directory.\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 0)
    retval = Voctave_home;
  else
    print_usage ("OCTAVE_HOME");

  return retval;
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
