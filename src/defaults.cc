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

#include "dir-ops.h"
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

std::string Vlocal_ver_oct_file_dir;
std::string Vlocal_api_oct_file_dir;
std::string Vlocal_oct_file_dir;

std::string Vlocal_ver_fcn_file_dir;
std::string Vlocal_api_fcn_file_dir;
std::string Vlocal_fcn_file_dir;

std::string Voct_file_dir;
std::string Vfcn_file_dir;

std::string Vimage_dir;

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

static std::string VIMAGE_PATH;

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

  Vload_path_dir_path = dir_path (VLOADPATH, "");

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
set_default_local_ver_oct_file_dir (void)
{
  Vlocal_ver_oct_file_dir = subst_octave_home (OCTAVE_LOCALVEROCTFILEDIR);
}

static void
set_default_local_api_oct_file_dir (void)
{
  Vlocal_api_oct_file_dir = subst_octave_home (OCTAVE_LOCALAPIOCTFILEDIR);
}

static void
set_default_local_oct_file_dir (void)
{
  Vlocal_oct_file_dir = subst_octave_home (OCTAVE_LOCALOCTFILEDIR);
}

static void
set_default_local_ver_fcn_file_dir (void)
{
  Vlocal_ver_fcn_file_dir = subst_octave_home (OCTAVE_LOCALVERFCNFILEDIR);
}

static void
set_default_local_api_fcn_file_dir (void)
{
  Vlocal_api_fcn_file_dir = subst_octave_home (OCTAVE_LOCALAPIFCNFILEDIR);
}

static void
set_default_local_fcn_file_dir (void)
{
  Vlocal_fcn_file_dir = subst_octave_home (OCTAVE_LOCALFCNFILEDIR);
}

static void
set_default_fcn_file_dir (void)
{
  Vfcn_file_dir = subst_octave_home (OCTAVE_FCNFILEDIR);
}

static void
set_default_image_dir (void)
{
  Vimage_dir = subst_octave_home (OCTAVE_IMAGEDIR);
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

void
set_exec_path (const std::string& path)
{
  VEXEC_PATH = Vlocal_ver_arch_lib_dir + dir_path::path_sep_str
    + Vlocal_arch_lib_dir + dir_path::path_sep_str
    + Varch_lib_dir + dir_path::path_sep_str
    + Vbin_dir;
  
  // This is static so that even if set_exec_path is called more than
  // once, shell_path is the original PATH from the environment,
  // before we start modifying it.
  static std::string shell_path = octave_env::getenv ("PATH");

  if (! shell_path.empty ())
    VEXEC_PATH += dir_path::path_sep_str + shell_path;

  std::string tpath = path;

  if (tpath.empty ())
    tpath = octave_env::getenv ("OCTAVE_EXEC_PATH");

  if (! tpath.empty ())
    VEXEC_PATH = tpath + dir_path::path_sep_str + VEXEC_PATH;

  octave_env::putenv ("PATH", VEXEC_PATH);
}

static std::string
genpath (const std::string& dirname)
{
  std::string retval;

  std::string full_dirname = file_ops::tilde_expand (dirname);

  dir_entry dir (full_dirname);

  if (dir)
    {
      retval = dirname;

      string_vector dirlist = dir.read ();
      
      octave_idx_type len = dirlist.length ();

      for (octave_idx_type i = 0; i < len; i++)
	{
	  std::string elt = dirlist[i];

	  if (elt != "." && elt != ".." && elt != "private")
	    {
	      std::string nm = full_dirname + file_ops::dir_sep_str + elt;

	      file_stat fs (nm);

	      if (fs && fs.is_dir ())
		retval += dir_path::path_sep_str + genpath (nm);
	    }
	}
    }

  return retval;
}

static void
maybe_add_path_elts (std::string& pathvar, const std::string& dir)
{
  std::string tpath = genpath (dir);

  if (! tpath.empty ())
    pathvar += dir_path::path_sep_str + tpath;
}

void
set_load_path (const std::string& path)
{
  VDEFAULT_LOADPATH = ":";

  maybe_add_path_elts (VDEFAULT_LOADPATH, Vlocal_ver_oct_file_dir);
  maybe_add_path_elts (VDEFAULT_LOADPATH, Vlocal_api_oct_file_dir);
  maybe_add_path_elts (VDEFAULT_LOADPATH, Vlocal_oct_file_dir);
  maybe_add_path_elts (VDEFAULT_LOADPATH, Vlocal_ver_fcn_file_dir);
  maybe_add_path_elts (VDEFAULT_LOADPATH, Vlocal_api_fcn_file_dir);
  maybe_add_path_elts (VDEFAULT_LOADPATH, Vlocal_fcn_file_dir);
  maybe_add_path_elts (VDEFAULT_LOADPATH, Voct_file_dir);
  maybe_add_path_elts (VDEFAULT_LOADPATH, Vfcn_file_dir);

  std::string tpath = path;

  if (tpath.empty ())
    tpath = octave_env::getenv ("OCTAVE_LOADPATH");

  VLOADPATH = ".";

  if (! tpath.empty ())
    VLOADPATH += dir_path::path_sep_str + tpath;

  if (VDEFAULT_LOADPATH != ":")
    VLOADPATH += VDEFAULT_LOADPATH;

  update_load_path_dir_path ();
}

void
set_image_path (const std::string& path)
{
  VIMAGE_PATH = ".";

  std::string tpath = path;

  if (tpath.empty ())
    tpath = octave_env::getenv ("OCTAVE_IMAGE_PATH");

  if (! tpath.empty ())
    VIMAGE_PATH += dir_path::path_sep_str + tpath;

  maybe_add_path_elts (VIMAGE_PATH, Vimage_dir);
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

  set_default_local_ver_oct_file_dir ();
  set_default_local_api_oct_file_dir ();
  set_default_local_oct_file_dir ();

  set_default_local_ver_fcn_file_dir ();
  set_default_local_api_fcn_file_dir ();
  set_default_local_fcn_file_dir ();

  set_default_fcn_file_dir ();
  set_default_oct_file_dir ();

  set_default_image_dir ();

  set_default_bin_dir ();

  set_exec_path ();

  set_load_path ();

  set_image_path ();

  set_default_info_file ();

  set_default_info_prog ();

  set_default_fftw_wisdom_prog ();

  set_default_editor ();

  set_local_site_defaults_file ();

  set_site_defaults_file ();
}

DEFUN (genpath, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} genpath (@var{dir})\n\
Return a path constructed from @var{dir} and all its subdiretories.\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    {
      std::string dirname = args(0).string_value ();

      if (! error_state)
	retval = genpath (dirname);
      else
	error ("genpath: expecting argument to be a character string");
    }
  else
    print_usage ();

  return retval;
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

DEFUN (EXEC_PATH, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{val} =} EXEC_PATH ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} EXEC_PATH (@var{new_val})\n\
Query or set the internal variable that specifies a colon separated\n\
list of directories to search when executing external programs.\n\
Its initial value is taken from the environment variable\n\
@code{OCTAVE_EXEC_PATH} (if it exists) or @code{PATH}, but that\n\
value can be overridden by the command line argument\n\
@code{--exec-path PATH}.  At startup, an additional set of\n\
directories (including the shell PATH) is appended to the path\n\
specified in the environment or on the command line.  If you use\n\
the @code{EXEC_PATH} function to modify the path, you should take\n\
care to preserve these additional directories.\n\
@end deftypefn")
{
  std::string saved_exec_path = VEXEC_PATH;

  octave_value retval = SET_NONEMPTY_INTERNAL_STRING_VARIABLE (EXEC_PATH);

  if (VEXEC_PATH != saved_exec_path)
    octave_env::putenv ("PATH", VEXEC_PATH);

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

DEFUN (IMAGE_PATH, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{val} =} IMAGE_PATH ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} IMAGE_PATH (@var{new_val})\n\
Query or set the internal variable that specifies a colon separated\n\
list of directories in which to search for image files.\n\
@end deftypefn")
{
  return SET_NONEMPTY_INTERNAL_STRING_VARIABLE (IMAGE_PATH);
}

DEFUN (path, args, nargout,
    "-*- texinfo -*-\n\
@deftypefn {Function File} {} path (@dots{})\n\
Modify or display Octave's @code{LOADPATH}.\n\
\n\
If @var{nargin} and @var{nargout} are zero, display the elements of\n\
Octave's @code{LOADPATH} in an easy to read format.\n\
\n\
If @var{nargin} is zero and nargout is greater than zero, return the\n\
current value of @code{LOADPATH}.\n\
\n\
If @var{nargin} is greater than zero, concatenate the arguments,\n\
separating them with @code{pathsep()}.  Set the internal search path\n\
to the result and return it.\n\
\n\
No checks are made for duplicate elements.\n\
@seealso{addpath, rmpath, genpath, pathdef, savepath, pathsep}\n\
@end deftypefn")
{
  octave_value retval;

  int argc = args.length () + 1;

  string_vector argv = args.make_argv ("path");

  if (! error_state)
    {
      if (argc > 1)
	{
	  std::string path = argv[1];

	  for (int i = 2; i < argc; i++)
	    path += dir_path::path_sep_str;

	  size_t plen = path.length ();

	  if (! ((plen == 1 && path[0] == ':')
		 || (plen > 1
		     && path.substr (0, 2) == ("." + dir_path::path_sep_str))))
	    path = "." + dir_path::path_sep_str + path;

	  VLOADPATH = path;

	  // By resetting the last prompt time variable, we will force
	  // checks for out of date symbols even if the change to
	  // LOADPATH and subsequent function calls happen between
	  // prompts.

	  // FIXME -- maybe we should rename
	  // Vlast_prompt_time_stamp since the new usage doesn't really
	  // fit with the current name?

	  Vlast_prompt_time.stamp ();

	  update_load_path_dir_path ();
	}

      if (nargout > 0)
	retval = VLOADPATH;
      else if (argc == 1 && nargout == 0)
	{
	  octave_stdout << "\nOctave's search path contains the following directories:\n\n";

	  string_vector sv = Vload_path_dir_path.all_directories ();

	  sv.list_in_columns (octave_stdout);

	  octave_stdout << "\n";
	}
    }

  return retval;
}

DEFUN (pathdef, , ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{val} =} DEFAULT_LOADPATH ()\n\
Return the default list of directories in which to search for function\n\
files.\n\
@seealso{LOADPATH}\n\
@end deftypefn")
{
  return octave_value (VDEFAULT_LOADPATH);
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
    print_usage ();

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
    print_usage ();

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
