/*

Copyright (C) 1996, 1997, 1998, 1999, 2000, 2002, 2003, 2004, 2005,
              2006, 2007, 2008, 2009 John W. Eaton

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstdlib>

#include <algorithm>
#include <iostream>
#include <string>

#include <sys/types.h>
#include <unistd.h>

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
#include "load-path.h"
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
std::string Vlocal_api_arch_lib_dir;
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

// Name of the editor to be invoked by the edit_history command.
std::string VEDITOR;

static std::string VIMAGE_PATH;

std::string Vlocal_site_defaults_file;
std::string Vsite_defaults_file;

std::string
subst_octave_home (const std::string& s)
{
  std::string retval;

  std::string prefix = OCTAVE_PREFIX;

  retval = s;

  if (Voctave_home != prefix)
    {
      octave_idx_type len = prefix.length ();

      if (s.substr (0, len) == prefix)
	retval.replace (0, len, Voctave_home);
    }

  if (file_ops::dir_sep_char () != '/')
    std::replace (retval.begin (), retval.end (), '/',
		  file_ops::dir_sep_char ());

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
set_default_local_api_arch_lib_dir (void)
{
  Vlocal_api_arch_lib_dir = subst_octave_home (OCTAVE_LOCALAPIARCHLIBDIR);
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
  VEXEC_PATH = Vlocal_ver_arch_lib_dir + dir_path::path_sep_str ()
    + Vlocal_api_arch_lib_dir + dir_path::path_sep_str ()
    + Vlocal_arch_lib_dir + dir_path::path_sep_str ()
    + Varch_lib_dir + dir_path::path_sep_str ()
    + Vbin_dir;
  
  // This is static so that even if set_exec_path is called more than
  // once, shell_path is the original PATH from the environment,
  // before we start modifying it.
  static std::string shell_path = octave_env::getenv ("PATH");

  if (! shell_path.empty ())
    VEXEC_PATH += dir_path::path_sep_str () + shell_path;

  std::string tpath = path;

  if (tpath.empty ())
    tpath = octave_env::getenv ("OCTAVE_EXEC_PATH");

  if (! tpath.empty ())
    VEXEC_PATH = tpath + dir_path::path_sep_str () + VEXEC_PATH;

  octave_env::putenv ("PATH", VEXEC_PATH);
}

void
set_image_path (const std::string& path)
{
  VIMAGE_PATH = ".";

  std::string tpath = path;

  if (tpath.empty ())
    tpath = octave_env::getenv ("OCTAVE_IMAGE_PATH");

  if (! tpath.empty ())
    VIMAGE_PATH += dir_path::path_sep_str () + tpath;

  tpath = genpath (Vimage_dir, "");

  if (! tpath.empty ())
    VIMAGE_PATH += dir_path::path_sep_str () + tpath;
}

static void
set_default_doc_cache_file (void)
{
  std::string def_file = subst_octave_home (OCTAVE_DOC_CACHE_FILE);

  std::string env_file = octave_env::getenv ("OCTAVE_DOC_CACHE_FILE");

  Vdoc_cache_file = env_file.empty () ? def_file : env_file;
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
  std::string lsf = octave_env::getenv ("OCTAVE_SITE_INITFILE");

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
  std::string sf = octave_env::getenv ("OCTAVE_VERSION_INITFILE");

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

  set_default_local_ver_arch_lib_dir ();
  set_default_local_api_arch_lib_dir ();
  set_default_local_arch_lib_dir ();

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

  set_image_path ();

  set_default_doc_cache_file ();

  set_default_info_file ();

  set_default_info_prog ();

  set_default_editor ();

  set_local_site_defaults_file ();

  set_site_defaults_file ();
}

DEFUN (EDITOR, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{val} =} EDITOR ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} EDITOR (@var{new_val})\n\
Query or set the internal variable that specifies the editor to\n\
use with the @code{edit_history} command.  The default value is taken from\n\
the environment variable @w{@code{EDITOR}} when Octave starts.  If the\n\
environment variable is not initialized, @w{@code{EDITOR}} will be set to\n\
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
@w{@code{OCTAVE_EXEC_PATH}} (if it exists) or @code{PATH}, but that\n\
value can be overridden by the command line argument\n\
@code{--exec-path PATH}.  At startup, an additional set of\n\
directories (including the shell PATH) is appended to the path\n\
specified in the environment or on the command line.  If you use\n\
the @w{@code{EXEC_PATH}} function to modify the path, you should take\n\
care to preserve these additional directories.\n\
@end deftypefn")
{
  std::string saved_exec_path = VEXEC_PATH;

  octave_value retval = SET_NONEMPTY_INTERNAL_STRING_VARIABLE (EXEC_PATH);

  if (VEXEC_PATH != saved_exec_path)
    octave_env::putenv ("PATH", VEXEC_PATH);

  return retval;
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
