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

#include <cstdlib>

#include <algorithm>
#include <iostream>
#include <string>

#include "dir-ops.h"
#include "oct-env.h"
#include "file-stat.h"
#include "pathsearch.h"
#include "str-vec.h"

#include "defaults.h"
#include "defun.h"
#include "error.h"
#include "file-ops.h"
#include "errwarn.h"
#include "help.h"
#include "input.h"
#include "load-path.h"
#include "ovl.h"
#include "ov.h"
#include "parse.h"
#include "interpreter.h"
#include "unwind-prot.h"
#include "variables.h"
#include "version.h"

#if ! defined (OCTAVE_CANONICAL_HOST_TYPE)
#  define OCTAVE_CANONICAL_HOST_TYPE %OCTAVE_CANONICAL_HOST_TYPE%
#endif

#if ! defined (OCTAVE_DEFAULT_PAGER)
#  define OCTAVE_DEFAULT_PAGER %OCTAVE_DEFAULT_PAGER%
#endif

#if ! defined (OCTAVE_ARCHLIBDIR)
#  define OCTAVE_ARCHLIBDIR %OCTAVE_ARCHLIBDIR%
#endif

#if ! defined (OCTAVE_BINDIR)
#  define OCTAVE_BINDIR %OCTAVE_BINDIR%
#endif

#if ! defined (OCTAVE_DATADIR)
#  define OCTAVE_DATADIR %OCTAVE_DATADIR%
#endif

#if ! defined (OCTAVE_DATAROOTDIR)
#  define OCTAVE_DATAROOTDIR %OCTAVE_DATAROOTDIR%
#endif

#if ! defined (OCTAVE_DOC_CACHE_FILE)
#  define OCTAVE_DOC_CACHE_FILE %OCTAVE_DOC_CACHE_FILE%
#endif

#if ! defined (OCTAVE_TEXI_MACROS_FILE)
#  define OCTAVE_TEXI_MACROS_FILE %OCTAVE_TEXI_MACROS_FILE%
#endif

#if ! defined (OCTAVE_EXEC_PREFIX)
#  define OCTAVE_EXEC_PREFIX %OCTAVE_EXEC_PREFIX%
#endif

#if ! defined (OCTAVE_FCNFILEDIR)
#  define OCTAVE_FCNFILEDIR %OCTAVE_FCNFILEDIR%
#endif

#if ! defined (OCTAVE_IMAGEDIR)
#  define OCTAVE_IMAGEDIR %OCTAVE_IMAGEDIR%
#endif

#if ! defined (OCTAVE_INCLUDEDIR)
#  define OCTAVE_INCLUDEDIR %OCTAVE_INCLUDEDIR%
#endif

#if ! defined (OCTAVE_INFODIR)
#  define OCTAVE_INFODIR %OCTAVE_INFODIR%
#endif

#if ! defined (OCTAVE_INFOFILE)
#  define OCTAVE_INFOFILE %OCTAVE_INFOFILE%
#endif

#if ! defined (OCTAVE_LIBDIR)
#  define OCTAVE_LIBDIR %OCTAVE_LIBDIR%
#endif

#if ! defined (OCTAVE_LIBEXECDIR)
#  define OCTAVE_LIBEXECDIR %OCTAVE_LIBEXECDIR%
#endif

#if ! defined (OCTAVE_LIBEXECDIR)
#  define OCTAVE_LIBEXECDIR %OCTAVE_LIBEXECDIR%
#endif

#if ! defined (OCTAVE_LOCALAPIFCNFILEDIR)
#  define OCTAVE_LOCALAPIFCNFILEDIR %OCTAVE_LOCALAPIFCNFILEDIR%
#endif

#if ! defined (OCTAVE_LOCALAPIOCTFILEDIR)
#  define OCTAVE_LOCALAPIOCTFILEDIR %OCTAVE_LOCALAPIOCTFILEDIR%
#endif

#if ! defined (OCTAVE_LOCALARCHLIBDIR)
#  define OCTAVE_LOCALARCHLIBDIR %OCTAVE_LOCALARCHLIBDIR%
#endif

#if ! defined (OCTAVE_LOCALFCNFILEDIR)
#  define OCTAVE_LOCALFCNFILEDIR %OCTAVE_LOCALFCNFILEDIR%
#endif

#if ! defined (OCTAVE_LOCALOCTFILEDIR)
#  define OCTAVE_LOCALOCTFILEDIR %OCTAVE_LOCALOCTFILEDIR%
#endif

#if ! defined (OCTAVE_LOCALSTARTUPFILEDIR)
#  define OCTAVE_LOCALSTARTUPFILEDIR %OCTAVE_LOCALSTARTUPFILEDIR%
#endif

#if ! defined (OCTAVE_LOCALAPIARCHLIBDIR)
#  define OCTAVE_LOCALAPIARCHLIBDIR %OCTAVE_LOCALAPIARCHLIBDIR%
#endif

#if ! defined (OCTAVE_LOCALVERARCHLIBDIR)
#  define OCTAVE_LOCALVERARCHLIBDIR %OCTAVE_LOCALVERARCHLIBDIR%
#endif

#if ! defined (OCTAVE_LOCALVERFCNFILEDIR)
#  define OCTAVE_LOCALVERFCNFILEDIR %OCTAVE_LOCALVERFCNFILEDIR%
#endif

#if ! defined (OCTAVE_LOCALVEROCTFILEDIR)
#  define OCTAVE_LOCALVEROCTFILEDIR %OCTAVE_LOCALVEROCTFILEDIR%
#endif

#if ! defined (OCTAVE_MAN1DIR)
#  define OCTAVE_MAN1DIR %OCTAVE_MAN1DIR%
#endif

#if ! defined (OCTAVE_MAN1EXT)
#  define OCTAVE_MAN1EXT %OCTAVE_MAN1EXT%
#endif

#if ! defined (OCTAVE_MANDIR)
#  define OCTAVE_MANDIR %OCTAVE_MANDIR%
#endif

#if ! defined (OCTAVE_OCTDATADIR)
#  define OCTAVE_OCTDATADIR %OCTAVE_OCTDATADIR%
#endif

#if ! defined (OCTAVE_OCTFILEDIR)
#  define OCTAVE_OCTFILEDIR %OCTAVE_OCTFILEDIR%
#endif

#if ! defined (OCTAVE_OCTETCDIR)
#  define OCTAVE_OCTETCDIR %OCTAVE_OCTETCDIR%
#endif

#if ! defined (OCTAVE_OCTLOCALEDIR)
#  define OCTAVE_OCTLOCALEDIR %OCTAVE_OCTLOCALEDIR%
#endif

#if ! defined (OCTAVE_OCTINCLUDEDIR)
#  define OCTAVE_OCTINCLUDEDIR %OCTAVE_OCTINCLUDEDIR%
#endif

#if ! defined (OCTAVE_OCTLIBDIR)
#  define OCTAVE_OCTLIBDIR %OCTAVE_OCTLIBDIR%
#endif

#if ! defined (OCTAVE_OCTTESTSDIR)
#  define OCTAVE_OCTTESTSDIR %OCTAVE_OCTTESTSDIR%
#endif

#if ! defined (OCTAVE_PREFIX)
#  define OCTAVE_PREFIX %OCTAVE_PREFIX%
#endif

#if ! defined (OCTAVE_STARTUPFILEDIR)
#  define OCTAVE_STARTUPFILEDIR %OCTAVE_STARTUPFILEDIR%
#endif

#if ! defined (OCTAVE_RELEASE)
#  define OCTAVE_RELEASE %OCTAVE_RELEASE%
#endif

std::string Voctave_home;
std::string Voctave_exec_home;

std::string Vbin_dir;
std::string Vinfo_dir;
std::string Vdata_dir;
std::string Vdataroot_dir;
std::string Vinclude_dir;
std::string Vlib_dir;
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

std::string Voct_data_dir;
std::string Voct_lib_dir;
std::string Voct_include_dir;
std::string Voct_etc_dir;
std::string Voct_locale_dir;

std::string Vman1_dir = OCTAVE_MAN1DIR;
std::string Vman1_ext = OCTAVE_MAN1EXT;
std::string Vman_dir = OCTAVE_MANDIR;

std::string Vlocal_startup_file_dir;
std::string Voct_tests_dir;
std::string Vstartupfile_dir;

std::string Voct_file_dir;
std::string Vfcn_file_dir;

std::string Vimage_dir;

// The path that will be searched for programs that we execute.
// (--exec-path path)
static std::string VEXEC_PATH;

// Name of the editor to be invoked by the edit_history command.
std::string VEDITOR;

std::string Vdefault_pager = OCTAVE_DEFAULT_PAGER;
std::string Vcanonical_host_type = OCTAVE_CANONICAL_HOST_TYPE;
std::string Voctave_release = OCTAVE_RELEASE;

static std::string VIMAGE_PATH;

std::string Vlocal_site_defaults_file;
std::string Vsite_defaults_file;

std::string Vbuilt_in_docstrings_file;

// Variables that name directories or files are substituted into source
// files with "${prefix}/" stripped from the beginning of the string.

// All configure variables of this form should be specified as absolute
// directory names.  The only ones that should not be absolute here are
// ones that have had "${prefix}/" or "${exec_prefix} stripped.

static std::string
prepend_home_dir (const std::string& hd, const std::string& s)
{
  std::string retval = s;

  char dir_sep_char = octave::sys::file_ops::dir_sep_char ();

  if (! octave::sys::env::absolute_pathname (retval))
    retval = hd + dir_sep_char + s;

  if (dir_sep_char != '/')
    std::replace (retval.begin (), retval.end (), '/', dir_sep_char);

  return retval;
}

static std::string
prepend_octave_home (const std::string& s)
{
  return prepend_home_dir (Voctave_home, s);
}

static std::string
prepend_octave_exec_home (const std::string& s)
{
  return prepend_home_dir (Voctave_exec_home, s);
}

static void
set_octave_home (void)
{
  std::string op = OCTAVE_PREFIX;
  std::string oep = OCTAVE_EXEC_PREFIX;

  std::string oh = octave::sys::env::getenv ("OCTAVE_HOME");
  std::string oeh = octave::sys::env::getenv ("OCTAVE_EXEC_HOME");

  // If OCTAVE_HOME is set in the enviornment, use that.  Otherwise,
  // default to ${prefix} from configure.

  Voctave_home = (oh.empty () ? op : oh);

  // If OCTAVE_EXEC_HOME is set in the environment, use that.
  // Otherwise, if ${prefix} and ${exec_prefix} from configure are set
  // to the same value, use OCTAVE_HOME from the environment if it is set.
  // Othewise, default to ${exec_prefix} from configure.

  if (! oeh.empty ())
    Voctave_exec_home = oeh;
  else
    {
      if (op == oep && ! oh.empty ())
        Voctave_exec_home = oh;
      else
        Voctave_exec_home = oep;
    }
}

static void
set_default_info_dir (void)
{
  Vinfo_dir = prepend_octave_home (OCTAVE_INFODIR);
}

static void
set_default_man_vars (void)
{
  Vman_dir = prepend_octave_home (OCTAVE_MANDIR);
  Vman1_dir = prepend_octave_home (OCTAVE_MAN1DIR);
  Vman1_ext = prepend_octave_home (OCTAVE_MAN1EXT);
}

static void
set_default_data_dirs (void)
{
  Vdata_dir = prepend_octave_home (OCTAVE_DATADIR);

  Vdataroot_dir = prepend_octave_home (OCTAVE_DATAROOTDIR);
}

static void
set_default_lib_dir (void)
{
  Vlib_dir = prepend_octave_exec_home (OCTAVE_LIBDIR);
}

static void
set_default_libexec_dir (void)
{
  Vlibexec_dir = prepend_octave_exec_home (OCTAVE_LIBEXECDIR);
}

static void
set_default_arch_lib_dir (void)
{
  Varch_lib_dir = prepend_octave_exec_home (OCTAVE_ARCHLIBDIR);
}

static void
set_default_local_arch_lib_dir (void)
{
  Vlocal_arch_lib_dir = prepend_octave_exec_home (OCTAVE_LOCALARCHLIBDIR);
}

static void
set_default_local_api_arch_lib_dir (void)
{
  Vlocal_api_arch_lib_dir = prepend_octave_exec_home (OCTAVE_LOCALAPIARCHLIBDIR);
}

static void
set_default_local_ver_arch_lib_dir (void)
{
  Vlocal_ver_arch_lib_dir = prepend_octave_exec_home (OCTAVE_LOCALVERARCHLIBDIR);
}

static void
set_default_local_ver_oct_file_dir (void)
{
  Vlocal_ver_oct_file_dir = prepend_octave_exec_home (OCTAVE_LOCALVEROCTFILEDIR);
}

static void
set_default_local_api_oct_file_dir (void)
{
  Vlocal_api_oct_file_dir = prepend_octave_exec_home (OCTAVE_LOCALAPIOCTFILEDIR);
}

static void
set_default_local_oct_file_dir (void)
{
  Vlocal_oct_file_dir = prepend_octave_exec_home (OCTAVE_LOCALOCTFILEDIR);
}

static void
set_default_local_ver_fcn_file_dir (void)
{
  Vlocal_ver_fcn_file_dir = prepend_octave_home (OCTAVE_LOCALVERFCNFILEDIR);
}

static void
set_default_local_api_fcn_file_dir (void)
{
  Vlocal_api_fcn_file_dir = prepend_octave_home (OCTAVE_LOCALAPIFCNFILEDIR);
}

static void
set_default_local_fcn_file_dir (void)
{
  Vlocal_fcn_file_dir = prepend_octave_home (OCTAVE_LOCALFCNFILEDIR);
}

static void
set_default_fcn_file_dir (void)
{
  Vfcn_file_dir = prepend_octave_home (OCTAVE_FCNFILEDIR);
}

static void
set_default_image_dir (void)
{
  Vimage_dir = prepend_octave_home (OCTAVE_IMAGEDIR);
}

static void
set_default_oct_data_dir (void)
{
  Voct_data_dir = prepend_octave_home (OCTAVE_OCTDATADIR);
}

static void
set_default_include_dir (void)
{
  Vinclude_dir = prepend_octave_home (OCTAVE_INCLUDEDIR);
}

static void
set_default_oct_lib_dir (void)
{
  Voct_etc_dir = prepend_octave_exec_home (OCTAVE_OCTLIBDIR);
}

static void
set_default_oct_include_dir (void)
{
  Voct_include_dir = prepend_octave_home (OCTAVE_OCTINCLUDEDIR);
}

static void
set_default_oct_etc_dir (void)
{
  Voct_etc_dir = prepend_octave_home (OCTAVE_OCTETCDIR);
}

static void
set_default_oct_locale_dir (void)
{
  Voct_locale_dir = prepend_octave_home (OCTAVE_OCTLOCALEDIR);
}

static void
set_default_oct_file_dir (void)
{
  Voct_file_dir = prepend_octave_exec_home (OCTAVE_OCTFILEDIR);
}

static void
set_default_bin_dir (void)
{
  Vbin_dir = prepend_octave_exec_home (OCTAVE_BINDIR);
}

void
set_exec_path (const std::string& path_arg)
{
  std::string tpath = path_arg;

  if (tpath.empty ())
    tpath = octave::sys::env::getenv ("OCTAVE_EXEC_PATH");

  if (tpath.empty ())
    tpath = Vlocal_ver_arch_lib_dir + octave::directory_path::path_sep_str ()
            + Vlocal_api_arch_lib_dir + octave::directory_path::path_sep_str ()
            + Vlocal_arch_lib_dir + octave::directory_path::path_sep_str ()
            + Varch_lib_dir + octave::directory_path::path_sep_str ()
            + Vbin_dir;

  VEXEC_PATH = tpath;

  // FIXME: should we really be modifying PATH in the environment?
  // The way things are now, Octave will ignore directories set in the
  // PATH with calls like
  //
  //   setenv ("PATH", "/my/path");
  //
  // To fix this, I think Octave should be searching the combination of
  // PATH and EXEC_PATH for programs that it executes instead of setting
  // the PATH in the environment and relying on the shell to do the
  // searching.

  // This is static so that even if set_exec_path is called more than
  // once, shell_path is the original PATH from the environment,
  // before we start modifying it.
  static std::string shell_path = octave::sys::env::getenv ("PATH");

  if (! shell_path.empty ())
    tpath = shell_path + octave::directory_path::path_sep_str () + tpath;

  octave::sys::env::putenv ("PATH", tpath);
}

void
set_image_path (const std::string& path)
{
  VIMAGE_PATH = ".";

  std::string tpath = path;

  if (tpath.empty ())
    tpath = octave::sys::env::getenv ("OCTAVE_IMAGE_PATH");

  if (! tpath.empty ())
    VIMAGE_PATH += octave::directory_path::path_sep_str () + tpath;

  tpath = octave::genpath (Vimage_dir, "");

  if (! tpath.empty ())
    VIMAGE_PATH += octave::directory_path::path_sep_str () + tpath;
}

static void
set_default_doc_cache_file (void)
{
  if (Vdoc_cache_file.empty ())
    {
      std::string def_file = prepend_octave_home (OCTAVE_DOC_CACHE_FILE);

      std::string env_file = octave::sys::env::getenv ("OCTAVE_DOC_CACHE_FILE");

      Vdoc_cache_file = (env_file.empty () ? def_file : env_file);
    }
}

static void
set_default_texi_macros_file (void)
{
  if (Vtexi_macros_file.empty ())
    {
      std::string def_file = prepend_octave_home (OCTAVE_TEXI_MACROS_FILE);

      std::string env_file = octave::sys::env::getenv ("OCTAVE_TEXI_MACROS_FILE");

      Vtexi_macros_file = (env_file.empty () ? def_file : env_file);
    }
}

static void
set_default_info_file (void)
{
  if (Vinfo_file.empty ())
    {
      std::string std_info_file = prepend_octave_home (OCTAVE_INFOFILE);

      std::string oct_info_file = octave::sys::env::getenv ("OCTAVE_INFO_FILE");

      Vinfo_file = (oct_info_file.empty () ? std_info_file : oct_info_file);
    }
}

static void
set_default_info_prog (void)
{
  if (Vinfo_program.empty ())
    {
      std::string oct_info_prog = octave::sys::env::getenv ("OCTAVE_INFO_PROGRAM");

      if (oct_info_prog.empty ())
        Vinfo_program = "info";
      else
        Vinfo_program = std::string (oct_info_prog);
    }
}

static void
set_default_editor (void)
{
  VEDITOR = "emacs";

  std::string env_editor = octave::sys::env::getenv ("EDITOR");

  if (! env_editor.empty ())
    VEDITOR = env_editor;
}

static void
set_local_site_defaults_file (void)
{
  std::string lsf = octave::sys::env::getenv ("OCTAVE_SITE_INITFILE");

  if (lsf.empty ())
    {
      Vlocal_site_defaults_file
        = prepend_octave_home (OCTAVE_LOCALSTARTUPFILEDIR);
      Vlocal_site_defaults_file.append ("/octaverc");
    }
  else
    Vlocal_site_defaults_file = lsf;
}

static void
set_site_defaults_file (void)
{
  std::string sf = octave::sys::env::getenv ("OCTAVE_VERSION_INITFILE");

  if (sf.empty ())
    {
      Vsite_defaults_file = prepend_octave_home (OCTAVE_STARTUPFILEDIR);
      Vsite_defaults_file.append ("/octaverc");
    }
  else
    Vsite_defaults_file = sf;
}

static void
set_built_in_docstrings_file (void)
{
  if (Vbuilt_in_docstrings_file.empty ())
    {
      std::string df = octave::sys::env::getenv ("OCTAVE_BUILT_IN_DOCSTRINGS_FILE");

      if (df.empty ())
        Vbuilt_in_docstrings_file
          = Voct_etc_dir + octave::sys::file_ops::dir_sep_str () + "built-in-docstrings";
      else
        Vbuilt_in_docstrings_file = df;
    }
}

void
install_defaults (void)
{
  // OCTAVE_HOME must be set first!

  set_octave_home ();

  set_default_info_dir ();

  set_default_man_vars ();

  set_default_data_dirs ();
  set_default_lib_dir ();
  set_default_libexec_dir ();
  set_default_arch_lib_dir ();

  set_default_include_dir ();

  set_default_local_ver_arch_lib_dir ();
  set_default_local_api_arch_lib_dir ();
  set_default_local_arch_lib_dir ();

  set_default_local_ver_oct_file_dir ();
  set_default_local_api_oct_file_dir ();
  set_default_local_oct_file_dir ();

  set_default_local_ver_fcn_file_dir ();
  set_default_local_api_fcn_file_dir ();
  set_default_local_fcn_file_dir ();

  set_default_oct_data_dir ();
  set_default_oct_lib_dir ();
  set_default_oct_include_dir ();
  set_default_oct_etc_dir ();
  set_default_oct_locale_dir ();

  set_default_fcn_file_dir ();
  set_default_oct_file_dir ();

  set_default_image_dir ();

  set_default_bin_dir ();

  set_exec_path ();

  set_image_path ();

  set_default_doc_cache_file ();

  set_default_texi_macros_file ();

  set_default_info_file ();

  set_default_info_prog ();

  set_default_editor ();

  set_local_site_defaults_file ();

  set_site_defaults_file ();

  set_built_in_docstrings_file ();
}

DEFUN (EDITOR, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} EDITOR ()
@deftypefnx {} {@var{old_val} =} EDITOR (@var{new_val})
@deftypefnx {} {} EDITOR (@var{new_val}, "local")
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
  return SET_NONEMPTY_INTERNAL_STRING_VARIABLE (EDITOR);
}

/*
%!test
%! orig_val = EDITOR ();
%! old_val = EDITOR ("X");
%! assert (orig_val, old_val);
%! assert (EDITOR (), "X");
%! EDITOR (orig_val);
%! assert (EDITOR (), orig_val);

%!error (EDITOR (1, 2))
*/

DEFUN (EXEC_PATH, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} EXEC_PATH ()
@deftypefnx {} {@var{old_val} =} EXEC_PATH (@var{new_val})
@deftypefnx {} {} EXEC_PATH (@var{new_val}, "local")
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
  octave_value retval = SET_NONEMPTY_INTERNAL_STRING_VARIABLE (EXEC_PATH);

  if (args.length () > 0)
    set_exec_path (VEXEC_PATH);

  return retval;
}

/*
%!test
%! orig_val = EXEC_PATH ();
%! old_val = EXEC_PATH ("X");
%! assert (orig_val, old_val);
%! assert (EXEC_PATH (), "X");
%! EXEC_PATH (orig_val);
%! assert (EXEC_PATH (), orig_val);

%!error (EXEC_PATH (1, 2))
*/

DEFUN (IMAGE_PATH, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} IMAGE_PATH ()
@deftypefnx {} {@var{old_val} =} IMAGE_PATH (@var{new_val})
@deftypefnx {} {} IMAGE_PATH (@var{new_val}, "local")
Query or set the internal variable that specifies a colon separated
list of directories in which to search for image files.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.

@seealso{EXEC_PATH, OCTAVE_HOME, OCTAVE_EXEC_HOME}
@end deftypefn */)
{
  return SET_NONEMPTY_INTERNAL_STRING_VARIABLE (IMAGE_PATH);
}

/*
%!test
%! orig_val = IMAGE_PATH ();
%! old_val = IMAGE_PATH ("X");
%! assert (orig_val, old_val);
%! assert (IMAGE_PATH (), "X");
%! IMAGE_PATH (orig_val);
%! assert (IMAGE_PATH (), orig_val);

%!error (IMAGE_PATH (1, 2))
*/

DEFUN (OCTAVE_HOME, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} OCTAVE_HOME ()
Return the name of the top-level Octave installation directory.
OCTAVE_HOME corresponds to the configuration variable @var{prefix}.
@seealso{EXEC_PATH, IMAGE_PATH, OCTAVE_EXEC_HOME}
@end deftypefn */)
{
  if (args.length () != 0)
    print_usage ();

  return ovl (Voctave_home);
}

/*
%!assert (ischar (OCTAVE_HOME ()))
%!error OCTAVE_HOME (1)
*/

DEFUN (OCTAVE_EXEC_HOME, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} OCTAVE_HOME ()
Return the name of the top-level Octave installation directory for
architecture-dependendent files.  If not specified separately, the value
is the same as OCTAVE_HOME.  OCTAVE_EXEC_HOME corresponds to the
configuration variable @var{exec_prefix}.
@seealso{EXEC_PATH, IMAGE_PATH, OCTAVE_HOME}
@end deftypefn */)
{
  if (args.length () != 0)
    print_usage ();

  return ovl (Voctave_exec_home);
}

/*
%!assert (ischar (OCTAVE_EXEC_HOME ()))
%!error OCTAVE_EXEC_HOME (1)
*/

DEFUNX ("OCTAVE_VERSION", FOCTAVE_VERSION, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {} OCTAVE_VERSION ()
Return the version number of Octave as a string.
@seealso{ver, version}
@end deftypefn */)
{
  if (args.length () != 0)
    print_usage ();

  return ovl (OCTAVE_VERSION);
}

/*
%!assert (ischar (OCTAVE_VERSION ()))
%!error OCTAVE_VERSION (1)
*/
