////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2023 The Octave Project Developers
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

#include <cstdlib>

#include <algorithm>
#include <string>

#include "dir-ops.h"
#include "file-ops.h"
#include "oct-env.h"

#include "defaults.h"
#include "defun.h"
#include "error.h"
#include "file-ops.h"
#include "ovl.h"
#include "ov.h"
#include "variables.h"
#include "version.h"

#include "default-defs.h"

OCTAVE_BEGIN_NAMESPACE(octave)

OCTAVE_BEGIN_NAMESPACE(config)

// Variables that name directories or files are substituted into source
// files with "${prefix}/" stripped from the beginning of the string.

// All configure variables of this form should be specified as absolute
// directory names.  The only ones that should not be absolute here are
// ones that have had "${prefix}/" or "${exec_prefix} stripped.

static std::string
prepend_home_dir (const std::string& hd, const std::string& s)
{
  std::string retval = s;

  char dir_sep_char = sys::file_ops::dir_sep_char ();

  if (! sys::env::absolute_pathname (retval))
    retval = hd + dir_sep_char + s;

  if (dir_sep_char != '/')
    std::replace (retval.begin (), retval.end (), '/', dir_sep_char);

  return retval;
}

static std::string get_octave_home (void)
{
  std::string op = OCTAVE_PREFIX;

  std::string oh = sys::env::getenv ("OCTAVE_HOME");

  // If OCTAVE_HOME is set in the environment, use that.  Otherwise,
  // default to ${prefix} from configure.

  return oh.empty () ? op : oh;
}

static std::string get_octave_exec_home (void)
{
  std::string op = OCTAVE_PREFIX;
  std::string oep = OCTAVE_EXEC_PREFIX;

  std::string oh = sys::env::getenv ("OCTAVE_HOME");
  std::string oeh = sys::env::getenv ("OCTAVE_EXEC_HOME");

  // If OCTAVE_EXEC_HOME is set in the environment, use that.
  // Otherwise, if ${prefix} and ${exec_prefix} from configure are set
  // to the same value, use OCTAVE_HOME from the environment if it is set.
  // Otherwise, default to ${exec_prefix} from configure.

  if (! oeh.empty ())
    return oeh;

  if (op == oep && ! oh.empty ())
    return oh;

  return oep;
}

static std::string get_local_site_defaults_file (void)
{
  std::string lsf = sys::env::getenv ("OCTAVE_SITE_INITFILE");

  return lsf.empty () ? local_startupfile_dir () + "/octaverc" : lsf;
}

static std::string get_site_defaults_file (void)
{
  std::string sf = sys::env::getenv ("OCTAVE_VERSION_INITFILE");

  return sf.empty () ? startupfile_dir () + "/octaverc" : sf;
}

std::string prepend_octave_home (const std::string& s)
{
  return prepend_home_dir (octave_home (), s);
}

std::string prepend_octave_exec_home (const std::string& s)
{
  return prepend_home_dir (octave_exec_home (), s);
}

std::string canonical_host_type (void)
{
  static const std::string s_canonical_host_type
    = OCTAVE_CANONICAL_HOST_TYPE;

  return s_canonical_host_type;
}

std::string release (void)
{
  static const std::string s_octave_release = OCTAVE_RELEASE;

  return s_octave_release;
}

std::string default_pager (void)
{
  static const std::string s_default_pager = OCTAVE_DEFAULT_PAGER;

  return s_default_pager;
}

std::string octave_home (void)
{
  static const std::string s_octave_home = get_octave_home ();

  return s_octave_home;
}

std::string octave_exec_home (void)
{
  static const std::string s_octave_exec_home = get_octave_exec_home ();

  return s_octave_exec_home;
}

std::string bin_dir (void)
{
  static const std::string s_bin_dir
    = prepend_octave_exec_home (OCTAVE_BINDIR);

  return s_bin_dir;
}

std::string data_dir (void)
{
  static const std::string s_data_dir
    = prepend_octave_home (OCTAVE_DATADIR);

  return s_data_dir;
}

std::string dataroot_dir (void)
{
  static const std::string s_dataroot_dir
    = prepend_octave_home (OCTAVE_DATAROOTDIR);

  return s_dataroot_dir;
}

std::string include_dir (void)
{
  static const std::string s_include_dir
    = prepend_octave_home (OCTAVE_INCLUDEDIR);

  return s_include_dir;
}

std::string lib_dir (void)
{
  static const std::string s_lib_dir
    = prepend_octave_exec_home (OCTAVE_LIBDIR);

  return s_lib_dir;
}

std::string libexec_dir (void)
{
  static const std::string s_libexec_dir
    = prepend_octave_exec_home (OCTAVE_LIBEXECDIR);

  return s_libexec_dir;
}

std::string arch_lib_dir (void)
{
  static const std::string s_arch_lib_dir
    = prepend_octave_exec_home (OCTAVE_ARCHLIBDIR);

  return s_arch_lib_dir;
}

std::string info_dir (void)
{
  static const std::string s_info_dir
    = prepend_octave_exec_home (OCTAVE_INFODIR);

  return s_info_dir;
}

std::string local_ver_arch_lib_dir (void)
{
  static const std::string s_local_ver_arch_lib_dir
    = prepend_octave_exec_home (OCTAVE_LOCALVERARCHLIBDIR);

  return s_local_ver_arch_lib_dir;
}

std::string local_api_arch_lib_dir (void)
{
  static const std::string s_local_api_arch_lib_dir
    = prepend_octave_exec_home (OCTAVE_LOCALAPIARCHLIBDIR);

  return s_local_api_arch_lib_dir;
}

std::string local_arch_lib_dir (void)
{
  static const std::string s_local_arch_lib_dir
    = prepend_octave_exec_home (OCTAVE_LOCALARCHLIBDIR);

  return s_local_arch_lib_dir;
}

std::string local_ver_oct_file_dir (void)
{
  static const std::string s_local_ver_oct_file_dir
    = prepend_octave_exec_home (OCTAVE_LOCALVEROCTFILEDIR);

  return s_local_ver_oct_file_dir;
}

std::string local_api_oct_file_dir (void)
{
  static const std::string s_local_api_oct_file_dir
    = prepend_octave_exec_home (OCTAVE_LOCALAPIOCTFILEDIR);

  return s_local_api_oct_file_dir;
}

std::string local_oct_file_dir (void)
{
  static const std::string s_local_oct_file_dir
    = prepend_octave_exec_home (OCTAVE_LOCALOCTFILEDIR);

  return s_local_oct_file_dir;
}

std::string oct_file_dir (void)
{
  static const std::string s_oct_file_dir
    = prepend_octave_exec_home (OCTAVE_OCTFILEDIR);

  return s_oct_file_dir;
}

std::string local_ver_fcn_file_dir (void)
{
  static const std::string s_local_ver_fcn_file_dir
    = prepend_octave_home (OCTAVE_LOCALVERFCNFILEDIR);

  return s_local_ver_fcn_file_dir;
}

std::string local_api_fcn_file_dir (void)
{
  static const std::string s_local_api_fcn_file_dir
    = prepend_octave_home (OCTAVE_LOCALAPIFCNFILEDIR);

  return s_local_api_fcn_file_dir;
}

std::string local_fcn_file_dir (void)
{
  static const std::string s_local_fcn_file_dir
    = prepend_octave_home (OCTAVE_LOCALFCNFILEDIR);

  return s_local_fcn_file_dir;
}

std::string fcn_file_dir (void)
{
  static const std::string s_fcn_file_dir
    = prepend_octave_home (OCTAVE_FCNFILEDIR);

  return s_fcn_file_dir;
}

std::string oct_data_dir (void)
{
  static const std::string s_oct_data_dir
    = prepend_octave_home (OCTAVE_OCTDATADIR);

  return s_oct_data_dir;
}

std::string oct_doc_dir (void)
{
  static const std::string s_oct_doc_dir
    = prepend_octave_home (OCTAVE_OCTDOCDIR);

  return s_oct_doc_dir;
}

std::string oct_etc_dir (void)
{
  static const std::string s_oct_etc_dir
    = prepend_octave_home (OCTAVE_OCTETCDIR);

  return s_oct_etc_dir;
}

std::string oct_fonts_dir (void)
{
  static const std::string s_oct_fonts_dir
    = prepend_octave_home (OCTAVE_OCTFONTSDIR);

  return s_oct_fonts_dir;
}

std::string oct_include_dir (void)
{
  static const std::string s_oct_include_dir
    = prepend_octave_home (OCTAVE_OCTINCLUDEDIR);

  return s_oct_include_dir;
}

std::string oct_lib_dir (void)
{
  static const std::string s_oct_lib_dir
    = prepend_octave_exec_home (OCTAVE_OCTLIBDIR);

  return s_oct_lib_dir;
}

std::string oct_locale_dir (void)
{
  static const std::string s_oct_locale_dir
    = prepend_octave_home (OCTAVE_OCTLOCALEDIR);

  return s_oct_locale_dir;
}

std::string oct_tests_dir (void)
{
  static const std::string s_oct_tests_dir
    = prepend_octave_home (OCTAVE_OCTTESTSDIR);

  return s_oct_tests_dir;
}

std::string man_dir (void)
{
  static const std::string s_man_dir
    = prepend_octave_home (OCTAVE_MANDIR);

  return s_man_dir;
}

std::string man1_dir (void)
{
  static const std::string s_man1_dir
    = prepend_octave_home (OCTAVE_MAN1DIR);

  return s_man1_dir;
}

std::string man1_ext (void)
{
  static const std::string s_man1_ext = OCTAVE_MAN1EXT;

  return s_man1_ext;
}

std::string image_dir (void)
{
  static const std::string s_image_dir
    = prepend_octave_home (OCTAVE_IMAGEDIR);

  return s_image_dir;
}

std::string local_startupfile_dir (void)
{
  static const std::string s_local_startupfile_dir
    = prepend_octave_home (OCTAVE_LOCALSTARTUPFILEDIR);

  return s_local_startupfile_dir;
}

std::string startupfile_dir (void)
{
  static const std::string s_startupfile_dir
    = prepend_octave_home (OCTAVE_STARTUPFILEDIR);

  return s_startupfile_dir;
}

std::string local_site_defaults_file (void)
{
  static const std::string s_local_site_defaults_file
    = get_local_site_defaults_file ();

  return s_local_site_defaults_file;
}

std::string site_defaults_file (void)
{
  static const std::string s_site_defaults_file
    = get_site_defaults_file ();

  return s_site_defaults_file;
}

OCTAVE_END_NAMESPACE(config)

DEFUN (OCTAVE_HOME, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{dir} =} OCTAVE_HOME ()
Return the name of the top-level Octave installation directory.

OCTAVE_HOME corresponds to the configuration variable @var{prefix}.
@seealso{EXEC_PATH, IMAGE_PATH, OCTAVE_EXEC_HOME}
@end deftypefn */)
{
  if (args.length () != 0)
    print_usage ();

  return ovl (config::octave_home ());
}

/*
%!assert (ischar (OCTAVE_HOME ()))
%!error OCTAVE_HOME (1)
*/

DEFUN (OCTAVE_EXEC_HOME, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{dir} =} OCTAVE_EXEC_HOME ()
Return the name of the top-level Octave installation directory for
architecture-dependent files.

If not specified separately, the value is the same as OCTAVE_HOME@.
OCTAVE_EXEC_HOME corresponds to the configuration variable @var{exec_prefix}.
@seealso{EXEC_PATH, IMAGE_PATH, OCTAVE_HOME}
@end deftypefn */)
{
  if (args.length () != 0)
    print_usage ();

  return ovl (config::octave_exec_home ());
}

/*
%!assert (ischar (OCTAVE_EXEC_HOME ()))
%!error OCTAVE_EXEC_HOME (1)
*/

DEFUNX ("OCTAVE_VERSION", FOCTAVE_VERSION, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {@var{verstr} =} OCTAVE_VERSION ()
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

DEFUN (user_config_dir, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {cfg_dir =} user_config_dir ()
Return the (platform-specific) directory for user configuration.
@seealso{user_data_dir}
@end deftypefn */)
{
  if (args.length () != 0)
    print_usage ();

  return ovl (sys::env::get_user_config_directory ());
}

/*
%!assert (ischar (user_config_dir ()))
%!error user_config_dir (1)
*/

DEFUN (user_data_dir, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {data_dir =} user_data_dir ()
Return the (platform-specific) directory for user data.
@seealso{user_config_dir}
@end deftypefn */)
{
  if (args.length () != 0)
    print_usage ();

  return ovl (sys::env::get_user_data_directory ());
}

/*
%!assert (ischar (user_data_dir ()))
%!error user_data_dir (1)
*/

OCTAVE_END_NAMESPACE(octave)
