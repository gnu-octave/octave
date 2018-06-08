/*

Copyright (C) 1996-2018 John W. Eaton

This file is part of Octave.

Octave is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<https://www.gnu.org/licenses/>.

*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <algorithm>
#include <string>

#include "dir-ops.h"
#include "file-ops.h"
#include "oct-env.h"

#include "defun.h"
#include "installation-data.h"
#include "interpreter-private.h"
#include "interpreter.h"
#include "ovl.h"
#include "ov.h"
#include "variables.h"
#include "version.h"

#include "default-defs.h"

// Variables that name directories or files are substituted into source
// files with "${prefix}/" stripped from the beginning of the string.

// All configure variables of this form should be specified as absolute
// directory names.  The only ones that should not be absolute here are
// ones that have had "${prefix}/" or "${exec_prefix} stripped.

namespace octave
{
  static std::string
  prepend_dir (const std::string& dir, const std::string& s)
  {
    std::string retval = s;

    char dir_sep_char = octave::sys::file_ops::dir_sep_char ();

    if (! octave::sys::env::absolute_pathname (retval))
      retval = dir + dir_sep_char + s;

    if (dir_sep_char != '/')
      std::replace (retval.begin (), retval.end (), '/', dir_sep_char);

    return retval;
  }

  installation_data::installation_data (void)
  {
    m_canonical_host_type = OCTAVE_CANONICAL_HOST_TYPE;
    m_release = OCTAVE_RELEASE;
    m_default_pager = OCTAVE_DEFAULT_PAGER;

    // OCTAVE_HOME must be set before other variables that depend on it.

    set_home ();

    m_bin_dir = prepend_exec_home (OCTAVE_BINDIR);
    m_data_dir = prepend_home (OCTAVE_DATADIR);
    m_dataroot_dir = prepend_home (OCTAVE_DATAROOTDIR);
    m_include_dir = prepend_home (OCTAVE_INCLUDEDIR);
    m_lib_dir = prepend_exec_home (OCTAVE_LIBDIR);
    m_libexec_dir = prepend_exec_home (OCTAVE_LIBEXECDIR);

    m_local_ver_arch_lib_dir
      = prepend_exec_home (OCTAVE_LOCALVERARCHLIBDIR);
    m_local_api_arch_lib_dir
      = prepend_exec_home (OCTAVE_LOCALAPIARCHLIBDIR);
    m_local_arch_lib_dir = prepend_exec_home (OCTAVE_LOCALARCHLIBDIR);
    m_arch_lib_dir = prepend_exec_home (OCTAVE_ARCHLIBDIR);

    m_local_ver_oct_file_dir
      = prepend_exec_home (OCTAVE_LOCALVEROCTFILEDIR);
    m_local_api_oct_file_dir
      = prepend_exec_home (OCTAVE_LOCALAPIOCTFILEDIR);
    m_local_oct_file_dir = prepend_exec_home (OCTAVE_LOCALOCTFILEDIR);
    m_oct_file_dir = prepend_exec_home (OCTAVE_OCTFILEDIR);

    m_local_ver_fcn_file_dir = prepend_home (OCTAVE_LOCALVERFCNFILEDIR);
    m_local_api_fcn_file_dir = prepend_home (OCTAVE_LOCALAPIFCNFILEDIR);
    m_local_fcn_file_dir = prepend_home (OCTAVE_LOCALFCNFILEDIR);
    m_fcn_file_dir = prepend_home (OCTAVE_FCNFILEDIR);

    m_oct_data_dir = prepend_home (OCTAVE_OCTDATADIR);
    m_oct_doc_dir = prepend_home (OCTAVE_OCTDOCDIR);
    m_oct_etc_dir = prepend_home (OCTAVE_OCTETCDIR);
    m_oct_fonts_dir = prepend_home (OCTAVE_OCTFONTSDIR);
    m_oct_include_dir = prepend_home (OCTAVE_OCTINCLUDEDIR);
    m_oct_lib_dir = prepend_exec_home (OCTAVE_OCTLIBDIR);
    m_oct_locale_dir = prepend_home (OCTAVE_OCTLOCALEDIR);
    m_oct_tests_dir = prepend_home (OCTAVE_OCTTESTSDIR);

    m_info_dir = prepend_home (OCTAVE_INFODIR);

    m_man_dir = prepend_home (OCTAVE_MANDIR);
    m_man1_dir = prepend_home (OCTAVE_MAN1DIR);
    m_man1_ext = OCTAVE_MAN1EXT;

    m_image_dir = prepend_home (OCTAVE_IMAGEDIR);

    m_local_startupfile_dir = prepend_home (OCTAVE_LOCALSTARTUPFILEDIR);
    m_startupfile_dir = prepend_home (OCTAVE_STARTUPFILEDIR);

    set_local_site_defaults_file ();

    set_site_defaults_file ();
  }

  std::string installation_data::prepend_home (const std::string& s) const
  {
    return prepend_dir (m_home, s);
  }

  std::string installation_data::prepend_exec_home (const std::string& s) const
  {
    return prepend_dir (m_exec_home, s);
  }

  void installation_data::set_home (void)
  {
    std::string op = OCTAVE_PREFIX;
    std::string oep = OCTAVE_EXEC_PREFIX;

    std::string oh = sys::env::getenv ("OCTAVE_HOME");
    std::string oeh = sys::env::getenv ("OCTAVE_EXEC_HOME");

    // If OCTAVE_HOME is set in the enviornment, use that.  Otherwise,
    // default to ${prefix} from configure.

    m_home = (oh.empty () ? op : oh);

    // If OCTAVE_EXEC_HOME is set in the environment, use that.
    // Otherwise, if ${prefix} and ${exec_prefix} from configure are set
    // to the same value, use OCTAVE_HOME from the environment if it is set.
    // Othewise, default to ${exec_prefix} from configure.

    if (! oeh.empty ())
      m_exec_home = oeh;
    else
      {
        if (op == oep && ! oh.empty ())
          m_exec_home = oh;
        else
          m_exec_home = oep;
      }
  }

  void installation_data::set_local_site_defaults_file (void)
  {
    std::string lsf = sys::env::getenv ("OCTAVE_SITE_INITFILE");

    if (lsf.empty ())
      m_local_site_defaults_file = m_local_startupfile_dir + "/octaverc";
    else
      m_local_site_defaults_file = lsf;
  }

  void installation_data::set_site_defaults_file (void)
  {
    std::string sf = sys::env::getenv ("OCTAVE_VERSION_INITFILE");

    if (sf.empty ())
      m_site_defaults_file = m_startupfile_dir + "/octaverc";
    else
      m_site_defaults_file = sf;
  }
}

DEFMETHOD (OCTAVE_HOME, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} OCTAVE_HOME ()
Return the name of the top-level Octave installation directory.
OCTAVE_HOME corresponds to the configuration variable @var{prefix}.
@seealso{EXEC_PATH, IMAGE_PATH, OCTAVE_EXEC_HOME}
@end deftypefn */)
{
  if (args.length () != 0)
    print_usage ();

  octave::installation_data& inst_data = interp.get_installation_data ();

  return ovl (inst_data.home ());
}

/*
%!assert (ischar (OCTAVE_HOME ()))
%!error OCTAVE_HOME (1)
*/

DEFMETHOD (OCTAVE_EXEC_HOME, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} OCTAVE_HOME ()
Return the name of the top-level Octave installation directory for
architecture-dependent files.  If not specified separately, the value
is the same as OCTAVE_HOME@.  OCTAVE_EXEC_HOME corresponds to the
configuration variable @var{exec_prefix}.
@seealso{EXEC_PATH, IMAGE_PATH, OCTAVE_HOME}
@end deftypefn */)
{
  if (args.length () != 0)
    print_usage ();

  octave::installation_data& inst_data = interp.get_installation_data ();

  return ovl (inst_data.exec_home ());
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
