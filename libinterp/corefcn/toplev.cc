////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1995-2023 The Octave Project Developers
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

#include <cerrno>
#include <cstdlib>

#include <new>
#include <sstream>
#include <string>

#if defined (OCTAVE_USE_WINDOWS_API)
#  define WIN32_LEAN_AND_MEAN 1
#  include <windows.h>
#endif

#include "async-system-wrapper.h"
#include "child-list.h"
#include "lo-error.h"
#include "oct-fftw.h"
#include "oct-locbuf.h"
#include "oct-syscalls.h"
#include "signal-wrappers.h"
#include "str-vec.h"
#include "wait-for-input.h"

#include "build-env.h"
#include "liboctinterp-build-info.h"
#include "defaults.h"
#include "defun.h"
#include "error.h"
#include "help.h"
#include "interpreter-private.h"
#include "octave.h"
#include "oct-map.h"
#include "ovl.h"
#include "ov.h"
#include "pager.h"
#include "procstream.h"
#include "sysdep.h"
#include "unwind-prot.h"
#include "utils.h"
#include "version.h"

#if ! defined (SHELL_PATH)
#  define SHELL_PATH "/bin/sh"
#endif

OCTAVE_BEGIN_NAMESPACE(octave)

#define STRINGIFY(s) STRINGIFY1(s)
#define STRINGIFY1(s) #s

DEFUN (warranty, , ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} warranty ()
Describe the conditions for copying and distributing Octave.
@end deftypefn */)
{
  octave_stdout << "\n" << octave_name_version_and_copyright () << "\n\
\n\
GNU Octave is free software: you can redistribute it and/or modify it\n\
under the terms of the GNU General Public License as published by\n\
the Free Software Foundation, either version 3 of the License, or\n\
(at your option) any later version.\n\
\n\
GNU Octave is distributed in the hope that it will be useful, but\n\
WITHOUT ANY WARRANTY; without even the implied warranty of\n\
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n\
GNU General Public License for more details.\n\
\n\
You should have received a copy of the GNU General Public License\n\
along with GNU Octave; see the file COPYING.  If not, see\n\
<https://www.gnu.org/licenses/>.\n\
\n";

  return ovl ();
}

// Execute a shell command.

static octave_value_list
run_command_and_return_output (const std::string& cmd_str)
{
  octave_value_list retval;
  unwind_protect frame;

  iprocstream *cmd = new iprocstream (cmd_str.c_str ());
  frame.add_delete (cmd);

  child_list& kids = __get_child_list__ ();
  frame.add (&child_list::remove, kids, cmd->pid ());

  if (! *cmd)
    error ("system: unable to start subprocess for '%s'", cmd_str.c_str ());

  int fid = cmd->file_number ();

  std::ostringstream output_buf;

  char ch;

  for (;;)
    {
      if (cmd->get (ch))
        output_buf.put (ch);
      else
        {
          if (! cmd->eof () && errno == EAGAIN)
            {
              cmd->clear ();

              if (octave_wait_for_input (fid) != 1)
                break;
            }
          else
            break;
        }
    }

  int cmd_status = cmd->close ();

  if (sys::wifexited (cmd_status))
    cmd_status = sys::wexitstatus (cmd_status);
  else
    cmd_status = 127;

  retval = ovl (cmd_status, output_buf.str ());

  return retval;
}

// Combine alloc+get in one action.

static void *
get_signal_mask (void)
{
  void *mask = octave_alloc_signal_mask ();

  octave_get_signal_mask (mask);

  return mask;
}

// Combine set+free in one action.

static void
restore_signal_mask (void *mask)
{
  octave_set_signal_mask (mask);

  octave_free_signal_mask (mask);
}

enum system_exec_type { et_sync, et_async };

DEFUN (system, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {} system ("@var{string}")
@deftypefnx {} {} system ("@var{string}", @var{return_output})
@deftypefnx {} {} system ("@var{string}", @var{return_output}, @var{type})
@deftypefnx {} {[@var{status}, @var{output}] =} system (@dots{})
Execute a shell command specified by @var{string}.

If @var{system} is called with one or more output arguments, or if the optional
argument @var{return_output} is true and the subprocess is started
synchronously, then the output from the command is returned as a variable.
Otherwise, if the subprocess is executed synchronously, its output is sent to
the standard output.  To send the output of a command executed with
@code{system} through the pager, use a command like

@example
@group
[~, text] = system ("cmd");
more on;
disp (text);
@end group
@end example

@noindent
or

@example
@group
more on;
printf ("%s\n", nthargout (2, "system", "cmd"));
@end group
@end example

If the optional argument @var{type} is @qcode{"async"}, the process is started
in the background and the process ID of the child process is returned
immediately.  Otherwise, the child process is started and Octave waits until it
exits.  If the @var{type} argument is omitted, it defaults to the value
@qcode{"sync"}.

The @code{system} function can return two values.  The first is the exit status
of the command and the second is any output from the command that was written
to the standard output stream.  For example,

@example
[status, output] = system ("echo foo & exit 2");
@end example

@noindent
will set the variable @var{output} to the string @samp{foo}, and the variable
@var{status} to the integer @samp{2}.

For commands run asynchronously, @var{status} is the process id of the command
shell that is started to run the command.

The shell used for executing commands varies with operating system and is
typically @file{/bin/sh} for UNIX systems and @nospell{@file{cmd.exe}} for
Windows systems.
@seealso{unix, dos}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin == 0 || nargin > 3)
    print_usage ();

  system_exec_type type = et_sync;
  if (nargin == 3)
    {
      std::string type_str = args(2).xstring_value ("system: TYPE must be a string");

      if (type_str == "sync")
        type = et_sync;
      else if (type_str == "async")
        type = et_async;
      else
        error (R"(system: TYPE must be "sync" or "async")");
    }

  octave_value_list retval;

  bool return_output = (nargin == 1 && nargout > 1);

  if (nargin > 1)
    {
      try
        {
          return_output = args(1).is_true ();
        }
      catch (execution_exception& ee)
        {
          error (ee, "system: RETURN_OUTPUT must be boolean value true or false");
        }
    }

  if (return_output && type == et_async)
    error ("system: can't return output from commands run asynchronously");

  std::string cmd_str = args(0).xstring_value ("system: first argument must be a string");

#if defined (OCTAVE_USE_WINDOWS_API)
  // Work around weird double-quote handling on Windows systems.
  if (type == et_sync)
    cmd_str = '"' + cmd_str + '"';
#endif

  unwind_action restore_mask
  ([] (void *mask) { restore_signal_mask (mask); }, get_signal_mask ());

  octave_unblock_async_signals ();
  octave_unblock_signal_by_name ("SIGTSTP");

  if (type == et_async)
    retval(0) = octave_async_system_wrapper (cmd_str.c_str ());
  else if (return_output)
    retval = run_command_and_return_output (cmd_str);
  else
    {
      int status = sys::system (cmd_str);

      // The value in status is as returned by waitpid.  If
      // the process exited normally, extract the actual exit
      // status of the command.  Otherwise, return 127 as a
      // failure code.

      if (sys::wifexited (status))
        status = sys::wexitstatus (status);

      retval(0) = status;
    }

  return retval;
}

/*
%!test
%! cmd = ls_command ();
%! [status, output] = system (cmd);
%! assert (status, 0);
%! assert (ischar (output));
%! assert (! isempty (output));

%!error system ()
%!error system (1, 2, 3)
*/

static octave_value
find_config_info (const octave_scalar_map& m, const std::string& key)
{
  if (m.isfield (key))
    {
      Cell c = m.contents (key);

      if (! c.isempty ())
        return c(0);
    }

  return octave_value ();
}

DEFUN (__octave_config_info__, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{S} =} __octave_config_info__ ()
@deftypefnx {} {@var{S} =} __octave_config_info__ (@var{option})
Return a structure containing configuration and installation information for
Octave.

If @var{option} is a string, return the configuration information for the
specified option.

@seealso{computer}
@end deftypefn */)
{
  static octave_scalar_map config;
  static octave_scalar_map build_env;
  static octave_scalar_map build_features;

  static bool initialized = false;

  if (! initialized)
    {
      std::map<std::string, octave_value> conf_info_map
      = {{ "DEFAULT_PAGER", config::default_pager () },

#if defined (OCTAVE_ENABLE_64)
        { "ENABLE_64", true },
#else
        { "ENABLE_64", false },
#endif

#if defined (OCTAVE_ENABLE_COMMAND_LINE_PUSH_PARSER)
        { "ENABLE_COMMAND_LINE_PUSH_PARSER", true },
#else
        { "ENABLE_COMMAND_LINE_PUSH_PARSER", false },
#endif

#if defined (ENABLE_DOCS)
        { "ENABLE_DOCS", true },
#else
        { "ENABLE_DOCS", false },
#endif

#if defined (OCTAVE_ENABLE_FLOAT_TRUNCATE)
        { "ENABLE_FLOAT_TRUNCATE", true },
#else
        { "ENABLE_FLOAT_TRUNCATE", false },
#endif

#if defined (OCTAVE_ENABLE_OPENMP)
        { "ENABLE_OPENMP", true },
#else
        { "ENABLE_OPENMP", false },
#endif

        { "api_version", OCTAVE_API_VERSION },
        { "archlibdir", config::arch_lib_dir () },
        { "bindir", config::bin_dir () },
        { "canonical_host_type", config::canonical_host_type () },
        { "datadir", config::data_dir () },
        { "datarootdir", config::dataroot_dir () },
        { "fcnfiledir", config::fcn_file_dir () },
        { "fftw_version", fftw_version () },
        { "fftwf_version", fftwf_version () },
        { "imagedir", config::image_dir () },
        { "includedir", config::include_dir () },
        { "infodir", config::info_dir () },
        { "libdir", config::lib_dir () },
        { "libexecdir", config::libexec_dir () },
        // Each library and executable has its own definition of the hg
        // id.  We check for consistency when Octave starts so we just
        // store and report one of them here.
        { "hg_id", liboctinterp_hg_id () },
        { "localapiarchlibdir", config::local_api_arch_lib_dir () },
        { "localapifcnfiledir", config::local_api_fcn_file_dir () },
        { "localapioctfiledir", config::local_api_oct_file_dir () },
        { "localarchlibdir", config::local_arch_lib_dir () },
        { "localfcnfiledir", config::local_fcn_file_dir () },
        { "localoctfiledir", config::local_oct_file_dir () },
        { "localstartupfiledir", config::local_startupfile_dir () },
        { "localverarchlibdir", config::local_ver_arch_lib_dir () },
        { "localverfcnfiledir", config::local_ver_fcn_file_dir () },
        { "localveroctfiledir", config::local_ver_oct_file_dir () },
        { "major_version", STRINGIFY (OCTAVE_MAJOR_VERSION) },
        { "man1dir", config::man1_dir () },
        { "man1ext", config::man1_ext () },
        { "mandir", config::man_dir () },
        { "minor_version", STRINGIFY (OCTAVE_MINOR_VERSION) },
        { "octdatadir", config::oct_data_dir () },
        { "octdocdir", config::oct_doc_dir () },
        { "octetcdir", config::oct_etc_dir () },
        { "octfiledir", config::oct_file_dir () },
        { "octfontsdir", config::oct_fonts_dir () },
        { "octincludedir", config::oct_include_dir () },
        { "octlibdir", config::oct_lib_dir () },
        { "octtestsdir", config::oct_tests_dir () },
        { "patch_version", STRINGIFY (OCTAVE_PATCH_VERSION) },
        { "release_date", OCTAVE_RELEASE_DATE },
        { "startupfiledir", config::startupfile_dir () },
        { "version", OCTAVE_VERSION }
      };

      std::map<std::string, octave_value> build_env_map
      = {{ "AMD_CPPFLAGS", build_env::AMD_CPPFLAGS },
        { "AMD_LDFLAGS", build_env::AMD_LDFLAGS },
        { "AMD_LIBS", build_env::AMD_LIBS },
        { "AR", build_env::AR },
        { "ARFLAGS", build_env::ARFLAGS },
        { "ARPACK_CPPFLAGS", build_env::ARPACK_CPPFLAGS },
        { "ARPACK_LDFLAGS", build_env::ARPACK_LDFLAGS },
        { "ARPACK_LIBS", build_env::ARPACK_LIBS },
        { "BLAS_LIBS", build_env::BLAS_LIBS },
        { "CAMD_CPPFLAGS", build_env::CAMD_CPPFLAGS },
        { "CAMD_LDFLAGS", build_env::CAMD_LDFLAGS },
        { "CAMD_LIBS", build_env::CAMD_LIBS },
        { "CARBON_LIBS", build_env::CARBON_LIBS },
        { "CC", build_env::CC },
        { "CCOLAMD_CPPFLAGS", build_env::CCOLAMD_CPPFLAGS },
        { "CCOLAMD_LDFLAGS", build_env::CCOLAMD_LDFLAGS },
        { "CCOLAMD_LIBS", build_env::CCOLAMD_LIBS },
        { "CFLAGS", build_env::CFLAGS },
        { "CHOLMOD_CPPFLAGS", build_env::CHOLMOD_CPPFLAGS },
        { "CHOLMOD_LDFLAGS", build_env::CHOLMOD_LDFLAGS },
        { "CHOLMOD_LIBS", build_env::CHOLMOD_LIBS },
        { "COLAMD_CPPFLAGS", build_env::COLAMD_CPPFLAGS },
        { "COLAMD_LDFLAGS", build_env::COLAMD_LDFLAGS },
        { "COLAMD_LIBS", build_env::COLAMD_LIBS },
        { "CPICFLAG", build_env::CPICFLAG },
        { "CPPFLAGS", build_env::CPPFLAGS },
        { "CURL_CPPFLAGS", build_env::CURL_CPPFLAGS },
        { "CURL_LDFLAGS", build_env::CURL_LDFLAGS },
        { "CURL_LIBS", build_env::CURL_LIBS },
        { "CXSPARSE_CPPFLAGS", build_env::CXSPARSE_CPPFLAGS },
        { "CXSPARSE_LDFLAGS", build_env::CXSPARSE_LDFLAGS },
        { "CXSPARSE_LIBS", build_env::CXSPARSE_LIBS },
        { "CXX", build_env::CXX },
        { "CXXCPP", build_env::CXXCPP },
        { "CXXFLAGS", build_env::CXXFLAGS },
        { "CXXPICFLAG", build_env::CXXPICFLAG },
        { "DEFS", build_env::DEFS },
        { "DL_LDFLAGS", build_env::DL_LDFLAGS },
        { "GCC_VERSION", build_env::GCC_VERSION },
        { "GXX_VERSION", build_env::GXX_VERSION },
        { "EXEEXT", build_env::EXEEXT },
        { "F77", build_env::F77 },
        { "F77_FLOAT_STORE_FLAG", build_env::F77_FLOAT_STORE_FLAG },
        { "F77_INTEGER_8_FLAG", build_env::F77_INTEGER_8_FLAG },
        { "FFLAGS", build_env::FFLAGS },
        { "FFTW3_CPPFLAGS", build_env::FFTW3_CPPFLAGS },
        { "FFTW3_LDFLAGS", build_env::FFTW3_LDFLAGS },
        { "FFTW3_LIBS", build_env::FFTW3_LIBS },
        { "FFTW3F_CPPFLAGS", build_env::FFTW3F_CPPFLAGS },
        { "FFTW3F_LDFLAGS", build_env::FFTW3F_LDFLAGS },
        { "FFTW3F_LIBS", build_env::FFTW3F_LIBS },
        { "FLIBS", build_env::FLIBS },
        { "FLTK_CPPFLAGS", build_env::FLTK_CPPFLAGS },
        { "FLTK_LDFLAGS", build_env::FLTK_LDFLAGS },
        { "FLTK_LIBS", build_env::FLTK_LIBS },
        { "FONTCONFIG_CPPFLAGS", build_env::FONTCONFIG_CPPFLAGS },
        { "FONTCONFIG_LIBS", build_env::FONTCONFIG_LIBS },
        { "FPICFLAG", build_env::FPICFLAG },
        { "FT2_CPPFLAGS", build_env::FT2_CPPFLAGS },
        { "FT2_LIBS", build_env::FT2_LIBS },
        { "GLPK_CPPFLAGS", build_env::GLPK_CPPFLAGS },
        { "GLPK_LDFLAGS", build_env::GLPK_LDFLAGS },
        { "GLPK_LIBS", build_env::GLPK_LIBS },
        { "GNUPLOT", build_env::GNUPLOT },
        { "HDF5_CPPFLAGS", build_env::HDF5_CPPFLAGS },
        { "HDF5_LDFLAGS", build_env::HDF5_LDFLAGS },
        { "HDF5_LIBS", build_env::HDF5_LIBS },
        { "LAPACK_LIBS", build_env::LAPACK_LIBS },
        { "LDFLAGS", build_env::LDFLAGS },
        { "LD_STATIC_FLAG", build_env::LD_STATIC_FLAG },
        { "LEX", build_env::LEX },
        { "LEXLIB", build_env::LEXLIB },
        { "LFLAGS", build_env::LFLAGS },
        { "LIBOCTAVE", build_env::LIBOCTAVE },
        { "LIBOCTINTERP", build_env::LIBOCTINTERP },
        { "LIBS", build_env::LIBS },
        { "LN_S", build_env::LN_S },
        { "MAGICK_CPPFLAGS", build_env::MAGICK_CPPFLAGS },
        { "MAGICK_LDFLAGS", build_env::MAGICK_LDFLAGS },
        { "MAGICK_LIBS", build_env::MAGICK_LIBS },
        { "MKOCTFILE_DL_LDFLAGS", build_env::MKOCTFILE_DL_LDFLAGS },
        { "OCTAVE_LINK_DEPS", build_env::OCTAVE_LINK_DEPS },
        { "OCTAVE_LINK_OPTS", build_env::OCTAVE_LINK_OPTS },
        { "OCT_LINK_DEPS", build_env::OCT_LINK_DEPS },
        { "OCT_LINK_OPTS", build_env::OCT_LINK_OPTS },
        { "OPENGL_LIBS", build_env::OPENGL_LIBS },
        { "PCRE_CPPFLAGS", build_env::PCRE_CPPFLAGS },
        { "PCRE_LDFLAGS", build_env::PCRE_LDFLAGS },
        { "PCRE_LIBS", build_env::PCRE_LIBS },
        { "PTHREAD_CFLAGS", build_env::PTHREAD_CFLAGS },
        { "PTHREAD_LIBS", build_env::PTHREAD_LIBS },
        { "QHULL_CPPFLAGS", build_env::QHULL_CPPFLAGS },
        { "QHULL_LDFLAGS", build_env::QHULL_LDFLAGS },
        { "QHULL_LIBS", build_env::QHULL_LIBS },
        { "QRUPDATE_CPPFLAGS", build_env::QRUPDATE_CPPFLAGS },
        { "QRUPDATE_LDFLAGS", build_env::QRUPDATE_LDFLAGS },
        { "QRUPDATE_LIBS", build_env::QRUPDATE_LIBS },
        { "QT_CPPFLAGS", build_env::QT_CPPFLAGS },
        { "QT_LDFLAGS", build_env::QT_LDFLAGS },
        { "QT_LIBS", build_env::QT_LIBS },
        { "QT_OPENGL_LIBS", build_env::QT_OPENGL_LIBS },
        { "RANLIB", build_env::RANLIB },
        { "RDYNAMIC_FLAG", build_env::RDYNAMIC_FLAG },
        { "READLINE_LIBS", build_env::READLINE_LIBS },
        { "SHARED_LIBS", build_env::SHARED_LIBS },
        { "SH_LDFLAGS", build_env::SH_LDFLAGS },
        { "STATIC_LIBS", build_env::STATIC_LIBS },
        { "SUITESPARSECONFIG_LIBS", build_env::SUITESPARSECONFIG_LIBS },
        { "UMFPACK_CPPFLAGS", build_env::UMFPACK_CPPFLAGS },
        { "UMFPACK_LDFLAGS", build_env::UMFPACK_LDFLAGS },
        { "UMFPACK_LIBS", build_env::UMFPACK_LIBS },
        { "WARN_CFLAGS", build_env::WARN_CFLAGS },
        { "WARN_CXXFLAGS", build_env::WARN_CXXFLAGS },
        { "X11_INCFLAGS", build_env::X11_INCFLAGS },
        { "X11_LIBS", build_env::X11_LIBS },
        { "XTRA_CFLAGS", build_env::XTRA_CFLAGS },
        { "XTRA_CXXFLAGS", build_env::XTRA_CXXFLAGS },
        { "YACC", build_env::YACC },
        { "YFLAGS", build_env::YFLAGS },
        { "Z_CPPFLAGS", build_env::Z_CPPFLAGS },
        { "Z_LDFLAGS", build_env::Z_LDFLAGS },
        { "Z_LIBS", build_env::Z_LIBS },
        { "config_opts", build_env::config_opts }
      };

      config = octave_scalar_map (conf_info_map);
      build_env = octave_scalar_map (build_env_map);
      build_features = build_env::features ();

      bool unix_system = true;
      bool mac_system = false;
      bool windows_system = false;

#if defined (__WIN32__)
      windows_system = true;
#if ! defined (__CYGWIN__)
      unix_system = false;
#endif
#endif

#if defined (OCTAVE_USE_OS_X_API)
      mac_system = true;
#endif

      config.assign ("unix", octave_value (unix_system));
      config.assign ("mac", octave_value (mac_system));
      config.assign ("windows", octave_value (windows_system));

      mach_info::float_format ff = mach_info::native_float_format ();
      config.assign ("float_format",
                     octave_value (mach_info::float_format_as_string (ff)));

      config.assign ("words_big_endian",
                     octave_value (mach_info::words_big_endian ()));

      config.assign ("words_little_endian",
                     octave_value (mach_info::words_little_endian ()));

      config.assign ("build_environment", octave_value (build_env));

      config.assign ("build_features", octave_value (build_features));

      initialized = true;
    }

  int nargin = args.length ();

  if (nargin > 1)
    print_usage ();

  octave_value_list retval;

  if (nargin == 1)
    {
      std::string arg = args(
                          0).xstring_value ("__octave_config_info__: OPTION argument must be a string");

      octave_value info = find_config_info (config, arg);

      if (info.is_undefined ())
        info = find_config_info (build_env, arg);

      if (info.is_undefined ())
        info = find_config_info (build_features, arg);

      if (info.is_undefined ())
        error ("__octave_config_info__: no info for '%s'", arg.c_str ());

      return info;
    }
  else
    retval = ovl (config);

  return retval;
}

/*
%!assert (ischar (__octave_config_info__ ("version")))
%!assert (__octave_config_info__ ("version"), OCTAVE_VERSION ())
%!test
%! x = __octave_config_info__ ();
%! assert (isstruct (x));
%! assert (! isempty (x));
%! assert (x.version, OCTAVE_VERSION ());

%!error __octave_config_info__ (1, 2)
*/

OCTAVE_END_NAMESPACE(octave)

#if defined (__GNUG__) && defined (DEBUG_NEW_DELETE)

int debug_new_delete = 0;

typedef void (*vfp)(void);
extern vfp __new_handler;

void *
__builtin_new (std::size_t sz)
{
  void *p;

  // malloc (0) is unpredictable; avoid it.
  if (sz == 0)
    sz = 1;
  p = std::malloc (sz);
  while (p == 0)
    {
      (*__new_handler) ();
      p = std::malloc (sz);
    }

  if (debug_new_delete)
    std::cerr << "__builtin_new: " << p << std::endl;

  return p;
}

void
__builtin_delete (void *ptr)
{
  if (debug_new_delete)
    std::cerr << "__builtin_delete: " << ptr << std::endl;

  free (ptr);
}

#endif
