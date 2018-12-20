/*

Copyright (C) 1995-2018 John W. Eaton

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
#include "call-stack.h"
#include "defaults.h"
#include "defun.h"
#include "error.h"
#include "file-io.h"
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
  octave::unwind_protect frame;

  iprocstream *cmd = new iprocstream (cmd_str.c_str ());
  frame.add_delete (cmd);

  octave::child_list& kids
    = octave::__get_child_list__ ("run_command_and_return_output");
  frame.add_method (kids, &octave::child_list::remove, cmd->pid ());

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

  if (octave::sys::wifexited (cmd_status))
    cmd_status = octave::sys::wexitstatus (cmd_status);
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

If the optional argument @var{type} is @qcode{"async"}, the process is
started in the background and the process ID of the child process is
returned immediately.  Otherwise, the child process is started and Octave
waits until it exits.  If the @var{type} argument is omitted, it defaults to
the value @qcode{"sync"}.

If @var{system} is called with one or more output arguments, or if the
optional argument @var{return_output} is true and the subprocess is started
synchronously, then the output from the command is returned as a variable.
Otherwise, if the subprocess is executed synchronously, its output is sent
to the standard output.  To send the output of a command executed with
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

The @code{system} function can return two values.  The first is the
exit status of the command and the second is any output from the
command that was written to the standard output stream.  For example,

@example
[status, output] = system ("echo foo & exit 2");
@end example

@noindent
will set the variable @var{output} to the string @samp{foo}, and the
variable @var{status} to the integer @samp{2}.

For commands run asynchronously, @var{status} is the process id of the
command shell that is started to run the command.

The shell used for executing commands varies with operating system and is
typically @file{/bin/sh} for UNIX systems and @nospell{@file{cmd.exe}} for
Windows
systems.
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
      catch (octave::execution_exception& e)
        {
          error (e, "system: RETURN_OUTPUT must be boolean value true or false");
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

  octave::unwind_protect frame;

  frame.add_fcn (restore_signal_mask, get_signal_mask ());

  octave_unblock_async_signals ();

  if (type == et_async)
    retval(0) = octave_async_system_wrapper (cmd_str.c_str ());
  else if (return_output)
    retval = run_command_and_return_output (cmd_str);
  else
    {
      int status = system (cmd_str.c_str ());

      // The value in status is as returned by waitpid.  If
      // the process exited normally, extract the actual exit
      // status of the command.  Otherwise, return 127 as a
      // failure code.

      if (octave::sys::wifexited (status))
        status = octave::sys::wexitstatus (status);

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
@deftypefn  {} {} __octave_config_info__ ()
@deftypefnx {} {} __octave_config_info__ (@var{option})
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
        = {{ "DEFAULT_PAGER", octave::config::default_pager () },

#if defined (OCTAVE_ENABLE_64)
           { "ENABLE_64", true },
#else
           { "ENABLE_64", false },
#endif

#if defined (OCTAVE_ENABLE_ATOMIC_REFCOUNT)
           { "ENABLE_ATOMIC_REFCOUNT", true },
#else
           { "ENABLE_ATOMIC_REFCOUNT", false },
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

#if defined (ENABLE_JIT)
           { "ENABLE_JIT", true },
#else
           { "ENABLE_JIT", false },
#endif

#if defined (OCTAVE_ENABLE_OPENMP)
           { "ENABLE_OPENMP", true },
#else
           { "ENABLE_OPENMP", false },
#endif

           { "api_version", OCTAVE_API_VERSION },
           { "archlibdir", octave::config::arch_lib_dir () },
           { "bindir", octave::config::bin_dir () },
           { "canonical_host_type", octave::config::canonical_host_type () },
           { "datadir", octave::config::data_dir () },
           { "datarootdir", octave::config::dataroot_dir () },
           { "fcnfiledir", octave::config::fcn_file_dir () },
           { "fftw_version", octave::fftw_version () },
           { "fftwf_version", octave::fftwf_version () },
           { "imagedir", octave::config::image_dir () },
           { "includedir", octave::config::include_dir () },
           { "infodir", octave::config::info_dir () },
           { "libdir", octave::config::lib_dir () },
           { "libexecdir", octave::config::libexec_dir () },
           // Each library and executable has its own definition of the hg
           // id.  We check for consistency when Octave starts so we just
           // store and report one of them here.
           { "hg_id", liboctinterp_hg_id () },
           { "localapiarchlibdir", octave::config::local_api_arch_lib_dir () },
           { "localapifcnfiledir", octave::config::local_api_fcn_file_dir () },
           { "localapioctfiledir", octave::config::local_api_oct_file_dir () },
           { "localarchlibdir", octave::config::local_arch_lib_dir () },
           { "localfcnfiledir", octave::config::local_fcn_file_dir () },
           { "localoctfiledir", octave::config::local_oct_file_dir () },
           { "localstartupfiledir", octave::config::local_startupfile_dir () },
           { "localverarchlibdir", octave::config::local_ver_arch_lib_dir () },
           { "localverfcnfiledir", octave::config::local_ver_fcn_file_dir () },
           { "localveroctfiledir", octave::config::local_ver_oct_file_dir () },
           { "man1dir", octave::config::man1_dir () },
           { "man1ext", octave::config::man1_ext () },
           { "mandir", octave::config::man_dir () },
           { "octdatadir", octave::config::oct_data_dir () },
           { "octdocdir", octave::config::oct_doc_dir () },
           { "octetcdir", octave::config::oct_etc_dir () },
           { "octfiledir", octave::config::oct_file_dir () },
           { "octfontsdir", octave::config::oct_fonts_dir () },
           { "octincludedir", octave::config::oct_include_dir () },
           { "octlibdir", octave::config::oct_lib_dir () },
           { "octtestsdir", octave::config::oct_tests_dir () },
           { "release_date", OCTAVE_RELEASE_DATE },
           { "startupfiledir", octave::config::startupfile_dir () },
           { "version", OCTAVE_VERSION }};

      std::map<std::string, octave_value> build_env_map
        = {{ "AMD_CPPFLAGS", octave::build_env::AMD_CPPFLAGS },
           { "AMD_LDFLAGS", octave::build_env::AMD_LDFLAGS },
           { "AMD_LIBS", octave::build_env::AMD_LIBS },
           { "AR", octave::build_env::AR },
           { "ARFLAGS", octave::build_env::ARFLAGS },
           { "ARPACK_CPPFLAGS", octave::build_env::ARPACK_CPPFLAGS },
           { "ARPACK_LDFLAGS", octave::build_env::ARPACK_LDFLAGS },
           { "ARPACK_LIBS", octave::build_env::ARPACK_LIBS },
           { "BLAS_LIBS", octave::build_env::BLAS_LIBS },
           { "CAMD_CPPFLAGS", octave::build_env::CAMD_CPPFLAGS },
           { "CAMD_LDFLAGS", octave::build_env::CAMD_LDFLAGS },
           { "CAMD_LIBS", octave::build_env::CAMD_LIBS },
           { "CARBON_LIBS", octave::build_env::CARBON_LIBS },
           { "CC", octave::build_env::CC },
           { "CCOLAMD_CPPFLAGS", octave::build_env::CCOLAMD_CPPFLAGS },
           { "CCOLAMD_LDFLAGS", octave::build_env::CCOLAMD_LDFLAGS },
           { "CCOLAMD_LIBS", octave::build_env::CCOLAMD_LIBS },
           { "CFLAGS", octave::build_env::CFLAGS },
           { "CHOLMOD_CPPFLAGS", octave::build_env::CHOLMOD_CPPFLAGS },
           { "CHOLMOD_LDFLAGS", octave::build_env::CHOLMOD_LDFLAGS },
           { "CHOLMOD_LIBS", octave::build_env::CHOLMOD_LIBS },
           { "COLAMD_CPPFLAGS", octave::build_env::COLAMD_CPPFLAGS },
           { "COLAMD_LDFLAGS", octave::build_env::COLAMD_LDFLAGS },
           { "COLAMD_LIBS", octave::build_env::COLAMD_LIBS },
           { "CPICFLAG", octave::build_env::CPICFLAG },
           { "CPPFLAGS", octave::build_env::CPPFLAGS },
           { "CURL_CPPFLAGS", octave::build_env::CURL_CPPFLAGS },
           { "CURL_LDFLAGS", octave::build_env::CURL_LDFLAGS },
           { "CURL_LIBS", octave::build_env::CURL_LIBS },
           { "CXSPARSE_CPPFLAGS", octave::build_env::CXSPARSE_CPPFLAGS },
           { "CXSPARSE_LDFLAGS", octave::build_env::CXSPARSE_LDFLAGS },
           { "CXSPARSE_LIBS", octave::build_env::CXSPARSE_LIBS },
           { "CXX", octave::build_env::CXX },
           { "CXXCPP", octave::build_env::CXXCPP },
           { "CXXFLAGS", octave::build_env::CXXFLAGS },
           { "CXXPICFLAG", octave::build_env::CXXPICFLAG },
           { "DEFS", octave::build_env::DEFS },
           { "DL_LD", octave::build_env::DL_LD },
           { "DL_LDFLAGS", octave::build_env::DL_LDFLAGS },
           { "DL_LIBS", octave::build_env::DL_LIBS },
           { "GCC_VERSION", octave::build_env::GCC_VERSION },
           { "GXX_VERSION", octave::build_env::GXX_VERSION },
           { "EXEEXT", octave::build_env::EXEEXT },
           { "F77", octave::build_env::F77 },
           { "F77_FLOAT_STORE_FLAG", octave::build_env::F77_FLOAT_STORE_FLAG },
           { "F77_INTEGER_8_FLAG", octave::build_env::F77_INTEGER_8_FLAG },
           { "FFLAGS", octave::build_env::FFLAGS },
           { "FFTW3_CPPFLAGS", octave::build_env::FFTW3_CPPFLAGS },
           { "FFTW3_LDFLAGS", octave::build_env::FFTW3_LDFLAGS },
           { "FFTW3_LIBS", octave::build_env::FFTW3_LIBS },
           { "FFTW3F_CPPFLAGS", octave::build_env::FFTW3F_CPPFLAGS },
           { "FFTW3F_LDFLAGS", octave::build_env::FFTW3F_LDFLAGS },
           { "FFTW3F_LIBS", octave::build_env::FFTW3F_LIBS },
           { "FLIBS", octave::build_env::FLIBS },
           { "FLTK_CPPFLAGS", octave::build_env::FLTK_CPPFLAGS },
           { "FLTK_LDFLAGS", octave::build_env::FLTK_LDFLAGS },
           { "FLTK_LIBS", octave::build_env::FLTK_LIBS },
           { "FONTCONFIG_CPPFLAGS", octave::build_env::FONTCONFIG_CPPFLAGS },
           { "FONTCONFIG_LIBS", octave::build_env::FONTCONFIG_LIBS },
           { "FPICFLAG", octave::build_env::FPICFLAG },
           { "FT2_CPPFLAGS", octave::build_env::FT2_CPPFLAGS },
           { "FT2_LIBS", octave::build_env::FT2_LIBS },
           { "GLPK_CPPFLAGS", octave::build_env::GLPK_CPPFLAGS },
           { "GLPK_LDFLAGS", octave::build_env::GLPK_LDFLAGS },
           { "GLPK_LIBS", octave::build_env::GLPK_LIBS },
           { "GNUPLOT", octave::build_env::GNUPLOT },
           { "HDF5_CPPFLAGS", octave::build_env::HDF5_CPPFLAGS },
           { "HDF5_LDFLAGS", octave::build_env::HDF5_LDFLAGS },
           { "HDF5_LIBS", octave::build_env::HDF5_LIBS },
           { "LAPACK_LIBS", octave::build_env::LAPACK_LIBS },
           { "LDFLAGS", octave::build_env::LDFLAGS },
           { "LD_CXX", octave::build_env::LD_CXX },
           { "LD_STATIC_FLAG", octave::build_env::LD_STATIC_FLAG },
           { "LEX", octave::build_env::LEX },
           { "LEXLIB", octave::build_env::LEXLIB },
           { "LFLAGS", octave::build_env::LFLAGS },
           { "LIBOCTAVE", octave::build_env::LIBOCTAVE },
           { "LIBOCTINTERP", octave::build_env::LIBOCTINTERP },
           { "LIBS", octave::build_env::LIBS },
           { "LLVM_CPPFLAGS", octave::build_env::LLVM_CPPFLAGS },
           { "LLVM_LDFLAGS", octave::build_env::LLVM_LDFLAGS },
           { "LLVM_LIBS", octave::build_env::LLVM_LIBS },
           { "LN_S", octave::build_env::LN_S },
           { "MAGICK_CPPFLAGS", octave::build_env::MAGICK_CPPFLAGS },
           { "MAGICK_LDFLAGS", octave::build_env::MAGICK_LDFLAGS },
           { "MAGICK_LIBS", octave::build_env::MAGICK_LIBS },
           { "MKOCTFILE_DL_LDFLAGS", octave::build_env::MKOCTFILE_DL_LDFLAGS },
           { "OCTAVE_LINK_DEPS", octave::build_env::OCTAVE_LINK_DEPS },
           { "OCTAVE_LINK_OPTS", octave::build_env::OCTAVE_LINK_OPTS },
           { "OCT_LINK_DEPS", octave::build_env::OCT_LINK_DEPS },
           { "OCT_LINK_OPTS", octave::build_env::OCT_LINK_OPTS },
           { "OPENGL_LIBS", octave::build_env::OPENGL_LIBS },
           { "OSMESA_CPPFLAGS", octave::build_env::OSMESA_CPPFLAGS },
           { "OSMESA_LDFLAGS", octave::build_env::OSMESA_LDFLAGS },
           { "OSMESA_LIBS", octave::build_env::OSMESA_LIBS },
           { "PCRE_CPPFLAGS", octave::build_env::PCRE_CPPFLAGS },
           { "PCRE_LDFLAGS", octave::build_env::PCRE_LDFLAGS },
           { "PCRE_LIBS", octave::build_env::PCRE_LIBS },
           { "PTHREAD_CFLAGS", octave::build_env::PTHREAD_CFLAGS },
           { "PTHREAD_LIBS", octave::build_env::PTHREAD_LIBS },
           { "QHULL_CPPFLAGS", octave::build_env::QHULL_CPPFLAGS },
           { "QHULL_LDFLAGS", octave::build_env::QHULL_LDFLAGS },
           { "QHULL_LIBS", octave::build_env::QHULL_LIBS },
           { "QRUPDATE_CPPFLAGS", octave::build_env::QRUPDATE_CPPFLAGS },
           { "QRUPDATE_LDFLAGS", octave::build_env::QRUPDATE_LDFLAGS },
           { "QRUPDATE_LIBS", octave::build_env::QRUPDATE_LIBS },
           { "QT_CPPFLAGS", octave::build_env::QT_CPPFLAGS },
           { "QT_LDFLAGS", octave::build_env::QT_LDFLAGS },
           { "QT_LIBS", octave::build_env::QT_LIBS },
           { "QT_OPENGL_LIBS", octave::build_env::QT_OPENGL_LIBS },
           { "RANLIB", octave::build_env::RANLIB },
           { "RDYNAMIC_FLAG", octave::build_env::RDYNAMIC_FLAG },
           { "READLINE_LIBS", octave::build_env::READLINE_LIBS },
           { "SHARED_LIBS", octave::build_env::SHARED_LIBS },
           { "SH_LD", octave::build_env::SH_LD },
           { "SH_LDFLAGS", octave::build_env::SH_LDFLAGS },
           { "STATIC_LIBS", octave::build_env::STATIC_LIBS },
           { "SUITESPARSECONFIG_LIBS", octave::build_env::SUITESPARSECONFIG_LIBS },
           { "TERM_LIBS", octave::build_env::TERM_LIBS },
           { "UMFPACK_CPPFLAGS", octave::build_env::UMFPACK_CPPFLAGS },
           { "UMFPACK_LDFLAGS", octave::build_env::UMFPACK_LDFLAGS },
           { "UMFPACK_LIBS", octave::build_env::UMFPACK_LIBS },
           { "WARN_CFLAGS", octave::build_env::WARN_CFLAGS },
           { "WARN_CXXFLAGS", octave::build_env::WARN_CXXFLAGS },
           { "X11_INCFLAGS", octave::build_env::X11_INCFLAGS },
           { "X11_LIBS", octave::build_env::X11_LIBS },
           { "XTRA_CFLAGS", octave::build_env::XTRA_CFLAGS },
           { "XTRA_CXXFLAGS", octave::build_env::XTRA_CXXFLAGS },
           { "YACC", octave::build_env::YACC },
           { "YFLAGS", octave::build_env::YFLAGS },
           { "Z_CPPFLAGS", octave::build_env::Z_CPPFLAGS },
           { "Z_LDFLAGS", octave::build_env::Z_LDFLAGS },
           { "Z_LIBS", octave::build_env::Z_LIBS },
           { "config_opts", octave::build_env::config_opts }};

      config = octave_scalar_map (conf_info_map);
      build_env = octave_scalar_map (build_env_map);
      build_features = octave::build_env::features ();

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

      octave::mach_info::float_format ff = octave::mach_info::native_float_format ();
      config.assign ("float_format",
                     octave_value (octave::mach_info::float_format_as_string (ff)));

      config.assign ("words_big_endian",
                     octave_value (octave::mach_info::words_big_endian ()));

      config.assign ("words_little_endian",
                     octave_value (octave::mach_info::words_little_endian ()));

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
      std::string arg = args(0).xstring_value ("__octave_config_info__: OPTION argument must be a string");

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

#if defined (__GNUG__) && defined (DEBUG_NEW_DELETE)

int debug_new_delete = 0;

typedef void (*vfp)(void);
extern vfp __new_handler;

void *
__builtin_new (size_t sz)
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
