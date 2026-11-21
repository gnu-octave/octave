########################################################################
##
## Copyright (C) 2006-2026 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

## -*- texinfo -*-
## @deftypefn  {} {} mkoctfile [-options] file @dots{}
## @deftypefnx {} {[@var{output}, @var{status}] =} mkoctfile (@dots{})
##
## The @code{mkoctfile} function compiles source code written in C, C++, or
## Fortran.  Depending on the options used with @code{mkoctfile}, the
## compiled code can be called within Octave or can be used as a stand-alone
## application.
##
## @code{mkoctfile} can be called from the shell prompt or from the Octave
## prompt.  Calling it from the Octave prompt simply delegates the call to
## the shell prompt.  Any output is stored in the @var{output} variable and
## the exit status is in the @var{status} variable.  If called with no outputs
## and the compilation fails then Octave will emit an error.  If the programmer
## requests @var{output} or @var{status}, however, Octave will merely issue
## a warning and it is the programmer's responsibility to verify the command
## was successful.
##
## @code{mkoctfile} accepts the following options, all of which are optional
## except for the filename of the code you wish to compile:
##
## @table @samp
## @item -I@var{dir}
## Add directory @var{dir} to compiler search path.
##
## @item -D@var{def}[=@var{val}]
## Define @var{def} in compile command.
##
## @item -U@var{def}
## Cancel any previous definition of @var{def}.
##
## @item -L@var{dir}
## Add directory @var{dir} to linker search path.
##
## @item -l@var{lib}
## Add library @var{lib} to link command.
##
## @item -pthread
## Use POSIX threads.
##
## @item -R@var{dir}
## Add @code{-R@var{dir}} to link command.
##
## @item @nospell{-Wl,@var{option}}
## Pass @var{option} to the linker like @nospell{@qcode{"-Wl,-rpath=@dots{}"}}.
## The quotes are needed since commas are interpreted as command separators.
##
## @item -W@var{warn}
## Pass @var{warn} option to the compiler.
##
## @item  -c
## @itemx --compile
## Compile, but do not link.
##
## @item  -M
## @itemx --depend
## Generate dependency files (@file{.d}) for C and C++ source files.
##
## @item -g
## Enable debugging options for compilers.
##
## @item  -o @var{file}
## @itemx --output @var{file}
## Output filename.@*
## Default extension is @file{.oct} (or @file{.mex} if @samp{--mex} is
## specified) unless linking a stand-alone executable.
##
## @item  -p @var{var}
## @itemx --print @var{var}
## Print configuration variable @var{var}.@*
## There are three categories of variables:
##
## Octave configuration variables that users may override with environment
## variables.  These are used in commands that @code{mkoctfile} executes.
##
## @example
##    ALL_CFLAGS                  FPICFLAG
##    ALL_CXXFLAGS                INCFLAGS
##    ALL_FFLAGS                  INCLUDEDIR
##    ALL_LDFLAGS                 LDFLAGS
##    CC                          LD_STATIC_FLAG
##    CFLAGS                      LIBDIR
##    CPICFLAG                    OCTAVE_LINK_OPTS
##    CPPFLAGS                    OCTINCLUDEDIR
##    CXX                         OCTAVE_LIBS
##    CXXFLAGS                    OCTAVE_LINK_DEPS
##    CXXLD                       OCTLIBDIR
##    CXXPICFLAG                  OCT_LINK_DEPS
##    DEPEND_FLAGS                OCT_LINK_OPTS
##    DL_LDFLAGS                  RDYNAMIC_FLAG
##    F77                         SPECIAL_MATH_LIB
##    F77_INTEGER8_FLAG           XTRA_CFLAGS
##    FFLAGS                      XTRA_CXXFLAGS
## @end example
##
## Octave configuration variables as above, but currently unused by
## @code{mkoctfile}.
##
## @example
## @group
##    AR                          FFTW_LIBS
##    BLAS_LIBS                   FLIBS
##    FFTW3F_LDFLAGS              LAPACK_LIBS
##    FFTW3F_LIBS                 LIBS
##    FFTW3_LDFLAGS               RANLIB
##    FFTW3_LIBS                  READLINE_LIBS
## @end group
## @end example
##
## Octave configuration variables that are provided for informational
## purposes only.  Except for @w{@samp{OCTAVE_HOME}}@ and
## @w{@samp{OCTAVE_EXEC_HOME}}, users may not override these variables.
##
## If @w{@env{OCTAVE_HOME}}@ or @w{@env{OCTAVE_EXEC_HOME}}@ are set in the
## environment, then other variables are adjusted accordingly with
## @w{@env{OCTAVE_HOME}}@ or @w{@env{OCTAVE_EXEC_HOME}}@ substituted for the
## original value of the directory specified by the @option{--prefix} or
## @option{--exec-prefix} options that were used when Octave was configured.
##
## @example
## @group
##    API_VERSION                 LOCALAPIOCTFILEDIR
##    ARCHLIBDIR                  LOCALAPIPKGDIR
##    BINDIR                      LOCALARCHLIBDIR
##    CANONICAL_HOST_TYPE         LOCALFCNFILEDIR
##    DATADIR                     LOCALOCTFILEDIR
##    DATAROOTDIR                 LOCALSTARTUPFILEDIR
##    DEFAULT_PAGER               LOCALVERARCHLIBDIR
##    EXEC_PREFIX                 LOCALVERFCNFILEDIR
##    EXEEXT                      LOCALVEROCTFILEDIR
##    FCNFILEDIR                  MAN1DIR
##    IMAGEDIR                    MAN1EXT
##    INFODIR                     MANDIR
##    INFOFILE                    OCTAVE_EXEC_HOME
##    LIBEXECDIR                  OCTAVE_HOME
##    LIBOCTAVE                   OCTAVE_VERSION
##    LIBOCTINTERP                OCTDATADIR
##    LIBOCTMEX                   OCTDOCDIR
##    LOCALAPIARCHLIBDIR          OCTFILEDIR
##    LOCALAPIFCNFILEDIR          OCTFONTSDIR
##                                STARTUPFILEDIR
## @end group
## @end example
##
## @item --link-stand-alone
## Link a stand-alone executable file.
##
## @item --mex
## Create a MEX file.@*
## Set the default output extension to @file{.mex}.
## Link to @file{liboctmex} instead of @file{liboctinterp} and
## @file{liboctave}.
##
## @item  -s
## @itemx --strip
## Strip output file.
##
## @item  -n
## @itemx --just-print
## @itemx --dry-run
## Print commands, but do not execute them.
##
## @item  -v
## @itemx --verbose
## Echo commands as they are executed.
##
## @item  -V
## @itemx --version
## Print version information.
##
## @item --quiet
## Ignored.  Intended to suppress output from compiler steps.
##
## @item file
## Compile or link @file{file}.@*
## Recognized file types are:
##
## @example
## @group
##   .c    C source
##   .cc   C++ source
##   .cp   C++ source
##   .cpp  C++ source
##   .CPP  C++ source
##   .cxx  C++ source
##   .c++  C++ source
##   .C    C++ source
##   .f    Fortran source (fixed form)
##   .F    Fortran source (fixed form)
##   .f90  Fortran source (free form)
##   .F90  Fortran source (free form)
##   .o    object file
##   .a    library file
## @end group
## @end example
##
## @end table
## @end deftypefn

function [output, status] = mkoctfile (varargin)

  bindir = __octave_config_info__ ("bindir");
  ext = __octave_config_info__ ("EXEEXT");

  shell_script = fullfile (bindir,
                           sprintf ("mkoctfile-%s%s", OCTAVE_VERSION, ext));

  if (! exist (shell_script, "file"))
    __gripe_missing_component__ ("mkoctfile", "mkoctfile");
  endif

  cmd = ['"' shell_script '"'];
  for i = 1:nargin
    cmd = [cmd ' "' varargin{i} '"'];
  endfor

  [sts, out] = system (cmd);

  if (nargout > 0)
    [output, status] = deal (strtrim (out), sts);
    if (sts != 0)
      warning ("mkoctfile: building exited with failure status\n");
    endif
  else
    printf ("%s", out);
    if (sts != 0)
      error ("mkoctfile: building exited with failure status\n");
    endif
  endif

endfunction
