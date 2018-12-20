## Copyright (C) 2006-2018 Keith Goodman
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
## the shell prompt.  The output is stored in the @var{output} variable and
## the exit status in the @var{status} variable.
##
## @code{mkoctfile} accepts the following options, all of which are optional
## except for the filename of the code you wish to compile:
##
## @table @samp
## @item -I DIR
## Add the include directory DIR to compile commands.
##
## @item -D DEF
## Add the definition DEF to the compiler call.
##
## @item -l LIB
## Add the library LIB to the link command.
##
## @item -L DIR
## Add the library directory DIR to the link command.
##
## @item  -M
## @itemx --depend
## Generate dependency files (.d) for C and C++ source files.
##
## @item -R DIR
## Add the run-time path to the link command.
##
## @item @nospell{-Wl,@dots{}}
## Pass options to the linker like @nospell{"-Wl,-rpath=@dots{}"}.
## The quotes are needed since commas are interpreted as command
## separators.
##
## @item -W@dots{}
## Pass options to the assembler like @nospell{"-Wa,OPTION"}.
##
## @item -c
## Compile but do not link.
##
## @item -g
## Enable debugging options for compilers.
##
## @item  -o FILE
## @itemx --output FILE
## Output filename.  Default extension is .oct (or .mex if @samp{--mex} is
## specified) unless linking a stand-alone executable.
##
## @item  -p VAR
## @itemx --print VAR
## Print configuration variable VAR@.  There are three categories of
## variables:
##
## Octave configuration variables that users may override with environment
## variables.  These are used in commands that @code{mkoctfile} executes.
##
## @example
##    ALL_CFLAGS                  LAPACK_LIBS
##    ALL_CXXFLAGS                LDFLAGS
##    ALL_FFLAGS                  LD_CXX
##    ALL_LDFLAGS                 LD_STATIC_FLAG
##    BLAS_LIBS                   LFLAGS
##    CC                          LIBDIR
##    CFLAGS                      LIBOCTAVE
##    CPICFLAG                    LIBOCTINTERP
##    CPPFLAGS                    OCTAVE_LINK_OPTS
##    CXX                         OCTINCLUDEDIR
##    CXXFLAGS                    OCTAVE_LIBS
##    CXXPICFLAG                  OCTAVE_LINK_DEPS
##    DL_LD                       OCTLIBDIR
##    DL_LDFLAGS                  OCT_LINK_DEPS
##    F77                         OCT_LINK_OPTS
##    F77_INTEGER8_FLAG           RDYNAMIC_FLAG
##    FFLAGS                      SPECIAL_MATH_LIB
##    FPICFLAG                    XTRA_CFLAGS
##    INCFLAGS                    XTRA_CXXFLAGS
##    INCLUDEDIR
## @end example
##
## Octave configuration variables as above, but currently unused by
## @code{mkoctfile}.
##
## @example
##    AR
##    DEPEND_EXTRA_SED_PATTERN
##    DEPEND_FLAGS
##    FFTW3F_LDFLAGS
##    FFTW3F_LIBS
##    FFTW3_LDFLAGS
##    FFTW3_LIBS
##    FFTW_LIBS
##    FLIBS
##    LIBS
##    RANLIB
##    READLINE_LIBS
## @end example
##
## Octave configuration variables that are provided for informational
## purposes only.  Except for @samp{OCTAVE_HOME} and @samp{OCTAVE_EXEC_HOME},
## users may not override these variables.
##
## If @env{OCTAVE_HOME} or @env{OCTAVE_EXEC_HOME} are set in the environment,
## then other variables are adjusted accordingly with @env{OCTAVE_HOME} or
## @env{OCTAVE_EXEC_HOME} substituted for the original value of the directory
## specified by the @samp{--prefix} or @samp{--exec-prefix} options that were
## used when Octave was configured.
##
## @example
##    API_VERSION                 LOCALFCNFILEDIR
##    ARCHLIBDIR                  LOCALOCTFILEDIR
##    BINDIR                      LOCALSTARTUPFILEDIR
##    CANONICAL_HOST_TYPE         LOCALVERARCHLIBDIR
##    DATADIR                     LOCALVERFCNFILEDIR
##    DATAROOTDIR                 LOCALVEROCTFILEDIR
##    DEFAULT_PAGER               MAN1DIR
##    EXEC_PREFIX                 MAN1EXT
##    EXEEXT                      MANDIR
##    FCNFILEDIR                  OCTAVE_EXEC_HOME
##    IMAGEDIR                    OCTAVE_HOME
##    INFODIR                     OCTDATADIR
##    INFOFILE                    OCTDOCDIR
##    LIBEXECDIR                  OCTFILEDIR
##    LOCALAPIARCHLIBDIR          OCTFONTSDIR
##    LOCALAPIFCNFILEDIR          STARTUPFILEDIR
##    LOCALAPIOCTFILEDIR          VERSION
##    LOCALARCHLIBDIR
## @end example
##
## @item --link-stand-alone
## Link a stand-alone executable file.
##
## @item --mex
## Assume we are creating a MEX file.  Set the default output extension to
## ".mex".
##
## @item  -s
## @itemx --strip
## Strip the output file.
##
## @item  -v
## @itemx --verbose
## Echo commands as they are executed.
##
## @item file
## The file to compile or link.  Recognized file types are:
##
## @example
## @group
##    .c    C source
##    .cc   C++ source
##    .cp   C++ source
##    .cpp  C++ source
##    .CPP  C++ source
##    .cxx  C++ source
##    .c++  C++ source
##    .C    C++ source
##    .f    Fortran source (fixed form)
##    .F    Fortran source (fixed form)
##    .f90  Fortran source (free form)
##    .F90  Fortran source (free form)
##    .o    object file
##    .a    library file
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

  [sys, out] = system (cmd);

  if (nargout > 0)
    [output, status] = deal (out, sys);
  else
    printf ("%s", out);
  endif

  if (sys != 0)
    warning ("mkoctfile: building exited with failure status\n");
  endif

endfunction
