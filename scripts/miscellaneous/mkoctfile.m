## Copyright (C) 2006-2012 Keith Goodman
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {Command} {} mkoctfile [-options] file @dots{}
## @deftypefnx {Function File} {[@var{output}, @var{status} =} mkoctfile (@dots{})
##
## The @code{mkoctfile} function compiles source code written in C,
## C++, or Fortran.  Depending on the options used with @code{mkoctfile}, the
## compiled code can be called within Octave or can be used as a stand-alone
## application.
##
## @code{mkoctfile} can be called from the shell prompt or from the Octave
## prompt.  Calling it from the Octave prompt simply delegates the
## call to the shell prompt.  The output is stored in the @var{output}
## variable and the exit status in the @var{status} variable.
##
## @code{mkoctfile} accepts the following options, all of which are optional
## except for the file name of the code you wish to compile:
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
## @item -M
## @itemx --depend
## Generate dependency files (.d) for C and C++ source files.
##
## @item -R DIR
## Add the run-time path to the link command.
##
## @item -Wl,@dots{}
## Pass flags though the linker like "-Wl,-rpath=@dots{}".
## The quotes are needed since commas are interpreted as command
## separators.
##
## @item -W@dots{}
## Pass flags though the compiler like "-Wa,OPTION".
##
## @item -c
## Compile but do not link.
##
## @item -g
## Enable debugging options for compilers.
##
## @item -o FILE
## @itemx --output FILE
## Output file name.  Default extension is .oct
## (or .mex if @samp{--mex} is specified) unless linking
## a stand-alone executable.
##
## @item -p VAR
## @itemx --print VAR
## Print the configuration variable VAR@.  Recognized variables are:
##
## @example
##    ALL_CFLAGS                FFTW3F_LIBS
##    ALL_CXXFLAGS              FLIBS
##    ALL_FFLAGS                FPICFLAG
##    ALL_LDFLAGS               INCFLAGS
##    BLAS_LIBS                 LAPACK_LIBS
##    CC                        LDFLAGS
##    CFLAGS                    LD_CXX
##    CPICFLAG                  LD_STATIC_FLAG
##    CPPFLAGS                  LFLAGS
##    CXX                       LIBCRUFT
##    CXXFLAGS                  LIBOCTAVE
##    CXXPICFLAG                LIBOCTINTERP
##    DEPEND_EXTRA_SED_PATTERN  LIBS
##    DEPEND_FLAGS              OCTAVE_LIBS
##    DL_LD                     OCTAVE_LINK_DEPS
##    DL_LDFLAGS                OCT_LINK_DEPS
##    EXEEXT                    RDYNAMIC_FLAG
##    F77                       READLINE_LIBS
##    F77_INTEGER_8_FLAG        SED
##    FFLAGS                    XTRA_CFLAGS
##    FFTW3_LDFLAGS             XTRA_CXXFLAGS
##    FFTW3_LIBS
##    FFTW3F_LDFLAGS
##
## @end example
##
## @item --link-stand-alone
## Link a stand-alone executable file.
##
## @item --mex
## Assume we are creating a MEX file.  Set the default output extension
## to ".mex".
##
## @item -s
## @itemx --strip
## Strip the output file.
##
## @item -v
## @itemx --verbose
## Echo commands as they are executed.
##
## @item file
## The file to compile or link.  Recognized file types are
##
## @example
## @group
##    .c    C source
##    .cc   C++ source
##    .C    C++ source
##    .cpp  C++ source
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

  bindir = octave_config_info ("bindir");

  shell_script = fullfile (bindir, sprintf ("mkoctfile-%s", OCTAVE_VERSION));

  cmd = cstrcat ("\"", shell_script, "\"");
  for i = 1:nargin
    cmd = cstrcat (cmd, " \"", varargin{i}, "\"");
  endfor

  [sys, out] = system (cmd);

  if (nargout > 0)
    [output, status] = deal (out, sys);
  else
    printf ("%s", out);
  endif

  if (sys == 127)
    warning ("unable to find mkoctfile in expected location: '%s'",
             shell_script);

    warning ("mkoctfile exited with failure status");
  endif

endfunction
