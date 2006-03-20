## Copyright (C) 2006 Keith Goodman
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} mkoctfile [-options] file ...
## 
## The @code{mkoctfile} function compiles source code written in C,
## C++, or Fortran.  Depending on the options used with @code{mkoctfile}, the
## compiled code can be called within Octave or can be used as a stand-alone
## application.
##
## @code{mkoctfile} can be called from the shell prompt or from the Octave
## prompt.
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
## @item -M|--depend 
## Generate dependency files (.d) for C and C++ source files.
##          
## @item -c
## Compile but do not link.
##
## @item -o FILE|--output FILE
## Output file name; by default extension is .oct.
##
## @item -p VAR|--print VAR
## Print the configuration variable VAR.  Recognized variables are: 
##
## @example             
##    ALL_CFLAGS                FFTW_LIBS     
##    ALL_CXXFLAGS              FLIBS       
##    ALL_FFLAGS                FPICFLAG      
##    ALL_LDFLAGS               INCFLAGS      
##    BLAS_LIBS                 LDFLAGS             
##    CC                        LD_CXX              
##    CFLAGS                    LD_STATIC_FLAG
##    CPICFLAG                  LFLAGS              
##    CPPFLAGS                  LIBCRUFT      
##    CXX                       LIBOCTAVE     
##    CXXFLAGS                  LIBOCTINTERP  
##    CXXPICFLAG                LIBREADLINE   
##    DEPEND_EXTRA_SED_PATTERN  LIBS        
##    DEPEND_FLAGS              OCTAVE_LIBS   
##    DL_LD                     RDYNAMIC_FLAG 
##    DL_LDFLAGS                RLD_FLAG      
##    F2C                       SED         
##    F2CFLAGS                  XTRA_CFLAGS   
##    F77                       XTRA_CXXFLAGS 
##    FFLAGS
## @end example
##
## @item -s|--strip
## Strip the output file.
##
## @item -v|--verbose
## Echo commands as they are executed.
##
## @item file
## The file to compile or link. Recognised file types are
##
## @example
##                   .c    C source
##                   .cc   C++ source
##                   .C    C++ source
##                   .cpp  C++ source
##                   .f    Fortran source
##                   .F    Fortran source
##                   .o    object file
## @end example
##
## @end table
## @end deftypefn

## PKG_ADD: mark_as_command mkoctfile

function mkoctfile (varargin)

  mkoctpath = strcat (octave_config_info.bindir, filesep, "mkoctfile");
  
  options = "";
  for i = 1:nargin
    options = strcat (options, " ", varargin{i});
  endfor
  
  cmd = strcat (mkoctpath, options);
   
  status = system (cmd);

  if (status == 127)
    warning ("unable to find mkoctfile in expected location: %s", mkoctpath);
  elseif (status != 0)
    warning ("mkoctfile exited with failure status");
  endif

endfunction
