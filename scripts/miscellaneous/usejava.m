## Copyright (C) 2012 Rik Wehbring
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
## @deftypefn {Function File} {} usejava (@var{feature})
## Return true if the specific Sun Java element @var{feature} is available.
##
## Possible features are:
##
## @table @asis
## @item "awt"
## Abstract Window Toolkit for GUIs.
##
## @item "desktop"
## Interactive desktop is running.
##
## @item "jvm"
## Java Virtual Machine.
##
## @item "swing"
## Swing components for lightweight GUIs.
## @end table
##
## This function is provided for compatibility with @sc{matlab} scripts which
## may alter their behavior based on the availability of Java.  Octave does
## not implement an interface to Java and this function always returns
## @code{false}.
## @end deftypefn

function retval = usejava (feature)

  if (nargin != 1 || ! ischar (feature))
    print_usage ();
  endif

  if (! any (strcmp (feature, {"awt", "desktop", "jvm", "swing"})))
    error ("usejava: unrecognized feature '%s'", feature);
  endif

  retval = false;

endfunction


%!assert (usejava ("awt"), false)

%% Test input validation
%!error usejava ()
%!error usejava (1, 2)
%!error usejava (1)
%!error <unrecognized feature> usejava ("abc")

