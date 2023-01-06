########################################################################
##
## Copyright (C) 2012-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## or <https://octave.org/copyright/>.
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
## @deftypefn {} {@var{tf} =} usejava (@var{feature})
## Return true if the Java element @var{feature} is available.
##
## Possible features are:
##
## @table @asis
## @item @nospell{@qcode{"awt"}}
## Abstract Window Toolkit for GUIs.
##
## @item @qcode{"desktop"}
## Interactive desktop is running.
##
## @item @qcode{"jvm"}
## Java Virtual Machine.
##
## @item @qcode{"swing"}
## Swing components for lightweight GUIs.
## @end table
##
## @code{usejava} determines if specific Java features are available in an
## Octave session.  This function is provided for scripts which may alter
## their behavior based on the availability of Java.  The feature
## @qcode{"desktop"} always returns @code{false} as Octave has no Java-based
## desktop.  Other features may be available if Octave was compiled with the
## Java Interface and Java is installed.
## @seealso{javachk}
## @end deftypefn

function tf = usejava (feature)

  if (nargin < 1 || ! ischar (feature))
    print_usage ();
  endif

  tf = false;

  switch (feature)
    ## For each feature, try methods() on a Java class of a feature
    case "awt"
      try
        dum = methods ("java.awt.Frame");
        tf = ! javaMethod ("isHeadless", "java.awt.GraphicsEnvironment");
      end_try_catch
    case "desktop"
      ## Octave has no Java based GUI/desktop, leave tf = false
    case "jvm"
      try
        dum = methods ("java.lang.Runtime");
        tf = true;
      end_try_catch
    case "swing"
      try
        dum = methods ("javax.swing.Popup");
        tf = ! javaMethod ("isHeadless", "java.awt.GraphicsEnvironment");
      end_try_catch
    otherwise
      error ("usejava: unrecognized FEATURE '%s'", feature);
  endswitch

endfunction


%!assert (usejava ("desktop"), false)

%!testif HAVE_JAVA; usejava ("jvm")
%! assert (usejava ("jvm"), true);

## Test input validation
%!error <Invalid call> usejava ()
%!error <Invalid call> usejava (1)
%!error <unrecognized FEATURE> usejava ("abc")
