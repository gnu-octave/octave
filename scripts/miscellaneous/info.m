########################################################################
##
## Copyright (C) 2008-2023 The Octave Project Developers
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
## @deftypefn {} {} info ()
## Display contact information for the GNU Octave community.
## @end deftypefn

function info ()

  printf ("\n\
  Additional information about GNU Octave is available at\n\
  https://www.octave.org\n\
\n\
  Links to the mailing list and other resources for getting help with\n\
  Octave are available at\n\
  https://www.octave.org/support.html\n\
\n\
  The Octave Wiki has user-generated content on a variety of subjects\n\
  including installation and is available at\n\
  https://wiki.octave.org\n\
\n\
  Additional functionality can be enabled by using packages from\n\
  the Octave Forge project, which may be found at\n\
  https://octave.sourceforge.io\n\
\n\
  Report bugs to the bug tracker at\n\
  https://bugs.octave.org\n\
  But first, please read the guidelines to writing a helpful report at\n\
  https://www.octave.org/bugs.html\n");

endfunction


## Mark file as being tested.  No real test needed for a documentation .m file
%!assert (1)
