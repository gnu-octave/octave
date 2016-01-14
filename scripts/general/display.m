## Copyright (C) 2008-2015 David Bateman
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
## @deftypefn {} {} display (@var{obj})
## Display the contents of the object @var{obj}.
##
## The Octave interpreter calls the @code{display} function whenever it needs
## to present a class on-screen.  Typically, this would be a statement which
## does not end in a semicolon to suppress output.  For example, 
##
## @example
## myobj = myclass (@dots{})
## @end example
##
## User-defined classes should overload the @code{display} method so that
## something useful is printed for a class object.  Otherwise, Octave will
## report only that the object is an instance of its class.
##
## @example
## myobj = myclass (@dots{})
##   @result{} myobj = <class myclass>
## @end example
## 
## @seealso{class, subsref, subsasgn}
## @end deftypefn

function display (obj)

  if (nargin != 1)
    print_usage ();
  endif

  ## Only reason we got here is that there was no overloaded display function.
  ## This may mean it is a built-in class.
  str = disp (obj);
  if (isempty (strfind (str, "<class ")))
    disp (str);
  else
    error ('display: not defined for class "%s"', class (obj));
  endif

endfunction

