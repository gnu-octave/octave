## Copyright (C) 2008-2016 David Bateman
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
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
## does not end in a semicolon to suppress output.  For example:
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
## @group
## myobj = myclass (@dots{})
##   @result{} myobj = <class myclass>
## @end group
## @end example
##
## @seealso{class, subsref, subsasgn}
## @end deftypefn

function display (obj)

  if (nargin != 1)
    print_usage ();
  endif

  ## Only reason we got here is that there was no overloaded display function.
  ## If obj is truly an instance of a class then there is nothing to be done.
  ## However, if obj is really a built-in like 'double' then we can display it.
  if (isobject (obj))
    error ('display: not defined for class "%s"', class (obj));
  endif

  varname = inputname (1);
  if (! isempty (varname))
    evalin ("caller", varname);
  else
    disp (obj);
  endif

endfunction


%!test
%! str = evalc ("x = 1.1; display (x)");
%! assert (str, "x =  1.1000\n");

%!test
%! str = evalc ("display (1.1)");
%! assert (str, " 1.1000\n"); 

## Test input validation
%!error display ()
%!error display (1,2)
