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
## @deftypefn {} {@var{idx} =} subsindex (@var{obj})
## Convert an object to an index vector.
##
## When @var{obj} is a class object defined with a class constructor, then
## @code{subsindex} is the overloading method that allows the conversion of
## this class object to a valid indexing vector.  It is important to note that
## @code{subsindex} must return a zero-based real integer vector of the class
## @qcode{"double"}.  For example, if the class constructor were
##
## @example
## @group
## function obj = myclass (a)
##   obj = class (struct ("a", a), "myclass");
## endfunction
## @end group
## @end example
##
## @noindent
## then the @code{subsindex} function
##
## @example
## @group
## function idx = subsindex (obj)
##   idx = double (obj.a) - 1.0;
## endfunction
## @end group
## @end example
##
## @noindent
## could be used as follows
##
## @example
## @group
## a = myclass (1:4);
## b = 1:10;
## b(a)
## @result{} 1  2  3  4
## @end group
## @end example
##
## @seealso{class, subsref, subsasgn}
## @end deftypefn

function idx = subsindex (obj)

  if (nargin < 1)
    print_usage ();
  endif

  ## Only way to get here is if subsindex has not been overloaded by a class.
  error ('subsindex: not defined for class "%s"', class (obj));

endfunction


%!error <not defined for class "double"> subsindex (1)

## Test input validation
%!error <Invalid call> subsindex ()
