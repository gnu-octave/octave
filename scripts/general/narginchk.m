## Copyright (C) 2011 Carnë Draug
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
## @deftypefn {Function File} {} narginchk (@var{minargs}, @var{maxargs})
## Check for correct number of arguments.
##
## This function returns an error unless the number of arguments in its caller
## is between the values of @var{minargs} and @var{maxargs}. It does nothing
## otherwise.
##
## Both @var{minargs} and @var{maxargs} need to be a numeric scalar. Zero, Inf
## and negative are all valid, and they can have the same value.
##
## Note that this function evaluates the value of @code{nargin} on the caller so
## its value must have not been tampered with.
##
## @seealso{nargchk, nargoutchk, error, nargout, nargin}
## @end deftypefn

## Author: Carnë Draug <carandraug+dev@gmail.com>

function narginchk (minargs, maxargs)

  ## it requires always two arguments (can't specify only min)
  ## zero, negative and inf are all valid arguments and they can be equal
  ## thanks to Oldak in ##matlab for the help in checking these corner cases
  ## tested compatibility in version 2011b

  if (nargin != 2)
    print_usage;
  elseif (!isnumeric (minargs) || !isscalar (minargs))
    error ("minargs must be a numeric scalar");
  elseif (!isnumeric (maxargs) || !isscalar (maxargs))
    error ("maxargs must be a numeric scalar");
  elseif (minargs > maxargs)
    error ("minargs cannot be larger than maxargs")
  endif

  args = evalin ("caller", "nargin;");

  if (args < minargs)
    error ("Not enough input arguments.");
  elseif (args > maxargs)
    error ("Too many input arguments.");
  endif

endfunction
