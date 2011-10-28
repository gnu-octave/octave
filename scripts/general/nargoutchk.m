## Copyright (C) 2008-2011 Bill Denney
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
## @deftypefn  {Function File} {@var{msgstr} =} nargoutchk (@var{minargs}, @var{maxargs}, @var{nargs})
## @deftypefnx {Function File} {@var{msgstr} =} nargoutchk (@var{minargs}, @var{maxargs}, @var{nargs}, "string")
## @deftypefnx {Function File} {@var{msgstruct} =} nargoutchk (@var{minargs}, @var{maxargs}, @var{nargs}, "struct")
## Return an appropriate error message string (or structure) if the
## number of outputs requested is invalid.
##
## This is useful for checking to see that the number of output
## arguments supplied to a function is within an acceptable range.
## @seealso{nargchk, narginchk, error, nargout, nargin}
## @end deftypefn

## Author: Bill Denney <bill@denney.ws>

function msg = nargoutchk (minargs, maxargs, nargs, outtype)

  if (nargin < 3 || nargin > 4)
    print_usage ();
  elseif (minargs > maxargs)
    error ("nargoutchk: MINARGS must be <= MAXARGS");
  elseif (nargin == 3)
    outtype = "string";
  elseif (! any (strcmpi (outtype, {"string" "struct"})))
    error ("nargoutchk: output type must be either string or struct");
  elseif (! (isscalar (minargs) && isscalar (maxargs) && isscalar (nargs)))
    error ("nargoutchk: MINARGS, MAXARGS, and NARGS must be scalars");
  endif

  msg = struct ("message", "", "identifier", "");
  if (nargs < minargs)
    msg.message = "not enough output arguments";
    msg.identifier = "Octave:nargoutchk:not-enough-outputs";
  elseif (nargs > maxargs)
    msg.message = "too many output arguments";
    msg.identifier = "Octave:nargoutchk:too-many-outputs";
  endif

  if (strcmpi (outtype, "string"))
    msg = msg.message;
  elseif (isempty (msg.message))
    ## Compatability: Matlab returns a 0x1 empty struct when nargchk passes
    msg = resize (msg, 0, 1);
  endif

endfunction


## Tests
%!shared stnul, stmin, stmax
%!  stnul = resize (struct ("message", "", "identifier", ""), 0, 1);
%!  stmin = struct ("message", "not enough output arguments",
%!                  "identifier", "Octave:nargoutchk:not-enough-outputs");
%!  stmax = struct ("message", "too many output arguments",
%!                  "identifier", "Octave:nargoutchk:too-many-outputs");
%!assert (nargoutchk (0, 1, 0), "")
%!assert (nargoutchk (0, 1, 1), "")
%!assert (nargoutchk (1, 1, 0), "not enough output arguments")
%!assert (nargoutchk (0, 1, 2), "too many output arguments")
%!assert (nargoutchk (0, 1, 2, "string"), "too many output arguments")
## Struct outputs
%!assert (isequal (nargoutchk (0, 1, 0, "struct"), stnul))
%!assert (isequal (nargoutchk (0, 1, 1, "struct"), stnul))
%!assert (nargoutchk (1, 1, 0, "struct"), stmin)
%!assert (nargoutchk (0, 1, 2, "struct"), stmax)

