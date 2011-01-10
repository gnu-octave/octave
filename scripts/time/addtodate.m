## Copyright (C) 2008, 2009 Bill Denney
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
## @deftypefn {Function File} {@var{d} =} addtodate (@var{d}, @var{q}, @var{f})
## Add @var{q} amount of time (with units @var{f}) to the datenum, @var{d}.
##
## @var{f} must be one of "year", "month", "day", "hour", "minute", or
## "second".
## @seealso{datenum, datevec}
## @end deftypefn

## Author: Bill Denney <bill@denney.ws>

function d = addtodate (d, q, f)

  if (nargin != 3)
    print_usage ();
  elseif (! (ischar (f) && rows (f) == 1))
    ## FIXME: enhance the function so that it works with cellstrs of the
    ## same size as the output.
    error ("addtodate: F must be a single row character string");
  endif

  if (numel (d) == 1 && numel (q) > 1)
    ## expand d to the size of q if d only has one element to make
    ## addition later eaiser.
    d = d.*ones (size (q));
  endif

  ## in case the user gives f as a plural, remove the s
  if ("s" == f(end))
    f(end) = [];
  endif

  if (any (strcmpi ({"year" "month"}, f)))
    dtmp = datevec (d);
    if (strcmpi ("year", f))
      dtmp(:,1) += q(:);
    elseif (strcmpi ("month", f))
      dtmp(:,2) += q(:);
      ## adjust the years and months if the date rolls over a year
      dtmp(:,1) += floor ((dtmp(:,2)-1)/12);
      dtmp(:,2) = mod (dtmp(:,2)-1, 12) + 1;
    endif
    dnew = datenum (dtmp);
    ## make the output the right shape
    if (numel (d) == numel (dnew))
      d = reshape (dnew, size (d));
    else
      d = reshape (dnew, size (q));
    endif
  elseif (any (strcmpi ({"day" "hour" "minute" "second"}, f)))
    mult = struct ("day", 1, "hour", 1/24, "minute", 1/1440, "second", 1/86400);
    d += q.*mult.(f);
  else
    error ("addtodate: Invalid time unit: %s", f);
  endif

endfunction

## tests
%!shared d
%!  d = datenum (2008, 1, 1);
## Identity
%!assert (addtodate (d, 0, "year"), d)
%!assert (addtodate (d, 0, "month"), d)
%!assert (addtodate (d, 0, "day"), d)
%!assert (addtodate (d, 0, "hour"), d)
%!assert (addtodate (d, 0, "minute"), d)
%!assert (addtodate (d, 0, "second"), d)
## Add one of each
## leap year
%!assert (addtodate (d, 1, "year"), d+366)
%!assert (addtodate (d, 1, "month"), d+31)
%!assert (addtodate (d, 1, "day"), d+1)
%!assert (addtodate (d, 1, "hour"), d+1/24)
%!assert (addtodate (d, 1, "minute"), d+1/1440)
%!assert (addtodate (d, 1, "second"), d+1/86400)
## substract one of each
%!assert (addtodate (d, -1, "year"), d-365)
%!assert (addtodate (d, -1, "month"), d-31)
%!assert (addtodate (d, -1, "day"), d-1)
%!assert (addtodate (d, -1, "hour"), d-1/24)
%!assert (addtodate (d, -1, "minute"), d-1/1440)
%!assert (addtodate (d, -1, "second"), d-1/86400)
## rollover
%!assert (addtodate (d, 12, "month"), d+366)
%!assert (addtodate (d, 13, "month"), d+366+31)
## multiple inputs and output orientation
%!assert (addtodate ([d d], [1 13], "month"), [d+31 d+366+31])
%!assert (addtodate ([d;d], [1;13], "month"), [d+31;d+366+31])
%!assert (addtodate (d, [1;13], "month"), [d+31;d+366+31])
%!assert (addtodate (d, [1 13], "month"), [d+31 d+366+31])
%!assert (addtodate ([d;d+1], 1, "month"), [d+31;d+1+31])
%!assert (addtodate ([d d+1], 1, "month"), [d+31 d+1+31])
