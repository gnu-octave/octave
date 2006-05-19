## Copyright (C) 2000, 2001, 2004, 2005 Paul Kienzle
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
## @deftypefn {Function File} {[@var{n}, @var{s}] =} weekday (@var{d}, [@var{form}])
## Return the day of week as a number in @var{n} and a string in @var{s},
## for example @code{[1, "Sun"]}, @code{[2, "Mon"]}, @dots{}, or
## @code{[7, "Sat"]}.
##
## @var{d} is a serial date number or a date string.
##
## If the string @var{form} is given and is @code{"long"}, @var{s} will
## contain the full name of the weekday; otherwise (or if @var{form} is
## @code{"short"}), @var{s} will contain the abbreviated name of the weekday.
## @seealso{datenum, datevec, eomday}
## @end deftypefn

## Author: pkienzle <pkienzle@users.sf.net>
## Created: 10 October 2001 (CVS)
## Adapted-By: William Poetra Yoga Hadisoeseno <williampoetra@gmail.com>

function [n, s] = weekday (d, form)

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  if (nargin < 2)
    form = "short";
  endif

  v = datevec (d);
  t = strptime (sprintf ("%d-%d-%d", v(3), v(2), v(1)), "%d-%m-%Y");

  n = t.wday + 1;

  if (nargout > 1)
    if (strcmpi (form, "long"))
      s = strftime ("%A", t);
    else
      s = strftime ("%a", t);
    endif
  endif

endfunction

# tests
%!assert(weekday(728647),2)
%!assert(weekday('19-Dec-1994'),2)
# demos
%!demo
%! [n, s] = weekday (now ())
%!demo
%! [n, s] = weekday (728647)
%!demo
%! [n, s] = weekday ('19-Dec-1994')
