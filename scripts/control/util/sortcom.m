## Copyright (C) 1996, 2000, 2002, 2004, 2005, 2006, 2007
##               Auburn University.  All rights reserved.
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
## @deftypefn {Function File} {[@var{yy}, @var{idx}] =} sortcom (@var{xx}[, @var{opt}])
## Sort a complex vector.
##
## @strong{Inputs}
## @table @var
## @item xx
## Complex vector
## @item opt
## sorting option:
## @table @code
## @item "re"
## Real part (default);
## @item "mag"
## By magnitude;
## @item "im"
## By imaginary part.
## @end table
## if @var{opt} is not chosen as @code{"im"}, then complex conjugate pairs are grouped together,
## @math{a - jb} followed by @math{a + jb}.
## @end table
##
## @strong{Outputs}
## @table @var
## @item yy
## Sorted values
## @item idx
## Permutation vector: @code{yy = xx(idx)}
## @end table
## @end deftypefn

## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>
## Created: June 1995

function [yy, idx] = sortcom (xx, opt)

  if (nargin < 1 || nargin > 2)
     print_usage ();
  elseif (! (isvector (xx) || isempty (xx)))
    error ("sortcom: first argument must be a vector");
  endif

  if (nargin == 1)
    opt = "re";
  else
    if (! ischar (opt))
      error ("sortcom: second argument must be a string");
    endif
  endif

  if (isempty (xx))
    yy = idx = [];
  else
    if (strcmp (opt, "re"))
      datavec = real (xx);
    elseif (strcmp (opt, "im"))
      datavec = imag (xx);
    elseif (strcmp (opt, "mag"))
      datavec = abs (xx);
    else
      error ("sortcom: invalid option = %s", opt);
    endif

    [datavec, idx] = sort (datavec);
    yy= xx(idx);

    if (strcmp (opt, "re") || strcmp (opt, "mag"))
      ## sort so that complex conjugate pairs appear together

      ddiff = diff (datavec);
      zidx = find (ddiff == 0);

      ## sort common datavec values
      if (! isempty (zidx))
        for iv = create_set (datavec(zidx))
          vidx = find (datavec == iv);
          [vals, imidx] = sort (imag (yy(vidx)));
          yy(vidx)  = yy(vidx(imidx));
          idx(vidx) = idx(vidx(imidx));
        endfor
      endif
    endif
  endif
endfunction

