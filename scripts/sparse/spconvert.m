## Copyright (C) 2004 David Bateman & Andy Adler
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

## -*- texinfo -*-
## @deftypefn {Function File} {@var{x} =} spconvert (@var{m})
## This function converts for a simple sparse matrix format easily
## produced by other programs into Octave's internal sparse format. The
## input @var{x} is either a 3 or 4 column real matrix, containing
## the row, column, real and imaginary parts of the elements of the
## sparse matrix. An element with a zero real and imaginay part can
## be used to force a particular matrix size.
## @end deftypefn

function s = spconvert (m)

  if issparse(m)
    s = m;
  else
    sz = size(m);
    if (nargin != 1 || !ismatrix(m) || !isreal(m) || length(sz) != 2 || 
	(sz(2) != 3 && sz(2) != 4))
      error ("spconvert: input matrix must be either sparse or a three or four column");
      error ("           real matrix");
    elseif (sz(2) == 3)
      s = sparse (m(:,1), m(:,2), m(:,3));
    else
      s = sparse (m(:,1), m(:,2), m(:,3) + 1i*m(:,4));
    endif
  endif
endfunction
