## Copyright (C) 1996, 2006, 2007 Kurt Hornik
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
## @deftypefn {Function File} {} strvcat (@var{s_1}, @dots{}, @var{s_n})
## Return a matrix containing the strings (and cell-strings) 
## @var{s_1}, @dots{}, @var{s_n} as
## its rows.  Each string is padded with blanks in order to form a valid
## matrix.  Unlike @var{str2mat}, empty strings are ignored.
## @seealso{strcat, str2mat}
## @end deftypefn

## Author: Kurt Hornik <Kurt.Hornik@ci.tuwien.ac.at>
## Adapted-By: jwe
## Modified: Paul Kienzle <pkienzle@kienzle.powernet.co.uk> converted
##           str2mat to strvcat.  Same function except that strvcat
##           ignores empty strings.
## Modified by Alois Schloegl <a.schloegl@ieee.org> Mar 2005
##	     added support for cell-strings 
## Modifed by David Bateman for NDArrays

function retval = strvcat (varargin)

  if (nargin == 0)
    print_usage ();
  endif

  nr = zeros (nargin, 1);
  nc = zeros (nargin, 1);
  K = 0; 
  nd = ndims (varargin {1});
  sz = size (varargin {1});
  for k = 1 : nargin
    s = varargin{k};
    if (iscell (s))
      for k1 = 1:length(s)
	K = K+1;
	nr(K) = size (s{k1}, 1);
	nc(K) = size (s{k1}, 2);
	if (ndims (s{k1}) != nd)
	  error ("strvcat: dimension mismatch");
	else
	  if (any (sz(3:nd) != size (s{k1}) (3:nd)))
	    error ("strvcat: dimension mismatch");
	  endif
	endif
      endfor
    else
      K = K + 1;
      nr(K) = size (s, 1);
      nc(K) = size (s, 2);
      if (ndims (s) != nd)
	error ("strvcat: dimension mismatch");
      else
	if (any (sz(3:nd) != size (s) (3:nd)))
	  error ("strvcat: dimension mismatch");
	endif
      endif
    endif
  endfor

  sz(1) = sum (nr);
  sz(2) = max (nc);
  retval = char (ones (sz) * toascii (" "));

  idx = cell(nd,1);
  for k = 3 : nd;
    idx {k} = sz {k};
  endfor

  K = 0;
  row_offset = 0;
  for k = 1 : nargin
    s = varargin{k};
    if (iscell (s))
      for k1 = 1:length(s)
	K = K + 1;
	idx{1} = [row_offset + 1 : row_offset + nr(k)];
	idx{2} = [1 : nc(K)];
	retval(idx{:}) = char(s{k1});
	row_offset = row_offset + size (s{k1}, 1);
      endfor
    else
      K = K + 1;
      if (nc(K) > 0)
    	retval ((row_offset + 1) : (row_offset + nr(K)), 1:nc(K)) = char(s);
      endif
      row_offset = row_offset + nr(K);
    endif
  endfor

endfunction

%!shared s1,s2,s3,s4,c
%! s1 = "quick"; s2 = "brown"; s3 = "fox"; s4 = ["quick";"brown";"fox  "];
%! c{1} = s1; c{2} = s2; c{3} = s3;
%!assert(strvcat(s1,s2,s3),s4)
%!assert(strvcat(c),s4)
%!assert(strvcat(s4,s4),[s4;s4]);
