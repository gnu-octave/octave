## Copyright (C) 2000 Paul Kienzle
## Copyright (C) 2003 Alois Schloegl
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
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

## usage: strmatch(s, A [, 'exact'])
## Determines which entries of A match string s. A can be a string matrix
## or a cell array of strings. If 'exact' is not given, then s only needs 
## to match A up to the length of s. Null characters match blanks.
## Results are returned as a column vector.

## Author: Paul Kienzle, Alois Schloegl
## Adapted-by: jwe

function idx = strmatch (s, A, exact)

  if (nargin < 2 || nargin > 3)
    usage ("strmatch (s, A, \"exact\")");
  endif

  [nr, nc] = size (A);
  nel = numel (A);
  if (iscell (A))
    match = zeros (nel, 1);
    if (nargin > 2)
      for k = 1:nel
	match(k) = strcmp (s, A{k}); 
      end
    else
      for k = 1:nel
	match(k) = strncmp (s, A{k}, length (s));
      end
    end
    idx = find (match);
  elseif (length (s) > nc)
    idx = [];
  else
    if (nargin == 3 && length(s) < nc)
      s(1,nc) = " ";
    endif
    s(s == 0) = " ";
    A(A == 0) = " ";
    match = s(ones(size(A,1),1),:) == A(:,1:length(s));
    if (length(s) == 1)
      idx = find (match);
    else
      idx = find (all (match')');
    endif
  endif
    
endfunction 
