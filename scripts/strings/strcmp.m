## Copyright (C) 1996, 1997 John W. Eaton
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
## @deftypefn {Function File} {} strcmp (@var{s1}, @var{s2})
## Compares two strings, returning 1 if they are the same, and 0 otherwise.
##
## @strong{Caution:}  For compatibility with @sc{Matlab}, Octave's strcmp
## function returns 1 if the strings are equal, and 0 otherwise.  This is
## just the opposite of the corresponding C library function.
## @end deftypefn

## Author: jwe

function retval = strcmp (s1, s2)

  if (nargin != 2)
    usage ("strcmp (s, t)");
  endif

  retval = 0;

  if (isstr (s1))
    [r1, c1] = size (s1);
    if (isstr (s2))
      [r2, c2] = size (s2);
      if (r1 == r2 && c1 == c2)
	if (c1 == 0)
          retval = 1;
	else
          retval = all (all (s1 == s2));
	endif
      endif
    elseif (iscellstr (s2))
      [r2, c2] = size (s2);
      if (r1 == 1)
	t2 = s2(:);
	n = length (t2);
	retval = zeros (n, 1);
	for i = 1:n
	  retval(i) = strcmp (s1, t2{i});
	endfor
	retval = reshape (retval, r2, c2);
      elseif (r1 > 1)
	if (r2 == 1 && c2 == 1)
	  t2 = s2{1};
	  retval = zeros (r1, 1);
	  for i = 1:r1
	    retval(i) = strcmp (deblank (s1(i,:)), t2);
	  endfor
	else
	  t2 = s2(:);
	  n = length (t2);
	  if (n == r1)
	    retval = zeros (n, 1);
	    for i = 1:n
	      retval(i) = strcmp (deblank (s1(i,:)), t2{i});
	    endfor
	    retval = reshape (retval, r2, c2);
	  endif
	endif
      endif
    endif
  elseif (iscellstr (s1))
    [r1, c1] = size (s1);
    if (isstr (s2))
      [r2, c2] = size (s2);
      if (r2 == 1)
	t1 = s1(:);
	n = length (t1);
	retval = zeros (n, 1);
	for i = 1:n
	  retval(i) = strcmp (t1{i}, s2);
	endfor
	retval = reshape (retval, r1, c1);
      elseif (r2 > 1)
	if (r1 == 1 && c1 == 1)
	  t1 = s1{1};
	  retval = zeros (r2, 1);
	  for i = 1:r2
	    retval(i) = strcmp (t1, deblank (s2(i,:)));
	  endfor
	else
	  t1 = s1(:);
	  n = length (t1);
	  if (n == r2)
	    retval = zeros (n, 1);
	    for i = 1:n
	      retval(i) = strcmp (t1{i}, deblank (s2(i,:)));
	    endfor
	    retval = reshape (retval, r1, c1);
	  endif
	endif
      endif      
    elseif (iscellstr (s2))
      [r2, c2] = size (s2);
      if (r1 == 1 && c1 == 1)
	t1 = s1{:};
	t2 = s2(:);
	n = length (t2);
	retval = zeros (n, 1);
	for i = 1:n
	  retval(i) = strcmp (t1, t2{i});
	endfor
	retval = reshape (retval, r2, c2);
      elseif (r2 == 1 && c2 == 1)
	t1 = s1(:);
	t2 = s2{:};
	n = length (t1);
	retval = zeros (n, 1);
	for i = 1:n
	  retval(i) = strcmp (t1{i}, t2);
	endfor
	retval = reshape (retval, r1, c1);
      elseif (r1 == r2 && c1 == c2)
	t1 = s1(:);
	t2 = s2(:);
	n = length (t1);
	for i = 1:n
	  retval(i) = strcmp (t1{i}, t2{i});
	endfor
	retval = reshape (retval, r1, c1);
      else
	error ("strcmp: nonconformant cell arrays");
      endif
    endif
  endif

endfunction
