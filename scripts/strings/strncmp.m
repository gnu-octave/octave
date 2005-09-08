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
## @deftypefn {Function File} {} strncmp (@var{s1}, @var{s2}, @var{n})
## Compares the first @var{n} characters (columns) of two character
## strings, returning true if they are the same, and false otherwise.
##
## @strong{Caution:} For compatibility with @sc{Matlab}, Octave's strncmp
## function returns true if the character strings are equal, and false
## otherwise.  This is just the opposite of the corresponding C library
## function.
## @end deftypefn

## Author: jwe
## Adapted from strcmp.m by Tom Holroyd <tomh@kurage.nimh.nih.gov>

function retval = strncmp (s1, s2, n)

  if (nargin != 3)
    usage ("strncmp (s, t, n)");
  endif

  retval = false;

  if (ischar (s1))
    [r1, c1] = size (s1);
    if (ischar (s2))
      [r2, c2] = size (s2);
      if (r1 == r2 && c1 == c2)
	if (c1 == 0)
	  retval = true;
	else
	  if (c1 > n)
	    t1 = s1(:, 1:n);
	    t2 = s2(:, 1:n);
	    retval = all (all (t1 == t2));
	  else
	    retval = all (all (s1 == s2));
	  endif
	endif
      elseif (r1 == r2)
	if (r1 == 0)
	  retval = true;
	else
	  l1 = min(n, c1);
	  l2 = min(n, c2);
	  if (l1 == l2)
	    t1 = s1(:, 1:l1);
	    t2 = s2(:, 1:l2);
	    retval = all (all (t1 == t2));
	  endif
	endif
      endif
    elseif (iscellstr (s2))
      [r2, c2] = size (s2);
      if (r1 == 1)
	t2 = s2(:);
	m = length (t2);
	retval = zeros (m, 1, "logical");
	for i = 1:m
	  retval(i) = strncmp (s1, t2{i}, n);
	endfor
	retval = reshape (retval, r2, c2);
      elseif (r1 > 1)
	if (r2 == 1 && c2 == 1)
	  t2 = s2{1};
	  retval = zeros (r1, 1, "logical");
	  for i = 1:r1
	    retval(i) = strncmp (deblank (s1(i,:)), t2, n);
	  endfor
	else
	  t2 = s2(:);
	  m = length (t2);
	  if (m == r1)
	    retval = zeros (m, 1, "logical");
	    for i = 1:m
	      retval(i) = strncmp (deblank (s1(i,:)), t2{i}, n);
	    endfor
	    retval = reshape (retval, r2, c2);
	  else
	    error ("strncmp: nonconformant arrays");
	  endif
	endif
      endif
    endif
  elseif (iscellstr (s1))
    [r1, c1] = size (s1);
    if (ischar (s2))
      retval = strncmp (s2, s1, n, "logical");
    elseif (iscellstr (s2))
      [r2, c2] = size (s2);
      if (r1 == 1 && c1 == 1)
	t1 = s1{:};
	t2 = s2(:);
	m = length (t2);
	retval = zeros (m, 1);
	for i = 1:m
	  retval(i) = strncmp (t1, t2{i}, n);
	endfor
	retval = reshape (retval, r2, c2);
      elseif (r2 == 1 && c2 == 1)
	## retval = strncmp (s2, s1, n);
	t1 = s1(:);
	t2 = s2{:};
	m = length (t1);
	retval = zeros (m, 1, "logical");
	for i = 1:m
	  retval(i) = strncmp (t1{i}, t2, n);
	endfor
	retval = reshape (retval, r1, c1);
      elseif (r1 == r2 && c1 == c2)
	t1 = s1(:);
	t2 = s2(:);
	m = length (t1);
	for i = 1:m
	  retval(i) = strncmp (t1{i}, t2{i}, n);
	endfor
	retval = reshape (retval, r1, c1);
      else
	error ("strncmp: nonconformant cell arrays");
      endif
    endif
  endif

  if (n < 1)
    retval = zeros (size (retval), "logical");
  endif

endfunction
