## Copyright (C) 1996 Kurt Hornik
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
## @deftypefn {Function File} {} index (@var{s}, @var{t})
## Return the position of the first occurrence of the string @var{t} in the
## string @var{s}, or 0 if no occurrence is found.  For example,
##
## @example
## index ("Teststring", "t")
##      @result{} 4
## @end example
##
## @strong{Caution:}  This function does not work for arrays of strings.
## @end deftypefn

## Author: Kurt Hornik <Kurt.Hornik@wu-wien.ac.at>
## Adapted-By: jwe

function n = index (s, t)

  ## This is patterned after the AWK function of the same name.

  if (nargin != 2)
    print_usage ();
  endif
  
  if (!ischar (s) || !ischar (t) || all (size (s) > 1) || all (size (t) > 1) )
    error ("index: expecting string arguments");
  endif

  l_s = length (s);
  l_t = length (t);
  
  if ( l_s == 0 || l_s < l_t )
    ## zero length source, or target longer than source
    v = [];
    
  elseif ( l_t == 0 )
    ## zero length target: return first
    v = 1;
    
  elseif ( l_t == 1 )
    ## length one target: simple find
    v = find (s==t);
    
  elseif ( l_t == 2 )
    ## length two target: find first at i and second at i+1
    v = find (s (1 : l_s-1) == t (1) & s (2 : l_s) == t (2));
    
  else
    ## length three or more: match the first three by find then go through
    ## the much smaller list to determine which of them are real matches
    limit = l_s - l_t + 1;
    v = find (s (1 : limit) == t(1) & s (2 : limit+1) == t (2)
	      & s (3 : limit+2) == t(3) );
  endif

  if (l_t > 3)
    
    ## force strings to be both row vectors or both column vectors
    if (all (size (s) != size (t)))
      t = t.';
    endif
    
    ## search index vector for a match
    ind = 0 : l_t - 1;
    n = 0; # return 0 if loop terminates without finding any match
    for idx = 1:length(v)
      if (s (v(idx) + ind) == t)
	n = v(idx);
	break;
      endif
    endfor

  elseif (length(v) > 0)
    n = v(1);

  else
    n = 0;

  endif

endfunction
