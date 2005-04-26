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
## @deftypefn {Function File} {} strcat (@var{s1}, @var{s2}, @dots{})
## Return a string containing all the arguments concatenated.  For example,
##
## @example
## @group
## s = [ "ab"; "cde" ];
## strcat (s, s, s)
##      @result{} "ab ab ab "
##         "cdecdecde"
## @end group
## @end example
## @end deftypefn

## Author: jwe

function st = strcat (s, t, varargin)

  if (nargin > 1)
    save_warn_empty_list_elements = warn_empty_list_elements;
    unwind_protect
      warn_empty_list_elements = 0;
      if (isstr (s) && isstr (t))
        tmpst = [s, t];
      else
        error ("strcat: all arguments must be strings");
      endif
      n = nargin - 2;
      k = 1;
      while (n--)
        tmp = varargin{k++};
        if (isstr (tmp))
          tmpst = [tmpst, tmp];
        else
          error ("strcat: all arguments must be strings");
        endif
      endwhile
    unwind_protect_cleanup
      warn_empty_list_elements = save_warn_empty_list_elements;
    end_unwind_protect
  else
    usage ("strcat (s, t, ...)");
  endif

  st = tmpst;

endfunction
