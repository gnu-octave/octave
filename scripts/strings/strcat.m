## Copyright (C) 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2002, 2003,
##               2005, 2006, 2007 John W. Eaton
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

function st = strcat (varargin)

  if (nargin > 0)
    if (nargin == 1)
      st = varargin{1};
    elseif (nargin > 1)
      ## Convert to cells of strings
      numstrs(nargin) = 0;
      dims{nargin} = [];
      allchar = true;
      for nv = 1:nargin
        if (ischar (varargin{nv}))
          varargin{nv} = cellstr (varargin{nv});
        elseif (iscell (varargin{nv}))
          allchar = false;
        else
          error ("strcat: inputs must be strings or cells of strings.")
        endif
        dims{nv} = size (varargin{nv});
        numstrs(nv) = numel (varargin{nv});
      endfor

      ## Set all cells to a common size
      n = find (numstrs == max (numstrs), 1);
      maxstrs = numstrs (n);
      dim = dims{n};
      for nv = find (numstrs == 1)
        str = varargin{nv}{1};
        varargin{nv} = cell (dim);
        varargin{nv}{:} = str;
      endfor

      ## Concatenate the strings
      st = varargin{1};
      for ns = 1:maxstrs
        for nv = 2:nargin
          if (size_equal (st, varargin{nv}))
            st{ns} = [st{ns}, varargin{nv}{ns}];
          else
            error ("strcat: arguments must be the same size, or be scalars.");
          endif
        endfor
      endfor

      if (allchar)
        ## If all inputs were strings, return strings.
        st = char (st);
      endif
    endif
  else
    print_usage ();
  endif

endfunction

## test the dimensionality
## 1d
%!assert(strcat("ab ", "ab "), "abab")
%!assert(strcat({"ab "}, "ab "), {"ab ab"})
%!assert(strcat("ab ", {"ab "}), {"abab "})
%!assert(strcat({"ab "}, {"ab "}), {"ab ab "})
## 2d
%!assert(strcat(["ab ";"cde"], ["ab ";"cde"]), ["abab  ";"cdecde"])

## test for deblanking implied trailing spaces of character input
%!assert((strcmp (strcat ("foo", "bar"), "foobar") &&
%!        strcmp (strcat (["a"; "bb"], ["foo"; "bar"]), ["afoo "; "bbbar"])));

## test for mixing character and cell inputs
%!assert(all (strcmp (strcat ("a", {"bc", "de"}, "f"), {"abcf", "adef"})))

## test for scalar strings with vector strings
%!assert(all (strcmp (strcat (["a"; "b"], "c"), ["ac"; "bc"])))

## test with cells with strings of differing lengths
%!assert(all (strcmp (strcat ({"a", "bb"}, "ccc"), {"accc", "bbccc"})))
%!assert(all (strcmp (strcat ("a", {"bb", "ccc"}), {"abb", "accc"})))

%!error strcat ();

%!error strcat (1, 2);

