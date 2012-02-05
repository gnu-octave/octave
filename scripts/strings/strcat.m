## Copyright (C) 1994-2012 John W. Eaton
## Copyright (C) 2009 Jaroslav Hajek
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
## Return a string containing all the arguments concatenated
## horizontally.  If the arguments are cells strings,  @code{strcat}
## returns a cell string with the individual cells concatenated.
## For numerical input, each element is converted to the
## corresponding ASCII character.  Trailing white space is eliminated.
## For example:
##
## @example
## @group
## s = [ "ab"; "cde" ];
## strcat (s, s, s)
##     @result{}
##         "ab ab ab "
##         "cdecdecde"
## @end group
## @end example
##
## @example
## @group
## s = @{ "ab"; "cde" @};
## strcat (s, s, s)
##     @result{}
##         @{
##           [1,1] = ababab
##           [2,1] = cdecdecde
##         @}
## @end group
## @end example
##
## @seealso{cstrcat, char, strvcat}
## @end deftypefn

## Author: jwe

function st = strcat (varargin)

  if (nargin > 0)
    if (nargin == 1)
      st = varargin{1};
    elseif (nargin > 1)
      ## Convert to cells of strings
      uo = "uniformoutput";
      reals = cellfun ("isreal", varargin);
      if (any (reals))
        varargin(reals) = cellfun ("char", varargin(reals), uo, false);
      endif
      chars = cellfun ("isclass", varargin, "char");
      allchar = all (chars);
      varargin(chars) = cellfun ("cellstr", varargin(chars), uo, false);
      if (! all (cellfun ("isclass", varargin, "cell")))
        error ("strcat: inputs must be strings or cells of strings");
      endif

      ## We don't actually need to bring all cells to common size, because
      ## cellfun can now expand scalar cells.
      err = common_size (varargin{:});

      if (err)
        error ("strcat: arguments must be the same size, or be scalars");
      endif

      ## Cellfun handles everything for us.
      st = cellfun ("horzcat", varargin{:}, uo, false);

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
%!assert(strcat("", "ab"), "ab")
%!assert(strcat("", {"ab"}, {""}), {"ab"})
## 2d
%!assert(strcat(["ab ";"cde"], ["ab ";"cde"]), ["abab  ";"cdecde"])

## test for deblanking implied trailing spaces of character input
%!assert((strcmp (strcat ("foo", "bar"), "foobar")
%!        && strcmp (strcat (["a"; "bb"], ["foo"; "bar"]), ["afoo "; "bbbar"])));

## test for mixing character and cell inputs
%!assert(all (strcmp (strcat ("a", {"bc", "de"}, "f"), {"abcf", "adef"})))

## test for scalar strings with vector strings
%!assert(all (strcmp (strcat (["a"; "b"], "c"), ["ac"; "bc"])))

## test with cells with strings of differing lengths
%!assert(all (strcmp (strcat ({"a", "bb"}, "ccc"), {"accc", "bbccc"})))
%!assert(all (strcmp (strcat ("a", {"bb", "ccc"}), {"abb", "accc"})))

%!error strcat ();

%!assert (strcat (1, 2), strcat (char(1), char(2)))

%!assert (strcat ('', 2), strcat ([], char(2)))

