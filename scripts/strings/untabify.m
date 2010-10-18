## Copyright (C) 2010 Ben Abbott
## 
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or (at 
## your option) any later version.
## 
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
## 
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {Function file} untabify (@var{t})
## @deftypefnx {Function file} untabify (@var{t}, @var{tw})
## @deftypefnx {Function file} untabify (@var{t}, @var{tw}, @var{deblank})
## Replace TAB characters in @var{t}, with spaces.
## The tab width is specified by @var{tw}, or defaults to eight.
## The input, @var{t}, may be either a 2D character array, or a cell
## array of character strings.  The output is the same class
## as the input.
##
## If the optional argument @var{deblank} is true, then the spaces will
## be removed from the end of the character data.
##
## The following example reads a file and writes an untabified version
## of the same file with trailing spaces stripped.
##
## @example
## @group
## fid = fopen ("tabbed_script.m");
## text = char (fread (fid, "uchar")');
## fclose (fid);
## fid = fopen ("untabified_script.m", "w");
## text = untabify (strsplit (text, "\n"), 8, true);
## fprintf (fid, "%s\n", text{:});
## fclose (fid);
## @end group
## @end example
##
## @seealso{strjust, strsplit, deblank}
## @end deftypefn

## Author: Ben Abbott <bpabbott@mac.com>
## Created: 2010-10-15

function s = untabify (t, tw = 8, db = false)

 if (nargin > 0 && nargin < 4 && (ischar (t) || iscellstr (t)))
   if (ischar (t))
     s = replace_tabs (t, tw);
   else
     s = cellfun (@(str) replace_tabs (str, tw), t, "uniformoutput", false);
   endif
   if (db)
     s = deblank (s);
   endif
 else
   print_usage ();
 endif

endfunction

function s = replace_tabs (t, tw)
 if (ndims (t) == 2)
   if (isempty (t))
     s = t;
   else
     nr = rows (t);
     sc = cell (nr, 1);
     for j = 1:nr
       n = 1:numel(t(j,:));
       m = find (t(j,:) == "\t");
       t(j,m) = " ";
       for i = 1:numel(m)
         k = tw * ceil (n(m(i)) / tw);
         dn = k - n(m(i));
         n(m(i):end) += dn;
       endfor
       sc{j} = blanks (n(end));
       sc{j}(n) = t(j,:);
     endfor
     s = char (sc);
   endif
 else
   error ("untabify: character strings to untabify must have 2 dimensions");
 endif
endfunction

%!test
%! s = untabify ("\thello\t");
%! assert (isequal (s, horzcat (blanks(8), "hello   ")))

%!test
%! s = untabify ("\thello\t", 4, true);
%! assert (isequal (s, horzcat (blanks(4), "hello")))

%!test
%! s = untabify ("\thello\t", 2, true);
%! assert (isequal (s, horzcat (blanks(2), "hello")))

%!test
%! s = untabify ("");
%! assert (isempty (s))

%!test
%! s = char (fix (100 + 10*rand (3,3)));
%! assert (isequal (untabify (s), untabify ({s}){1}))

