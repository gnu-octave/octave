## Copyright (C) 2013 Michael D. Godfrey
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or
## modify it under the terms of the GNU General Public
## License as published by the Free Software Foundation;
## either version 3 of the License, or (at your option) any
## later version.
##
## Octave is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied
## warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
## PURPOSE. See the GNU General Public License for more
## details.
##
## You should have received a copy of the GNU General Public
## License along with Octave; see the file COPYING. If not,
## see <http://www.gnu.org/licenses/>.


## -*- texinfo -*-
## @deftypefn  {Function File} {} stemleaf (@var{x})
## @deftypefnx {Function File} {@var{plot} =} stemleaf (@var{x}, @var{opt})
##
## Compute and display a stem and leaf plot of the vector @var{x}.
##
## The @var{x} vector is converted to integer by @var{x} = @code{fix} (@var{x}). 
## If an output argument is provided, the plot is returned as
## an array of strings.  The first element is the heading
## followed by an element for each stem.
## The default stem step is 10.  
## The @var{x} vector should be integers.  It will be treated so that
## the last digit is the leaf value and the other digits are
## the stems.
## The leaf digits are not sorted.  If sorted leaf values
## are wanted, use @code{sort} (@var{x}) before calling @code{stemleaf} (@var{x}).
## The stem and leaf plot is described in: Ch. 3,
## Exploratory Data Analysis by J. W. Tukey, Addison-Wesley, 1977.
## @seealso{hist, printd}
## @end deftypefn

## Author: Michael D. Godfrey <michaeldgodfrey@gmail.com>
## Description: Compute stem and leaf plot

function varargout = stemleaf (x, stem_unit)
% Compute and display a stem and leaf plot of the vector x.
% The x vector is converted to integer by x = fix(x). 
% If an output argument is provided, the plot is returned as
% an array of strings.  The first element is the heading
% followed by an element for each stem.
% The default stem step is 10.  If stem_unit is provided
% the stem step is set to: 10^(stem_unit+1)
% The x vector should be integers.  It will be treated so that
% the last digit is the leaf value and the other digits are
% the stems.

% When we first implemented stem and leaf plots in the early
% 1960's there was some discussion about sorting vs. leaving
% the leaf entries in the original order in the data. We
% decided in favor or sorting the leaves for most purposes.
% This is the choice implemented in the SNAP/IEDA system that
% was written at that time.
% SNAP/IEDA and particularly its stem and leaf plotting were further
% developed by Hale Trotter, David Hoagland (at Princeton and MIT)
% and others.
% Tukey, in EDA, generally uses unsorted leaves.  In addition,
% he described a wide range of additional display formats.  This 
% implementation does not sort the leaves, but if the x vector is
% sorted then the leaves come out sorted.  A simple display
% format is used.
% I doubt if providing other options is worthwhile.  The
% code can quite easily be modified to provide specific
% display results.  Or, the returned output string can be edited.
% The returned output is an array of strings with each row
% containing a line of the plot preceded by the lines of header
% text as the first row.  This facilitates annotation.
%
% Note that the code has some added complexity due to the need
% to distinguish both + and - 0 stems.
% The +- stem values are essential for all plots which span 0.
% After dealing with +-0 stems, the added complexity of putting
% +- data values in the correct stem is minor, but the sign of
% 0 leaves must be checked.  And, the cases where the stems start 
% or end at +- 0 must also be considered.
% 
% The fact that IEEE floating point defines +- 0 helps make this
% easier.
%
% Michael D. Godfrey   January 2013

% More could be implemented for better data scaling. And, of course,
% other options for the kinds of plots described by Tukey could be
% provided.  This may best be left to users.

  if (nargin >= 2)
    stem_step = 10^(stem_unit+1);
  else
    stem_step = 10;
  endif
  if (any (x == int32 (x)) == 0)
    printf ('Input vector truncated to integer values.\n')
    x = fix (x);
  endif
% Avoid use of int32 due to:
% floor (int32 (-44)/10) == -4 and floor (int32 (-46)/10) = -5 !!!
% x  = sort (fix (x));  % User can decide about sorting x.
% x  = fix (x);
%  Adjust scale if too small.
%  while any(abs((fix(x) - x)) >= abs(x/100))
%    x =10*x;
%  endwhile
% Note that IEEE 754 states that -+ 0 should compare equal.
% This has led to C sort (and therefore Octave) treating them
% as equal.  Thus, sort([ -1 0 -0 1]) yields: -1 0 -0 1.
% and, sort([-1 -0 0 1]) yields: -1 -0 0 1.
% This means that stem-and-leaf plotting cannot rely on sort
% to order the data as needed for display.

% 
  if (all((sort(x) == x)) == 1)
    hsort = 'sorted.';
  else
    hsort = 'unsorted.';
  endif
  nx = max (size (x));
% Determine stem values
  if (min(x) < 0)
    if (signbit(max(x)) == 0)     % max is positive
      stems = [fix(min(x)/stem_step)-1 : -1 -0];
      stems = [stems 0 : fix(max(x)/stem_step)+1 ];
    else
      if (max(x) < 0)
        stems = [(fix(min(x)/stem_step)-1) : fix(max(x)/stem_step)];
      else
        stems = [(fix(min(x)/stem_step)-1) : -1 -0];
        stems = [stems 0 : fix(max(x)/stem_step)];
      endif
    endif
  else                            % All stems are > 0
    stems = [fix(min(x)/stem_step) : fix(max(x)/stem_step) + 1];
  endif
%stems
%x
  nstems = max(size(stems));
% compute hinges at +- 1.5 * quartiles
% this requires sorted data!
  xs = sort (x);                   % Note that sort preserves -0
  threeh = 1.5;
  two    = 2.0;
  j  = idivide(nx, 4, "fix") + 1;  % Use F95 truncation.
  k  = nx - j + 1;
  hl = xs (j);
  hu = xs (k);
  if ( (nx + 1) ==  (4 * j) ) 
    hl = (xs (j + 1) + hl) / two;
    hu = (xs (k - 1) + hu) / two;
  endif
%
%     ::::::::  determine h-spread (dh) and fences  ::::::::
  dh = hu - hl;
  fu = hu + threeh * dh;
  fl = hl - threeh * dh;

%     ::::::::  find value adjacent to lower fence  ::::::::
  for i = 1:j
    if ( xs (i) >= fl ) continue; endif
  endfor
  ilow = i;
  xlo = xs (ilow);
%
%     :::::::: find value adjacent to upper fence  ::::::::
  for  i = 1:j
    if ( xs (nx -i + 1) <= fu )continue; endif
  endfor
%
  ihi = nx - i + 1;
  xhi = xs (ihi);
%
% Heading for output:
  plot_out = [''];
  plot_out = [plot_out sprintf('stem step: %i, data: %s\nHinges:    lo: %g, hi: %g\n',
             stem_step, hsort, xlo, xhi)];
% This may appear to be a good place to use vectorization using the stem and data arrays
% but the necessary special case treatment of 0 and -0 seems to result in little reduction
% of complexity, and since this algorithm is for small data vectors only there would be
% practically no performance improvement.

% Determine leaves for each stem:
    for kx = 2:nstems
      line_out = [''];
      steml    = [''];
% Build a string of leaf digits for stem(kx) if stem(kx) <= 0, or stem(kx-1) if stem(kx) > 0
% stems -+ 0 have to be handled as special cases.
      for xi = 1:nx
        if(signbit(stems(kx)) == 1)
          t1 = ((x(xi) <= stems(kx)*10) && (x(xi) > (stems(kx-1)*10)));
        else
          t1 = ((x(xi) < stems(kx)*10) && (x(xi) >= (stems(kx-1)*10)));
        endif
% Special tests for stem -+ 0
        if ((stems(kx) == 0) && signbit(stems(kx)) && (x(xi) == 0)) && !signbit(x(xi))
           t1 = 0;
        endif
        if ((stems(kx-1) == 0) && !signbit(stems(kx-1)) && (x(xi) == 0)) && signbit(x(xi))
           t1 = 0;
        endif
% Create line as a string
        if t1
          if (stems(kx) <= 0)
            xz =  abs (x(xi) - stems(kx)*10);
          else
            xz =  abs (x(xi) - stems(kx-1)*10);
          endif
          if ((stems(kx) == 0) && signbit(stems(kx)))
            steml = [steml sprintf('%d', abs(x(xi) - stems(kx)*10))];
          else
            steml = [steml sprintf('%d', xz)];
          endif
        endif    %  t1
      endfor    % xi = 1:nx
% Set correct -0
      if ((stems(kx) == 0) && signbit(stems(kx)))
        line_out = [line_out sprintf('  -0 | %s',  steml)];  % -0 stem.
      else
        if( stems(kx) < 0)
          line_out = [line_out sprintf('%4d | %s', stems(kx), steml)];
        else
          if stems(kx) > 0
          line_out = [line_out sprintf('%4d | %s', stems(kx-1), steml)];
          endif
        endif
      endif
      plot_out = [plot_out; line_out];
    endfor    % kx = 2:nstems
  if (nargout == 0)
    rows = size (plot_out)(1);
    cols = size (plot_out)(2);
    for k = 1:rows
      printf('%s\n', plot_out(k,1:cols));
    endfor
  else
    varargout{1} = plot_out;
  endif
endfunction

%!demo
%! ## Unsorted plot:
%! x = [-22 12 -28 52  39 -2 12 10 11 11 42 38 44 18 44];
%! stemleaf (x, 0);

%!demo
%! ## Sorted leaves:
%! x = [-22 12 -28 52  39 -2 12 10 11 11 42 38 44 18 44];
%! y = sort(x);
%! stemleaf (y, 0);

%!demo
%! ## More data (sorted)
%! x = [-22 12 -28 52  39 -2 12 10 11 11 42 38 44 18 44 37 113 124 37 48 127 36 29 31 125 139 131 115 105 132 104 123 35 113 122 42 117 119 58 109 23 105 63 27 44 105 99 41 128 121 116 125 32 61 37 127 29 113 121 58 114 126 53 114 96 25 109 7 31 141 46 -13 71 43 117 116 27 7 68 40 31 115 124 42 128 52 71 118 117 38 27 106 33 117 116 111 40 119 47 105 57 122 109 124 115 43 120 43 27 27 18 28 48 125 107 114 34 133 45 120 30 127 31 116 146 21 23 30 10 20 21 30 0 100 110 1 20 0 ];
%! y = sort(x);
%! stemleaf (y, 0);

%!test
%! ## test minus to plus
%! x = [-22 12 -28 52  39 -2 12 10 11 11 42 38 44 18 44 37 113 124 37 48 127 36 29 31 125 139 131 115 105 132 104 123 35 113 122 42 117 119 58 109 23 105 63 27 44 105 99 41 128 121 116 125 32 61 37 127 29 113 121 58 114 126 53 114 96 25 109 7 31 141 46 -13 71 43 117 116 27 7 68 40 31 115 124 42 128 52 71 118 117 38 27 106 33 117 116 111 40 119 47 105 57 122 109 124 115 43 120 43 27 27 18 28 48 125 107 114 34 133 45 120 30 127 31 116 146 21 23 30 10 20 21 30 0 100 110 1 20 0 ];
%! x = sort(x);
%! r2 = ["stem step: 10, data: sorted.\nHinges:    lo: 30, hi: 116\n";...
%! "  -2 | 82";"  -1 | 3";"  -0 | 2";"   0 | 00177";...
%! "   1 | 00112288";"   2 | 001133577777899";...
%! "   3 | 000111123456777889";"   4 | 00122233344456788";...
%! "   5 | 223788";"   6 | 138";"   7 | 11";"   8 | ";...
%! "   9 | 69";"  10 | 04555567999";"  11 | 0133344455566667777899";...
%! "  12 | 0011223444555677788";"  13 | 1239";"  14 | 16"];
%! rx = stemleaf (x, 0);
%! assert(r2, rx);
%!test
%! ## positive values above 0
%! x = [22 12 28 52  39 12 11 11 42 38 44 18 44 ];
%! r2 = ["stem step: 10, data: unsorted.\nHinges:    lo: 12, hi: 42\n";...
%! "   1 | 22118";"   2 | 28";"   3 | 98";"   4 | 244";"   5 | 2"];
%! rx = stemleaf (x, 0);
%! assert(r2, rx);
%!test
%! ## negative values below 0
%! x = [22 12 28 52  39 12 11 11 42 38 44 18 44];
%! x = -x;
%! r2 = ["stem step: 10, data: unsorted.\nHinges:    lo: -42, hi: -12\n";...
%! "  -5 | 2";"  -4 | 244";"  -3 | 98";"  -2 | 28";"  -1 | 22118"];
%! rx = stemleaf (x, 0);
%! assert(r2, rx);
%!test
%! ## positive values from 0
%! x = [22 12 28 52  39 2 12 0 11 11 42 38 44 18 44];
%! r2 = ["stem step: 10, data: unsorted.\nHinges:    lo: 11, hi: 42\n";...
%! "   0 | 20";"   1 | 22118";"   2 | 28";"   3 | 98";"   4 | 244";"   5 | 2"];
%! rx = stemleaf (x, 0);
%! assert(r2, rx);
%!test
%! ## negative values from 0
%! x = [22 12 28 52  39 2 12 0 11 11 42 38 44 18 44];
%! x = -x;
%! r2 = ["stem step: 10, data: unsorted.\nHinges:    lo: -42, hi: -11\n";...
%! "  -5 | 2";"  -4 | 244";"  -3 | 98";"  -2 | 28";"  -1 | 22118";"  -0 | 20"];
%! rx = stemleaf (x, 0);
%! assert(r2, rx);

