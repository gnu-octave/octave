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
## @deftypefnx {Function File} {} stemleaf (@var{x}, @var{stem_sz})
## @deftypefnx {Function File} {@var{plotstr} =} stemleaf (@dots{})
## Compute and display a stem and leaf plot of the vector @var{x}.
##
## The input @var{x} should be a vector of integers.  Any non-integer values
## will be converted to integer by @code{@var{x} = fix (@var{x})}.  Each
## element of @var{x} will be plotted with the last digit of the element as a
## leaf value and the remaining digits as the stem.  For example, 123 will be
## plotted with the stem @samp{12} and the leaf @samp{3}.
##
## The optional input @var{stem_sz} sets the width of each stem.
## The stem width is determined by @code{10^(@var{stem_sz} + 1)}.
## The default stem width is 10.
##
## With no return argument, the plot is immediately displayed.  If an output
## argument is provided, the plot is returned as an array of strings. 
##
## The leaf digits are not sorted.  If sorted leaf values are desired, use
## @code{@var{xs} = sort (@var{x})} before calling @code{stemleaf (@var{xs})}.
##
## The stem and leaf plot is described in: Ch. 3, @cite{Exploratory
## Data Analysis} by J. W. Tukey, Addison-Wesley, 1977.
## @seealso{hist, printd}
## @end deftypefn

## Author: Michael D. Godfrey <michaeldgodfrey@gmail.com>
## Description: Compute stem and leaf plot

function plotstr = stemleaf (x, stem_sz)
  ## Compute and display a stem and leaf plot of the vector x.  The x
  ## vector is converted to integer by x = fix(x).  If an output argument
  ## is provided, the plot is returned as an array of strings.  The
  ## first element is the heading followed by an element for each stem.
  ##
  ## The default stem step is 10.  If stem_sz is provided the stem
  ## step is set to: 10^(stem_sz+1).  The x vector should be integers.
  ## It will be treated so that the last digit is the leaf value and the
  ## other digits are the stems.
  ##
  ## When we first implemented stem and leaf plots in the early 1960's
  ## there was some discussion about sorting vs. leaving the leaf
  ## entries in the original order in the data.  We decided in favor of
  ## sorting the leaves for most purposes.  This is the choice
  ## implemented in the SNAP/IEDA system that was written at that time.
  ##
  ## SNAP/IEDA, and particularly its stem and leaf plotting, were further
  ## developed by Hale Trotter, David Hoagland (at Princeton and MIT),
  ## and others.
  ##
  ## Tukey, in EDA, generally uses unsorted leaves.  In addition, he
  ## described a wide range of additional display formats.  This
  ## implementation does not sort the leaves, but if the x vector is
  ## sorted then the leaves come out sorted.  A simple display format is
  ## used.
  ##
  ## I doubt if providing other options is worthwhile.  The code can
  ## quite easily be modified to provide specific display results.  Or,
  ## the returned output string can be edited.  The returned output is an
  ## array of strings with each row containing a line of the plot
  ## preceded by the lines of header text as the first row.  This
  ## facilitates annotation.
  ##
  ## Note that the code has some added complexity due to the need to
  ## distinguish both + and - 0 stems. The +- stem values are essential
  ## for all plots which span 0.  After dealing with +-0 stems, the added
  ## complexity of putting +- data values in the correct stem is minor,
  ## but the sign of 0 leaves must be checked.  And, the cases where the
  ## stems start or end at +- 0 must also be considered.
  ##
  ## The fact that IEEE floating point defines +- 0 helps make this
  ## easier.
  ##
  ## Michael D. Godfrey   January 2013

  ## More could be implemented for better data scaling.  And, of course,
  ## other options for the kinds of plots described by Tukey could be
  ## provided.  This may best be left to users.

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  if (! isvector (x))
    error ("stemleaf: X must be a vector");
  endif

  if (isinteger (x))
    ## Avoid use of int32 because rounding rules do not use fix():
    ##  floor (int32 (-44)/10) == -4 and floor (int32 (-46)/10) = -5 !!!
    x = single (x);
  elseif (isfloat (x))
    xint = fix (x);
    if (any (x != xint))
      warning ("stemleaf: X truncated to integer values");
      x = xint;
    endif
  else
    error ("stemleaf: X must be a numeric vector");
  endif

  if (nargin == 1)
    stem_step = 10;
  else
    if (isscalar (stem_sz) && stem_sz >= 0 && isreal (stem_sz))
      stem_step = 10^(stem_sz+1);
    else
      error ("stemleaf: STEM_SZ must be a real integer >= 0");
    endif
  endif

  ## Note that IEEE 754 states that -+ 0 should compare equal.  This has
  ## led to C sort (and therefore Octave) treating them as equal.  Thus,
  ## sort([-1 0 -0 1]) yields [-1 0 -0 1], and sort([-1 -0 0 1])
  ## yields: [-1 -0 0 1].  This means that stem-and-leaf plotting cannot
  ## rely on sort to order the data as needed for display.
  ## This also applies to min()/max() so these routines can't be relied
  ## upon if the max or min is -+ 0.

  if (issorted (x))
    hsort = "sorted.";
  else
    hsort = "unsorted.";
  endif

  ## Determine stem values
  min_x = min (x);
  max_x = max (x);
  if (min_x > 0)      # all stems > 0
    stems = [fix(min(x)/stem_step) : (fix(max(x)/stem_step)+1)];
  elseif (max_x < 0)  # all stems < 0
    stems = [(fix(min_x/stem_step)-1) : fix(max_x/stem_step)];
  elseif (min_x < 0 && max_x > 0)  # range crosses 0
    stems = [(fix(min_x/stem_step)-1) : -1 , -0];
    stems = [stems, 0 : fix(max_x/stem_step)+1 ];
  else   # range includes a zero which may be +0 or -0
    if (min_x == 0)
      if (any (x == 0 & signbit (x)))
        min_x = -0;
      else
        min_x = +0;
      endif
    endif
    if (max_x == 0)
      if (any (x == 0 & ! signbit (x)))
        max_x = +0;
      else
        max_x = -0;
      endif
    endif
    stems = [];
    if (signbit (min_x))
      stems = [(fix(min_x/stem_step)-1) : -1 , -0];
    endif
    if (! signbit (max_x))
      stems = [stems, 0 : fix(max_x/stem_step)+1 ];
    endif
  endif

  nx = numel (x);
  nstems = numel (stems);
  ## compute hinges at +- 1.5 * quartiles
  ## this requires sorted data!
  xs = sort (x);                  # Note that sort preserves -0
  j  = fix (nx/4) + 1;
  k  = nx - j + 1;
  hl = xs(j);
  hu = xs(k);
  ## FIXME: Is this really the only case where we want to average
  ##        values to determine the quartile?
  if ( (nx + 1) ==  (4 * j) )
    hl = (xs(j + 1) + hl) / 2;
    hu = (xs(k - 1) + hu) / 2;
  endif

  ## determine h-spread (dh) and fences
  dh = hu - hl;
  fu = hu + 1.5 * dh;
  fl = hl - 1.5 * dh;

  ## find value adjacent to lower fence
  for i = 1:j
    if (xs(i) >= fl)
      break;
    endif
  endfor
  xlo = xs(i);

  ## find value adjacent to upper fence
  for i = 1:j
    if (xs(nx -i + 1) <= fu)
      break;
    endif
  endfor
  xhi = xs(nx - i + 1);

  ## Summary at start of output:
  plot_out = sprintf ("stem step: %i, data: %s", stem_step, hsort);
  plot_out = [plot_out; sprintf("Hinges:    lo: %g, hi: %g", xlo, xhi)];
  plot_out = [plot_out; " "];

  ## This may appear to be a good place to use vectorization using the
  ## stem and data arrays but the necessary special case treatment of 0
  ## and -0 seems to result in little reduction of complexity, and since
  ## this algorithm is for small data vectors only there would be
  ## practically no performance improvement.

  ## Determine leaves for each stem:
  for kx = 2:nstems
    line  = "";
    steml = "";
    ## Build a string of leaf digits for
    ## stem(kx)   if stem(kx) <= 0, or
    ## stem(kx-1) if stem(kx) > 0

    ## stems -+ 0 have to be handled as special cases.
    for xi = 1:nx
      if (signbit (stems(kx)) != 0)
        t1 = ((x(xi) <= stems(kx)*10) && (x(xi) > (stems(kx-1)*10)));
      else
        t1 = ((x(xi) < stems(kx)*10) && (x(xi) >= (stems(kx-1)*10)));
      endif
      ## Special tests for stem -+ 0
      if ((stems(kx) == 0)
          && signbit (stems(kx)) && (x(xi) == 0)) && !signbit (x(xi))
        t1 = 0;
      endif
      if ((stems(kx-1) == 0)
          && !signbit (stems(kx-1)) && (x(xi) == 0)) && signbit (x(xi))
        t1 = 0;
      endif
      ## Create line as a string
      if (t1)
        if (stems(kx) <= 0)
          xz = abs (x(xi) - stems(kx)*10);
        else
          xz = abs (x(xi) - stems(kx-1)*10);
        endif
        if ((stems(kx) == 0) && signbit (stems(kx)))
          steml = [steml sprintf("%d", abs(x(xi) - stems(kx)*10))];
        else
          steml = [steml sprintf("%d", xz)];
        endif
      endif    #  t1
    endfor    # xi = 1:nx

    ## Set correct -0
    if ((stems(kx) == 0) && signbit (stems(kx)))
      line = [line sprintf("  -0 | %s",  steml)];  # -0 stem.
    else
      if (stems(kx) < 0)
        line = [line sprintf("%4d | %s", stems(kx), steml)];
      else
        if (stems(kx) > 0)
          line = [line sprintf("%4d | %s", stems(kx-1), steml)];
        endif
      endif
    endif
    plot_out = [plot_out; line];
  endfor    # kx = 2:nstems

  if (nargout == 0)
    disp (plot_out);
  else
    plotstr = plot_out;
  endif

endfunction


%!demo
%! ## Unsorted plot:
%! x = [-22 12 -28 52  39 -2 12 10 11 11 42 38 44 18 44];
%! stemleaf (x);

%!demo
%! ## Sorted leaves:
%! x = [-22 12 -28 52  39 -2 12 10 11 11 42 38 44 18 44];
%! y = sort (x);
%! stemleaf (y);

%!demo
%! ## Sorted leaves (large dataset):
%! x = [-22 12 -28 52  39 -2 12 10 11 11 42 38 44 18 44 37 113 124 37 48 127  \
%!      36 29 31 125 139 131 115 105 132 104 123 35 113 122 42 117 119 58 109 \
%!      23 105 63 27 44 105 99 41 128 121 116 125 32 61 37 127 29 113 121 58  \
%!      114 126 53 114 96 25 109 7 31 141 46 -13 71 43 117 116 27 7 68 40 31  \
%!      115 124 42 128 52 71 118 117 38 27 106 33 117 116 111 40 119 47 105 57\
%!      122 109 124 115 43 120 43 27 27 18 28 48 125 107 114 34 133 45 120 30 \
%!      127 31 116 146 21 23 30 10 20 21 30 0 100 110 1 20 0];
%! y = sort (x);
%! stemleaf (y);

%!demo
%! ## Gaussian leaves:
%! x = fix (30 * randn (300,1));
%! stemleaf (x);

%!test
%! ## test minus to plus
%! x = [-22 12 -28 52  39 -2 12 10 11 11 42 38 44 18 44 37 113 124 37 48 127  \
%!      36 29 31 125 139 131 115 105 132 104 123 35 113 122 42 117 119 58 109 \
%!      23 105 63 27 44 105 99 41 128 121 116 125 32 61 37 127 29 113 121 58  \
%!      114 126 53 114 96 25 109 7 31 141 46 -13 71 43 117 116 27 7 68 40 31  \
%!      115 124 42 128 52 71 118 117 38 27 106 33 117 116 111 40 119 47 105 57\
%!      122 109 124 115 43 120 43 27 27 18 28 48 125 107 114 34 133 45 120 30 \
%!      127 31 116 146 21 23 30 10 20 21 30 0 100 110 1 20 0];
%! x = sort (x);
%! rexp = char (
%! "stem step: 10, data: sorted." ,
%! "Hinges:    lo: -28, hi: 146"  ,
%! " "                            ,
%! "  -2 | 82"                    ,
%! "  -1 | 3"                     ,
%! "  -0 | 2"                     ,
%! "   0 | 00177"                 ,
%! "   1 | 00112288"              ,
%! "   2 | 001133577777899"       ,
%! "   3 | 000111123456777889"    ,
%! "   4 | 00122233344456788"     ,
%! "   5 | 223788"                ,
%! "   6 | 138"                   ,
%! "   7 | 11"                    ,
%! "   8 | "                      ,
%! "   9 | 69"                    ,
%! "  10 | 04555567999"           ,
%! "  11 | 0133344455566667777899",
%! "  12 | 0011223444555677788"   ,
%! "  13 | 1239"                  ,
%! "  14 | 16"                    );
%! r = stemleaf (x, 0);
%! assert (r, rexp);

%!test
%! ## positive values above 0
%! x = [22 12 28 52 39 12 11 11 42 38 44 18 44];
%! rexp = char (
%! "stem step: 10, data: unsorted." ,
%! "Hinges:    lo: 11, hi: 52"      ,
%! " "                              ,
%! "   1 | 22118"                   ,
%! "   2 | 28"                      ,
%! "   3 | 98"                      ,
%! "   4 | 244"                     ,
%! "   5 | 2"                       );
%! r = stemleaf (x);
%! assert (r, rexp);

%!test
%! ## negative values below 0
%! x = [22 12 28 52 39 12 11 11 42 38 44 18 44];
%! x = -x;
%! rexp = char (
%! "stem step: 10, data: unsorted." ,
%! "Hinges:    lo: -52, hi: -11"    ,
%! " "                              ,
%! "  -5 | 2"                       ,
%! "  -4 | 244"                     ,
%! "  -3 | 98"                      ,
%! "  -2 | 28"                      ,
%! "  -1 | 22118"                   );
%! r = stemleaf (x);
%! assert (r, rexp);

%!test
%! ## positive values from 0
%! x = [22 12 28 52 39 2 12 0 11 11 42 38 44 18 44];
%! rexp = char (
%! "stem step: 10, data: unsorted." ,
%! "Hinges:    lo: 0, hi: 52"       ,
%! " "                              ,
%! "   0 | 20"                      ,
%! "   1 | 22118"                   ,
%! "   2 | 28"                      ,
%! "   3 | 98"                      ,
%! "   4 | 244"                     ,
%! "   5 | 2"                       );
%! r = stemleaf (x);
%! assert (r, rexp);

%!test
%! ## negative values from 0
%! x = [22 12 28 52 39 2 12 0 11 11 42 38 44 18 44];
%! x = -x;
%! rexp = char (
%! "stem step: 10, data: unsorted." ,
%! "Hinges:    lo: -52, hi: -0"     ,
%! " "                              ,
%! "  -5 | 2"                       ,
%! "  -4 | 244"                     ,
%! "  -3 | 98"                      ,
%! "  -2 | 28"                      ,
%! "  -1 | 22118"                   ,
%! "  -0 | 20"                      );
%! r = stemleaf (x);
%! assert (r, rexp);

%!test
%! ## both +0 and -0 present
%! x = [-9 -7 -0 0];
%! rexp = char (
%! "stem step: 10, data: sorted." ,
%! "Hinges:    lo: -9, hi: 0"     ,
%! " "                            ,
%! "  -0 | 970"                   ,
%! "   0 | 0"                     );
%! r = stemleaf (x);
%! assert (r, rexp);

## Test input validation
%!error stemleaf ()
%!error stemleaf (1, 2, 3)
%!error <X must be a vector> stemleaf (ones (2,2))
%!warning <X truncated to integer values> tmp = stemleaf ([0 0.5 1]);
%!error <X must be a numeric vector> stemleaf ("Hello World")
%!error <STEM_SZ must be a real integer> stemleaf (1, ones (2,2))
%!error <STEM_SZ must be a real integer> stemleaf (1, -1)
%!error <STEM_SZ must be a real integer> stemleaf (1, 1+i)

