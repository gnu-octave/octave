########################################################################
##
## Copyright (C) 2018-2024 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

## -*- texinfo -*-
## @deftypefn  {} {@var{slcidx} =} movslice (@var{N}, @var{wlen})
## @deftypefnx  {} {@var{slcidx} =} movslice (@var{N}, @var{wlen}, @var{samplepoints})
## @deftypefnx {} {[@var{slcidx}, @var{C}, @var{Cpre}, @var{Cpost}, @var{win}, @var{wlen}, @var{scalar_wlen}] =} movslice (@dots{})
## Generate indices to slice a vector of length @var{N} into windows of length
## @var{wlen}.
##
## The input @var{N} must be a positive integer.
##
## The moving window length input @var{wlen} can either be a numeric scalar
## or a 2-element numeric array.  The elements included in the moving window
## will depend on the size and value of @var{wlen} as well as whether the
## @var{samplepoints} input was specified.
##
## The optional input @var{samplepoints} is a sorted, numeric vector of unique
## positions of the @var{N} data points.  The default value is the vector
## @qcode{[1 : @var{N}]}.  When a non-default @var{samplepoints} vector is
## specified, the moving window length @var{wlen} is measured
## against the @var{samplepoints} positions to determine which points are
## included in each window slice.  It should be noted that @var{samplepoints}
## need not be uniformly spaced which can result in window slices containing
## different numbers of points.  Because of this, as specified below the
## shape and content of some @code{movslice} outputs will be different when
## a non-default @var{samplepoints} is used.
##
## The moving window size and included elements will be defined as follows:
## @itemize
## @item
## If @var{samplepoints} has the default value of @qcode{1:@var{N}} (or has
## not been specified):
##
## @itemize
## @item
## For integer-valued @var{wlen}:
## @itemize
## @item
## For odd, integer-valued, scalar @var{wlen} the window is symmetric and
## includes @w{@code{(@var{wlen} - 1) / 2}} elements on either side of the
## central element.  For example, the window slice at index 5 with a window
## length of 3 will include the elements @w{@code{[4, 5, 6]}}.
## @item
## For even, integer-valued, scalar @var{wlen} the window is asymmetric and
## has @w{@code{@var{wlen}/2}} elements to the left of the central element
## and @w{@code{@var{wlen}/2 - 1}} elements to the right of the central
## element.  For example, the window slice at index 5 with a window length of
## 4 will include the elements @w{@code{[3, 4, 5, 6]}}.
## @item
## For integer-valued vector @var{wlen} of the form
## @w{@qcode{[@var{nb}, @var{na}]}} where @var{nb} and @var{na} are integer
## valued the window includes @var{nb} elements to the left of the central
## element and @var{na} elements to the right of the central element.  For
## example, given @w{@code{@var{wlen} = [3, 1]}}, the window slice at index 5
## will include the elements @w{@code{[2, 3, 4, 5, 6]}}.
## @end itemize
##
## @item
## For non-integer-valued scalar @var{wlen}:
## @itemize
## @item
## Non-integer-valued scalar @var{wlen} will be converted to
## two-element vector form with
## @w{@code{@var{nb} = @var{na} = fix (@var{wlen} / 2)}}, and then processed
## as stated above for integer-valued vectors.  For example, the window slice
## at index 5 with @w{@code{@var{wlen} = 2.5}} will include the elements
## @w{@code{[3, 4, 5, 6, 7]}}.
## @item
## Non-integer-valued vector @var{wlen} will be truncated to integer values
## with @w{@code{@var{wlen} = fix (@var{wlen})}} and then processed as
## stated above for integer-valued vectors.  For example, the window slice
## at index 5 with @w{@code{@var{wlen} = [1.2, 2.3]}} will include the
## elements @w{@code{[4, 5, 6, 7]}}.
## @end itemize
## @end itemize
##
## @item
## If @var{samplepoints} has been specified with a non-default vector:
##
## @itemize
## @item
## For vector @var{wlen} specified as @w{@qcode{[@var{nb}, @var{na}]}}, the
## window will include all points within a distance less than or equal to
## @var{nb} before and @var{na} after the central element's position, with
## point positions defined by the elements of @var{samplepoints}.  For
## example, at index 5 with @w{@code{@var{wlen} = [2, 3]}} and the 3rd-8th
## elements of @var{samplepoints} being @w{@code{[1, 3, 5, 7, 8 ,9]}}, the
## window slice will include the elements @w{@code{[4, 5, 6, 7]}}
## corresponding to @var{samplepoints} @w{@code{[3, 5, 7, 8]}}.
## @item
## Scalar @var{wlen} will be converted to two-element vector form with
## @w{@code{@var{nb} = @var{na} = @var{wlen} / 2}}.  The window will then
## include all points within a distance of less than or equal to @var{nb}
## before and less than, but not equal to, @var{na} after the central
## element's position, @w{@qcode{[@var{nb}, @var{na})}}.  For example, at
## index 5 with @w{@code{@var{wlen} = [2, 3]}} and the 3rd-8th elements of
## @var{samplepoints} being @w{@code{[1, 3, 5, 7, 8 ,9]}}, the window slice
## will include the elements @w{@code{[4, 5, 6]}} corresponding to
## @var{samplepoints} @w{@code{[3, 5, 7]}}.
## @end itemize
## @end itemize
##
## The output @var{slcidx} is an array of indices of the slices of the vector.
## @itemize
## @item
## If @var{samplepoints} is default or unspecified, @var{slcidx} will contain
## only the indices of the slices that that fit fully within the vector.  Each
## column will be the indices of one slice as the window moves from left to
## right.  The slices will have @w{@code{fix (@var{wlen})}} elements for
## scalar @var{wlen}, or @w{@code{@var{nb} + @var{na} + 1}} elements for array
## valued @var{wlen}.
##
## @item
## If a non-default @var{samplepoints} has been specified, @var{slcidx} will
## be a 2x@var{N} array with the first and second rows containing the first
## and last elements of each slice, respectively.
## @end itemize
##
## Optional output @var{C} is a row vector of window center positions where
## the window stays fully within the vector.
##
## Optional outputs @var{Cpre} and @var{Cpost} contain the vector elements at
## the start and end of the vector, respectively, that result in the window
## extending beyond the ends of the vector.
##
## Optional output @var{win} contains information for creating the moving
## window.
##
## @itemize
## @item
## If @var{samplepoints} is default or unspecified, @var{win} is a column
## vector with the same number of rows as @var{slcidx} that contains the
## moving window defined as a center relative position stencil.
##
## @item
## If a non-default @var{samplepoints} has been specified, @var{win} will
## be a 2x@var{N} array with the first and second rows containing the left and
## right bounds of each window slice, respectively, using the same
## coordinates as @var{samplepoints}.  These bounds may lie outside of the
## position vector specified by @var{samplepoints}.
## @end itemize
##
## Optional output @var{wlen} returns the window length used by
## @code{movslice} in two-element @w{@qcode{[@var{nb}, @var{na}]}} form.
##
## Optional logical output @var{scalar_wlen} returns the scalar or vector
## state of the input @var{wlen} so that calling functions can determinine
## whether the moving window should be inclusive or exclusive of the right
## window endpoints.  I.e., inclusive @w{@qcode{[@var{nb}, @var{na}]}} for
## vector @var{wlen} or exclusive @w{@qcode{[@var{nb}, @var{na})}} for scalar
## @var{wlen}.
##
## @seealso{movfun}
## @end deftypefn

function [slcidx, C, Cpre, Cpost, win, wlen, scalar_wlen] = movslice (N, wlen, samplepoints = [])

  if (nargin < 2)
    print_usage ();
  endif

  ## Validate N
  if (! (isnumeric (N) && isscalar (N) && isindex (N)))
    error ("Octave:invalid-input-arg",
           "movslice: N must be a positive integer");
  endif

  ## Validate window length
  if (! (isnumeric (wlen) && all (wlen >= 0)))
    error ("Octave:invalid-input-arg",
           "movslice: WLEN must be a positive scalar or 2-element array");
  endif

  ## Validate or set default samplepoints
  if (isempty (samplepoints))
    samplepoints = [1:N].';
    standard_samplepoints = true;
  else
    samplepoints = samplepoints(:);
    standard_samplepoints = all (diff (samplepoints, 1, 1) == 1);
    if (numel (samplepoints) != N)
      error ("Octave:invalid-input-arg",
             "movslice: SamplePoints must have length N");
    endif
  endif

  scalar_wlen = isscalar (wlen);
  if (all (numel (wlen) != [1, 2]))
    error ("Octave:invalid-input-arg",
           "movslice: WLEN must be a positive scalar or 2-element array");
  endif

  ## Process multiple forms of wlen.
  intvalued_wlen = all (fix (wlen) == wlen);

  if (standard_samplepoints && intvalued_wlen && scalar_wlen)
    if (mod (wlen, 2) == 1)
      ## Symmetric window
      nb = na = (wlen - 1) / 2;
      wlen = [nb, na];
    else
      ## Asymmetric window
      nb = wlen / 2;
      na = nb - 1;
      wlen = [nb, na];
    endif
  else
    if (scalar_wlen)
      wlen = [wlen, wlen] / 2;
    endif

    if (standard_samplepoints)
      wlen = fix (wlen);
    endif
  endif

  if (standard_samplepoints)
    Cpre  = 1 : min (wlen(1), N);     # centers that can't fit the pre-window
    Cnf   = max (1, N - wlen(2) + 1); # first center that can't fit the post-window
    Cpost = Cnf:N;                     # centers that can't fit centered post-window
    C     = (wlen(1) + 1):(Cnf - 1);
    ## Convert C to minimum unsigned integer array large enough to hold indices.
    ## Because the size of slcidx is numel(C) x numel(win), for large N  and/or
    ## window sizes this can save significant memory in resulting slcidx array
    ## over using an 8 byte double.
    if (N <= 255)
      C = uint8 (C);
    elseif (N <= 65535)
      C = uint16 (C);
    elseif (N <= 4294967295)
      C = uint32 (C);
    else
      C = uint64 (C);
    endif
    win   = (-wlen(1):wlen(2)).';
    slcidx = C + win;

  else

    sp_end_spacing = [samplepoints(2) - samplepoints(1), ...
                      samplepoints(end) - samplepoints(end-1)];
    Cpre = samplepoints - wlen(1) <= samplepoints(1) - sp_end_spacing(1);

    if (! scalar_wlen)
      Cpost =  samplepoints + wlen(2) >= samplepoints(end) + sp_end_spacing(2);
    else
      Cpost = samplepoints + wlen(2) > samplepoints(end) + sp_end_spacing(2);
    endif

    win = [samplepoints.' - wlen(1); samplepoints.' + wlen(2)];

    C = ! (Cpre | Cpost);

    ## Convert to linear indices.
    Cpre = [1:N](Cpre);
    Cpost = [1:N](Cpost);
    C = [1:N](C);

    ## Create a 2xN index of start and end samplepoint indices for each slice.
    ## Use same uintX class assignment as above.
    if (N <= 255)
      slccls = "uint8";
    elseif (N <= 65535)
      slccls = "uint16";
    elseif (N <= 4294967295)
      slccls = "uint32";
    else
      slccls = "uint64";
    endif

    slcidx = zeros (2, N, slccls);
    [slcidx(1,:), ~] = find (diff ([false(1, N); samplepoints >= win(1, :)], 1, 1));

    if (scalar_wlen)
      [slcidx(2,:), ~] = find (-diff ([(win(2, :) > samplepoints); false(1, N)], 1, 1));
    else
      [slcidx(2,:), ~] = find (diff ([(win(2, :) >= samplepoints); false(1, N)], 1, 1));
    endif
  endif

endfunction

%!assert (double (movslice (10, 2)), [1:9; 2:10])
%!assert (double (movslice (10, 9)), [1:9; 2:10].')
%!assert (double (movslice (10, [1, 0])), [1:9; 2:10])
%!assert (double (movslice (10, [1, 1])), [1:8; 2:9; 3:10])
%!assert (double (movslice (10, [1, 1])), [1:8; 2:9; 3:10])
%!assert (double (movslice (10, [3, 2])), [1:5] + [0:5].')

%!test
%! [sl, c, cpre, cpost, win, wlen, sw] = movslice (10, 4);
%! assert (double (sl), [1:7; 2:8; 3:9; 4:10]);
%! assert (double (c), 3:9);
%! assert (cpre, 1:2);
%! assert (cpost, 10);
%! assert (win, [-2:1:1].');
%! assert (wlen, [2, 1]);
%! assert (sw, true);

%!test
%! [sl, c, cpre, cpost, win, wlen, sw] = movslice (10, [2, 2]);
%! assert (double (sl), [1:6; 2:7; 3:8; 4:9; 5:10]);
%! assert (double (c), 3:8);
%! assert (cpre, 1:2);
%! assert (cpost, 9:10);
%! assert (win, [-2:1:2].');
%! assert (wlen, [2, 2]);
%! assert (sw, false);

%!test
%! [sl, c, cpre, cpost, win, wlen, sw] = movslice (10, 3);
%! assert (double (sl), [1:8; 2:9; 3:10]);
%! assert (double (c), 2:9);
%! assert (cpre, 1);
%! assert (cpost, 10);
%! assert (win, [-1, 0, 1].');
%! assert (wlen, [1, 1]);
%! assert (sw, true);

%!test
%! [sl, c, cpre, cpost, win, wlen, sw] = movslice (10, [1, 1]);
%! assert (double (sl), [1:8; 2:9; 3:10]);
%! assert (double (c), 2:9);
%! assert (cpre, 1);
%! assert (cpost, 10);
%! assert (win, [-1, 0, 1].');
%! assert (wlen, [1, 1]);
%! assert (sw, false);

%!test
%! [sl, c, cpre, cpost, win, wlen, sw] = movslice (10, 10);
%! assert (double (sl), [1:10].');
%! assert (double (c), 6);
%! assert (cpre, 1:5);
%! assert (cpost, 7:10);
%! assert (win, [-5:1:4].');
%! assert (wlen, [5, 4]);
%! assert (sw, true);


## Verify uint output.  Don't test uint64 due to excessive memory usage.
%!test
%! [sl, c] = movslice (10, 10);
%! assert (class (sl), "uint8");
%! assert (class (c), "uint8");
%! [sl, c] = movslice (1000, 1000);
%! assert (class (sl), "uint16");
%! assert (class (c), "uint16");
%! [sl, c] = movslice (100000, 100000);
%! assert (class (sl), "uint32");
%! assert (class (c), "uint32");

## Test non-integer wlen
%!assert <*65928> (double (movslice (10, 2.2)), [1:8; 2:9; 3:10])
%!assert <*65928> (double (movslice (10, 9.1)), [1:9; 2:10].')
%!assert <*65928> (double (movslice (10, 9.999)), [1:9; 2:10].')
%!assert <*65928> (double (movslice (10, [3.2, 0])), [1:7] + [0:3].')
%!assert <*65928> (double (movslice (10, [3.2, 2.1])), [1:5] + [0:5].')

## Test wlen extending beyond N
%!test <*65928>
%! [sl, c, cpre, cpost, win, wlen, sw] = movslice (10, 11);
%! assert (double (sl), zeros (11, 0));
%! assert (double (c), zeros (1, 0));
%! assert (cpre, 1:5);
%! assert (cpost, 6:10);
%! assert (win, [-5:1:5].');
%! assert (wlen, [5, 5]);
%! assert (sw, true);

%!test <*65928>
%! [sl, c, cpre, cpost, win, wlen, sw] = movslice (10, 99);
%! assert (double (sl), zeros (99, 0));
%! assert (double (c), zeros (1, 0));
%! assert (cpre, 1:10);
%! assert (cpost, 1:10);
%! assert (win, [-49:1:49].');
%! assert (wlen, [49, 49]);
%! assert (sw, true);

%!test <*65928>
%! [sl, c, cpre, cpost, win, wlen, sw] = movslice (10, [0, 20]);
%! assert (double (sl), zeros (21, 0));
%! assert (double (c), zeros (1, 0));
%! assert (cpre, zeros (1, 0));
%! assert (cpost, 1:10);
%! assert (win, [0:20].');
%! assert (wlen, [0, 20]);
%! assert (sw, false);

%!test <*65928>
%! [sl, c, cpre, cpost, win, wlen, sw] = movslice (10, [5, 6]);
%! assert (double (sl), zeros (12, 0));
%! assert (double (c), zeros (1, 0));
%! assert (cpre, 1:5);
%! assert (cpost, 5:10);
%! assert (win, [-5:1:6].');
%! assert (wlen, [5, 6]);
%! assert (sw, false);

## Test wlen = 1 or [0, 0]
%!test <*65928>
%! [sl, c, cpre, cpost, win, wlen, sw] = movslice (10, 1);
%! assert (double (sl), 1:10);
%! assert (double (c), 1:10);
%! assert (cpre, zeros (1,0));
%! assert (cpost, zeros (1,0));
%! assert (win, 0);
%! assert (wlen, [0, 0]);
%! assert (sw, true);

%!test <*65928>
%! [sl, c, cpre, cpost, win, wlen, sw] = movslice (10, [0, 0]);
%! assert (double (sl), 1:10);
%! assert (double (c), 1:10);
%! assert (cpre, zeros (1,0));
%! assert (cpost, zeros (1,0));
%! assert (win, 0);
%! assert (wlen, [0, 0]);
%! assert (sw, false);

## Test samplepoints
%!test <*66025> # Standard samplepoints, scalar integer wlen, no output change
%! [sl1, c1, cpre1, cpost1, win1, wlen1, sw1] = movslice (10, 3);
%! [sl2, c2, cpre2, cpost2, win2, wlen2, sw2] = movslice (10, 3, 1:10);
%! assert (sl1, sl2);
%! assert (c1, c2);
%! assert (cpre1, cpre2);
%! assert (cpost1, cpost2);
%! assert (win1, win2);
%! assert (wlen1, wlen2);
%! assert (sw1, sw2);

%!test <*66025> # Standard samplepoints, vector integer wlen, no output change
%! [sl1, c1, cpre1, cpost1, win1, wlen1, sw1] = movslice (10, [2, 3]);
%! [sl2, c2, cpre2, cpost2, win2, wlen2, sw2] = movslice (10, [2, 3], 1:10);
%! assert (sl1, sl2);
%! assert (c1, c2);
%! assert (cpre1, cpre2);
%! assert (cpost1, cpost2);
%! assert (win1, win2);
%! assert (wlen1, wlen2);
%! assert (sw1, sw2);

%!test <*66025> # Standard samplepoints, decimal wlen, no output change
%! [sl1, c1, cpre1, cpost1, win1, wlen1, sw1] = movslice (10, 3.3);
%! [sl2, c2, cpre2, cpost2, win2, wlen2, sw2] = movslice (10, 3.3, 1:10);
%! assert (sl1, sl2);
%! assert (c1, c2);
%! assert (cpre1, cpre2);
%! assert (cpost1, cpost2);
%! assert (win1, win2);
%! assert (wlen1, wlen2);
%! assert (sw1, sw2);

%!test <*66025>
%! [sl, c, cpre, cpost, win, wlen, sw] = movslice (10, 4, 0.5:0.5:5);
%! assert (double(sl), [1, 1, 1, 1, 1:6; 4:10, 10, 10, 10]);
%! assert (c, 5:7);
%! assert (cpre, 1:4);
%! assert (cpost, 8:10);
%! assert (win, [0.5:0.5:5] + [-2;2]);
%! assert (wlen, [2, 2]);
%! assert (sw, true);

%!test <*66025>
%! [sl, c, cpre, cpost, win, wlen, sw] = movslice (10, [2, 2], 0.5:0.5:5);
%! assert (double(sl), [1, 1, 1, 1, 1:6; 5:10, 10, 10, 10, 10]);
%! assert (c, 5:6);
%! assert (cpre, 1:4);
%! assert (cpost, 7:10);
%! assert (win, [0.5:0.5:5] + [-2;2]);
%! assert (wlen, [2, 2]);
%! assert (sw, false);

%!test <*66025>
%! [sl, c, cpre, cpost, win, wlen, sw] = movslice (10, 4.1, 0.5:0.5:5);
%! assert (double(sl), [1, 1, 1, 1, 1:6; 5:10, 10, 10, 10, 10]);
%! assert (c, 5:6);
%! assert (cpre, 1:4);
%! assert (cpost, 7:10);
%! assert (win, [0.5:0.5:5] + [-2.05;2.05]);
%! assert (wlen, [2.05, 2.05]);
%! assert (sw, true);

%!test <*66025>
%! [sl, c, cpre, cpost, win, wlen, sw] = movslice (10, 4, [1:9, 11]);
%! assert (double(sl), [1, 1, 1:7, 9; 2:9, 9, 10]);
%! assert (c, 3:10)
%! assert (cpre, 1:2);
%! assert (cpost, ones(1,0));
%! assert (win, [1:9, 11] + [-2;2]);
%! assert (wlen, [2, 2]);
%! assert (sw, true);

%!test <*66025>
%! [sl, c, cpre, cpost, win, wlen, sw] = movslice (10, [2, 2], [1:9, 11]);
%! assert (double(sl), [1, 1, 1:7, 9; 3:9, 9, 10, 10]);
%! assert (c, 3:9);
%! assert (cpre, 1:2);
%! assert (cpost, 10);
%! assert (win, [1:9, 11] + [-2;2]);
%! assert (wlen, [2, 2]);
%! assert (sw, false);


## Test input validation
%!error <Invalid call> movslice ()
%!error <Invalid call> movslice (1)
%!error <N must be a positive integer> movslice ([1 2], 2)
%!error <N must be a positive integer> movslice (0, 2)
%!error <N must be a positive integer> movslice ("N", 2)
%!error <N must be a positive integer> movslice ({1}, 2)
%!error <WLEN must be a positive scalar or 2-element array> movslice (5, {1})
%!error <WLEN must be a positive scalar or 2-element array> movslice (5, -1)
%!error <WLEN must be a positive scalar or 2-element array> movslice (5, "a")
%!error <WLEN must be a positive scalar or 2-element array> movslice (5, [1, 2, 3])
%!error <WLEN must be a positive scalar or 2-element array> movslice (5, [-1, 2])
%!error <WLEN must be a positive scalar or 2-element array> movslice (5, {1, 2})
%!error <WLEN must be a positive scalar or 2-element array> movslice (5, "ab")
%!error <SamplePoints must have length N> movslice (10, 2, 1:5)
