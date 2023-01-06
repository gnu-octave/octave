########################################################################
##
## Copyright (C) 2016-2023 The Octave Project Developers
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

## Undocumented internal function.

## -*- texinfo -*-
## @deftypefn {} {@var{isoval} =} __calc_isovalue_from_data__ (@var{data})
## Calculate a @nospell{"good"} iso value from histogram of data.
## @end deftypefn

## called from isocaps, isosurface

function isoval = __calc_isovalue_from_data__ (data)

  ## use a maximum of 10,000-20,000 samples to limit runtime of hist
  step = 1;
  ndata = numel (data);
  if (ndata > 20_000)
    step = floor (ndata / 10_000);
    data = data(1:step:end);
    ndata = numel (data);
  endif

  num_bins = 100;
  [bin_count, bin_centers] = hist (data(:), num_bins);

  ## if one of the first two bins contains more than 10 times the count as
  ## compared to equally distributed data, remove both (zero-padded + noise)
  if (any (bin_count(1:2) > 10 * (ndata / num_bins)))
    bin_count(1:2) = [];
    bin_centers(1:2) = [];
  endif

  ## if bins have low counts, remove them (but keep them if we would lose
  ## more than 90% of bins)
  bins_to_remove = find (bin_count < max (bin_count)/50);
  if (length (bins_to_remove) < .9 * num_bins)
    bin_centers(bins_to_remove) = [];
  endif

  ## select middle bar of histogram with previous conditions
  isoval = bin_centers(floor (numel (bin_centers) / 2));

endfunction
