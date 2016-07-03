## Copyright (C) 2016 Markus Muetzel
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

## Undocumented internal function.

## -*- texinfo -*-
## @deftypefn  {Function File} {@var{iso} =} __calc_isovalue_from_data__ (@var{data})
## Undocumented internal function.
## @end deftypefn

## calculate a "good" iso value from histogram of data
## called from isocaps, isosurface


function iso = __calc_isovalue_from_data__ (data)

  ## use a maximum of 10000-20000 samples to limit run-time of hist
  step = 1;
  data_numel = numel (data);
  if (data_numel > 20000)
    step = floor (data_numel / 10000);
    data = data(1:step:end);
    data_numel = numel (data);
  endif

  num_bins = 100;
  [bin_count, bin_centers] = hist (data(:), num_bins);

  ## if one of the first two bins contains more than 10 times the count as
  ## compared to equally distributed data, remove both (zero-padded + noise)
  if (any (bin_count(1:2) > 10 * (data_numel / num_bins)))
    bin_count(1:2) = [];
    bin_centers(1:2) = [];
  endif

  ## if bins have low counts, remove them (but keep them if we would loose more than 90 % of bins)
  bins_to_remove = find (bin_count < max (bin_count)/50);
  if length (bins_to_remove) < .9 * num_bins
    bin_centers(bins_to_remove) = [];
  endif

  ## select middle bar of histogram with previous conditions
  iso = bin_centers(floor (length (bin_centers) / 2));

endfunction
