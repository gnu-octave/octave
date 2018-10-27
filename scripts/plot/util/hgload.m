## Copyright (C) 2014-2018 Massimiliano Fasi
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

## -*- texinfo -*-
## @deftypefn {} {@var{h} =} hgload (@var{filename})
## @deftypefnx {} {[@var{h}, @var{old_prop}] =} hgload (@var{filename}, @var{prop_struct})
## Load the graphics object in @var{filename} into the graphics handle @var{h}.
##
## If @var{filename} has no extension, Octave will try to find the file with
## and without the standard extension of @file{.ofig}.
##
## If provided, the elements of structure @var{prop_struct} will be used to
## override the properties of top-level object stored in @var{filename} and the
## old values will be stored in @var{old_prop}.  @var{old_prop} is a cell array
## matching the size of @var{h}; each cell contains a structure of the existing
## property names and values before being overriden.
##
## @seealso{openfig, hgsave, struct2hdl}
## @end deftypefn

## Author: Massimiliano Fasi

function [h, old_prop] = hgload (filename, prop_struct = struct ())

  ## Check input arguments
  if (nargin == 0 || nargin > 2)
    print_usage ();
  endif

  ## Check second input argument
  if (! isstruct(prop_struct))
    error ("hgload: 'prop_struct' must be a struct");
  endif

  ## Check file existence
  [~, ~, ext] = fileparts (filename);
  if (isempty (ext))
    if (! isempty (file_in_loadpath ([filename ".ofig"])))
      filename = [filename ".ofig"];
    elseif (isempty (file_in_loadpath (filename)))
      error ("hgload: unable to locate file %s", filename);
    endif
  else
    if (isempty (file_in_loadpath (filename)))
      error ("hgload: unable to locate file %s", filename);
    endif
  endif

  ## Load the handle structure
  hgs = {"s_oct40", "hgS_050200", "hgS_070000"};
  hg = load (filename);
  i = isfield (hg, hgs);
  if (nnz (i) == 1)
    hg = hg.(hgs{i});
  else
    error ("hgload: could not load hgsave-formatted object in file %s", filename);
  endif

  ## Override properties of top-level object
  old_prop = [];
  fn_new = fieldnames (prop_struct);
  fn_old = fieldnames (hg.properties);
  for i = 1:numel (fn_new)
    idx = ismember (tolower (fn_old), tolower (fn_new{i}));
    if (any (idx))
      old_prop.(fn_new{i}) = hg.properties.(fn_old{idx});
      hg.properties.(fn_old{idx}) = prop_struct.(fn_new{i});
    endif
  endfor
  old_prop = { old_prop };

  ## Build the graphics handle object
  h = struct2hdl (hg);

endfunction


## Functional test for hgload/hgsave pair is in hgsave.m

## Test input validation
%!error hgload ()
%!error hgload (1, 2, 3)
%!error hgload (1, {})
%!error <unable to locate file> hgload ("%%_A_REALLY_UNLIKELY_FILENAME_%%")

## Test second input and output
%!test
%! unwind_protect
%!   h1 = figure ("Visible", "off");
%!   col = get (h1, "Color");
%!   ftmp = [tempname() ".ofig"];
%!   hgsave (h1, ftmp);
%!   close (h1);
%!   [h2, old] = hgload (ftmp);
%!   assert (old, {[]});
%!   [h3, old] = hgload (ftmp, struct ("Color", [1 0 0]));
%!   assert (get (h3, "Color"), [1 0 0]);
%!   assert (iscell (old) && numel (old) == 1);
%!   assert (isstruct (old{1}) && isfield (old{1}, "Color"));
%!   assert (old{1}.Color, col);
%! unwind_protect_cleanup
%!   unlink (ftmp);
%!   try, close (h1); end_try_catch
%!   try, close (h2); end_try_catch
%!   try, close (h3); end_try_catch
%! end_unwind_protect
