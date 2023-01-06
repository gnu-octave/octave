########################################################################
##
## Copyright (C) 2005-2023 The Octave Project Developers
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
## @deftypefn {} {@var{h} =} __line__ (@var{hp}, @dots{})
## Create line object with parent handle @var{hp}.
##
## Return handle @var{h} to created line objects.
## @end deftypefn

## __line__ (hp, x, y, z)
## Create line object from x, y, and z with parent p.
## Return handle to line object.

function h = __line__ (hp, varargin)

  nvargs = nargin - 1;     # remove argument 'hp'
  have_data_prop = false;
  if (nvargs > 1 && ! ischar (varargin{1}) && ! ischar (varargin{2}))
    if (nvargs > 2 && ! ischar (varargin{3}))
      num_data_args = 3;
    else
      num_data_args = 2;
    endif
  else
    num_data_args = 0;
    if (any (strcmpi ("xdata", varargin(1:2:end))))
      have_data_prop = true;
    endif
  endif

  if (rem (nvargs - num_data_args, 2) != 0)
    error ("line: invalid number of PROPERTY / VALUE pairs");
  endif

  other_args = {};
  if (nvargs > num_data_args)
    other_args = varargin(num_data_args+1:end);
  endif

  if (num_data_args == 0)
    if (have_data_prop)
      ## Low-level calling form with a single line
      handles(1) = __go_line__ (hp, other_args{:});
    else
      ## line called without any data, use default data.
      handles(1) = __go_line__ (hp, "xdata", [0, 1], "ydata", [0, 1],
                                    other_args{:});
    endif
  else
    ## Normal case, extract and validate input data args
    ismat = false (1,3);

    ## Initialize loop variables
    tmpdata = varargin{1}(:,:);  # N-D arrays -> 2-D arrays
    if (isvector (tmpdata))
      tmpdata = tmpdata(:);      # Use column vectors by default
    else
      ismat(1) = true;
    endif
    [nr, nc] = size (tmpdata);
    if (islogical (tmpdata))
      tmpdata = uint8 (tmpdata);
    elseif (iscomplex (tmpdata))
      tmpdata = real (tmpdata);
    endif
    varargin{1} = tmpdata;
    reorient_done = false;

    for i = 2:num_data_args
      tmpdata = varargin{i}(:,:);  # N-D arrays -> 2-D arrays

      if (isvector (tmpdata))
        tmpdata = tmpdata(:);      # Use column vectors by default
        [r, c] = size (tmpdata);
        if (nr == r)
          ## Do nothing, properly oriented
        elseif (nc == r && nc > 1 && ! reorient_done)
          ## Re-orient first matrix
          varargin{i-1} = varargin{i-1}.';
          [nr, nc] = deal (nc, nr);  # swap rows and columns.
          reorient_done = true;
        else
          error ("line: number of X, Y, and Z points must be equal");
        endif
      else
        ismat(i) = true;
        [r, c] = size (tmpdata);
        if (nr == r && (nc == c || nc == 1))
          ## Do nothing, properly oriented
          nc = max (nc, c);
        elseif (nr == c)
          tmpdata = tmpdata.';
          [nr, nc] = deal (c, r);  # swap rows and columns.
        else
          error ("line: number of X, Y, and Z points must be equal");
        endif
      endif

      ## Convert logical or complex inputs
    if (islogical (tmpdata))
        tmpdata = uint8 (tmpdata);
      elseif (iscomplex (tmpdata))
        tmpdata = real (tmpdata);
      endif
      varargin{i} = tmpdata;

    endfor

    nlines = nc;

    data = cell (1, 3);
    data(1:num_data_args) = varargin(1:num_data_args);
    data_args = {"xdata", data{1}, "ydata", data{2}, "zdata", data{3}};
    mask = [false, ismat(1), false, ismat(2), false, ismat(3)];

    [colororder_idx, styleorder_idx] = get (hp, {"ColorOrderIndex",
                                                 "LinestyleOrderIndex"}){:};

    unwind_protect
      handles = zeros (nlines, 1);
      for i = 1:nlines
        data_args(mask) = cellindexmat (data(ismat), ":", i);

        ## FIXME: It would be potentially more efficient to write code to
        ##        cycle line styles and colors within __line__.m itself.
        ##        However, these are not easy routines to get exactly right,
        ##        and then there would be duplicate code.  For the time being,
        ##        use an unwind_protect block to restore any values we may
        ##        have modified.  Testing shows only 4 millisecond extra delay
        ##        when plotting 53 lines.
        [linestyle, marker] = __next_line_style__ ();
        if (nr == 1)
          ## Marker for a single point is always '.' (bug #38825).
          marker = '.';
        endif
        color = __next_line_color__ ();

        handles(i) = __go_line__ (hp, data_args{:},
                                  "color", color, "linestyle", linestyle,
                                  "marker", marker, other_args{:});
      endfor
    unwind_protect_cleanup
      set (hp, {"ColorOrderIndex", "LinestyleOrderIndex"},
               {colororder_idx, styleorder_idx});
    end_unwind_protect

  endif

  if (nargout > 0)
    h = handles;
  endif

endfunction
