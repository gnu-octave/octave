## Copyright (C) 2017 Rik Wehbring
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {@var{h} =} {} hgtransform ()
## @deftypefnx {@var{h} =} {} hgtransform (@var{property}, @var{value}, @dots{})
## @deftypefnx {@var{h} =} {} hgtransform (@var{hax}, @dots{})
##
## Create a graphics transform object.
##
## FIXME: Need to write documentation. 
## FIXME: Add 'makehgtform' to seealso list when it is implemented.
## @seealso{hggroup}
## @end deftypefn


## FIXME: hgtransform should be a C++ graphics object, not an m-file.
##        For the moment (3/7/17), it is quicker to implement something in
##        an m-file.  But, this approach requires double the memory (original
##        and transformed data), and a system of listeners and callbacks.
##        In OpenGL toolkits it should be possible to simply insert a transform
##        somewhere in gl-render.cc to have this done on the fly.

function h = hgtransform (varargin)

  [hax, varargin] = __plt_get_axis_arg__ ("hgtransform", varargin{:});

  if (isempty (hax))
    hax = gca ();
  endif

  htmp = hggroup (hax);

  addproperty ("matrix", htmp, "data", eye (4));
  addproperty ("__orig_data__", htmp, "any", struct ("h", {}));
  if (! isempty (varargin))
    set (htmp, varargin{:});
  endif
  addlistener (htmp, "matrix", @matrix_cb);
  addlistener (htmp, "children", @children_cb);

  if (nargout > 0)
    h = htmp;
  endif

endfunction

function matrix_cb (hgt, ~)

  M = get (hgt, "matrix");
  ## FIXME: Need better input validation on transform matrix M.
  ##        Disallow shear, perspective transforms.
  if (! isreal (M) || ! ismatrix (M) || rows (M) != 4 || columns (M) != 4)
    error ("hgtransform: transform must be 4x4 real matrix");
  endif

  hkids = get (hgt, "children");
  xform_data (hgt, hkids);

endfunction

function xform_data (hgt, hlist)

  M = get (hgt, "matrix");
  orig_data = get (hgt, "__orig_data__");

  for hk = hlist.'

    idx = find (hk == [orig_data.h]);
    if (! idx)
      warning ("hgtransform: original data not found for %f", hk);
      continue;
    endif

    xd = double (orig_data(idx).xdata);
    xsz = size (xd);

    yd = double (orig_data(idx).ydata);
    ysz = size (yd);

    zd = double (orig_data(idx).zdata);
    zsz = size (zd);
    z_empty = isempty (zd);

    if (isempty (zd))
      ## Common case of 2-D data.
      zd = zeros (1, numel (xd));
    elseif (isvector (xd) && isvector (yd))
      ## Handle surface data which may be a vector/matrix combination
      if (isvector (zd))
        ## Do nothing.  All data will be forced to row vectors below
      elseif (length (xd) == rows (zd) && length (yd) == columns (zd))
        [xd, yd] = meshgrid (xd, yd);
        xsz = size (xd);
        ysz = size (yd);
      endif
    endif

    ## Force row vectors for later concatenation
    xd = xd(:).';
    yd = yd(:).';
    zd = zd(:).';

    ## FIXME: To minimize memory, better to construct data matrix in-place?
    data = [xd; yd; zd; ones(1, columns(xd))];
    tol = 2 * max (eps (data(1:3,:)));
    data = M * data;
    ## Need to trim or rotations which produce values near 0 will be strange.
    data(abs (data) < tol) = 0;

    set (hk, "xdata", reshape (data(1,:), xsz));
    set (hk, "ydata", reshape (data(2,:), ysz));
    if (! z_empty)
      set (hk, "zdata", reshape (data(3,:), zsz));
    endif
  endfor

endfunction

function children_cb (hgt, ~)

  hkids = get (hgt, "children");
  orig_data = get (hgt, "__orig_data__");
  hlist = [orig_data.h];

  ## Delete any children that have been removed
  hdel = setdiff (hlist, hkids);
  if (! isempty (hdel))
    for hk = hdel.'
      idx = find (hk == hlist); 
      if (ishghandle (hk))
        ## child was re-parented to something else, restore data
        set (hk, "xdata", orig_data(idx).xdata);
        set (hk, "ydata", orig_data(idx).ydata);
        set (hk, "zdata", orig_data(idx).zdata);
      endif
    endfor
    orig_data = orig_data(hlist != hdel);
    hlist = hlist(hlist != hdel);
  endif

  ## Add new children
  hnew = setdiff (hkids, hlist);
  for hk = hnew.'
    orig_data(end+1).h = hk;
    orig_data(end).xdata = get (hk, "xdata");
    orig_data(end).ydata = get (hk, "ydata");
    orig_data(end).zdata = get (hk, "zdata");
  endfor
  
  set (hgt, "__orig_data__", orig_data);

  ## Update data of new children only
  xform_data (hgt, hnew);

endfunction


## Need BIST tests here
