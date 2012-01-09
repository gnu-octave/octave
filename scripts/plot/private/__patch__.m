## Copyright (C) 2007-2012 John W. Eaton, Shai Ayal, Kai Habel
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
## @deftypefn {Function File} {[@var{h}, @var{fail}] =} __patch__ (@var{p}, @dots{})
## Undocumented internal function.
## @end deftypefn

## __patch__ (p, x, y, c)
## Create patch object from x and y with color c and parent p.
## Return handle to patch object.

## Author: Kai Habel

function [h, failed] = __patch__ (p, varargin)

  h = NaN;
  failed = false;

  is_numeric_arg = cellfun (@isnumeric, varargin);

  if (isempty (varargin))
    args = {"xdata", [0; 1; 0], "ydata", [1; 1; 0], "facecolor", [0, 0, 0]};
    args = setvertexdata (args);
  elseif (isstruct (varargin{1}))
    if (isfield (varargin{1}, "vertices") && isfield (varargin{1}, "faces"))
      args{1} = "faces";
      args{2} = getfield(varargin{1}, "faces");
      args{3} = "vertices";
      args{4} = getfield(varargin{1}, "vertices");
      args{5} = "facevertexcdata";
      if (isfield (varargin{1}, "facevertexcdata"))
        args{6} = getfield(varargin{1}, "facevertexcdata");
      else
        args{6} = [];
      endif
      args = [args; varargin(2:end)];
      args = setdata (args);
    else
      failed = true;
    endif
  elseif (is_numeric_arg(1))
    if (nargin < 3 || ! is_numeric_arg(2))
      failed = true;
    else

      if (nargin > 4 && all (is_numeric_arg(1:4)))
        x = varargin{1};
        y = varargin{2};
        z = varargin{3};
        c = varargin{4};
        iarg = 5;
      elseif (nargin > 3 && all (is_numeric_arg(1:3)))
        x = varargin{1};
        y = varargin{2};
        iarg = 4;
        if (rem (nargin - iarg, 2) == 1)
          c = varargin {iarg};
          z = varargin{3};
          iarg = 5;
        else
          z = [];
          c = varargin{3};
        endif
      elseif (nargin > 2 && all (is_numeric_arg(1:2)))
        x = varargin{1};
        y = varargin{2};
        z = [];
        iarg = 3;
        if (rem (nargin - iarg, 2) == 1)
          c = varargin {iarg};
          iarg++; 
        else
          c = [];
        endif
      endif

      if (isvector (x))
        x = x(:);
        y = y(:);
        z = z(:);
        if (isnumeric (c))
          if (isvector (c) && numel (c) == numel (x))
            c = c(:);
          elseif (size (c, 1) != numel (x) && size (c, 2) == numel (x))
            c = c.';
          endif
        endif
      endif
      args{1} = "xdata";
      args{2} = x;
      args{3} = "ydata";
      args{4} = y;
      args{5} = "zdata";
      args{6} = z;

      if (isnumeric (c))

        if (ndims (c) == 3 && size (c, 2) == 1)
          c = permute (c, [1, 3, 2]);
        endif

        if (isvector (c) && numel (c) == columns (x))
          if (isnan (c))
            args{7} = "facecolor";
            args{8} = [1, 1, 1];
            args{9} = "cdata";
            args{10} = c;
          elseif (isnumeric (c))
            args{7} = "facecolor";
            args{8} = "flat";
            args{9} = "cdata";
            args{10} = c;
          else
            error ("patch: color value not valid");
          endif
        elseif (isvector (c) && numel (c) == 3)
          args{7} = "facecolor";
          args{8} = c;
          args{9} = "cdata";
          args{10} = [];
        elseif (ndims (c) == 3 && size (c, 3) == 3)
          ## CDATA is specified as RGB data
          if ((size (c, 1) == 1 && size (c, 2) == 1) ...
              || (size (c, 1) == 1 && size (c, 2) == columns (x)))
            ## Single patch color or per-face color
            args{7} = "facecolor";
            args{8} = "flat";
            args{9} = "cdata";
            args{10} = c;
          elseif (size (c, 1) == rows (x) && size (c, 2) == columns (x))
            ## Per-vertex color
            args{7} = "facecolor";
            args{8} = "interp";
            args{9} = "cdata";
            agrs{10} = c;
          else
            error ("patch: color value not valid");
          endif
        else
          ## Color Vectors
          if (isempty (c))
            args{7} = "facecolor";
            args{8} = "interp";
            args{9} = "cdata";
            args{10} = [];
          elseif (isequal (size (c), size (x)) && isequal (size (c), size (y)))
            args{7} = "facecolor";
            args{8} = "interp";
            args{9} = "cdata";
            args{10} = c;
          else
            error ("patch: size of x, y, and c must be equal");
          endif
        endif
      elseif (ischar (c) && rem (nargin - iarg, 2) == 0)
        ## Assume that any additional argument over an even number is
        ## color string.
        args{7} = "facecolor";
        args{8} =  tolower (c);
        args{9} = "cdata";
        args{10} = [];
      else
        args{7} = "facecolor";
        args{8} = [0, 1, 0];
        args{9} = "cdata";
        args{10} = [];
      endif

      args = [args, varargin(iarg:end)];
      args = setvertexdata (args);
    endif
  else
    args = varargin;
    if (any (strcmpi (args, "faces") | strcmpi (args, "vertices")))
      args = setdata (args);
    else
      args = setvertexdata (args);
    endif
  endif

  if (!failed)
    h = __go_patch__ (p, args {:});

    ## Setup listener functions
    addlistener (h, "xdata", @update_data);
    addlistener (h, "ydata", @update_data);
    addlistener (h, "zdata", @update_data);
    addlistener (h, "cdata", @update_data);

    addlistener (h, "faces", @update_fvc);
    addlistener (h, "vertices", @update_fvc);
    addlistener (h, "facevertexcdata", @update_fvc);
  endif
endfunction

function args = delfields(args, flds)
  idx = cellfun (@(x) any (strcmpi (x, flds)), args);
  if (rows (idx) == 1)
    idx = idx | [false, idx(1:end-1)];
  else
    idx = idx | [false; idx(1:end-1)];
  endif
  args (idx) = [];
endfunction

function args = setdata (args)
  args = delfields (args, {"xdata", "ydata", "zdata", "cdata"});
  ## Remove the readonly fields as well
  args = delfields (args, {"type", "uicontextmenu"});
  nargs = length (args);
  idx = find (strcmpi (args, "faces"), 1, "last") + 1;
  if (idx > nargs)
    faces = [];
  else
    faces = args {idx};
  endif
  idx = find (strcmpi (args, "vertices"), 1, "last") + 1;
  if (idx > nargs)
    vert = [];
  else
    vert = args {idx};
  endif
  idx = find (strcmpi (args, "facevertexcdata"), 1, "last") + 1;
  if (isempty(idx) || idx > nargs)
    fvc = [];
  else
    fvc = args {idx};
  endif
  idx = find (strcmpi (args, "facecolor"), 1, "last") + 1;
  if (isempty(idx) || idx > nargs)
    if (!isempty (fvc))
      fc = "flat";
    else
      fc = [0, 1, 0];
    endif
    args = {"facecolor", fc, args{:}};
  endif

  nc = size (faces, 1);
  idx = faces .';
  t1 = isnan (idx);
  for i = find (any (t1))
    first_idx_in_column = find (t1(:,i), 1);
    idx(first_idx_in_column:end,i) = idx(first_idx_in_column-1,i);
  endfor
  x = reshape (vert(:,1)(idx), size (idx));
  y = reshape (vert(:,2)(idx), size (idx));
  if (size(vert,2) > 2)
    z = reshape (vert(:,3)(idx), size (idx));
  else
    z = [];
  endif

  if (size(fvc, 1) == nc || size (fvc, 1) == 1)
    c = reshape (fvc, [1, size(fvc)]);
  else
    if (size(fvc, 2) == 3)
      c = cat(3, reshape (fvc(idx, 1), size(idx)),
              reshape (fvc(idx, 2), size(idx)),
              reshape (fvc(idx, 3), size(idx)));
    elseif (isempty (fvc))
      c = [];
    else ## if (size (fvc, 2) == 1)
      c = permute (fvc(faces), [2, 1]);
    endif
  endif
  args = {"xdata", x, "ydata", y, "zdata", z, "cdata", c, args{:}};
endfunction

function args = setvertexdata (args)
  args = delfields (args, {"vertices", "faces", "facevertexcdata"});
  ## Remove the readonly fields as well
  args = delfields (args, {"type", "uicontextmenu"});
  nargs = length (args);
  idx = find (strcmpi (args, "xdata"), 1, "last") + 1;
  if (idx > nargs)
    x = [];
  else
    x = args {idx};
  endif
  idx = find (strcmpi (args, "ydata"), 1, "last") + 1;
  if (idx > nargs)
    y = [];
  else
    y = args {idx};
  endif
  idx = find (strcmpi (args, "zdata"), 1, "last") + 1;
  if (isempty(idx) || idx > nargs)
    z = [];
  else
    z = args {idx};
  endif
  idx = find (strcmpi (args, "cdata"), 1, "last") + 1;
  if (isempty(idx) || idx > nargs)
    c = [];
  else
    c = args {idx};
  endif
  idx = find (strcmpi (args, "facecolor"), 1, "last") + 1;
  if (isempty(idx) || idx > nargs)
    if (!isempty (c))
      fc = "flat";
    else
      fc = [0, 1, 0];
    endif
    args = {"facecolor", fc, args{:}};
  endif

  [nr, nc] = size (x);
  if (nr == 1 && nc > 1)
    nr = nc;
    nc = 1;
  end
  if (!isempty (z))
    vert = [x(:), y(:), z(:)];
  else
    vert = [x(:), y(:)];
  endif
  faces = reshape (1:numel(x), nr, nc);
  faces = faces';

  if (ndims (c) == 3)
    fvc = reshape (c, size (c, 1) * size (c, 2), size(c, 3));
  else
    fvc = c(:);
  endif

  args = {"faces", faces, "vertices", vert, "facevertexcdata", fvc, args{:}};
endfunction

function update_data (h, d)
  update_handle (h, false);
endfunction

function update_fvc (h, d)
  update_handle (h, true);
endfunction

function update_handle (h, isfv)
  persistent recursive = false;

  if (! recursive)
    recursive = true;
    f = get (h);
    if (isfv)
      set (h, setdata ([fieldnames(f), struct2cell(f)].'(:)){:});
    else
      set (h, setvertexdata ([fieldnames(f), struct2cell(f)].'(:)){:});
    endif
    recursive = false;
  endif
endfunction
