## Copyright (C) 2008-2012 David Bateman
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
## @deftypefn {Function File} {@var{h} =} __clabel__ (@var{c}, @var{v}, @var{hparent}, @var{label_spacing}, @var{z}, @var{varargin})
## Undocumented internal function.
## @end deftypefn

function h = __clabel__ (c, v, hparent, label_spacing, z, varargin)
  ## FIXME
  ## Assume that the plot size is 4 by 3 inches.
  lims = axis ();
  xspacing = 72 * 4 / abs(lims(1) - lims(2));
  yspacing = 72 * 3 / abs(lims(3) - lims(4));

  if (isscalar (hparent) && ishandle(hparent)
      && strcmp (get (hparent, "type"), "hggroup"))
    x = get (hparent, "xdata");
    xmin = min (x(:));
    xmax = max (x(:));
    y = get (hparent, "ydata");
    ymin = min (y(:));
    ymax = max (y(:));
  else
    i1 = 1;
    while (i1 < length (c))
      clev = c(1,i1);
      clen = c(2,i1);
      p = c(:, i1+1:i1+clen);

      xmin = min (c(1,:));
      xmax = max (c(1,:));
      ymin = min (c(2,:));
      ymax = max (c(2,:));

      i1 += clen+1;
    endwhile
  endif

  ## Decode contourc output format and place labels.
  i1 = 1;
  h = [];
  while (i1 < length (c))
    clev = c(1,i1);
    clen = c(2,i1);

    if (!isempty (v) && ! any (find (clev == v)))
      i1 += clen+1;
      continue;
    endif

    p = c(:, i1+1:i1+clen) .* repmat ([xspacing; yspacing], 1, clen);
    d = sqrt (sumsq (diff (p, 1, 2)));
    cumd = cumsum (d);
    td = sum(d);
    ntag = ceil (td / label_spacing);

    if (all (c(:,i1+1) == c(:,i1+clen)))
      Spacing = td / ntag;
      pos = Spacing / 2 + [0:ntag-1] * Spacing;
    else
      pos = zeros(1, ntag);
      pos(1) = (td - label_spacing * (ntag - 1)) ./ 2;
      pos(2:ntag) = pos(1) + [1:ntag-1] * label_spacing;
    endif

    j1 = 2;
    tlabel = sprintf ("%g", clev);
    for i = 1 : ntag
      tagpos = pos(i);

      while (j1 < clen && cumd(j1) < tagpos)
        j1++;
      endwhile
      tpos = sum(c(:,i1+j1-1:i1+j1), 2) ./ 2;

      if (tpos(1) != xmin &&  tpos(1) != xmax
          && tpos(2) != ymin &&  tpos(2) != ymax)
        trot = 180 / pi * atan2 (diff (c(2,i1+j1-1:i1+j1)),
                                 diff (c(1,i1+j1-1:i1+j1)));

        if (ischar (z))
          ht = text (tpos(1), tpos(2), clev, tlabel, "rotation", trot,
                     "parent", hparent, "horizontalalignment", "center",
                     "userdata", clev, varargin{:});
        elseif (!isempty (z))
          ht = text (tpos(1), tpos(2), z, tlabel, "rotation", trot,
                     "parent", hparent, "horizontalalignment", "center",
                     "userdata", clev, varargin{:});
        else
          ht = text (tpos(1), tpos(2), tlabel, "rotation", trot,
                     "parent", hparent, "horizontalalignment", "center",
                     "userdata", clev, varargin{:});
        endif
        h = [h; ht];
      endif
    endfor
    i1 += clen+1;
  endwhile
endfunction