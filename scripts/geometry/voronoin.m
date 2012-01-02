## Copyright (C) 2000-2012 Kai Habel
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
## @deftypefn  {Function File} {[@var{C}, @var{F}] =} voronoin (@var{pts})
## @deftypefnx {Function File} {[@var{C}, @var{F}] =} voronoin (@var{pts}, @var{options})
## Compute N-dimensional Voronoi facets.  The input matrix @var{pts}
## of size [n, dim] contains n points in a space of dimension dim.
## @var{C} contains the points of the Voronoi facets.  The list @var{F}
## contains, for each facet, the indices of the Voronoi points.
##
## An optional second argument, which must be a string or cell array of strings,
## contains options passed to the underlying qhull command.
## See the documentation for the Qhull library for details
## @url{http://www.qhull.org/html/qh-quick.htm#options}.
## @seealso{voronoi, convhulln, delaunayn}
## @end deftypefn

## Author: Kai Habel <kai.habel@gmx.de>
## First Release: 20/08/2000

## 2003-12-14 Rafael Laboissiere <rafael@laboissiere.net>
## Added optional second argument to pass options to the underlying
## qhull command

function [C, F] = voronoin (pts, options)

  if (nargin != 1 && nargin != 2)
    print_usage ();
  endif

  [np, dim] = size (pts);

  if (np <= dim)
    error ("voronoin: number of points must be greater than their dimension");
  endif

  caller = "voronoin";

  if (nargin == 1)
    [C, F] = __voronoi__ (caller, pts);
  else
    [C, F] = __voronoi__ (caller, pts, options);
  endif

endfunction


%% FIXME: Need functional tests

%% FIXME: Need input validation tests

