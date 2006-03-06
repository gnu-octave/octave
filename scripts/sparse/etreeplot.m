## Copyright (C) 2005 Ivana Varekova
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301  USA

## -*- texinfo -*-
## @deftypefn {Function File} {} etreeplot (@var{tree})
## @deftypefnx {Function File} {} etreeplot (@var{tree}, @var{node_style}, @var{edge_style})
## Plot the elimination tree of the matrix @var{s} or
## @code{@var{s}+@var{s}'}  if @var{s} in non-symmetric.  The optional
## parameters @var{line_style} and @var{edge_style} define the output
## style.
## @seealso{treeplot, gplot}
## @end deftypefn

function etreeplot (s, varargin)
  treeplot (etree (s+s'), varargin{:});
endfunction
