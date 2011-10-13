## Copyright (C) 2011 Michael Goffioul
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
## @deftypefn  {Function File} {[@var{p}, @var{args}] =} __uiobject_split_args__ (@var{who}, @dots{})
## @end deftypefn

## Author: goffioul

function [parent, args] = __uiobject_split_args__ (who, varargin)

  parent = [];
  args = {};
  offset = 1;

  if (nargin > 1)
    if (ishandle (varargin{1}))
      parent = varargin{1};
      offset = 2;
    elseif (! ischar (varargin{1}))
      error ("%s: invalid parent handle.", who);
    endif

    if (nargin > offset)
      args = varargin(offset:end);
    endif
  endif

  if (! isempty (args))
    i = find (strcmpi (args(1:2:end), "parent"), 1, "first");
    if (! isempty (i) && length (args) >= 2*i)
      parent = args{2*i};
      if (! ishandle (parent))
        error ("%s: invalid parent handle.", who);
      endif
      args([2*i-1, 2*i]) = [];
    endif
  endif

  if (! isempty (parent))
    if (isempty (find (strcmpi (get (parent, "type"), {"figure", "uipanel", "uibuttongroup"}))))
      error ("%s: invalid parent, the parent must be a figure, uipanel or uibuttongroup handle", who);
    endif
  else
    parent = gcf ();
  endif

endfunction
