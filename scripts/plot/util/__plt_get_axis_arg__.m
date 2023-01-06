########################################################################
##
## Copyright (C) 1996-2023 The Octave Project Developers
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
## @deftypefn {} {[@var{h}, @var{varargin}, @var{narg}] =} __plt_get_axis_arg__ (@var{caller}, @var{varargin})
## Undocumented internal function.
## @end deftypefn

function [h, varargin, narg] = __plt_get_axis_arg__ (caller, varargin)

  h = [];

  ## Look for a scalar which is a graphics handle but not the
  ## root object (0) or an ordinary figure (integer).
  if (! isempty (varargin) && isscalar (varargin{1})
      && ishghandle (varargin{1}) && varargin{1} != 0
      && ! isfigure (varargin{1}))
    htmp = varargin{1};
    if (! isaxes (htmp))
      error ("%s: first argument must be axes handle", caller);
    endif
    if (! strcmp (get (htmp, "tag"), "legend"))
      h = htmp;
      varargin(1) = [];
    endif
  ## Look for "parent"/axis prop/value pair
  elseif (numel (varargin) > 1)
    ## FIXME: This can be fooled by any string "parent" such as
    ##        the prop/val pair "tag"/"parent".
    ## varargin may contain char arrays. Silence respective warning.
    warning ("off", "Octave:charmat-truncated", "local");
    parent = find (strcmpi (varargin, "parent"), 1, "last");
    if (! isempty (parent))
      if (parent == numel (varargin) || ! ishghandle (varargin{parent+1}))
        error ('%s: "parent" value must be an axes handle', caller);
      endif
      htmp = varargin{parent+1};
      if (isaxes (htmp) && ! strcmp (get (htmp, "tag"), "legend"))
        h = htmp;
        varargin(parent:parent+1) = [];
      else
        ## "parent" property for some other type like hggroup
        h = [ancestor(htmp, "axes"), htmp];
      endif
    endif
  endif

  narg = length (varargin);

endfunction


## No test needed for internal helper function.
%!assert (1)
