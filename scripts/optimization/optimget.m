## Copyright (C) 2008-2012 Jaroslav Hajek
## Copyright (C) 2009 VZLU Prague
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
## @deftypefn  {Function File} {} optimget (@var{options}, @var{parname})
## @deftypefnx {Function File} {} optimget (@var{options}, @var{parname}, @var{default})
## Return a specific option from a structure created by
## @code{optimset}.  If @var{parname} is not a field of the @var{options}
## structure, return @var{default} if supplied, otherwise return an
## empty matrix.
## @end deftypefn

function retval = optimget (options, parname, default)

  if (nargin < 2 || nargin > 4 || ! isstruct (options) || ! ischar (parname))
    print_usage ();
  endif

  opts = __all_opts__ ();
  idx = lookup (tolower (opts), tolower (parname), "m");

  if (idx)
    parname = opts{idx};
  else
    warning ("unrecognized option: %s", parname);
  endif
  if (isfield (options, parname))
    retval = options.(parname);
  elseif (nargin > 2)
    retval = default;
  else
    retval = [];
  endif

endfunction

