## Copyright (C) 2012 John W. Eaton
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
## @deftypefn  {Function File} {@var{current_state}} recycle ()
## @deftypefnx {Function File} {@var{old_state}} recycle (@var{new_state})
## Query or set the preference for recycling deleted files.
##
## Recycling files instead of permanently deleting them is currently not
## implemented in Octave.  To help avoid accidental data loss it
## is an error to attempt enable file recycling.
## @seealso{delete}
## @end deftypefn

## Author: jwe

function retval = recycle (state)

  persistent current_state = "off";

  if (nargin > 1)
    print_usage ();
  endif

  if (nargin == 0 || nargout > 0)
    retval = current_state;
  endif

  if (nargin == 1)
    if (ischar (state))
      if (strcmpi (state, "on"))
        error ("recycle: recycling files is not implemented");
      elseif (strcmpi (state, "off"))
        current_state = "off";
      else
        error ("recycle: invalid value of STATE = '%s'", state);
      endif
    else
      error ("recycle: expecting STATE to be a character string");
    endif
  endif

endfunction

%!error recycle ("on");
%!error recycle ("on", "and I mean it");
%!error recycle (1);

%!test
%! recycle ("off");
%! assert (recycle ("off"), "off");
