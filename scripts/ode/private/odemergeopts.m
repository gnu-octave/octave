## Copyright (C) 2016 Francesco Faccio <francesco.faccio@mail.polimi.it>
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

function options = odemergeopts (caller, useroptions, options, classes,
                                 attributes);

  for [value, key] = options;

    if (isfield (useroptions, key) && ! isempty (useroptions.(key)))

      if (! strcmp (classes.(key), "char"))
        validateattributes (useroptions.(key), classes.(key),
                            attributes.(key), caller, key);

      elseif (ischar (useroptions.(key)))
        validatestring (useroptions.(key), attributes.(key), caller, key);

      else
        error ("Octave:invalid-input-arg",
               [caller ": invalid value assigned to field '%s'"], key);
      endif

      options.(key) = useroptions.(key);

    endif

  endfor

endfunction

