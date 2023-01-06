########################################################################
##
## Copyright (C) 2016-2023 The Octave Project Developers
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
## @deftypefn {} {@var{options} =} odemergeopts (@var{caller}, @var{useroptions}, @var{options}, @var{classes}, @var{attributes}, @var{solver})
## Undocumented internal function.
## @end deftypefn

## FIXME: there are some calls to odemergeopts with a "solver" argument
## but we don't use that here.  Should the calls be fixed or should we
## do something with the solver argument here?

function options = odemergeopts (caller, useroptions, options, classes,
                                 attributes, solver);

  for [value, key] = options

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
