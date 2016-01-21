## Copyright (C) 2015, Carlo de Falco
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
## @deftypefn {} {@var{val} =} known_option_names ()
## Return a list of known names for ode options.
## @seealso{odeset, odeget}
## @end deftypefn

function ret = known_option_names ()

ret = {"AbsTol"; "BDF"; "Events"; "InitialSlope";
       "InitialStep"; "Jacobian"; "JConstant"; "JPattern";
       "Mass"; "MassConstant"; "MassSingular"; "MaxOrder";
       "MaxStep"; "MStateDependence"; "MvPattern";
       "NonNegative"; "NormControl"; "OutputFcn"; "OutputSel";
       "Refine"; "RelTol"; "Stats"; "Vectorized";
       "TimeStepSize"; "TimeStepNumber"};

endfunction
