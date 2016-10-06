## Copyright (C) 2016, Carlo de Falco
## Copyright (C) 2016, Francesco Faccio <francesco.faccio@mail.polimi.it>
## Copyright (C) 2013-2016 Roberto Porcu' <roberto.porcu@polimi.it>
## Copyright (C) 2006-2012 Thomas Treichl <treichl@users.sourceforge.net>
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
## @deftypefn  {} {@var{val} =} odeget (@var{ode_opt}, @var{field})
## @deftypefnx {} {@var{val} =} odeget (@var{ode_opt}, @var{field}, @var{default})
##
## Query the value of the property @var{field} in the ODE options structure
## @var{ode_opt}.
##
## If called with two input arguments and the first input argument
## @var{ode_opt} is an ODE option structure and the second input argument
## @var{field} is a string specifying an option name, then return the option
## value @var{val} corresponding to @var{field} from @var{ode_opt}.
##
## If called with an optional third input argument, and @var{field} is
## not set in the structure @var{ode_opt}, then return the default value
## @var{default} instead.
## @seealso{odeset}
## @end deftypefn

## FIXME: 4th input argument "opt" is undocumented.

function val = odeget (ode_opt, field, default = [], opt = "")

  validateattributes (ode_opt, {'struct'}, {'nonempty'});
  validateattributes (field, {'char'}, {'nonempty'});
  
  if (! isfield (ode_opt, field))
    error ('Octave:odeget:InvalidPropName',
           'odeget: Unrecognized property name "%s".', field);
  else
    val = ode_opt.(field);
    if (isempty (val)) 
      val = default;
    endif
  endif
  
endfunction


%!demo
%! # Return the manually changed value RelTol of the ODE options
%! # structure A.  If RelTol wouldn't have been changed then an
%! # empty matrix value would have been returned.
%!
%! A = odeset ("RelTol", 1e-1, "AbsTol", 1e-2);
%! odeget (A, "RelTol", [])

%!assert (odeget (odeset (), "RelTol"), [])
%!assert (odeget (odeset ("RelTol", 10), "RelTol"), 10)
%!assert (odeget (odeset (), "RelTol", 10), 10)
%!assert (odeget (odeset (), "Stats"), [])
%!assert (odeget (odeset (), "Stats", "on"), "on")
%!assert (odeget (odeset (), "Mass"), [])
%!assert (odeget (odeset (), "AbsTol", 1e-9), 1e-9)
%!assert (odeget (odeset ("AbsTol", 1e-9), "AbsTol", []), 1e-9)
%!assert (odeget (odeset ('foo', 42), 'foo'), 42)

%!error odeget ()
%!error odeget (1)
%!error odeget (1,2,3,4,5)
%!error odeget (1, "opt1")
%!error odeget (struct ("opt1", 1), 1)
%!error odeget (struct ("opt1", 1), "foo");

