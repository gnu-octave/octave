## Copyright (C) 2013, Roberto Porcu' <roberto.porcu@polimi.it>
## Copyright (C) 2006-2012, Thomas Treichl <treichl@users.sourceforge.net>
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
## @deftypefn  {Function File} {@var{option} =} odeget (@var{ode_opt}, @var{field})
## @deftypefnx {Function File} {@var{option} =} odeget (@var{ode_opt}, @var{field}, @var{default})
##
## Query the value of the property @var{field} in the ODE options structure
## @var{ode_opt}.
##
## If this function is called with two input arguments and the first input
## argument @var{ode_opt} is of type structure array and the second input
## argument @var{field} is of type string then return the option value
## @var{res} that is specified by the option name @var{field} in the ODE
## option structure @var{ode_opt}.  Optionally if this function is called
## with a third input argument then return the default value @var{default} if
## @var{field} is not set in the structure @var{ode_opt}.
## @seealso{odeset}
## @end deftypefn

## Note: 2006-10-22, Thomas Treichl
##   We cannot create a function of the form odeget (@var{odestruct},
##   @var{name1}, @var{name2}) because we would get a mismatch with
##   the function form 1 like described above.

function res = odeget (ode_opt, field, default, opt)

  ## Check number and types of input arguments
  if (nargin == 1)
    error ("OdePkg:InvalidArgument",
           "input arguments number must be at least 2")
  endif

  if (isempty (ode_opt))
    if (nargin == 2)
      res = [];
      return
    endif
    res = default;
    return
  endif

  if (! isstruct (ode_opt))
    error ("OdePkg:InvalidArgument",
           "first input argument must be a valid ODE_STRUCT.");
  endif

  if (! ischar (field))
    error ("OdePkg:InvalidArgument",
           "second input argument must be a string.");
  endif

  if (nargin == 2)
    default = [];
  endif

  if ((nargin == 4)
      && strcmp (lower (deblank (opt)), "fast"))
    if (isfield (ode_opt, field))
      res = ode_opt.(field);
    else
      res = default;
    endif
    return
  endif

  if ((nargin == 4)
      && strcmp (lower (deblank (opt)), "fast_not_empty"))
    if (isfield (ode_opt, field)
        && ! isempty (ode_opt.(field)) )
      res = ode_opt.(field);
    else
      res = default;
    endif
    return
  endif

  ## check if the given struct is a valid OdePkg struct
  ode_struct_value_check (ode_opt);

  ## define all the possible OdePkg fields
  options = ["AbsTol"; "Algorithm"; "BDF"; "Choice"; "Eta"; "Events";
             "Explicit"; "InexactSolver"; "InitialSlope"; "InitialStep";
             "Jacobian";"JConstant";"JPattern";"Mass"; "MassConstant";
             "MassSingular"; "MaxNewtonIterations"; "MaxOrder"; "MaxStep";
             "MStateDependence"; "MvPattern"; "NewtonTol"; "NonNegative";
             "NormControl"; "OutputFcn"; "OutputSave"; "OutputSel";
             "PolynomialDegree"; "QuadratureOrder"; "Refine"; "RelTol";
             "Restart"; "Stats"; "TimeStepNumber"; "TimeStepSize";
             "UseJacobian"; "Vectorized"];

  while (1)
    pos = fuzzy_compare (field, options);

    if (size (pos, 1) == 0) # no matching for the given option
      if (nargin == 2)
        error ("OdePkg:InvalidArgument",
               "invalid property. No property found with name ''%s''.", field);
      endif
      warning ("odeget:NoExactMatching",
               "no property found with name ''%s''. ",
               "Assuming default value.", field);
      res = default;
      return
    endif

    if (size (pos, 1) == 1) # one matching
      if (! strcmp (lower (deblank (field)),
                    lower (deblank (options(pos,:)))) )
        warning ("OdePkg:InvalidArgument",
                 "no exact matching for ''%s''. ",
                 "Assuming you was intending ''%s''.",
                 field, deblank (options(pos,:)));
      endif
      res = ode_opt.(deblank (options(pos,:)));
      if (isempty (res))
        res = default;
        return
      endif
      return
    endif

    ## if there are more matching, ask the user to be more precise
    warning ("OdePkg:InvalidArgument", ...
             "no exact matching for ''%s''. %d possible fields were found.",
             field, size (pos, 1));
    for j = 1:(size (pos, 1))
      fprintf ("%s\n", deblank (options(pos(j),:)));
    endfor
    do
      fprintf ("Please insert field name again.\n");
      field = input ("New field name: ");
    until (ischar (field))
  endwhile

endfunction

%! ## Turn off output of warning messages for all tests, turn them on
%! ## again if the last test is called
%!  warning ('off', 'OdePkg:InvalidArgument');
%!test assert (odeget (odeset (), 'RelTol'), []);
%!test assert (odeget (odeset (), 'RelTol', 10), 10);
%!test assert (odeget (odeset (), 'Stats'), []);
%!test assert (odeget (odeset (), 'Stats', 'on'), 'on');
%!test assert (odeget (odeset (), 'AbsTol', 1.e-6, 'fast'), []);
%!test assert (odeget (odeset (), 'AbsTol', 1.e-6, 'fast_not_empty'), 1.e-6);
%!test assert (odeget (odeset (), 'AbsTol', 1e-9), 1e-9);
%!
%!  warning ('on', 'OdePkg:InvalidArgument');

%!demo
%! # Return the manually changed value RelTol of the OdePkg options
%! # strutcure A. If RelTol wouldn't have been changed then an
%! # empty matrix value would have been returned.
%!
%! A = odeset ('RelTol', 1e-1, 'AbsTol', 1e-2);
%! odeget (A, 'RelTol', [])

## Local Variables: ***
## mode: octave ***
## End: ***
