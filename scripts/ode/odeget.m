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
## @deftypefn  {Function File} {@var{val} =} odeget (@var{ode_opt}, @var{field})
## @deftypefnx {Function File} {@var{val} =} odeget (@var{ode_opt}, @var{field}, @var{default})
##
## Query the value of the property @var{field} in the ODE options structure
## @var{ode_opt}.
##
## If called with two input arguments and the first input argument @var{ode_opt}
## is a structure and the second input argument @var{field} is a string then
## return the option value @var{val} that is specified by the option name
## @var{field} in the ODE option structure @var{ode_opt}.
##
## If called called with an optional third input argument then return the
## default value @var{default} if @var{field} is not set in the structure
## @var{ode_opt}.
## @seealso{odeset}
## @end deftypefn

## FIXME: 4th input argument 'opt' is undocumented.

## Note: 2006-10-22, Thomas Treichl
##   We cannot create a function of the form odeget (@var{odestruct},
##   @var{name1}, @var{name2}) because we would get a mismatch with
##   the function form 1 like described above.

function val = odeget (ode_opt, field, default = [], opt)

  if (nargin == 1 || nargin > 4)
    print_usage ();
  endif

  ## Shortcut for empty options structures
  if (isempty (ode_opt))
    if (nargin < 3)
      val = [];
    else
      val = default;
    endif
    return;
  endif

  if (! isstruct (ode_opt))
    error ("odeget: ODE_OPT must be a valid ODE_STRUCT");
  elseif (! ischar (field))
    error ("odeget: FIELD must be a string");
  endif

  if (nargin == 4 && strcmpi (opt, "fast"))
    try
      val = ode_opt.(field);
    catch
      val = default;
    end_try_catch
    return;
  endif

  if (nargin == 4 && strcmpi (opt, "fast_not_empty"))
    try
      val = ode_opt.(field);
      if (isempty (val))
        val = default;
      endif
    catch
      val = default;
    end_try_catch
    return;
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

    if (isempty (pos))  # no match for the given option
      if (nargin == 2)
        error ("odeget: invalid property. No property found with name '%s'",
               field);
      endif
      warning ("odeget:NoExactMatching",
               "no property found with name '%s'. ",
               "Assuming default value.", field);
      val = default;
      return;
    endif

    if (rows (pos) == 1)  # one matching
      if (! strcmp (lower (deblank (field)),
                    lower (deblank (options(pos,:)))) )
        warning ("odeget:InvalidArgument",
                 "no exact matching for '%s'. ",
                 "Assuming you were intending '%s'.",
                 field, deblank (options(pos,:)));
      endif
      val = ode_opt.(deblank (options(pos,:)));
      if (isempty (val))
        val = default;
      endif
      return;
    endif

    ## FIXME: Do we really need interactive selection?
    ##        Matlab doesn't appear to offer this.
    ## if there are more matching, ask the user to be more precise
    warning ("OdePkg:InvalidArgument",
             "no exact matching for '%s'. %d possible fields were found.",
             field, rows (pos));
    for j = 1:(rows (pos))
      printf ("%s\n", deblank (options(pos(j),:)));
    endfor
    do
      printf ("Please insert field name again.\n");
      field = input ("New field name: ");
    until (ischar (field))
  endwhile

endfunction


%!demo
%! # Return the manually changed value RelTol of the OdePkg options
%! # structure A.  If RelTol wouldn't have been changed then an
%! # empty matrix value would have been returned.
%!
%! A = odeset ("RelTol", 1e-1, "AbsTol", 1e-2);
%! odeget (A, "RelTol", [])

%!test
%! wstate = warning ("off", "OdePkg:InvalidArgument");
%! unwind_protect
%!   assert (odeget (odeset (), "RelTol"), []);
%!   assert (odeget (odeset (), "RelTol", 10), 10);
%!   assert (odeget (odeset (), "Stats"), []);
%!   assert (odeget (odeset (), "Stats", "on"), "on");
%!   assert (odeget (odeset (), "AbsTol", 1e-6, "fast"), []);
%!   assert (odeget (odeset (), "AbsTol", 1e-6, "fast_not_empty"), 1e-6);
%!   assert (odeget (odeset (), "AbsTol", 1e-9), 1e-9);
%! unwind_protect_cleanup
%!   warning (wstate);
%! end_unwind_protect

