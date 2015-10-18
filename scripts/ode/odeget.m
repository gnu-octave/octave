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
## is an ODE option structure and the second input argument @var{field} is a
## string specifying an option name, then return the option value @var{val}
## corresponding to to @var{field} from @var{ode_opt}.
##
## If called called with an optional third input argument, and @var{field} is
## not set in the structure @var{ode_opt}, then return the default value
## @var{default} instead.
## @seealso{odeset}
## @end deftypefn

## FIXME: 4th input argument "opt" is undocumented.

function val = odeget (ode_opt, field, default = [], opt)

  if (nargin < 1 || nargin > 4)
    print_usage ();
  endif

  ## Shortcut for empty option structures
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

  ## Check if the given struct is a valid ODEOPT struct
  ode_struct_value_check ("odeget", ode_opt);

  ## Define all the possible ODEOPT fields
  persistent options = {"AbsTol"; "BDF"; "Events"; "InitialSlope";
                        "InitialStep"; "Jacobian"; "JConstant"; "JPattern";
                        "Mass"; "MassConstant"; "MassSingular"; "MaxOrder";
                        "MaxStep"; "MStateDependence"; "MvPattern";
                        "NonNegative"; "NormControl"; "OutputFcn"; "OutputSel";
                        "Refine"; "RelTol"; "Stats"; "Vectorized"};

  exactmatch = true;
  match = find (strcmpi (field, options));
  if (isempty (match))
    match = find (strncmpi (field, options, length (field)));
    exactmatch = false;
  endif

  if (isempty (match))
    ## Possibly a custom user-defined option
    try
      val = ode_opt.(field);
    catch
      warning ("Octave:invalid-input-arg",
               "odeget: no field '%s' exists in ODE_OPT\n", field);
      val = default;
    end_try_catch
  elseif (numel (match) == 1)
    if (! exactmatch)
      warning ("odeget:NoExactMatching",
               "odeget: no exact match for '%s'.  Assuming '%s'.\n",
               field, options{match});
    endif
    val = [];
    try
      val = ode_opt.(options{match});
    end_try_catch
    if (isempty (val))
      val = default;
    endif
  else
    error ("odeget: no exact match for '%s'.  Possible fields found: %s.",
           field, strjoin (options(match), ", "));
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
%!assert (odeget (odeset (), "AbsTol", 1e-6, "fast"), [])
%!assert (odeget (odeset (), "AbsTol", 1e-6, "fast_not_empty"), 1e-6)
%!assert (odeget (odeset (), "AbsTol", 1e-9), 1e-9)

%!error odeget ()
%!error odeget (1)
%!error odeget (1,2,3,4,5)
%!error <ODE_OPT must be a valid ODE_STRUCT> odeget (1, "opt1")
%!error <FIELD must be a string> odeget (struct ("opt1", 1), 1)
%!warning <no field 'foo' exists> odeget (struct ("opt1", 1), "foo");
%!warning <no exact match for 'Rel'.  Assuming 'RelTol'> odeget (struct ("RelTol", 1), "Rel");
%!error <Possible fields found: InitialSlope, InitialStep> odeget (odeset (), "Initial")

