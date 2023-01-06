########################################################################
##
## Copyright (C) 2006-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{odestruct} =} odeset ()
## @deftypefnx {} {@var{odestruct} =} odeset (@var{"field1"}, @var{value1}, @var{"field2"}, @var{value2}, @dots{})
## @deftypefnx {} {@var{odestruct} =} odeset (@var{oldstruct}, @var{"field1"}, @var{value1}, @var{"field2"}, @var{value2}, @dots{})
## @deftypefnx {} {@var{odestruct} =} odeset (@var{oldstruct}, @var{newstruct})
## @deftypefnx {} {} odeset ()
##
## Create or modify an ODE options structure.
##
## When called with no input argument and one output argument, return a new ODE
## options structure that contains all possible fields initialized to their
## default values.  If no output argument is requested, display a list of
## the common ODE solver options along with their default value.
##
## If called with name-value input argument pairs @var{"field1"},
## @var{"value1"}, @var{"field2"}, @var{"value2"}, @dots{} return a new
## ODE options structure with all the most common option fields
## initialized, @strong{and} set the values of the fields @var{"field1"},
## @var{"field2"}, @dots{} to the values @var{value1}, @var{value2},
## @dots{}.
##
## If called with an input structure @var{oldstruct} then overwrite the
## values of the options @var{"field1"}, @var{"field2"}, @dots{} with
## new values @var{value1}, @var{value2}, @dots{} and return the
## modified structure.
##
## When called with two input ODE options structures @var{oldstruct} and
## @var{newstruct} overwrite all values from the structure
## @var{oldstruct} with new values from the structure @var{newstruct}.
## Empty values in @var{newstruct} will not overwrite values in
## @var{oldstruct}.
##
## The most commonly used ODE options, which are always assigned a value
## by @code{odeset}, are the following:
##
## @table @asis
## @item @code{AbsTol}: positive scalar | vector, def. @code{1e-6}
## Absolute error tolerance.
##
## @item @code{BDF}: @{@qcode{"off"}@} | @qcode{"on"}
## Use BDF formulas in implicit multistep methods.
## @emph{Note}: This option is not yet implemented.
##
## @item @code{Events}: function_handle
## Event function.  An event function must have the form
## @code{[value, isterminal, direction] = my_events_f (t, y)}
##
## @item @code{InitialSlope}: vector
## Consistent initial slope vector for DAE solvers.
##
## @item @code{InitialStep}: positive scalar
## Initial time step size.
##
## @item @code{Jacobian}: matrix | function_handle
## Jacobian matrix, specified as a constant matrix or a function of
## time and state.
##
## @item @code{JConstant}: @{@qcode{"off"}@} | @qcode{"on"}
## Specify whether the Jacobian is a constant matrix or depends on the
## state.
##
## @item @code{JPattern}: sparse matrix
## If the Jacobian matrix is sparse and non-constant but maintains a
## constant sparsity pattern, specify the sparsity pattern.
##
## @item @code{Mass}: matrix | function_handle
## Mass matrix, specified as a constant matrix or a function of
## time and state.
##
## @item @code{MassSingular}: @{@qcode{"maybe"}@} | @qcode{"yes"} | @qcode{"on"}
## Specify whether the mass matrix is singular.
##
## @item @code{MaxOrder}: @{@qcode{5}@} | @qcode{4} | @qcode{3} | @qcode{2} | @qcode{1}
## Maximum order of formula.
##
## @item @code{MaxStep}: positive scalar
## Maximum time step value.
##
## @item @code{MStateDependence}: @{@qcode{"weak"}@} | @qcode{"none"} | @qcode{"strong"}
## Specify whether the mass matrix depends on the state or only on time.
##
## @item @code{MvPattern}: sparse matrix
## If the mass matrix is sparse and non-constant but maintains a
## constant sparsity pattern, specify the sparsity pattern.
## @emph{Note}: This option is not yet implemented.
##
## @item @code{NonNegative}: scalar | vector
## Specify elements of the state vector that are expected to remain
## non-negative during the simulation.
##
## @item @code{NormControl}: @{@qcode{"off"}@} | @qcode{"on"}
## Control error relative to the 2-norm of the solution, rather than its
## absolute value.
##
## @item @code{OutputFcn}: function_handle
## Function to monitor the state during the simulation.  For the form of
## the function to use @pxref{XREFodeplot,,@code{odeplot}}.
##
## @item @code{OutputSel}: scalar | vector
## Indices of elements of the state vector to be passed to the output
## monitoring function.
##
## @item @code{Refine}: positive scalar
## Specify whether output should be returned only at the end of each
## time step or also at intermediate time instances.  The value should be
## a scalar indicating the number of equally spaced time points to use
## within each timestep at which to return output.
##
## @item @code{RelTol}: positive scalar
## Relative error tolerance.
##
## @item @code{Stats}: @{@qcode{"off"}@} | @qcode{"on"}
## Print solver statistics after simulation.
##
## @item @code{Vectorized}: @{@qcode{"off"}@} | @qcode{"on"}
## Specify whether @code{odefcn} can be passed multiple values of the
## state at once.
##
## @end table
##
## Field names that are not in the above list are also accepted and
## added to the result structure.
##
## @seealso{odeget}
## @end deftypefn

function odestruct = odeset (varargin)

  persistent p;

  if (isempty (p))
    ## FIXME: Add an inexact match option once it is available in inputParser.
    ## See bug #49364.
    p = inputParser ();
    p.addParameter ("AbsTol", []);
    p.addParameter ("BDF", []);
    p.addParameter ("Events", []);
    p.addParameter ("InitialSlope", []);
    p.addParameter ("InitialStep", []);
    p.addParameter ("Jacobian", []);
    p.addParameter ("JConstant", []);
    p.addParameter ("JPattern", []);
    p.addParameter ("Mass", []);
    p.addParameter ("MassSingular", []);
    p.addParameter ("MaxOrder", []);
    p.addParameter ("MaxStep", []);
    p.addParameter ("MStateDependence", []);
    p.addParameter ("MvPattern", []);
    p.addParameter ("NonNegative", []);
    p.addParameter ("NormControl", []);
    p.addParameter ("OutputFcn", []);
    p.addParameter ("OutputSel", []);
    p.addParameter ("Refine", []);
    p.addParameter ("RelTol", []);
    p.addParameter ("Stats", []);
    p.addParameter ("Vectorized", []);
    p.KeepUnmatched = true;
  endif

  if (nargin == 0 && nargout == 0)
    print_options ();
  else
    p.parse (varargin{:});
    odestruct = p.Results;
    odestruct_extra = p.Unmatched;

    xtra_fields = fieldnames (odestruct_extra);
    if (! isempty (xtra_fields))
      ## Merge extra fields into existing odestruct
      for fldname = sort (xtra_fields.')
        fldname = fldname{1};
        warning ("Octave:invalid-input-arg",
                 "odeset: unknown option \"%s\"\n", fldname);
        odestruct.(fldname) = odestruct_extra.(fldname);
      endfor
    endif

  endif

endfunction

## function to print all possible options
function print_options ()

  disp ("List of the most common ODE solver options.");
  disp ("Default values are in square brackets.");
  disp ("");
  disp ('             AbsTol:  scalar or vector, >0, [1e-6]');
  disp ('                BDF:  binary, {["off"], "on"}');
  disp ('             Events:  function_handle, []');
  disp ('       InitialSlope:  vector, []');
  disp ('        InitialStep:  scalar, >0, []');
  disp ('           Jacobian:  matrix or function_handle, []');
  disp ('          JConstant:  binary, {["off"], "on"}');
  disp ('           JPattern:  sparse matrix, []');
  disp ('               Mass:  matrix or function_handle, []');
  disp ('       MassSingular:  switch, {["maybe"], "no", "yes"}');
  disp ('           MaxOrder:  switch, {[5], 1, 2, 3, 4, }');
  disp ('            MaxStep:  scalar, >0, []');
  disp ('   MStateDependence:  switch, {["weak"], "none", "strong"}');
  disp ('          MvPattern:  sparse matrix, []');
  disp ('        NonNegative:  vector of integers, []');
  disp ('        NormControl:  binary, {["off"], "on"}');
  disp ('          OutputFcn:  function_handle, []');
  disp ('          OutputSel:  scalar or vector, []');
  disp ('             Refine:  scalar, integer, >0, []');
  disp ('             RelTol:  scalar, >0, [1e-3]');
  disp ('              Stats:  binary, {["off"], "on"}');
  disp ('         Vectorized:  binary, {["off"], "on"}');

endfunction


%!demo
%! ## A new ODE options structure with default values is created.
%!
%! odeoptA = odeset ();

%!demo
%! ## A new ODE options structure with manually set options
%! ## for "AbsTol" and "RelTol" is created.
%!
%! odeoptB = odeset ("AbsTol", 1e-2, "RelTol", 1e-1);

%!demo
%! ## A new ODE options structure is created from odeoptB with
%! ## a modified value for option "NormControl".
%!
%! odeoptB = odeset ("AbsTol", 1e-2, "RelTol", 1e-1);
%! odeoptC = odeset (odeoptB, "NormControl", "on");

%!test
%! odeoptA = odeset ();
%! assert (isstruct (odeoptA));
%! assert (numfields (odeoptA), 22);
%! assert (all (structfun ("isempty", odeoptA)));

%!shared odeoptB, odeoptC
%!test
%! odeoptB = odeset ("ABSTOL", 1e-2, "reltol", 1e-1);
%! assert (odeoptB.AbsTol, 1e-2);  # Check canonicalization of name
%! assert (odeoptB.RelTol, 1e-1);

%!test
%! odeoptC = odeset (odeoptB, "NormControl", "on");
%! assert (odeoptC.AbsTol, 1e-2);       # check values from first struct copied
%! assert (odeoptC.NormControl, "on");  # check new values override old ones

%!test
%! odeoptD = odeset (odeoptB, odeoptC);
%! assert (odeoptD, odeoptC);

## Test custom user-defined option
%!test
%! warning ("off", "Octave:invalid-input-arg", "local");
%! odeopt = odeset ("NewtonTol", 3);
%! assert (odeopt.NewtonTol, 3);

## FIXME: Add an inexact match option once it is available in inputParser.
## See bug #49364.
## %!warning <no exact match for 'Rel'.  Assuming 'RelTol'> odeset ("Rel", 1);
## %!error <Possible fields found: InitialSlope, InitialStep> odeset ("Initial", 1)

## Test input validation
%!error <argument 'OPT1' is not a valid parameter> odeset ("opt1")
%!error odeset (1, 1)
%!error <argument 'OPT1' is not a valid parameter> odeset (odeset (), "opt1")
%!error odeset (odeset (), 1, 1)
