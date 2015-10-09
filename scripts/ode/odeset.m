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
## @deftypefn  {Function File} {} odeset ()
## @deftypefnx {Function File} {@var{odestruct} =} odeset (@var{"field1"}, @var{value1}, @var{"field2"}, @var{value2}, @dots{})
## @deftypefnx {Function File} {@var{odestruct} =} odeset (@var{oldstruct}, @var{"field1"}, @var{value1}, @var{"field2"}, @var{value2}, @dots{})
## @deftypefnx {Function File} {@var{odestruct} =} odeset (@var{oldstruct}, @var{newstruct})
##
## Create or modify an ODE options structure.
##
## When called without an input argument, return a new ODE options structure
## that contains all possible fields initialized to their default values.
##
## If called with string input arguments @var{"field1"}, @var{"field2"},
## @dots{} identifying valid ODE options then return a new ODE options
## structure with all possible fields initialized @strong{and} set the values
## of the fields @var{"field1"}, @var{"field2"}, @dots{} to the values
## @var{value1}, @var{value2}, @dots{}
##
## If called with an input structure @var{oldstruct} then overwrite the values
## of the options @var{"field1"}, @var{"field2"}, @dots{} with new values
## @var{value1}, @var{value2}, @dots{} and return the modified structure.
##
## When called with two input ODE options structures @var{oldstruct} and
## @var{newstruct} overwrite all values from the structure @var{oldstruct} with
## new values from the structure @var{newstruct}.  Empty values in
## @var{newstruct} will not overwrite values in @var{oldstruct}.
## @seealso{odeget}
## @end deftypefn

function odestruct = odeset (varargin)

  ## Special calling syntax to display defaults
  if (nargin == 0 && nargout == 0)
    print_options ();
    return;
  endif

  ## Column vector of all possible OdePkg options
  persistent options = {"AbsTol"; "Algorithm"; "BDF"; "Choice"; "Eta"; "Events";
                        "Explicit"; "InexactSolver"; "InitialSlope";
                        "InitialStep"; "Jacobian"; "JConstant"; "JPattern";
                        "Mass"; "MassConstant"; "MassSingular";
                        "MaxNewtonIterations"; "MaxOrder"; "MaxStep";
                        "MStateDependence"; "MvPattern"; "NewtonTol";
                        "NonNegative"; "NormControl"; "OutputFcn"; "OutputSave";
                        "OutputSel"; "PolynomialDegree"; "QuadratureOrder";
                        "Refine"; "RelTol"; "Restart"; "Stats";
                        "TimeStepNumber"; "TimeStepSize"; "UseJacobian";
                        "Vectorized"};

  ## initialize output
  odestruct = cell2struct (cell (numel (options), 1), options);

  if (nargin == 0)
    return;
  endif

  if (isstruct (varargin{1}))
    oldstruct = varargin{1};
    ode_struct_value_check (oldstruct);

    oldstruct_fldnames = (fieldnames (oldstruct)).';

    ## Copy oldstruct values into output odestruct
    for fldname = oldstruct_fldnames
      name = lower (fldname{1});

      exactmatch = true;
      match = find (strcmpi (name, options));
      if (isempty (match))
        match = find (strncmpi (name, options, length (name)));
        exactmatch = false;
      endif

      if (isempty (match))
        error ("odeset: invalid property '%s'", fldname{1});
      elseif (numel (match) == 1)
        if (! exactmatch)
          warning ("odeset:NoExactMatching",
                   "no exact match for '%s'.  Assuming '%s'.",
                   name, options{match});
        endif
        odestruct.(options{match}) = oldstruct.(fldname{1});
      else
        error ("odeset: no exact match for '%s'.  Possible fields found: %s.",
               name, strjoin (options(match), ", "));
      endif
    endfor

    ## At this point, odestruct has been initialized with default values,
    ## and if oldstruct was present it has overwritten fields in odestruct.

    if (nargin == 2 && isstruct (varargin{2}))
      newstruct = varargin{2};
      ode_struct_value_check (newstruct);

      newstruct_fldnames = (fieldnames (newstruct)).';

      ## Update the first struct with the values from the second one
      for fldname = newstruct_fldnames
        name = lower (fldname{1});

        exactmatch = true;
        match = find (strcmpi (name, options));
        if (isempty (match))
          match = find (strncmpi (name, options, length (name)));
          exactmatch = false;
        endif

        if (isempty (match))
          error ("odeset: invalid property '%s'", fldname{1});
        elseif (numel (match) == 1)
          if (! exactmatch)
            warning ("odeset:NoExactMatching",
                     "no exact match for '%s'.  Assuming '%s'.",
                     name, options{match});
          endif
          odestruct.(options{match}) = newstruct.(fldname{1});
        else
          error ("odeset: no exact match for '%s'.  Possible fields found: %s.",
                 name, strjoin (options(match), ", "));
        endif
      endfor

      ## Done copying newstruct to oldstruct
      return;
    endif

    ## Second argument is not a struct
    if (mod (nargin, 2) != 1)
      error ("odeset: FIELD/VALUE arguments must occur in pairs");
    endif
    if (! all (cellfun ("isclass", varargin(2:2:end), "char")))
      error ("odeset: All FIELD names must be strings");
    endif

    ## Write new field/value pairs into odestruct
    for i = 2:2:nargin
      name = lower (varargin{i});

      exactmatch = true;
      match = find (strcmpi (name, options));
      if (isempty (match))
        match = find (strncmpi (name, options, length (name)));
        exactmatch = false;
      endif

      if (isempty (match))
        error ("odeset: invalid property '%s'", varargin{i});
      elseif (numel (match) == 1)
        if (! exactmatch)
          warning ("odeset:NoExactMatching",
                   "no exact match for '%s'.  Assuming '%s'.",
                   name, options{match});
        endif
        odestruct.(options{match}) = varargin{i+1};
      else
        error ("odeset: no exact match for '%s'.  Possible fields found: %s.",
               name, strjoin (options(match), ", "));
      endif
    endfor

    ## Check if all changes have resulted in a valid OdePkg struct
    ode_struct_value_check (odestruct);

  else
    ## First input argument was not a struct, must be field/value pairs
    if (mod (nargin, 2) != 0)
      error ("odeset: FIELD/VALUE arguments must occur in pairs");
    elseif (! all (cellfun ("isclass", varargin(1:2:end), "char")))
      error ("odeset: All FIELD names must be strings");
    endif

    for i = 1:2:nargin
      name = lower (varargin{i});

      exactmatch = true;
      match = find (strcmpi (name, options));
      if (isempty (match))
        match = find (strncmpi (name, options, length (name)));
        exactmatch = false;
      endif

      if (isempty (match))
        error ("odeset: invalid property '%s'", varargin{i});
      elseif (numel (match) == 1)
        if (! exactmatch)
          warning ("odeset:NoExactMatching",
                   "no exact match for '%s'.  Assuming '%s'.",
                   name, options{match});
        endif
        odestruct.(options{match}) = varargin{i+1};
      else
        error ("odeset: no exact match for '%s'.  Possible fields found: %s.",
               name, strjoin (options(match), ", "));
      endif
    endfor

    ## Check if all changes have resulted in a valid OdePkg struct
    ode_struct_value_check (odestruct);

  endif

endfunction

## function useful to print all the possible options
function print_options ()
  
  disp ("List of all possible ODE solver options.");
  disp ("Default values are in square brackets.");
  disp ("");
  disp ("             AbsTol:  scalar or vector, >0, [1e-6]");
  disp ("          Algorithm:  string, {['gmres'], 'pcg', 'bicgstab'}");
  disp ("                BDF:  binary, {'on', ['off']}");
  disp ("             Choice:  switch, {[1], 2}");
  disp ("                Eta:  scalar, >=0, <1, [0.5]");
  disp ("             Events:  function_handle, []");
  disp ("           Explicit:  binary, {'yes', ['no']}");
  disp ("      InexactSolver:  string, {'inexact_newton', 'fsolve', []}");
  disp ("       InitialSlope:  vector, []");
  disp ("        InitialStep:  scalar, >0, []");
  disp ("           Jacobian:  matrix or function_handle, []");
  disp ("          JConstant:  binary, {'on', ['off']}");
  disp ("           JPattern:  sparse matrix, []");
  disp ("               Mass:  matrix or function_handle, []");
  disp ("       MassConstant:  binary, {'on', ['off']}");
  disp ("       MassSingular:  switch, {'yes', ['maybe'], 'no'}");
  disp ("MaxNewtonIterations:  scalar, integer, >0, [1e3]");
  disp ("           MaxOrder:  switch, {1, 2, 3, 4, [5]}");
  disp ("            MaxStep:  scalar, >0, []");
  disp ("   MStateDependence:  switch, {'none', ['weak'], 'strong'}");
  disp ("          MvPattern:  sparse matrix, []");
  disp ("          NewtonTol:  scalar or vector, >0, []");
  disp ("        NonNegative:  vector of integers, []");
  disp ("        NormControl:  binary, {'on', ['off']}");
  disp ("          OutputFcn:  function_handle, []");
  disp ("         OutputSave:  scalar, integer, >0, []");
  disp ("          OutputSel:  scalar or vector, []");
  disp ("   PolynomialDegree:  scalar, integer, >0, []");
  disp ("    QuadratureOrder:  scalar, integer, >0, []");
  disp ("             Refine:  scalar, integer, >0, []");
  disp ("             RelTol:  scalar, >0, [1e-3]");
  disp ("            Restart:  scalar, integer, >0, [20]");
  disp ("              Stats:  binary, {'on', ['off']}");
  disp ("     TimeStepNumber:  scalar, integer, >0, []");
  disp ("       TimeStepSize:  scalar, >0, []");
  disp ("        UseJacobian:  binary, {'yes', ['no']}");
  disp ("         Vectorized:  binary, {'on', ['off']}");

endfunction


%!demo
%! # A new OdePkg options structure with default values is created.
%!
%! odeoptA = odeset ();

%!demo
%! # A new OdePkg options structure with manually set options 
%! # for "AbsTol" and "RelTol" is created.
%!
%! odeoptB = odeset ("AbsTol", 1e-2, "RelTol", 1e-1);

%!demo
%! # A new OdePkg options structure is created from odeoptB with
%! # a modified value for option "NormControl".
%!
%! odeoptB = odeset ("AbsTol", 1e-2, "RelTol", 1e-1);
%! odeoptC = odeset (odeoptB, "NormControl", "on");

## All tests that are needed to check if a correct resp. valid option
## has been set are implemented in ode_struct_value_check.m.
%!test
%! odeoptA = odeset ();
%! assert (isstruct (odeoptA));
%! fields = fieldnames (odeoptA); 
%! assert (numel (fields), 37); 
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

