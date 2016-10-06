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
## @deftypefn  {} {} odeset ()
## @deftypefnx {} {@var{odestruct} =} odeset (@var{"field1"}, @var{value1}, @var{"field2"}, @var{value2}, @dots{})
## @deftypefnx {} {@var{odestruct} =} odeset (@var{oldstruct}, @var{"field1"}, @var{value1}, @var{"field2"}, @var{value2}, @dots{})
## @deftypefnx {} {@var{odestruct} =} odeset (@var{oldstruct}, @var{newstruct})
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

  persistent p;

  if (isempty (p))

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
    p.addParameter ("MassConstant", []);
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

    s1 = cellfun (@(x) ifelse (iscell(x), {x}, x),
                  struct2cell(odestruct),
                  'UniformOutput', false);

    s2 = cellfun (@(x) ifelse (iscell(x), {x}, x),
                  struct2cell(odestruct_extra),
                  'UniformOutput', false);
    
    C = [fieldnames(odestruct)       s1;
         fieldnames(odestruct_extra) s2];
    
    odestruct = struct (C'{:});
  endif
  
endfunction

## function to print all possible options
function print_options ()

  disp ("List of the most common ODE solver options.");
  disp ("Default values are in square brackets.");
  disp ("");
  disp ("             AbsTol:  scalar or vector, >0, [1e-6]");
  disp ("                BDF:  binary, {'on', ['off']}");
  disp ("             Events:  function_handle, []");
  disp ("       InitialSlope:  vector, []");
  disp ("        InitialStep:  scalar, >0, []");
  disp ("           Jacobian:  matrix or function_handle, []");
  disp ("          JConstant:  binary, {'on', ['off']}");
  disp ("           JPattern:  sparse matrix, []");
  disp ("               Mass:  matrix or function_handle, []");
  disp ("       MassConstant:  binary, {'on', ['off']}");
  disp ("       MassSingular:  switch, {'yes', ['maybe'], 'no'}");
  disp ("           MaxOrder:  switch, {1, 2, 3, 4, [5]}");
  disp ("            MaxStep:  scalar, >0, []");
  disp ("   MStateDependence:  switch, {'none', ['weak'], 'strong'}");
  disp ("          MvPattern:  sparse matrix, []");
  disp ("        NonNegative:  vector of integers, []");
  disp ("        NormControl:  binary, {'on', ['off']}");
  disp ("          OutputFcn:  function_handle, []");
  disp ("          OutputSel:  scalar or vector, []");
  disp ("             Refine:  scalar, integer, >0, []");
  disp ("             RelTol:  scalar, >0, [1e-3]");
  disp ("              Stats:  binary, {'on', ['off']}");
  disp ("         Vectorized:  binary, {'on', ['off']}");

endfunction


%!demo
%! # A new ODE options structure with default values is created.
%!
%! odeoptA = odeset ();

%!demo
%! # A new ODE options structure with manually set options
%! # for "AbsTol" and "RelTol" is created.
%!
%! odeoptB = odeset ("AbsTol", 1e-2, "RelTol", 1e-1);

%!demo
%! # A new ODE options structure is created from odeoptB with
%! # a modified value for option "NormControl".
%!
%! odeoptB = odeset ("AbsTol", 1e-2, "RelTol", 1e-1);
%! odeoptC = odeset (odeoptB, "NormControl", "on");

%!test
%! odeoptA = odeset ();
%! assert (isstruct (odeoptA));
%! assert (numfields (odeoptA), 23);
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
%! wstate = warning ("off", "Octave:invalid-input-arg");
%! unwind_protect
%!   odeopt = odeset ("NewtonTol", 3);
%!   assert (odeopt.NewtonTol, 3);
%! unwind_protect_cleanup
%!   warning (wstate);
%! end_unwind_protect

## Test input validation
%!error <argument 'OPT1' is not a valid parameter> odeset ("opt1")
%!error  odeset (1, 1)
%!error <argument 'OPT1' is not a valid parameter> odeset (odeset (), "opt1")
%!error  odeset (odeset (), 1, 1)

##FIXME: Add not exact match option 
## %!warning <no exact match for 'Rel'.  Assuming 'RelTol'> odeset ("Rel", 1);
## %!error <Possible fields found: InitialSlope, InitialStep> odeset ("Initial", 1)


