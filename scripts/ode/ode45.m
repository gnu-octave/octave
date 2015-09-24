## Copyright (C) 2006-2012, Thomas Treichl <treichl@users.sourceforge.net>
## Copyright (C) 2013, Roberto Porcu' <roberto.porcu@polimi.it>
## Copyright (C) 2014, Jacopo Corno <jacopo.corno@gmail.com>
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
## @deftypefn {Function File} {[@var{sol}] =} ode45 (@var{fun}, @var{slot}, @var{init}, [@var{opt}], [@var{par1}, @var{par2}, @dots{}])
## @deftypefnx {Function File} {[@var{t}, @var{y}, [@var{xe}, @var{ye}, @var{ie}]] =} ode45 (@var{fun}, @var{slot}, @var{init}, [@var{opt}], [@var{par1}, @var{par2}, @dots{}])
##
## This function file can be used to solve a set of non--stiff ordinary
## differential equations (non--stiff ODEs) with the well known explicit
## Dormand-Prince method of order 4.
##
## This function can be called with two output arguments: @var{t} and @var{y}.
## Variable @var{t} is a column vector and contains the time stamps, instead
## @var{y} is a matrix in which each column refers to a different unknown of
## the problem and the rows number is the same of @var{t} rows number so that each
## row of @var{y} contains the values of all unknowns at the time value contained
## in the corresponding row in @var{t}.
##
## The first input argument must be a function_handle or an inline function that
## defines the set of ODE: @code{y' = f(t,y)}. As described above, this function
## must take two input arguments, where the first is the time and the second
## the unknowns, and must have just one output argument.
##
## The second input argument must contain time informations. Usually it should
## be a vector with at least two elements which define the initial and the final
## time instants; if the elements are more than two, then the solution will be
## evaluated also at these intermediate time instants unless the integrate function
## called is the @command{integrate_n_steps}. If there is only one time value,
## then it will give an error unless the options structure has no empty fields
## named @var{"TimeStepNumber"} and @var{"TimeStepSize"}. If the option
## @var{"TimeStepSize"} is not empty, then the stepper called will be
## @command{integrate_const}, if also @var{"TimeStepNumber"} is not empty it will
## be called the integrate function @command{integrate_n_steps}, otherwise it will
## be called @command{integrate_adaptive}. For this last possibility the user can
## set the tolerance for the timestep computation by setting a value to the option
## @var{"Tau"}, that as default value has @math{1.e-6}.
##
## The third input argument must contain the initial value for the unknown.
## If this is a vector then the solution @var{y} will be a matrix in which each
## column is the solution for the corresponding initial value in @var{init}.
##
## The fourth input argument is not mandatory and it should contain a structure
## with valid ODE fields.
##
## For example, solve an anonymous implementation of the Van der Pol equation
## @example
## fvdp = @@(t,y) [y(2); (1 - y(1)^2) * y(2) - y(1)];
## [T,Y] = ode45 (fvdp, [0 20], [2 0]);
## @end example
## @end deftypefn
##

function [varargout] = ode45 (vfun, vslot, vinit, varargin)

  vorder = 5; % runge_kutta_45_dorpri uses local extrapolation
  vsolver = "ode45";

  if (nargin == 0) ## Check number and types of all input arguments
    help (vsolver);
    error ("OdePkg:InvalidArgument", ...
           "number of input arguments must be greater than zero");
  endif

  if (nargin < 3)
    print_usage;
  endif
  
  if (nargin >= 4)
    if (~isstruct (varargin{1}))
      ## varargin{1:len} are parameters for vfun
      vodeoptions = odeset;
      vodeoptions.vfunarguments = varargin;
    elseif (length (varargin) > 1)
      ## varargin{1} is an OdePkg options structure vopt
      vodeoptions = odepkg_structure_check (varargin{1}, "ode45");
      vodeoptions.vfunarguments = {varargin{2:length(varargin)}};
    else ## if (isstruct (varargin{1}))
      vodeoptions = odepkg_structure_check (varargin{1}, "ode45");
      vodeoptions.vfunarguments = {};
    endif
  else ## if (nargin == 3)
    vodeoptions = odeset; 
    vodeoptions.vfunarguments = {};
  endif

  if (~isvector (vslot) || ~isnumeric (vslot))
    error ("OdePkg:InvalidArgument", ...
           "second input argument must be a valid vector");
  endif

  if (length (vslot) < 2 && ...
     (isempty (vodeoptions.TimeStepSize) ...
      || isempty (vodeoptions.TimeStepNumber)))
    error ("OdePkg:InvalidArgument", ...
           "second input argument must be a valid vector");
  elseif (vslot(2) == vslot(1))
    error ("OdePkg:InvalidArgument", ...
           "second input argument must be a valid vector");
  else
    vodeoptions.vdirection = sign (vslot(2) - vslot(1));
  endif
  vslot = vslot(:);

  if (~isvector (vinit) || ~isnumeric (vinit))
    error ("OdePkg:InvalidArgument", ...
           "third input argument must be a valid numerical value");
  endif
  vinit = vinit(:);

  if ~(isa (vfun, "function_handle") || isa (vfun, "inline"))
    error ("OdePkg:InvalidArgument", ...
           "first input argument must be a valid function handle");
  endif

  ## Start preprocessing, have a look which options are set in
  ## vodeoptions, check if an invalid or unused option is set
  if (isempty (vodeoptions.TimeStepNumber) ...
      && isempty (vodeoptions.TimeStepSize))
    integrate_func = "adaptive";
    vodeoptions.vstepsizefixed = false;
  elseif (~isempty (vodeoptions.TimeStepNumber) ...
          && ~isempty (vodeoptions.TimeStepSize))
    integrate_func = "n_steps";
    vodeoptions.vstepsizefixed = true;
    if (sign (vodeoptions.TimeStepSize) != vodeoptions.vdirection)
      warning ("OdePkg:InvalidArgument", ...
               "option ''TimeStepSize'' has a wrong sign", ...
               "it will be corrected automatically");
      vodeoptions.TimeStepSize = (-1)*vodeoptions.TimeStepSize;
    endif
  elseif (isempty (vodeoptions.TimeStepNumber) && ~isempty (vodeoptions.TimeStepSize))
    integrate_func = "const";
    vodeoptions.vstepsizefixed = true;
    if (sign (vodeoptions.TimeStepSize) != vodeoptions.vdirection)
      warning ("OdePkg:InvalidArgument", ...
               "option ''TimeStepSize'' has a wrong sign", ...
               "it will be corrected automatically");
      vodeoptions.TimeStepSize = (-1)*vodeoptions.TimeStepSize;
    endif
  else
    warning ("OdePkg:InvalidArgument", ...
             "assuming an adaptive integrate function");
    integrate_func = "adaptive";
  endif

  ## Get the default options that can be set with "odeset" temporarily
  vodetemp = odeset;

  ## Implementation of the option RelTol has been finished. This option
  ## can be set by the user to another value than default value.
  if (isempty (vodeoptions.RelTol) && ~vodeoptions.vstepsizefixed)
    vodeoptions.RelTol = 1e-3;
    warning ("OdePkg:InvalidArgument", ...
             "Option ''RelTol'' not set, new value %f is used", ...
             vodeoptions.RelTol);
  elseif (~isempty (vodeoptions.RelTol) && vodeoptions.vstepsizefixed)
    warning ("OdePkg:InvalidArgument", ...
             "Option ''RelTol'' will be ignored if fixed time stamps are given");
  endif

  ## Implementation of the option AbsTol has been finished. This option
  ## can be set by the user to another value than default value.
  if (isempty (vodeoptions.AbsTol) && ~vodeoptions.vstepsizefixed)
    vodeoptions.AbsTol = 1e-6;
    warning ("OdePkg:InvalidArgument", ...
             "Option ''AbsTol'' not set, new value %f is used", ...
             vodeoptions.AbsTol);
  elseif (~isempty (vodeoptions.AbsTol) && vodeoptions.vstepsizefixed)
    warning ("OdePkg:InvalidArgument", ...
             "Option ''AbsTol'' will be ignored if fixed time stamps are given");
  else
    vodeoptions.AbsTol = vodeoptions.AbsTol(:); ## Create column vector
  endif

  ## Implementation of the option NormControl has been finished. This
  ## option can be set by the user to another value than default value.
  if (strcmp (vodeoptions.NormControl, "on"))
    vodeoptions.vnormcontrol = true;
  else 
    vodeoptions.vnormcontrol = false; 
  endif

  ## Implementation of the option NonNegative has been finished. This
  ## option can be set by the user to another value than default value.
  if (~isempty (vodeoptions.NonNegative))
    if (isempty (vodeoptions.Mass))
      vodeoptions.vhavenonnegative = true;
    else
      vodeoptions.vhavenonnegative = false;
      warning ("OdePkg:InvalidArgument", ...
               "Option 'NonNegative' will be ignored if mass matrix is set");
    endif
  else 
    vodeoptions.vhavenonnegative = false;
  endif

  ## Implementation of the option OutputFcn has been finished. This
  ## option can be set by the user to another value than default value.
  if (isempty (vodeoptions.OutputFcn) && nargout == 0)
    vodeoptions.OutputFcn = @odeplot;
    vodeoptions.vhaveoutputfunction = true;
  elseif (isempty (vodeoptions.OutputFcn))
    vodeoptions.vhaveoutputfunction = false;
  else 
    vodeoptions.vhaveoutputfunction = true;
  endif

  ## Implementation of the option OutputSel has been finished. This
  ## option can be set by the user to another value than default value.
  if (~isempty (vodeoptions.OutputSel))
    vodeoptions.vhaveoutputselection = true;
  else 
    vodeoptions.vhaveoutputselection = false; 
  endif

  ## Implementation of the option OutputSave has been finished. This
  ## option can be set by the user to another value than default value.
  if (isempty (vodeoptions.OutputSave))
    vodeoptions.OutputSave = 1;
  endif

  ## Implementation of the option Refine has been finished. This option
  ## can be set by the user to another value than default value.
  if (vodeoptions.Refine > 0)
    vodeoptions.vhaverefine = true;
  else 
    vodeoptions.vhaverefine = false;
  endif

  ## Implementation of the option Stats has been finished. This option
  ## can be set by the user to another value than default value.

  ## Implementation of the option InitialStep has been finished. This
  ## option can be set by the user to another value than default value.
  if (isempty (vodeoptions.InitialStep) && strcmp (integrate_func, "adaptive"))
    vodeoptions.InitialStep = vodeoptions.vdirection* ...
      starting_stepsize (vorder, vfun, vslot(1), vinit, vodeoptions.AbsTol, ...
                         vodeoptions.RelTol, vodeoptions.vnormcontrol);
    warning ("OdePkg:InvalidArgument", ...
             "option ''InitialStep'' not set, estimated value %f is used", ...
             vodeoptions.InitialStep);
  elseif(isempty (vodeoptions.InitialStep))
    vodeoptions.InitialStep = odeget (vodeoptions, "TimeStepSize");
  endif

  ## Implementation of the option MaxStep has been finished. This option
  ## can be set by the user to another value than default value.
  if (isempty (vodeoptions.MaxStep) && ~vodeoptions.vstepsizefixed)
    vodeoptions.MaxStep = abs (vslot(end) - vslot(1)) / 10;
    warning ("OdePkg:InvalidArgument", ...
             "Option ''MaxStep'' not set, new value %f is used", ...
             vodeoptions.MaxStep);
  endif

  ## Implementation of the option Events has been finished. This option
  ## can be set by the user to another value than default value.
  if (~isempty (vodeoptions.Events))
    vodeoptions.vhaveeventfunction = true;
  else 
    vodeoptions.vhaveeventfunction = false;
  endif

  ## The options 'Jacobian', 'JPattern' and 'Vectorized' will be ignored
  ## by this solver because this solver uses an explicit Runge-Kutta
  ## method and therefore no Jacobian calculation is necessary
  if (~isequal (vodeoptions.Jacobian, vodetemp.Jacobian))
    warning ("OdePkg:InvalidArgument", ...
             "option ''Jacobian'' will be ignored by this solver");
  endif

  if (~isequal (vodeoptions.JPattern, vodetemp.JPattern))
    warning ("OdePkg:InvalidArgument", ...
             "option ''JPattern'' will be ignored by this solver");
  endif

  if (~isequal (vodeoptions.Vectorized, vodetemp.Vectorized))
    warning ("OdePkg:InvalidArgument", ...
             "option ''Vectorized'' will be ignored by this solver");
  endif

  if (~isequal (vodeoptions.NewtonTol, vodetemp.NewtonTol))
    warning ("OdePkg:InvalidArgument", ...
             "option ''NewtonTol'' will be ignored by this solver");
  endif

  if (~isequal (vodeoptions.MaxNewtonIterations,vodetemp.MaxNewtonIterations))
    warning ("OdePkg:InvalidArgument", ...
             "option ''MaxNewtonIterations'' will be ignored by this solver");
  endif

  ## Implementation of the option Mass has been finished. This option
  ## can be set by the user to another value than default value.
  if (~isempty (vodeoptions.Mass) && isnumeric (vodeoptions.Mass))
    vhavemasshandle = false;
    vmass = vodeoptions.Mass; ## constant mass
  elseif (isa (vodeoptions.Mass, "function_handle"))
    vhavemasshandle = true; ## mass defined by a function handle
  else ## no mass matrix - creating a diag-matrix of ones for mass
    vhavemasshandle = false; ## vmass = diag (ones (length (vinit), 1), 0);
  endif

  ## Implementation of the option MStateDependence has been finished.
  ## This option can be set by the user to another value than default
  ## value.
  if (strcmp (vodeoptions.MStateDependence, "none"))
    vmassdependence = false;
  else
    vmassdependence = true;
  endif

  ## Other options that are not used by this solver. Print a warning
  ## message to tell the user that the option(s) is/are ignored.
  if (~isequal (vodeoptions.MvPattern, vodetemp.MvPattern))
    warning ("OdePkg:InvalidArgument", ...
             "option ''MvPattern'' will be ignored by this solver");
  endif

  if (~isequal (vodeoptions.MassSingular, vodetemp.MassSingular))
    warning ("OdePkg:InvalidArgument", ...
             "option ''MassSingular'' will be ignored by this solver");
  endif

  if (~isequal (vodeoptions.InitialSlope, vodetemp.InitialSlope))
    warning ("OdePkg:InvalidArgument", ...
             "option ''InitialSlope'' will be ignored by this solver");
  endif

  if (~isequal (vodeoptions.MaxOrder, vodetemp.MaxOrder))
    warning ("OdePkg:InvalidArgument", ...
             "option ''MaxOrder'' will be ignored by this solver");
  endif

  if (~isequal (vodeoptions.BDF, vodetemp.BDF))
    warning ("OdePkg:InvalidArgument", ...
             "option ''BDF'' will be ignored by this solver");
  endif

  ## Starting the initialisation of the core solver ode45
  SubOpts = vodeoptions;
  
  if (vhavemasshandle)   ## Handle only the dynamic mass matrix,
    if (vmassdependence) ## constant mass matrices have already
      vmass = @(t,x) vodeoptions.Mass (t, x, vodeoptions.vfunarguments{:});
      vfun = @(t,x) vmass (t, x, vodeoptions.vfunarguments{:}) ...
        \ vfun (t, x, vodeoptions.vfunarguments{:});
    else                 ## if (vmassdependence == false)
      vmass = @(t) vodeoptions.Mass (t, vodeoptions.vfunarguments{:});
      vfun = @(t,x) vmass (t, vodeoptions.vfunarguments{:}) ...
        \ vfun (t, x, vodeoptions.vfunarguments{:});
    endif
  endif

  switch integrate_func
    case "adaptive"
      solution = integrate_adaptive (@runge_kutta_45_dorpri, ...
                                     vorder, vfun, vslot, vinit, SubOpts);
    case "n_steps"
      solution = integrate_n_steps (@runge_kutta_45_dorpri, ...
                                    vfun, vslot(1), vinit, ...
                                    vodeoptions.TimeStepSize, ...
                                    vodeoptions.TimeStepNumber, SubOpts);
    case "const"
      solution = integrate_const (@runge_kutta_45_dorpri, ...
                                  vfun, vslot, vinit, ...
                                  vodeoptions.TimeStepSize, SubOpts);
  endswitch

  ## Postprocessing, do whatever when terminating integration algorithm
  if (vodeoptions.vhaveoutputfunction) ## Cleanup plotter
    feval (vodeoptions.OutputFcn, solution.t(end), ...
      solution.x(end,:)', "done", vodeoptions.vfunarguments{:});
  endif
  if (vodeoptions.vhaveeventfunction)  ## Cleanup event function handling
    odepkg_event_handle (vodeoptions.Events, solution.t(end), ...
      solution.x(end,:)', "done", vodeoptions.vfunarguments{:});
  endif

  ## Print additional information if option Stats is set
  if (strcmp (vodeoptions.Stats, "on"))
    vhavestats = true;
    vnsteps    = solution.vcntloop-2;                    ## vcntloop from 2..end
    vnfailed   = (solution.vcntcycles-1)-(solution.vcntloop-2)+1; ## vcntcycl from 1..end
    vnfevals   = 7*(solution.vcntcycles-1);              ## number of ode evaluations
    vndecomps  = 0;                             ## number of LU decompositions
    vnpds      = 0;                             ## number of partial derivatives
    vnlinsols  = 0;                             ## no. of solutions of linear systems
    ## Print cost statistics if no output argument is given
    if (nargout == 0)
      vmsg = fprintf (1, "Number of successful steps: %d\n", vnsteps);
      vmsg = fprintf (1, "Number of failed attempts:  %d\n", vnfailed);
      vmsg = fprintf (1, "Number of function calls:   %d\n", vnfevals);
    endif
  else
    vhavestats = false;
  endif

  if (nargout == 1)                 ## Sort output variables, depends on nargout
    varargout{1}.x = solution.t;   ## Time stamps are saved in field x
    varargout{1}.y = solution.x; ## Results are saved in field y
    varargout{1}.solver = vsolver;  ## Solver name is saved in field solver
    if (vodeoptions.vhaveeventfunction) 
      varargout{1}.ie = solution.vevent{2};  ## Index info which event occured
      varargout{1}.xe = solution.vevent{3};  ## Time info when an event occured
      varargout{1}.ye = solution.vevent{4};  ## Results when an event occured
    endif
    if (vhavestats)
      varargout{1}.stats = struct;
      varargout{1}.stats.nsteps   = vnsteps;
      varargout{1}.stats.nfailed  = vnfailed;
      varargout{1}.stats.nfevals  = vnfevals;
      varargout{1}.stats.npds     = vnpds;
      varargout{1}.stats.ndecomps = vndecomps;
      varargout{1}.stats.nlinsols = vnlinsols;
    endif
  elseif (nargout == 2)
    varargout{1} = solution.t;     ## Time stamps are first output argument
    varargout{2} = solution.x;   ## Results are second output argument
  elseif (nargout == 5)
    varargout{1} = solution.t;     ## Same as (nargout == 2)
    varargout{2} = solution.x;   ## Same as (nargout == 2)
    varargout{3} = [];              ## LabMat doesn't accept lines like
    varargout{4} = [];              ## varargout{3} = varargout{4} = [];
    varargout{5} = [];
    if (vodeoptions.vhaveeventfunction) 
      varargout{3} = solution.vevent{3};     ## Time info when an event occured
      varargout{4} = solution.vevent{4};     ## Results when an event occured
      varargout{5} = solution.vevent{2};     ## Index info which event occured
    endif
  endif

endfunction

%! # We are using the "Van der Pol" implementation for all tests that
%! # are done for this function.
%! # For further tests we also define a reference solution (computed at high accuracy)
%!function [ydot] = fpol (vt, vy) ## The Van der Pol
%!  ydot = [vy(2); (1 - vy(1)^2) * vy(2) - vy(1)];
%!endfunction
%!function [vref] = fref ()         ## The computed reference sol
%!  vref = [0.32331666704577, -1.83297456798624];
%!endfunction
%!function [vjac] = fjac (vt, vy, varargin) ## its Jacobian
%!  vjac = [0, 1; -1 - 2 * vy(1) * vy(2), 1 - vy(1)^2];
%!function [vjac] = fjcc (vt, vy, varargin) ## sparse type
%!  vjac = sparse ([0, 1; -1 - 2 * vy(1) * vy(2), 1 - vy(1)^2])
%!function [vval, vtrm, vdir] = feve (vt, vy, varargin)
%!  vval = fpol (vt, vy, varargin); ## We use the derivatives
%!  vtrm = zeros (2,1);             ## that's why component 2
%!  vdir = ones (2,1);              ## seems to not be exact
%!function [vval, vtrm, vdir] = fevn (vt, vy, varargin)
%!  vval = fpol (vt, vy, varargin); ## We use the derivatives
%!  vtrm = ones (2,1);              ## that's why component 2
%!  vdir = ones (2,1);              ## seems to not be exact
%!function [vmas] = fmas (vt, vy, varargin)
%!  vmas = [1, 0; 0, 1];            ## Dummy mass matrix for tests
%!function [vmas] = fmsa (vt, vy, varargin)
%!  vmas = sparse ([1, 0; 0, 1]);   ## A sparse dummy matrix
%!function [vout] = fout (vt, vy, vflag, varargin)
%!  if (regexp (char (vflag), 'init') == 1)
%!    if (any (size (vt) ~= [2, 1])) error ('"fout" step "init"'); end
%!  elseif (isempty (vflag))
%!    if (any (size (vt) ~= [1, 1])) error ('"fout" step "calc"'); end
%!    vout = false;
%!  elseif (regexp (char (vflag), 'done') == 1)
%!    if (any (size (vt) ~= [1, 1])) error ('"fout" step "done"'); end
%!  else error ('"fout" invalid vflag');
%!  end
%!
%! ## Turn off output of warning messages for all tests, turn them on
%! ## again if the last test is called
%!error ## ouput argument
%!  warning ('off', 'OdePkg:InvalidArgument');
%!  B = ode45 (1, [0 25], [3 15 1]);
%!error ## input argument number one
%!  [vt, vy] = ode45 (1, [0 25], [3 15 1]);
%!error ## input argument number two
%!  [vt, vy] = ode45 (@fpol, 1, [3 15 1]);
%!test ## two output arguments
%!  [vt, vy] = ode45 (@fpol, [0 2], [2 0]);
%!  assert ([vt(end), vy(end,:)], [2, fref], 1e-2);
%!test ## not too many steps
%!  [vt, vy] = ode45 (@fpol, [0 2], [2 0]);
%!  assert (size (vt) < 20);
%!test ## anonymous function instead of real function
%!  fvdb = @(vt,vy) [vy(2); (1 - vy(1)^2) * vy(2) - vy(1)];
%!  [vt, vy] = ode45 (fvdb, [0 2], [2 0]);
%!  assert ([vt(end), vy(end,:)], [2, fref], 1e-2);
%!test ## extra input arguments passed through
%!  [vt, vy] = ode45 (@fpol, [0 2], [2 0], 12, 13, 'KL');
%!  assert ([vt(end), vy(end,:)], [2, fref], 1e-2);
%!test ## empty OdePkg structure *but* extra input arguments
%!  vopt = odeset;
%!  [vt, vy] = ode45 (@fpol, [0 2], [2 0], vopt, 12, 13, 'KL');
%!  assert ([vt(end), vy(end,:)], [2, fref], 1e-2);
%!error ## strange OdePkg structure
%!  vopt = struct ('foo', 1);
%!  [vt, vy] = ode45 (@fpol, [0 2], [2 0], vopt);
%!test ## Solve vdp in fixed step sizes
%!  vopt = odeset('TimeStepSize', 0.1);
%!  [vt, vy] = ode45 (@fpol, [0,2], [2 0], vopt);
%!  assert (vt(:), [0:0.1:2]', 1e-2);
%!test ## Solve another anonymous function below zero
%!  vref = [0, 14.77810590694212];
%!  [vt, vy] = ode45 (@(t,y) y, [-2 0], 2);
%!  assert ([vt(end), vy(end,:)], vref, 1e-1);
%!test ## InitialStep option
%!  vopt = odeset ('InitialStep', 1e-8);
%!  [vt, vy] = ode45 (@fpol, [0 0.2], [2 0], vopt);
%!  assert ([vt(2)-vt(1)], [1e-8], 1e-9);
%!test ## MaxStep option
%!  vopt = odeset ('MaxStep', 1e-3);
%!  vsol = ode45 (@fpol, [0 0.2], [2 0], vopt);
%!  assert ([vsol.x(5)-vsol.x(4)], [1e-3], 1e-3);
%!test ## Solve with intermidiate step
%!  vsol = ode45 (@fpol, [0 1 2], [2 0]);
%!  assert (any((vsol.x-1) == 0));
%!  assert ([vsol.x(end), vsol.y(end,:)], [2, fref], 1e-3);
%!test ## Solve in backward direction starting at t=0
%!  vref = [-1.205364552835178, 0.951542399860817];
%!  vsol = ode45 (@fpol, [0 -2], [2 0]);
%!  assert ([vsol.x(end), vsol.y(end,:)], [-2, vref], 1e-2);
%!test ## Solve in backward direction starting at t=2
%!  vref = [-1.205364552835178, 0.951542399860817];
%!  vsol = ode45 (@fpol, [2 -2], fref);
%!  assert ([vsol.x(end), vsol.y(end,:)], [-2, vref], 1e-2);
%!test ## Solve in backward direction starting at t=2, with intermidiate step
%!  vref = [-1.205364552835178, 0.951542399860817];
%!  vsol = ode45 (@fpol, [2 0 -2], fref);
%!  idx = find(vsol.x < 0, 1, 'first') - 1;
%!  assert ([vsol.x(idx), vsol.y(idx,:)], [0 2 0], 1e-2);
%!  assert ([vsol.x(end), vsol.y(end,:)], [-2, vref], 1e-2);
%!test ## Solve another anonymous function in backward direction
%!  vref = [-1, 0.367879437558975];
%!  vsol = ode45 (@(t,y) y, [0 -1], 1);
%!  assert ([vsol.x(end), vsol.y(end,:)], vref, 1e-3);
%!test ## Solve another anonymous function below zero
%!  vref = [0, 14.77810590694212];
%!  vsol = ode45 (@(t,y) y, [-2 0], 2);
%!  assert ([vsol.x(end), vsol.y(end,:)], vref, 1e-3);
%!test ## Solve in backward direction starting at t=0 with MaxStep option
%!  vref = [-1.205364552835178, 0.951542399860817];
%!  vopt = odeset ('MaxStep', 1e-3);
%!  vsol = ode45 (@fpol, [0 -2], [2 0], vopt);
%!  assert ([abs(vsol.x(8)-vsol.x(7))], [1e-3], 1e-3);
%!  assert ([vsol.x(end), vsol.y(end,:)], [-2, vref], 1e-3);
%!test ## AbsTol option
%!  vopt = odeset ('AbsTol', 1e-5);
%!  vsol = ode45 (@fpol, [0 2], [2 0], vopt);
%!  assert ([vsol.x(end), vsol.y(end,:)], [2, fref], 1e-3);
%!test ## AbsTol and RelTol option
%!  vopt = odeset ('AbsTol', 1e-8, 'RelTol', 1e-8);
%!  vsol = ode45 (@fpol, [0 2], [2 0], vopt);
%!  assert ([vsol.x(end), vsol.y(end,:)], [2, fref], 1e-3);
%!test ## RelTol and NormControl option -- higher accuracy
%!  vopt = odeset ('RelTol', 1e-8, 'NormControl', 'on');
%!  vsol = ode45 (@fpol, [0 2], [2 0], vopt);
%!  assert ([vsol.x(end), vsol.y(end,:)], [2, fref], 1e-5);
%!test ## Keeps initial values while integrating
%!  vopt = odeset ('NonNegative', 2);
%!  vsol = ode45 (@fpol, [0 2], [2 0], vopt);
%!  assert ([vsol.x(end), vsol.y(end,:)], [2, 2, 0], 0.5);
%!test ## Details of OutputSel and Refine can't be tested
%!  vopt = odeset ('OutputFcn', @fout, 'OutputSel', 1, 'Refine', 5);
%!  vsol = ode45 (@fpol, [0 2], [2 0], vopt);
%!test ## Details of OutputSave can't be tested
%!  vopt = odeset ('OutputSave', 1, 'OutputSel', 1);
%!  vsla = ode45 (@fpol, [0 2], [2 0], vopt);
%!  vopt = odeset ('OutputSave', 2);
%!  vslb = ode45 (@fpol, [0 2], [2 0], vopt);
%!  assert (length (vsla.x) + 1 >= 2 * length (vslb.x))
%!test ## Stats must add further elements in vsol
%!  vopt = odeset ('Stats', 'on');
%!  vsol = ode45 (@fpol, [0 2], [2 0], vopt);
%!  assert (isfield (vsol, 'stats'));
%!  assert (isfield (vsol.stats, 'nsteps'));
%!test ## Events option add further elements in vsol
%!  vopt = odeset ('Events', @feve);
%!  vsol = ode45 (@fpol, [0 10], [2 0], vopt);
%!  assert (isfield (vsol, 'ie'));
%!  assert (vsol.ie(1), 2);
%!  assert (isfield (vsol, 'xe'));
%!  assert (isfield (vsol, 'ye'));
%!test ## Events option, now stop integration
%!  vopt = odeset ('Events', @fevn, 'NormControl', 'on');
%!  vsol = ode45 (@fpol, [0 10], [2 0], vopt);
%!  assert ([vsol.ie, vsol.xe, vsol.ye], ...
%!    [2.0, 2.496110, -0.830550, -2.677589], 6e-1);
%!test ## Events option, five output arguments
%!  vopt = odeset ('Events', @fevn, 'NormControl', 'on');
%!  [vt, vy, vxe, vye, vie] = ode45 (@fpol, [0 10], [2 0], vopt);
%!  assert ([vie, vxe, vye], ...
%!    [2.0, 2.496110, -0.830550, -2.677589], 6e-1);
%!test ## Jacobian option
%!  vopt = odeset ('Jacobian', @fjac);
%!  vsol = ode45 (@fpol, [0 2], [2 0], vopt);
%!  assert ([vsol.x(end), vsol.y(end,:)], [2, fref], 1e-3);
%!test ## Jacobian option and sparse return value
%!  vopt = odeset ('Jacobian', @fjcc);
%!  vsol = ode45 (@fpol, [0 2], [2 0], vopt);
%!  assert ([vsol.x(end), vsol.y(end,:)], [2, fref], 1e-3);
%!
%! ## test for JPattern option is missing
%! ## test for Vectorized option is missing
%! ## test for NewtonTol option is missing
%! ## test for MaxNewtonIterations option is missing
%!
%!test ## Mass option as function
%!  vopt = odeset ('Mass', @fmas);
%!  vsol = ode45 (@fpol, [0 2], [2 0], vopt);
%!  assert ([vsol.x(end), vsol.y(end,:)], [2, fref], 1e-3);
%!test ## Mass option as matrix
%!  vopt = odeset ('Mass', eye (2,2));
%!  vsol = ode45 (@fpol, [0 2], [2 0], vopt);
%!  assert ([vsol.x(end), vsol.y(end,:)], [2, fref], 1e-3);
%!test ## Mass option as sparse matrix
%!  vopt = odeset ('Mass', sparse (eye (2,2)));
%!  vsol = ode45 (@fpol, [0 2], [2 0], vopt);
%!  assert ([vsol.x(end), vsol.y(end,:)], [2, fref], 1e-3);
%!test ## Mass option as function and sparse matrix
%!  vopt = odeset ('Mass', @fmsa);
%!  vsol = ode45 (@fpol, [0 2], [2 0], vopt);
%!  assert ([vsol.x(end), vsol.y(end,:)], [2, fref], 1e-3);
%!test ## Mass option as function and MStateDependence
%!  vopt = odeset ('Mass', @fmas, 'MStateDependence', 'strong');
%!  vsol = ode45 (@fpol, [0 2], [2 0], vopt);
%!  assert ([vsol.x(end), vsol.y(end,:)], [2, fref], 1e-3);
%!test ## Set BDF option to something else than default
%!  vopt = odeset ('BDF', 'on');
%!  [vt, vy] = ode45 (@fpol, [0 2], [2 0], vopt);
%!  assert ([vt(end), vy(end,:)], [2, fref], 1e-3);
%!
%! ## test for MvPattern option is missing
%! ## test for InitialSlope option is missing
%! ## test for MaxOrder option is missing
%!
%!  warning ('on', 'OdePkg:InvalidArgument');

## Local Variables: ***
## mode: octave ***
## End: ***
