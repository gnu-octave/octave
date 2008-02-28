## Copyright (C) 2007 David Bateman
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

## Undocumented internal function

function [h, needusage] = __ezplot__ (pfunc, varargin)

  func = cstrcat ("ez", pfunc);
  if (strncmp (pfunc, "contour", 7))
    iscontour = true;
  else
    iscontour = false;
  endif
  if (strcmp (pfunc, "plot3"))
    isplot3 = true;
    ispolar = false;
    nargs = 1
  elseif (strcmp (pfunc, "polar"))
    isplot3 = false;
    ispolar = true;
    nargs = 1;
  else
    isplot3 = false;
    ispolar = false;
    nargs = 2;
  endif

  [ax, varargin, nargin] = __plt_get_axis_arg__ (func, varargin{:});

  needusage = false;
  if (nargin < 1)
    needusage = true;
    return;
  endif

  parametric = false;
  fun = varargin {1};
  if (ischar (fun))
    fun = vectorize (inline (fun));
    if (length (argnames (fun)) != nargs)
      error ("%s: excepting a function of %d arguments", func, nargs);
    endif
    fstr = formula (fun);
    if (isplot3)
      xarg = "x";
      yarg = "y";
    elseif (ispolar)
      xarg = "";
      yarg = "";
    else
      xarg = argnames(fun){1};
      yarg = argnames(fun){2};
    endif
  elseif (strcmp (typeinfo (fun), "inline function"))
    if (length (argnames (fun)) != nargs)
      error ("%s: excepting a function of %d arguments", func, nargs);
    endif
    fun = vectorize (fun);
    fstr = formula (fun);
    if (isplot3)
      xarg = "x";
      yarg = "y";
    elseif (ispolar)
      xarg = "";
      yarg = "";
    else
      xarg = argnames(fun)(1);
      yarg = argnames(fun)(2);
    endif
  elseif (isa (fun, "function_handle"))
    fstr = func2str (fun);
    args = regexp (substr (fstr, 3, findstr (fstr, ")")(1) - 3), 
		   '(\w[\w\d]*)', 'tokens');
    if (length (args) != nargs)
      error ("%s: excepting a function of %d arguments", func, nargs);
    endif
    fstr = substr (fstr, findstr (fstr, ")")(1) + 1);
    if (isplot3)
      xarg = "x";
      yarg = "y";
    elseif (ispolar)
      xarg = "";
      yarg = "";
    else
      xarg = args{1}{1};
      yarg = args{2}{1};
    endif
  else
    error ("%s: expecting string, inline function or function handle", func);
  endif

  if (nargin > 2)
    funx = fun;
    fstrx = fstr;
    funy = varargin {2};
    if (ischar (funy) && ! strcmp (funy, "circ") && ! strcmp (funy, "animate"))
      parametric = true;
      funy = vectorize (inline (funy));
      if (length (argnames (funy)) != nargs)
	error ("%s: excepting a function of %d arguments", func, nargs);
      endif
      fstry = formula (funy);
    elseif (strcmp (typeinfo (funy), "inline function"))
      parametric = true;
      if (length (argnames (funy)) != nargs)
	error ("%s: excepting a function of %d arguments", func, nargs);
      endif
      funy = vectorize (funy);
      fstry = formula (funy);
    elseif (isa (funy, "function_handle"))
      parametric = true;
      fstry = func2str (funy);
      args = regexp (substr (fstry, 3, findstr (fstry, ")")(1) - 3), 
		     '(\w[\w\d]*)', 'tokens');
      if (length (args) != nargs)
	error ("%s: excepting a function of %d arguments", func, nargs);
      endif
      fstry = substr (fstry, findstr (fstry, ")")(1) + 1);
    endif

    if (parametric)
      funz = varargin {3};
      if (ischar (funz) && ! strcmp (funz, "circ") && 
	  ! strcmp (funy, "animate"))
	funz = vectorize (inline (funz));
	if (length (argnames (funz)) != nargs)
	  error ("%s: excepting a function of %d arguments", func, nargs);
	endif
	fstrz = formula (funz);
      elseif (strcmp (typeinfo (funz), "inline function"))
	if (length (argnames (funz)) != nargs)
	  error ("%s: excepting a function of %d arguments", func, nargs);
	endif
	funz = vectorize (funz);
	fstrz = formula (funz);
      elseif (isa (funz, "function_handle"))
	fstrz = func2str (funz);
	args = regexp (substr (fstrz, 3, findstr (fstrz, ")")(1) - 3), 
		       '(\w[\w\d]*)', 'tokens');
	if (length (args) != nargs)
	  error ("%s: excepting a function of %d arguments", func, nargs);
	endif
	fstrz = substr (fstrz, findstr (fstrz, ")")(1) + 1);
      else
	error ("%s: parametric plots expect 3 functions", func);
      endif
    endif
  endif

  n = 60;
  domain = [];
  circ = false;
  animate = false;
  if (parametric)
    iarg = 4;
  else
    iarg = 2;
  endif
  while (iarg <= nargin)
    arg = varargin{iarg++};
    if (ischar (arg) && strcmp (arg, "circ"))
      circ = true;
    elseif (ischar (arg) && strcmp (arg, "animate"))
      animate = true;
    elseif (isscalar (arg))
      n = arg;
    elseif (numel (arg) == 2)
      domain = [arg(:).' arg(:).'];
    elseif (numel (arg) == 4)
      domain = arg(:).';
    else
      error ("%s: expecting scalar, 2 or 4 element vector", func);
    endif
  endwhile

  if (isempty (domain))
    if (isplot3 || ispolar)
      domain = [0, 2*pi, 0, 2*pi];
    else
      domain = [-2*pi, 2*pi, -2*pi, 2*pi];
    endif
  endif

  if (circ)
    if (iscontour || isplot3)
      needusage = true;
      return;
    endif
    if (parametric)
      error ("%s: can not have both circular domain and parametric function", 
	     func);
    endif
    cent = [domain(1) + domain(2), domain(3) + domain(4)] / 2;
    funx = @(r,t) r .* cos (t) + cent (1);
    funy = @(r,t) r .* sin (t) + cent (2);
    domain = [0, sqrt((domain(2) - cent(1))^2 + (domain(4) - cent(2))^2), ...
	      -pi, pi];
    funz = fun;
    parametric = true;
  endif

  if (animate)
    if (!isplot3)
      error ("%s: animated graphs only valid with plot3", func);
    endif
    error ("%s: animated graphs not implemented", func);
  endif

  if (isplot3 || ispolar)
    X = linspace (domain (1), domain (2), n);
  else
    x = linspace (domain (1), domain (2), n);
    y = linspace (domain (3), domain (4), n);
    [X, Y] = meshgrid (x, y);
  endif
  if (parametric)
    if (isplot3)
      Z = feval (funz, X);
      XX = feval (funx, X);
      YY = feval (funy, X);
      X = XX;
      Y = YY;
    else
      Z = feval (funz, X, Y);
      XX = feval (funx, X, Y);
      YY = feval (funy, X, Y);
      X = XX;
      Y = YY;

      ## Eliminate the singularities
      X = __eliminate_sing__ (X);
      Y = __eliminate_sing__ (Y);
      Z = __eliminate_sing__ (Z);
    endif

    fstrx = regexprep (regexprep (regexprep (fstrx,'\.\^\s*','^'), 
		      '\./', '/'), '[\.]*\*', '');
    fstry = regexprep (regexprep (regexprep (fstry,'\.\^\s*','^'), 
		      '\./', '/'), '[\.]*\*', '');
    fstrz = regexprep (regexprep (regexprep (fstrz,'\.\^\s*','^'), 
		      '\./', '/'), '[\.]*\*', '');
    fstr = cstrcat ("[",fstrx,",",fstry,",",fstrz,"]");
  else
    if (isplot3)
      needusage = true;
      return;
    endif

    if (ispolar)
      Z = feval (fun, X);
    else
      Z = feval (fun, X, Y);

      ## Eliminate the singularities
      Z = __eliminate_sing__ (Z);
    endif

    fstr = regexprep (regexprep (regexprep (fstr,'\.\^\s*','^'), '\./', '/'), 
		      '[\.]*\*', '');
  endif

  oldax = gca (); 
  unwind_protect
    axes (ax);
    if (iscontour)
      [clev, h] = feval (pfunc, X, Y, Z);
    elseif (ispolar)
      h = feval (pfunc, X, Z);
    else
      h = feval (pfunc, X, Y, Z);
    endif
    xlabel (xarg);
    ylabel (yarg);
    title (fstr);
  unwind_protect_cleanup
    axes (oldax);
  end_unwind_protect

endfunction

function x = __eliminate_sing__ (x)
  x (isinf (x)) = NaN;
  x (abs (del2 (x)) > 0.2 * (max(x(:)) - min(x(:)))) = NaN;
endfunction
