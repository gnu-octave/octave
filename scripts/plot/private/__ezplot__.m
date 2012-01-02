## Copyright (C) 2007-2012 David Bateman
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
## @deftypefn {Function File} {[@var{h}, @var{needusage}] =} __ezplot__ (@var{pfunc}, @var{varargin})
## Undocumented internal function.
## @end deftypefn

function [h, needusage] = __ezplot__ (pfunc, varargin)

  func = cstrcat ("ez", pfunc);
  if (strncmp (pfunc, "contour", 7))
    iscontour = true;
  else
    iscontour = false;
  endif
  if (strcmp (pfunc, "plot"))
    isplot = true;
    isplot3 = false;
    ispolar = false;
    nargs = 1;
  elseif (strcmp (pfunc, "plot3"))
    isplot = false;
    isplot3 = true;
    ispolar = false;
    nargs = 1;
  elseif (strcmp (pfunc, "polar"))
    isplot = false;
    isplot3 = false;
    ispolar = true;
    nargs = 1;
  else
    isplot = false;
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
    if (exist (fun, "file") || exist (fun, "builtin"))
      fun = vectorize (inline (cstrcat (fun, "(t)")));
    else
      fun = vectorize (inline (fun));
    endif
    if (isplot && length (argnames (fun)) == 2)
      nargs = 2;
    elseif (length (argnames (fun)) != nargs)
      error ("%s: excepting a function of %d arguments", func, nargs);
    endif
    fstr = formula (fun);
    if (isplot)
      xarg = argnames(fun){1};
      if (nargs == 2)
        yarg = argnames(fun){2};
      else
        yarg = "";
      endif
    elseif (isplot3)
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
    if (isplot && length (argnames (fun)) == 2)
      nargs = 2;
    elseif (length (argnames (fun)) != nargs)
      error ("%s: excepting a function of %d arguments", func, nargs);
    endif
    fun = vectorize (fun);
    fstr = formula (fun);
    if (isplot)
      xarg = argnames(fun){1};
      if (nargs == 2)
        yarg = argnames(fun){2};
      else
        yarg = "";
      endif
    elseif (isplot3)
      xarg = "x";
      yarg = "y";
    elseif (isplot || ispolar)
      xarg = "";
      yarg = "";
    else
      xarg = argnames(fun)(1);
      yarg = argnames(fun)(2);
    endif
  elseif (isa (fun, "function_handle"))
    fstr = func2str (fun);
    if (length (findstr (fstr, ")")) != 0)
      args = regexp (substr (fstr, 3, findstr (fstr, ")")(1) - 3),
                     '(\w+)', 'tokens');
    fstr = substr (fstr, findstr (fstr, ")")(1) + 1);
    else
      args = {{"x"}};
    endif
    if (isplot && length (args) == 2)
      nargs = 2;
    elseif (length (args) != nargs)
      error ("%s: excepting a function of %d arguments", func, nargs);
    endif
    if (isplot)
      xarg = args{1}{1};
      if (nargs == 2)
        yarg = args{2}{1};
      else
        yarg = "";
      endif
    elseif (isplot3)
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

  if (nargin > 2 || (nargin == 2 && isplot))
    funx = fun;
    fstrx = fstr;
    funy = varargin {2};
    if (ischar (funy) && ! strcmp (funy, "circ") && ! strcmp (funy, "animate"))
      parametric = true;
      if (exist (funy, "file") || exist (funy, "builtin"))
        funy = vectorize (inline (cstrcat (funy, "(t)")));
      else
        funy = vectorize (inline (funy));
      endif
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
      if (length (findstr (fstry, ")")) != 0)
        args = regexp (substr (fstry, 3, findstr (fstry, ")")(1) - 3),
                       '(\w+)', 'tokens');
        fstry = substr (fstry, findstr (fstry, ")")(1) + 1);
      else
        args = {{"y"}};
      endif
      if (length (args) != nargs)
        error ("%s: excepting a function of %d arguments", func, nargs);
      endif
    endif

    if (parametric && isplot)
      xarg = "x";
      yarg = "y";
      if (nargs == 2)
        error ("%s: can not define a parametric function in this manner");
      endif
    endif

    if (!isplot && parametric)
      funz = varargin {3};
      if (ischar (funz) && ! strcmp (funz, "circ")
          && ! strcmp (funz, "animate"))
        if (exist (funz, "file") || exist (funz, "builtin"))
          funz = vectorize (inline (cstrcat (funz, "(t)")));
        else
          funz = vectorize (inline (funz));
        endif
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
                       '(\w+)', 'tokens');
        if (length (args) != nargs)
          error ("%s: excepting a function of %d arguments", func, nargs);
        endif
        fstrz = substr (fstrz, findstr (fstrz, ")")(1) + 1);
      else
        error ("%s: parametric plots expect 3 functions", func);
      endif
    endif
  endif

  if (isplot && nargs != 2)
    n = 500;
  else
    n = 60;
  endif
  domain = [];
  circ = false;
  animate = false;
  if (parametric)
    if (isplot)
      iarg = 3;
    else
      iarg = 4;
    endif
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
    if (iscontour || isplot3 || isplot)
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

  if (isplot3 || ispolar || (isplot && nargs == 1))
    X = linspace (domain (1), domain (2), n);
  elseif (isplot && numel (domain) == 2)
    x = linspace (domain (1), domain (2), n);
    [X, Y] = meshgrid (x, x);
  else
    x = linspace (domain (1), domain (2), n);
    y = linspace (domain (3), domain (4), n);
    [X, Y] = meshgrid (x, y);
  endif

  if (parametric)
    if (isplot)
      XX = feval (funx, X);
      Z = feval (funy, X);
      X = XX;
    elseif (isplot3)
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

    fstrx = regexprep (regexprep (regexprep (fstrx,'\s*\.?\^\s*','^'),
                      '\./', '/'), '\.?\*', '');
    fstry = regexprep (regexprep (regexprep (fstry,'\s*\.?\^\s*','^'),
                      '\./', '/'), '\.?\*', '');
    if (isplot)
      fstr = cstrcat ("x = ",fstrx,", y = ",fstry);
    else
      fstrz = regexprep (regexprep (regexprep (fstrz,'\s*\.?\^\s*','^'),
                                    '\./', '/'), '\.?\*', '');
      fstr = cstrcat ("x = ",fstrx,",y = ",fstry,", z = ",fstrz);
    endif
  else
    if (isplot3)
      needusage = true;
      return;
    endif

    fstr = regexprep (regexprep (regexprep (fstr,'\s*\.?\^\s*','^'), '\./', '/'),
                      '\.?\*', '');
    if (isplot && nargs == 2)
      if (strcmp (typeinfo (fun), "inline function")
          && !isempty (strfind (formula (fun) , "=")))
        fun = inline (cstrcat (strrep (formula (fun), "=", "- ("), ")"));
      else
        fstr = cstrcat (fstr, " = 0");
      endif

      Z = feval (fun, X, Y);

      ## Matlab returns line objects for this case and so can't call
      ## contour directly as it returns patch objects to allow colormaps
      ## to work with contours. Therefore recreate the lines from the
      ## output for contourc, and store in cell arrays.
      [c, lev] = contourc (X, Y, Z, [0, 0]);

      i1 = 1;
      XX = {};
      YY = {};
      while (i1 < length (c))
        clev = c(1,i1);
        clen = c(2,i1);
        XX = [XX, {c(1, i1+1:i1+clen)}];
        YY = [YY, {c(2, i1+1:i1+clen)}];
        i1 += clen+1;
      endwhile
    else
      if (ispolar)
        Z = feval (fun, X);
      elseif (isplot)
        Z = real (feval (fun, X));

        ## Eliminate the singularities. This seems to be what matlab
        ## does, but can't be sure.
        XX = sort (Z (isfinite (Z)));
        if (length (X) > 4)
          d = XX(fix (7 * length (XX) / 8)) - XX(fix (length (XX) / 8));
          yrange = [max(XX(1) - d/8, XX(fix (length (XX) / 8)) - d), ...
                    min(XX(end) + d/8, XX(fix (7 * length (XX) / 8)) + d)];
        else
          yrange = [XX(1), XX(end)];
        endif

        idx = 2 : length(Z);
        idx = find (((Z(idx) > yrange(2) / 2) & (Z(idx-1) < yrange(1) / 2)) |
                 ((Z(idx) < yrange(1) / 2) & (Z(idx-1) > yrange (2) / 2)));
        if (any(idx))
          Z(idx) = NaN;
        endif
      else
        Z = feval (fun, X, Y);

        ## Eliminate the singularities
        Z = __eliminate_sing__ (Z);
      endif
    endif
  endif

  oldax = gca ();
  unwind_protect
    axes (ax);
    if (iscontour)
      [clev, h] = feval (pfunc, X, Y, Z);
    elseif (isplot && nargs == 2)
      h = [];
      hold_state = get (ax, "nextplot");
      for i = 1 : length (XX)
        h = [h; plot(XX{i}, YY{i})];
        if (i == 1)
          set (ax, "nextplot", "add");
        endif
      endfor
      set (ax, "nextplot", hold_state);
    elseif (ispolar || isplot)
      h = feval (pfunc, X, Z);
      if (isplot && !parametric)
        axis ([X(1), X(end), yrange]);
      endif
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
