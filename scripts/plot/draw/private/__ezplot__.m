########################################################################
##
## Copyright (C) 2007-2023 The Octave Project Developers
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
## @deftypefn {} {[@var{h}, @var{needusage}] =} __ezplot__ (@var{pltfcn}, @var{varargin})
## Undocumented internal function.
## @end deftypefn

## Overview: This function is the back-end for the 9 ez* plot functions.
##           As such, most of the function is actually dedicated to sorting
##           out the inputs and verifying that the particular ez* function
##           called was called correctly.  The actual plotting occurs near
##           the end in an unwind_protect block.

function [h, needusage] = __ezplot__ (pltfcn, varargin)

  ezfcn = ["ez" pltfcn];

  [hax, varargin, nargin] = __plt_get_axis_arg__ (ezfcn, varargin{:});

  ## Define outputs early in case of shorting out of function with return;
  h = [];
  needusage = false;
  if (nargin < 1)
    needusage = true;
    return;
  endif

  iscontour = strncmp (pltfcn, "contour", 7);

  ## Defaults for ezplot
  isplot  = true;
  isplot3 = false;
  ispolar = false;
  nargs = 1;
  switch (pltfcn)
    case "plot"
      ## defaults already set

    case "plot3"
      isplot  = false;
      isplot3 = true;

    case "polar"
      isplot  = false;
      ispolar = true;

    otherwise
      ## contour, mesh, surf plots
      isplot  = false;
      nargs = 2;

  endswitch

  parametric = false;
  fcn = varargin{1};
  ## Don't warn about intentional use of inline functions (Bug #62682)
  warning ("off", "Octave:legacy-function", "local");
  if (ischar (fcn))
    if (exist (fcn, "file") || exist (fcn, "builtin"))
      fcn = str2func (fcn);            # convert to function handle
    else
      fcn = vectorize (inline (fcn));  # convert to inline function
    endif
  endif

  if (isa (fcn, "inline"))
    argids = argnames (fcn);
    if (isplot && length (argids) == 2)
      nargs = 2;
    elseif (numel (argids) != nargs)
      error ("%s: expecting a function of %d arguments", ezfcn, nargs);
    endif
    fcn = vectorize (fcn);
    fstr = formula (fcn);
    if (isplot)
      xarg = argids{1};
      if (nargs == 2)
        yarg = argids{2};
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
      xarg = argids{1};
      yarg = argids{2};
    endif
  elseif (is_function_handle (fcn))
    fstr = func2str (fcn);
    idx = index (fstr, ')');
    if (idx != 0)
      args = regexp (fstr(3:(idx-1)), '\w+', 'match');
      fstr = fstr(idx+2:end);  # remove '@(x) ' from string name
    else
      args = {"x"};
      try
        if (builtin ("nargin", fcn) == 2)
          args{2} = "y";
        endif
      end_try_catch
    endif
    if (isplot && length (args) == 2)
      nargs = 2;
    elseif (numel (args) != nargs)
      error ("%s: expecting a function of %d arguments", ezfcn, nargs);
    endif
    if (isplot)
      xarg = args{1};
      if (nargs == 2)
        yarg = args{2};
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
      xarg = args{1};
      yarg = args{2};
    endif
  else
    error ("%s: F must be a string or function handle", ezfcn);
  endif

  if (nargin > 2 || (nargin == 2 && isplot))
    funx = fcn;
    fstrx = fstr;
    funy = varargin{2};
    if (ischar (funy) && ! strcmp (funy, "circ") && ! strcmp (funy, "animate"))
      parametric = true;
      if (exist (funy, "file") || exist (funy, "builtin"))
        funy = inline ([funy "(t)"]);
      else
        funy = vectorize (inline (funy));
      endif
      if (numel (argnames (funy)) != nargs)
        error ("%s: expecting a function of %d arguments", ezfcn, nargs);
      endif
      fstry = formula (funy);
    elseif (isa (funy, "inline"))
      parametric = true;
      if (numel (argnames (funy)) != nargs)
        error ("%s: expecting a function of %d arguments", ezfcn, nargs);
      endif
      funy = vectorize (funy);
      fstry = formula (funy);
    elseif (is_function_handle (funy))
      parametric = true;
      fstry = func2str (funy);
      idx = index (fstry, ')');
      if (idx != 0)
        args = regexp (fstry(3:(idx-1)), '\w+', 'match');
        fstry = fstry(idx+2:end);  # remove '@(x) ' from string name
      else
        args = {"y"};
      endif
      if (numel (args) != nargs)
        error ("%s: expecting a function of %d arguments", ezfcn, nargs);
      endif
    endif

    if (! parametric && isplot3)
      needusage = true;  # Can't call non-parametric ezplot3
      return;
    elseif (parametric && isplot)
      if (nargs == 2)
        error ("%s: can not define a parametric function in this manner", ezfcn);
      else
        xarg = "x";
        yarg = "y";
      endif
    elseif (parametric)
      funz = varargin{3};
      if (ischar (funz) && ! strcmp (funz, "circ")
          && ! strcmp (funz, "animate"))
        if (exist (funz, "file") || exist (funz, "builtin"))
          funz = inline ([funz "(t)"]);
        else
          funz = vectorize (inline (funz));
        endif
        if (numel (argnames (funz)) > nargs)
          error ("%s: expecting a function of %d arguments", ezfcn, nargs);
        endif
        fstrz = formula (funz);
      elseif (isa (funz, "inline"))
        if (numel (argnames (funz)) != nargs)
          error ("%s: expecting a function of %d arguments", ezfcn, nargs);
        endif
        funz = vectorize (funz);
        fstrz = formula (funz);
      elseif (is_function_handle (funz))
        fstrz = func2str (funz);
        idx = index (fstrz, ')');
        if (idx != 0)
          args = regexp (fstrz(3:(idx-1)), '\w+', 'match');
          fstrz = fstrz(idx+2:end);  # remove '@(x) ' from string name
        else
          args = {"z"};
        endif
        if (numel (args) != nargs)
          error ("%s: expecting a function of %d arguments", ezfcn, nargs);
        endif
      else
        error ("%s: parametric plots require 3 functions", ezfcn);
      endif
    endif
  endif

  if ((isplot && nargs != 2) || isplot3 || ispolar)
    n = 500;   # default for point-style functions like plot
  else
    n = 60;    # default for meshgrid style functions like contour, surf
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
    elseif (isscalar (arg) && (n == 60 || n == 500))
      n = arg;
    elseif (numel (arg) == 2 && isempty (domain))
      domain = [arg(1) arg(2) arg(1) arg(2)];
    elseif (numel (arg) == 4 && isempty (domain))
      domain = arg(:).';
    else
      error ("%s: expecting scalar N, or 2-/4-element vector DOM", ezfcn);
    endif
  endwhile

  if (circ && (iscontour || isplot3 || isplot))
    needusage = true;
    return;
  elseif (circ && parametric)
    error ("%s: can not have both circular domain and parametric function",
           ezfcn);
  endif

  if (animate && ! isplot3)
    error ("%s: animate option only valid for ezplot3", ezfcn);
  endif

  if (parametric)
    ## Make the label strings pretty by removing extra spaces between base
    ## and exponent, the '.' in vectorized code, and the '*' for multiply.
    fstrx = regexprep (regexprep (regexprep (fstrx,
           '\s*\.?(?:\^|\*\*)\s*','^'), '\.([/+-])', '$1'), '\s*\.?\*\s*', ' ');
    fstry = regexprep (regexprep (regexprep (fstry,
           '\s*\.?(?:\^|\*\*)\s*','^'), '\.([/+-])', '$1'), '\s*\.?\*\s*', ' ');
    if (isplot)
      fstr = ["x = " fstrx ", y = " fstry];
    else
      fstrz = regexprep (regexprep (regexprep (fstrz,
           '\s*\.?(?:\^|\*\*)\s*','^'), '\.([/+-])', '$1'), '\s*\.?\*\s*', ' ');
      fstr = ["x = " fstrx ", y = " fstry ", z = " fstrz];
    endif
  else
    fstr = regexprep (regexprep (regexprep (fstr,
           '\s*\.?(?:\^|\*\*)\s*','^'), '\.([/+-])', '$1'), '\s*\.?\*\s*', ' ');
    if (isplot && nargs == 2)
      fstr = [fstr " = 0"];  # make title string of implicit function
    elseif (ispolar)
      fstr = ["r = " fstr];
    endif
  endif

  if (isempty (domain))
    auto_domain = true;
    if (isplot3 || ispolar)
      domain = [0, 2*pi, 0, 2*pi];
    else
      domain = [-2*pi, 2*pi, -2*pi, 2*pi];
    endif
  else
    auto_domain = false;
  endif

  auto_domain_done = false;
  do
    domain_ok = true;

    if ((isplot && nargs == 1) || isplot3 || ispolar)
      X = linspace (domain(1), domain(2), n);
    elseif (isplot && numel (domain) == 2)
      x = linspace (domain(1), domain(2), n);
      [X, Y] = meshgrid (x, x);
    elseif (circ)
      ## To plot on circular domain develop grid in polar coordinates
      ## and then switch these to Cartesian coordinates.
      cent = [domain(1) + domain(2), domain(3) + domain(4)] / 2;
      rmax = sqrt ((domain(2) - cent(1))^2 + (domain(4) - cent(2))^2);
      r = linspace (0, rmax, n);
      t = linspace (0, 2*pi, n);
      [T, R] = meshgrid (t, r);
      X = R .* cos (T) + cent(1);
      Y = R .* sin (T) + cent(2);
      domain = [-rmax+cent(1), +rmax+cent(1), -rmax+cent(2), +rmax+cent(2)];
    else  # contour, mesh, surf plots
      x = linspace (domain(1), domain(2), n);
      y = linspace (domain(3), domain(4), n);
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
    else  # non-parametric plots
      if (isplot && nargs == 2)
        Z = feval (fcn, X, Y);

        ## Matlab returns line objects for this case and so can't call
        ## contour directly as it returns patch objects to allow colormaps
        ## to work with contours.  Therefore recreate the lines from the
        ## output for contourc, and store in cell arrays.
        [c, ~] = contourc (X, Y, Z, [0, 0]);

        i = 1;
        XX = YY = {};
        while (i < length (c))
          clev = c(1,i);
          clen = c(2,i);
          XX = [XX, {c(1, i+1:i+clen)}];
          YY = [YY, {c(2, i+1:i+clen)}];
          i += clen+1;
        endwhile
      else
        if (ispolar)
          Z = feval (fcn, X);
          ## FIXME: Why aren't singularities eliminated for polar plots?
        elseif (isplot)
          Z = feval (fcn, X);
          ## Eliminate the singularities
          Z = __eliminate_sing__ (Z);
          domain = find_valid_domain (X, [], Z);
        elseif (iscontour)
          Z = feval (fcn, X, Y);
          Z = __eliminate_sing__ (Z);
        else  #  mesh, surf plots
          Z = feval (fcn, X, Y);
          Z = __eliminate_sing__ (Z);
          if (circ)
            ## Use domain calculated at the start.
            ## The X, Y grids are non-monotonic after conversion from polar
            ## coordinates and find_valid_domain fails.

          elseif (auto_domain && ! auto_domain_done)
            valid_domain = find_valid_domain (X, Y, Z);
            domain_ok = all (domain == valid_domain);
            domain = valid_domain;
            auto_domain_done = true;  # ensures only 1 round of do loop done
          else
            if (! auto_domain_done)
              domain = find_valid_domain (X, Y, Z);
            endif
          endif
        endif
      endif
    endif
  until (domain_ok)

  ## Now, actually call the correct plot function with valid data and domain.
  oldfig = [];
  if (! isempty (hax))
    oldfig = get (0, "currentfigure");
  endif
  unwind_protect
    hax = newplot (hax);
    if (iscontour)
      [~, h] = feval (pltfcn, hax, X, Y, Z);
    elseif (isplot && nargs == 2)
      h = zeros (length (XX), 1);
      hold_state = get (hax, "nextplot");
      for i = 1 : length (XX)
        if (i == 1)
          h(1) = plot (hax, XX{1}, YY{1});
          set (hax, "nextplot", "add");
          color = get (h(1), "color");
        else
          h(i) = plot (hax, XX{i}, YY{i}, "color", color);
        endif
      endfor
      set (hax, "nextplot", hold_state);
      axis (hax, domain);
    elseif (isplot || ispolar)
      h = feval (pltfcn, hax, X, Z);
      if (isplot)
        if (! parametric)
          axis (hax, domain);
        else
          axis ("equal");
        endif
      endif
    elseif (isplot3)
      if (animate)
        comet3 (hax, X, Y, Z);
      else
        h = feval (pltfcn, hax, X, Y, Z);
      endif
      grid (hax, "on");
      zlabel (hax, "z");
    else  # mesh and surf plots
      h = feval (pltfcn, hax, X, Y, Z);
      ## FIXME: surf, mesh should really do a better job of setting zlim
      if (! parametric)
        axis (hax, domain);
      endif
    endif
    xlabel (hax, xarg);
    ylabel (hax, yarg);
    title (hax, fstr);
  unwind_protect_cleanup
    if (! isempty (oldfig))
      set (0, "currentfigure", oldfig);
    endif
  end_unwind_protect

endfunction

## Eliminate bad data (complex values, infinities, singularities)
function x = __eliminate_sing__ (x)

  if (iscomplex (x))
    x(imag (x) != 0) = NaN;
  endif
  x(isinf (x)) = NaN;
  ## High rates of curvature are treated as singularities
  threshold = 0.2 * (max (x(:)) - min (x(:)));
  x(abs (del2 (x)) > threshold) = NaN;

endfunction

## Find: 1) range of function where there are not NaN values,
##       2) function is changing (not just flat surface)
function domain = find_valid_domain (X, Y, Z)

  if (isvector (Z))
    ## 2-D data for isplot
    domain = [X(1) X(end)];

    ## Guess a range which includes the "mass" of the data by using a
    ## median-based approach.  The center 3/4 of the data is used to
    ## determine the range of the data.
    ## This seems to be vaguely what Matlab does, but can't be sure.
    XX = sort (Z(isfinite (Z)));
    if (length (X) > 4)
      irlo = XX(fix (1/8 * length (XX)));
      irhi = XX(fix (7/8 * length (XX)));
      d = irhi - irlo;
      domain(3) = max (XX(1) - d/8, irlo - d);
      domain(4) = min (XX(end) + d/8, irhi + d);
    else
      domain(3:4) = [XX(1), XX(end)];
    endif

    ## Handle exceptional case of constant function
    if (domain(3) == domain(4))
      domain(3) -= 1;
      domain(4) += 1;
    endif

  else
    ## 3-D data such as mesh, surf
    Zfinite = ! isnan (Z);
    Zrows = any (Zfinite, 2);
    rmin = find (Zrows, 1, "first");
    rmax = find (Zrows, 1, "last");
    Zcols = any (Zfinite, 1);
    cmin = find (Zcols, 1, "first");
    cmax = find (Zcols, 1, "last");

    ## Handle nasty case of all NaNs
    if (isempty (rmin))
      rmin = 1; rmax = rows (Z);
    endif
    if (isempty (cmin))
      cmin = 1; cmax = columns (Z);
    endif

    if (   ! any (isnan (Z([rmin, rmax],:)(:)))
        && ! any (isnan (Z(:, [cmin, cmax])(:))))
      ## Exclude surfaces along borders which are flat (gradient =~ 0).
      ## Technically, this calculation might be better done with actual
      ## deltaX, deltaY values.  But, data is usually meshgridded
      ## (constant spacing) so working with deltaROW#, deltaCOL# is fine.
      [Zx, Zy] = gradient (Z(rmin:rmax, cmin:cmax));
      Zgrad = sqrt (Zx.^2 + Zy.^2);
      slope = ((max (Z(:)) - min (Z(:)))
                / sqrt ((rmax - rmin)^2 + (cmax - cmin)^2));
      slope /= 125;  # threshold for discarding points.
      Zrows = any (Zgrad > slope, 2);
      rmin += find (Zrows, 1, "first") - 1;
      rmax += find (Zrows, 1, "last") - rows (Zrows);
      Zcols = any (Zgrad > slope, 1);
      cmin += find (Zcols, 1, "first") - 1;
      cmax += find (Zcols, 1, "last") - columns (Zcols);
    endif

    domain = [X(1,cmin) X(1,cmax) Y(rmin,1) Y(rmax,1)];
  endif

endfunction
