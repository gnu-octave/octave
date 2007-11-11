## Copyright (C) 1996, 2000, 2002, 2005, 2006, 2007
##               Auburn University. All rights reserved.
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
## @deftypefn {Function File} {} sysout (@var{sys}, @var{opt})
## print out a system data structure in desired format
## @table @var
## @item  sys
## system data structure
## @item  opt
## Display option
## @table @code
## @item []
## primary system form (default)
## @item      "ss"
## state space form
## @item      "tf"
## transfer function form
## @item      "zp"
## zero-pole form
## @item      "all"
## all of the above
## @end table
## @end table
## @end deftypefn

## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>
## Created: 1995-1996

function retsys = sysout (sys, opt)

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  if (isempty (sys))
    retsys = sys;
    error ("sysout: empty system")
    return;
  endif

  if (! isstruct (sys))
    error ("sysout: input must be a system structure")
  endif

  ## set up output type array
  if (nargin == 1)
    opt = sysgettype (sys);
  elseif (! (strcmp (opt, "ss") || strcmp (opt, "tf")
	     || strcmp (opt, "zp") || strcmp (opt, "all")))
    error ("opt must be one of [], \"ss\", \"tf\", \"zp\", or \"all\"");
  endif

  ## now check output for each form:
  [nn, nz, mm, pp] = sysdimensions(sys);
  if (mm > 0)
    disp ("Input(s)")
    disp (__outlist__ (sysgetsignals (sys, "in"), "      "));
  else
    disp ("Input(s): none");
  endif
  if (pp > 0)
    disp ("Output(s):")
    disp (__outlist__ (sysgetsignals (sys, "out"),
		       "     ", sysgetsignals (sys, "yd")) );
  else
    disp ("Output(s): none");
  endif
  if (sysgettsam (sys) > 0)
    disp ("Sampling interval: %g", sysgettsam (sys));
    str = "z";
  else
    str = "s";
  endif

  ## transfer function form
  if (strcmp (opt, "tf") || strcmp (opt, "all"))
    sys = sysupdate (sys, "tf");          #make sure tf is up to date
    disp ("transfer function form:")
    [num, den] = sys2tf (sys);
    tfout (num, den, str);
  endif

  if (strcmp(opt, "zp") || strcmp(opt, "all"))
    sys = sysupdate (sys, "zp");          #make sure zp is up to date
    disp("zero-pole form:")
    [zer, pol, kk] = sys2zp (sys);
    zpout (zer, pol, kk, str)
  endif

  if (strcmp(opt, "ss") || strcmp(opt, "all"))
    sys = sysupdate (sys, "ss");
    disp ("state-space form:");
    printf ("%d continuous states, %d discrete states\n", nn, nz);
    if (nn+nz > 0)
      disp ("State(s):")
      xi = (nn+1):(nn+nz);
      xd = zeros (1, nn+nz);
      if (! isempty (xi))
        xd(xi) = 1;
      endif
      disp (__outlist__ (sysgetsignals (sys, "st"), "    ", xd));
    else
      disp ("State(s): none");
    endif

    ## display matrix values?
    dmat = (max ([nn+nz, mm, pp]) <= 32);

    printf ("A matrix: %d x %d\n", sysdimensions (sys, "st"),
            sysdimensions (sys, "st"));
    [aa, bb, cc, dd] = sys2ss (sys);
    if (dmat)
      disp (aa);
    endif

    printf ("B matrix: %d x %d\n", sysdimensions (sys, "st"),
            sysdimensions (sys, "in"));
    if (dmat)
      disp (bb);
    endif

    printf ("C matrix: %d x %d\n", sysdimensions (sys, "out"),
            sysdimensions (sys, "st"));
    if (dmat)
      disp (cc);
    endif

    printf("D matrix: %d x %d\n", sysdimensions (sys, "out"),
           sysdimensions (sys, "in"));
    if (dmat)
      disp (dd);
    endif
  endif

  if (nargout >= 1)
    retsys = sys;
  endif

endfunction
