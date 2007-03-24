## Copyright (C) 1996, 1998 Kai P. Mueller
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the
## Free Software Foundation; either version 2, or (at your option) any
## later version.
##
## Octave is distributed in the hope that it will be useful, but WITHOUT
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
## FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
## for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301 USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} dhinfdemo ()
## Demonstrate the functions available to design a discrete
## @iftex
## @tex
## $ { \cal H }_\infty $
## @end tex
## @end iftex
## @ifinfo
## H-infinity
## @end ifinfo
## controller.  This is not a true discrete design. The
## design is carried out in continuous time while the effect of sampling
## is described by a bilinear transformation of the sampled system.
## This method works quite well if the sampling period is "small"
## compared to the plant time constants.
##
## Continuous plant:
## @iftex
## @tex
## $$ G(s) = { 1 \over (s+2) (s+1) } $$
## @end tex
## @end iftex
## @ifinfo
## @example
## @group
##                   1
##      G(s) = --------------
##             (s + 2)(s + 1)
## @end group
## @end example
## @end ifinfo
##
## Discretised plant with @acronym{ZOH} (Sampling period = @var{Ts} = 1 second):
## @iftex
## @tex
## $$ G(z) = { 0.39958z + 0.14700 \over (z - 0.36788) (z - 0.13533) } $$
## @end tex
## @end iftex
## @ifinfo
## @example
## @group
##                 0.39958z + 0.14700
##      G(z) = --------------------------
##             (z - 0.36788)(z - 0.13533)
## @end group
## @end example
## @end ifinfo
##
## @example
## @group
##                               +----+
##          -------------------->| W1 |---> v1
##      z   |                    +----+
##      ----|-------------+                   || T   ||     => min.
##          |             |                       vz   infty
##          |    +---+    v      +----+
##          *--->| G |--->O--*-->| W2 |---> v2
##          |    +---+       |   +----+
##          |                |
##          |    +---+       |
##          -----| K |<-------
##               +---+
## @end group
## @end example
##
## @noindent
## W1 and W2 are the robustness and performancs weighting functions.
## @end deftypefn

## K. Mueller, <mueller@ifr.ing.tu-bs.de>
## Technical University of Braunschweig, IfR

echo off
disp(" ");
disp("    --------------------------------------------------");
disp("    Discrete H_infinity optimal control for the plant:");
disp(" ");
disp("                     0.39958z + 0.14700");
disp("          G(s) = --------------------------");
disp("                 (z - 0.36788)(z - 0.13533)");
disp("    --------------------------------------------------");
disp(" ");

disp("sampling time:")
cmd = "Ts = 1.0;";
disp(cmd);
eval(cmd);
disp("weighting on actuator value u");
cmd = "W1 = wgt1o(0.1, 200.0, 50.0);";
disp(cmd);
eval(cmd);
disp("weighting on controlled variable y");
cmd = "W2 = wgt1o(350.0, 0.05, 0.0002);";
disp(cmd);
eval(cmd);
## omega axis (column vector)
ww = vec(logspace(-4.99, 3.99, 100));

disp("Create ZOH equivalent model of a continuous plant");
cmd = "G = tf(2,[1 3 2]);  Gd = c2d(G, Ts);";
run_cmd

## w-plane (continuous representation of the sampled system)
disp("W-plane transform of discrete time system:");
cmd = "Gw = d2c(Gd, \"bi\");";
run_cmd

disp(" ");
disp(" o building P...");
## need One as the pseudo transfer function One = 1
cmd = "One = ugain(1);";
disp(cmd);
eval(cmd);
cmd = " psys = buildssic([1 4;2 4;3 1],[3],[2 3 5],[3 4],Gw,W1,W2,One);";
run_cmd;
disp(" o controller design...");
cmd = "[K, gfin, GWC] = hinfsyn(psys, 1, 1, 0.1, 10.0, 0.02);";
run_cmd

disp(" ");
fig_n = 1;
yn = input(" * Plot magnitudes of W1KS and W2S? [n]: ","S");
if (length(yn) >= 1)
  if ((yn(1) == "y") || (yn(1) == 'Y'))
    disp(" o magnitudes of W1KS and W2S...");
    gwx = sysprune(GWC, 1, 1);
    mag1 = bode(gwx, ww);
    if (columns(mag1) > 1);  mag1 = mag1';  endif
    gwx = sysprune(GWC, 2, 1);
    mag2 = bode(gwx, ww);
    if (columns(mag2) > 1);  mag2 = mag2';  endif
    figure(fig_n)
    fig_n = fig_n + 1;
    loglog(ww, [mag1 mag2]);
    grid ("on");
  endif
endif

Kd = c2d(K, "bi", Ts);
GG = buildssic([1 2; 2 1], [], [1 2], [-2], Gd, Kd);
disp(" o closed loop poles...");
damp(GG);

disp(" ");
yn = input(" * Plot closed loop step responses? [n]: ","S");
if (length(yn) >= 1)
  if ((yn(1) == "y") || (yn(1) == 'Y'))
    disp(" o step responses of T and KS...");
    figure(fig_n)
    step(GG, 1, 10);
  endif
endif

## --------- End of dhinfdemo/kpm
