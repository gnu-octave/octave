## Copyright (C) 1996, 1998, 2000, 2004, 2005, 2007
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

save_var = page_screen_output ();
page_screen_output (1);
disp("Description of system data structure:")
disp("A linear system is stored in a structure, and may be represented in")
disp("ss (state space), tf (transfer function),  and/or zp (zero-pole-gain)")
disp("form.")
disp(" ")
disp("variables in all representations:")
disp("inname: list of signal names (strings) containing name(s) of system ")
disp("        inputs; see is_signal_list");
disp("n: number of continuous states")
disp("nz: number of discrete states")
disp("outname: list of signal names (strings) containing name(s) of system ")
disp("        outputs");
disp(" ")
disp("variables in all representations:(cont'd)")
disp("sys: system status vector.  This vector indicates both what")
disp("     representation was used to initialize the system data structure")
disp("     (called the primary system type) and which other representations")
disp("     are currently up-to-date with the primary system type.")
disp("     sys(0): primary system type")
disp("           =0 for tf form")
disp("           =1 for zp form")
disp("           =2 for ss form")
disp("     sys(1:3): indicate whether tf, zp, or ss, respectively, are")
disp("          \"up to date\" (whether it is safe to use the variables")
disp("          associated with these representations)")
disp("     sys(1): = 1 if tf variables are up to date")
disp("             = 0 else");
disp("     sys(2): = 1 if zp variables are up to date")
disp("             = 0 else");
disp("     sys(3): = 1 if ss variables are up to date")
disp("             = 0 else");
disp("You can update alternative representations internally with the")
disp("sysupdate command:")
disp(" ")
help sysupdate
disp("===============================")
disp("More variables common to all representations in system data structures:");
disp("tsam: discrete time sampling interval ")
disp("      =0 if system is purely continuous");
disp("      >0 if system has discrete-time states or outputs");
disp("yd: vector indicating which outputs are discrete time (i.e.,")
disp("    produced by D/A converters) and which are continuous time.")
disp("    yd(ii) = 0 if output ii is continuous, = 1 if discrete.")
disp(" ")
disp("===============================")
disp("variables in tf representations (SISO only):")
disp("num: vector of numerator coefficients")
disp("den: vector of denominator coefficients")
disp(" ")
disp("===============================")
disp("variables in zp representations (SISO only):")
disp("zer: vector of system zeros")
disp("pol: vector of system poles")
disp("k: system leading coefficient")
disp(" ")
disp("===============================")
disp("variables in ss representations:")
disp("a,b,c,d: usual state-space matrices.  If a system has both")
disp("        continuous and discrete states, they are sorted so that")
disp("        continuous states come first, then discrete states")
disp(" ")
disp("WARNING: some functions (e.g., bode) will not accept systems")
disp("with both discrete and continuous states/outputs")
disp("stname: list of signal names (strings) containing name(s) of system ")
disp("        states");
disp("===============================")
disp("Object oriented programming:")
disp("It is recommended that users do not directly access the internal")
disp("variables themselves, but use the interface functions")
disp("  fir             ss              tf              sys2fir")
disp("  sys2ss          sys2tf          sys2zp          syschtsam")
disp("  sysdimensions   sysgetsignals   syssetsignals   sysgettype")
disp("  zp    ")
disp("to create/access internal variables.  ");
page_screen_output (save_var);
