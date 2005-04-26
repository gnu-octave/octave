## Copyright (C) 1996 Auburn University.  All rights reserved.
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
## @deftypefn {Function File} {} analdemo ()
## Octave Controls toolbox demo: State Space analysis demo
## @end deftypefn

## Author: David Clem
## Created: August 15, 1994
## Updated by John Ingram December 1996

function analdemo ()

  while (1)
    clc
    k=0;
    while(k > 8 || k < 1)
      k = menu("Octave State Space Analysis Demo", ...
        "System gramians (gram, dgram)", ...
        "System zeros (tzero)", ...
        "Continuous => Discrete and Discrete => Continuous conversions (c2d,d2c)", ...
        "Algebraic Riccati Equation (are, dare)", ...
        "Balanced realizations (balreal, dbalreal)", ...
        "Open loop truncations via Hankel singular values (balreal, dbalreal)", ...
        "SISO pole placement", ...
        "Return to main demo menu");
    endwhile
    if (k == 1)
      clc
      help dgram
      prompt

      clc
      disp("System Gramians: (see Moore, IEEE T-AC, 1981) \n");
      disp("Example #1, consider the discrete time state space system:\n");
      a=[1, 5, -8.4; 1.2, -3, 5; 1, 7, 9]
      b=[1, 5; 2, 6; -4.4, 5]
      c=[1, -1.5, 2; 6, -9.8, 1]
      d=0
      prompt
      disp("\nThe discrete controllability gramian is computed as follows:");
      cmd = "gramian = dgram(a, b);";
      run_cmd;
      disp("Results:\n");
      gramian = dgram(a,b)
      disp("Variable Description:\n");
      disp("gramian => discrete controllability gramian");
      disp("a, b => a and b matrices of discrete time system\n");
      disp("A dual approach may be used to compute the observability gramian.");
      prompt
      clc

      help gram
      prompt
      clc

      disp("Example #2, consider the continuous state space system:\n");
      a=[1, 3, -10.2; 3.7, -2, 9; 1, 3, 7]
      b=[1, 12; 6, 2; -3.8, 7]
      c=[1, -1.1, 7; 3, -9.8, 2]
      d=0
      prompt
      disp("\nThe continuous controllability gramian is computed as follows:");
      cmd = "gramian = gram(a, b);";
      run_cmd;
      disp("Results:\n");
      gramian = gram(a,b)
      disp("Variable Description:\n");
      disp("gramian => continuous controllability gramian");
      disp("a, b => a and b matrices of continuous time system\n");
      disp("A dual approach may be used to compute the observability gramian.");
      prompt
      clc


    elseif (k == 2)
      clc
      help tzero
      prompt

      disp("System zeros (tzero) example\n");
      disp("Example #1, consider the state space system:\n");
      a=[0, 1, 0; -10, -2, 0; -10, 0, -8]
      b=[0; 1; 9]
      c=[-10, 0, -4]
      d=1
      prompt
      disp("\nTo compute the zeros of this system, enter the following command:\n");
      cmd = "zer = tzero(a,b,c,d);";
      run_cmd;
      disp("Results:\n");
      zer = tzero(a,b,c,d)
      disp("Variable Description:\n");
      disp("zer => zeros of state space system");
      disp("a, b, c, d => state space system used as input argument");
      prompt
      clc

      disp("Example #2, consider the state space system from example 1 again:");
      cmd = "sys = ss(a,b,c,d);";
      disp(cmd);
      eval(cmd);
      sysout(sys);
      disp("\nThe zeros of this system can also be calculated directly from the");
      disp("system variable:");
      cmd = "zer = tzero(sys);";
      run_cmd;
      disp("Results:\n")
      zer = tzero(sys)
      disp("Variable Description:\n");
      disp("zer => zeros of state space system");
      disp("sys => state space system used as input argument");
      prompt
      clc

    elseif (k == 3)
      clc
      help c2d
      prompt

      clc
      disp("Continuous => Discrete and Discrete => Continuous conversions (c2d,d2c)");
      disp("\nExample #1, consider the following continuous state space system");
      cmd = "sys_cont = ss([-11, 6; -15, 8], [1; 2], [2, -1], 0);";
      eval(cmd);
      disp(cmd);
      disp("Examine the poles and zeros of the continuous system:");
      sysout(sys_cont,"all");
      disp("\nTo convert this to a discrete system, a sampling time is needed:");
      cmd = "Tsam = 0.5;";
      run_cmd;
      disp("\nNow convert to a discrete system with the command:");
      cmd = "sys_disc = c2d(sys_cont,Tsam);";
      run_cmd;
      disp("Examine the poles and zeros of the discrete system:");
      sysout(sys_disc,"all");
      prompt
      clc

      disp("\nNow we will convert the discrete system back to a continuous system");
      disp("using the d2c command:");
      help d2c
      prompt
      cmd = "new_sys_cont = d2c(sys_disc);";
      run_cmd;
      disp("\nExamine the poles and zeros of the discrete system:");
      sysout(new_sys_cont,"all");
      prompt

    elseif (k == 4)
      clc
      help are
      prompt
      clc

      disp("Algebraic Riccati Equation (are, dare)");

      disp("\nExample #1, consider the continuous state space system:\n");
      a=[1, 3, -10.2; 3.7, -2, 9; 1, 3, 7]
      b=[1, 12; 6, 2; -3.8, 7]
      c=[1, -1.1, 7; 3, -9.8, 2]
      d=0
      prompt
      disp("\nThe solution to the continuous algebraic riccati equation");
      disp("is computed as follows:");
      cmd = "x_cont = are(a, b, c);";
      run_cmd;
      disp("Results:\n")
      x_cont = are(a,b,c)
      disp("Variable Description:\n")
      disp("x_cont => solution to the continuous algebraic riccati equation");
      disp("a, b, c => a, b, and c matrices of continuous time system\n");
      prompt

      clc
      help dare
      prompt
      clc

      disp("Example #2, consider the discrete time state space system:\n");
      a=[1, 5, -8.4; 1.2, -3, 5; 1, 7, 9]
      b=[1, 5; 2, 6; -4.4, 5]
      c=[1, -1.5, 2; 6, -9.8, 1]
      d=0
      r=eye(columns(b))
      prompt
      disp("\nThe solution to the continuous algebraic riccati equation");
      disp("is computed as follows:");
      cmd = "x_disc = dare(a, b, c, r);";
      run_cmd;
      disp("Results:\n")
      x_disc = dare(a,b,c,r)
      disp("Variable Description:\n");
      disp("x_disc => solution to the discrete algebraic riccati equation");
      disp("a, b, c => a, b and c matrices of discrete time system\n");
      prompt
      clc

    elseif (k == 5)
      disp("--- Balanced realization: not yet implemented")
    elseif (k == 6)
      disp("--- Open loop balanced truncation: not yet implemented")
    elseif (k == 7)
      disp("SISO pole placement example:")
      cmd = "sys=tf(1, [1, -2, 1]);";
      run_cmd
      disp("System in zero-pole form is:")
      cmd = "sysout(sys,\"zp\");";
      run_cmd
      disp("and in state space form:")
      cmd = "sysout(sys,\"ss\");";
      run_cmd
      disp("Desired poles at -1, -1");
      cmd = "K=place(sys, [-1, -1])";
      run_cmd
      disp("Check results:")
      cmd = "[A,B] = sys2ss(sys);";
      run_cmd
      cmd = "poles=eig(A-B*K)";
      run_cmd
      prompt
    elseif (k == 8)
      return
    endif
  endwhile
endfunction

