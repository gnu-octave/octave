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
## Software Foundation, 59 Temple Place, Suite 330, Boston, MA 02111 USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} rldemo (@var{inputs})
## Octave Control toolbox demo: Root Locus demo.
## @end deftypefn

## Author: David Clem
## Created: August 15, 1994
## Updated by John Ingram December 1996

function rldemo ()

  while (1)
    clc
    k = menu("Octave Root Locus Demo", ...
        "Display continuous system's open loop poles and zeros (pzmap)", ...
        "Display discrete system's open loop poles and zeros (pzmap)", ...
        "Display root locus diagram of SISO continuous system (rlocus)", ...
        "Display root locus diagram of SISO discrete system (rlocus)", ...
        "Return to main demo menu");
    __gnuplot_set__ autoscale
    if (k == 1)
      clc
      help pzmap
      prompt

      clc
      disp("Display continuous system's open loop poles and zeros (pzmap)\n");
      disp("Example #1, Consider the following continuous transfer function:");
      cmd = "sys1 = tf([1.5, 18.5, 6], [1, 4, 155, 302, 5050]);";
      disp(cmd);
      eval(cmd);
      cmd ="sysout(sys1);";
      disp(cmd);
      eval(cmd);
      disp("\nPole-zero form can be obtained as follows:");
      cmd = "sysout(sys1,""zp"");";
      disp(cmd);
      eval(cmd);
      disp("View the system's open loop poles and zeros with the command:")
      cmd = "pzmap(sys1);";
      run_cmd
      prompt

      clc
      disp("Example #2, Consider the following set of poles and zeros:");
      cmd = "sys2 = zp([-1, 5, -23],[-1, -10, -7+5i, -7-5i],5);";
      disp(cmd);
      eval(cmd);
      cmd = "sysout(sys2);";
      disp(cmd);
      eval(cmd);
      disp("\nThe pzmap command for the zp form is the same as the tf form:")
      cmd = "pzmap(sys2);";
      run_cmd;
      disp("\nThe internal representation of the system is not important;");
      disp("pzmap automatically sorts it out internally.");
      prompt;

      clc
      disp("Example #3, Consider the following state space system:\n");
      cmd = "sys3=ss([0, 1; -10, -11], [0; 1], [0, -2], 1);";
      disp(cmd);
      eval(cmd);
      cmd = "sysout(sys3);";
      disp(cmd);
      eval(cmd);
      disp("\nPole-zero form can be obtained as follows:");
      cmd = "sysout(sys3,""zp"");";
      disp(cmd);
      eval(cmd);
      disp("\nOnce again, the pzmap command is the same:");
      cmd = "pzmap(sys3);";
      run_cmd;
      prompt;

      closeplot
      clc

    elseif (k == 2)
      clc
      help pzmap
      prompt

      clc
      disp("\nDisplay discrete system's open loop poles and zeros (pzmap)\n");
      disp("First we must define a sampling time, as follows:\n");
      cmd = "Tsam = 1;";
      run_cmd;
      disp("Example #1, Consider the following discrete transfer function:");
      cmd = "sys1 = tf([1.05, -0.09048], [1, -2, 1],Tsam);";
      disp(cmd);
      eval(cmd);
      cmd ="sysout(sys1);";
      disp(cmd);
      eval(cmd);
      disp("\nPole-zero form can be obtained as follows:");
      cmd = "sysout(sys1,""zp"");";
      disp(cmd);
      eval(cmd);
      disp("View the system's open loop poles and zeros with the command:")
      cmd = "pzmap(sys1);";
      run_cmd
      prompt

      clc
      disp("Example #2, Consider the following set of discrete poles and zeros:");
      cmd = "sys2 = zp(-0.717, [1, -0.368], 3.68, Tsam);";
      disp(cmd);
      eval(cmd);
      cmd = "sysout(sys2);";
      disp(cmd);
      eval(cmd);
      disp("\nThe pzmap command for the zp form is the same as the tf form:")
      cmd = "pzmap(sys2);";
      run_cmd;
      disp("\nThe internal representation of the system is not important;");
      disp("pzmap automatically sorts it out internally.");
      prompt;

      clc
      disp("Example #3, Consider the following discrete state space system:\n");
      cmd = "sys3=ss([1, 0.0952; 0, 0.905], [0.00484; 0.0952], [1, 0], 0, Tsam);";
      disp(cmd);
      eval(cmd);
      cmd = "sysout(sys3);";
      disp(cmd);
      eval(cmd);
      disp("\nPole-zero form can be obtained as follows:");
      cmd = "sysout(sys3,""zp"");";
      disp(cmd);
      eval(cmd);
      disp("\nOnce again, the pzmap command is the same:");
      cmd = "pzmap(sys3);";
      run_cmd;
      prompt;

      closeplot
      clc

    elseif (k == 3)
      clc
      help rlocus
      prompt;

      clc
      disp("Display root locus of a continuous SISO system (rlocus)\n")
      disp("Example #1, Consider the following continuous transfer function:");
      cmd = "sys1 = tf([1.5, 18.5, 6],[1, 4, 155, 302, 5050]);";
      disp(cmd);
      eval(cmd);
      cmd ="sysout(sys1);";
      disp(cmd);
      eval(cmd);
      disp("\nPole-zero form can be obtained as follows:");
      cmd = "sysout(sys1,""zp"");";
      disp(cmd);
      eval(cmd);
      disp("\nWhen using rlocus, inital system poles are displayed as X's.")
      disp("Moving poles are displayed as diamonds.  Zeros are displayed as")
      disp("boxes.  The simplest form of the rlocus command is as follows:")
      cmd = "rlocus(sys1);";
      run_cmd
      disp("\nrlocus automatically selects the minimum and maximum gains based")
      disp("on the real-axis locus breakpoints.  The plot limits are chosen")
      disp("to be no more than 10 times the maximum magnitude of the open")
      disp("loop poles/zeros.");
      prompt

      clc
      disp("Example #2, Consider the following set of poles and zeros:");
      cmd = "sys2 = zp([],[0, -20, -2, -0.1],5);";
      disp(cmd);
      eval(cmd);
      cmd = "sysout(sys2);";
      disp(cmd);
      eval(cmd);
      disp("\nThe rlocus command for the zp form is the same as the tf form:")
      cmd = "rlocus(sys2);";
      run_cmd;
      disp("\nThe internal representation of the system is not important;");
      disp("rlocus automatically sorts it out internally.");
      prompt;

      clc
      disp("Example #3, Consider the following state space system:\n");
      cmd = "sys3=ss([0, 1; -10, -11], [0; 1], [0, -2], 0);";
      disp(cmd);
      eval(cmd);
      cmd = "sysout(sys3);";
      disp(cmd);
      eval(cmd);
      disp("\nPole-zero form can be obtained as follows:");
      cmd = "sysout(sys3,""zp"");";
      disp(cmd);
      eval(cmd);
      disp("\nOnce again, the rlocus command is the same:");
      cmd = "rlocus(sys3);";
      run_cmd;

      disp("\nNo matter what form the system is in, the rlocus command works the");
      disp("the same.");
      prompt;

      closeplot
      clc

    elseif (k == 4)
      clc
      help rlocus
      prompt

      clc
      disp("Display root locus of a discrete SISO system (rlocus)\n")
      disp("First we must define a sampling time, as follows:\n");
      cmd = "Tsam = 1;";
      run_cmd;
      disp("Example #1, Consider the following discrete transfer function:");
      cmd = "sys1 = tf([1.05, -0.09048],[1, -2, 1],Tsam);";
      disp(cmd);
      eval(cmd);
      cmd ="sysout(sys1);";
      disp(cmd);
      eval(cmd);
      disp("\nPole-zero form can be obtained as follows:");
      cmd = "sysout(sys1,""zp"");";
      disp(cmd);
      eval(cmd);
      disp("\nWhen using rlocus, inital system poles are displayed as X's.")
      disp("Moving poles are displayed as diamonds.  Zeros are displayed as")
      disp("boxes.  The simplest form of the rlocus command is as follows:")
      cmd = "rlocus(sys1);";
      run_cmd
      disp("\nrlocus automatically selects the minimum and maximum gains based")
      disp("on the real-axis locus breakpoints.  The plot limits are chosen")
      disp("to be no more than 10 times the maximum magnitude of the open")
      disp("loop poles/zeros.");
      prompt

      clc
      disp("Example #2, Consider the following set of discrete poles and zeros:");
      cmd = "sys2 = zp(-0.717, [1, -0.368], 3.68, Tsam);";
      disp(cmd);
      eval(cmd);
      cmd = "sysout(sys2);";
      disp(cmd);
      eval(cmd);
      disp("\nThe rlocus command for the zp form is the same as the tf form:")
      cmd = "rlocus(sys2);";
      run_cmd;
      disp("\nThe internal representation of the system is not important;");
      disp("rlocus automatically sorts it out internally.  Also, it does not");
      disp("matter if the system is continuous or discrete.  rlocus also sorts");
      disp("this out automatically");
      prompt;

      clc
      disp("Example #3, Consider the following discrete state space system:\n");
      cmd = "sys3=ss([1, 0.0952; 0, 0.905], [0.00484; 0.0952], [1, 0], 0, Tsam);";
      disp(cmd);
      eval(cmd);
      cmd = "sysout(sys3);";
      disp(cmd);
      eval(cmd);
      disp("\nPole-zero form can be obtained as follows:");
      cmd = "sysout(sys3,""zp"");";
      disp(cmd);
      eval(cmd);
      disp("\nOnce again, the rlocus command is the same:");
      cmd = "rlocus(sys3);";
      run_cmd;

      disp("\nNo matter what form the system is in, the rlocus command works the");
      disp("the same.");

      prompt;

      closeplot
      clc

    elseif (k == 5)
      return
    endif
  endwhile
endfunction
