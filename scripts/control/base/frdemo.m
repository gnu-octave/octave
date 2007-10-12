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

## -*- texinfo -*-
## @deftypefn {Function File} {} frdemo ()
## Octave Control Toolbox demo: Frequency Response demo.
## @end deftypefn

## Author: David Clem
## Created: August 15, 1994
## a s hodel: updated to match new order of ss2zp outputs
## J Ingram:  updated for system data structure format August 1996

function frdemo ()

  disp("")
  clc
  j = 0;
  while (j != 4)
    disp("");
    j = menu("Octave Controls Systems Toolbox Frequency Response Demo",
             "Bode analysis (bode)",
             "Nyquist analysis (nyquist)",
             "Nichols analysis (nichols)",
             "Return to main demo menu");

    if (j == 1)
      k1 = 0;
      while (k1 != 4)
        disp("\n");
        clc

        k1 = menu("Bode analysis (bode)",
                  "Continuous system bode analysis",
                  "Discrete system bode analysis",
                  "Bode command description",
                  "Return to frdemo menu");

        if( k1 == 1 )
          disp(" ")
          clc
          disp("\nContinuous system bode analysis\n");
          disp("Example #1:")
          disp("\nConsider the system sys1=");
          sys1=tf([1, 1], [1, 0, -1]);
          sysout(sys1);
          disp("\nPole-zero form can be obtained as follows:")
          cmd = "sysout(sys1,""zp"");";
          run_cmd;
          disp("The systems bode plot is obtained as follows:");
          cmd = "bode(sys1);";
          run_cmd;
          disp("\nNotice that bode automatically labels the plots according to")
          disp("the selected input/output combinations.")
          disp(" ")
          disp("If the frequency range is not specified, bode automatically")
          disp("selects a frequency range based on the natural frequencies of")
          disp("of all poles away from s=0 (or z=1 in discrete time).  Bode")
          disp("then checks to make sure that the phase plot is sufficiently")
          disp("smooth that relevant plot behavior is captured.")
          disp("")
          disp("Bode exits with an error if the system is mixed (both continuous")
          disp("and discrete; see is_digital for conditions)")
          prompt
          disp("\nIf the plot magnitude, phase and frequency data is desired, the");
          disp("user can enter the following command:");
          disp("\n[Mag,Phase,w] = bode(sys);");
          disp("\nThis will return three vectors containing the magnitude,");
          disp("phase and frequency.\n");
          prompt;

          disp("")
          clc
          disp("Example #2, sys2=")
          cmd = "sys2=zp(1, [-1, -5], 10);";
          eval(cmd);
          cmd = "sysout(sys2);";
          eval(cmd);
          disp("\nThe bode plot command is identical to the tf form:")
          cmd = "bode(sys2);";
          run_cmd;
          disp("\nThe internal representation of the system is not important;")
          disp("bode automatically sorts it out internally.")
          prompt;

          disp("")
          clc
          disp("Example #3, Consider the following state space system sys3=:\n");
          cmd = "sys3=ss([0, 1; -1000, -1001], [0; 1], [0, -891], 1);";
          eval(cmd);
          cmd = "sysout(sys3);";
          eval(cmd);
          disp("\nOnce again, the bode plot command is the same:");
          cmd = "bode(sys3);";
          run_cmd;
          disp("\nSuppose the user is interested in the response of the system");
          disp("defined over the input frequency range of 1 - 1000 rad/s.\n");
          disp("First, a frequency vector is required.  It can be created");
          disp("with the command:\n");
          cmd = "wrange = logspace(log10(1),log10(1000),100);";
          disp(cmd);
          eval(cmd);
          disp("\nThis creates a logarithmically scaled frequency vector with");
          disp("100 values between 1 and 1000 rad/s\n");
          disp("Then, the bode command includes wrange in the input arguments");
          disp("like this:");
          cmd = "bode(sys3,wrange);";
          run_cmd;
          prompt;

          disp("")
          clc
          disp("\nExample #4, The state-space system from example 3 will be");
          disp("grouped with the system from example 2 to form a MIMO system");
          disp("The commands to do this grouping are as follows (changing signal");
          disp("names for clarity):");
          cmd = "sys2 = syssetsignals(sys2,\"out\",\"y_sys2\");";
          disp(cmd);  eval(cmd);
          cmd = "sys2 = syssetsignals(sys2,\"in\",\"u_sys2\");";
          disp(cmd);  eval(cmd);
          cmd = "nn = sysdimensions(sys2);";
          disp(cmd);  eval(cmd);
          cmd = "[nn,nz] = sysdimensions(sys2);";
          disp(cmd);  eval(cmd);
          cmd = "sys2 = syssetsignals(sys2,\"st\",__sysdefioname__(nn+nz,\"x_sys2\"));";
          disp(cmd);  eval(cmd);
          cmd = "sys_mimo = sysgroup(sys2,sys3);";
          disp(cmd); eval(cmd);
          disp("The resulting state-space system (after changing signal names");
          disp("in sys2) is");
          cmd = "sysout(sys_mimo)";
          eval(cmd);
          disp("\nNotice that there are now 2 inputs and 2 outputs, and that it did");
          disp("not matter what form the two systems were in when they were grouped.");
          disp(["\nTo view the system's bode plots, execute the",
                " following command:\n"])
          cmd = "bode(sys_mimo);";
          run_cmd;
          prompt
          disp("\nTo view the bode plots for selected  channels, the command form changes:")
          cmd = "wrange = [];";
          disp(cmd)
          eval(cmd);
          cmd = "out = 1;";
          disp(cmd)
          eval(cmd);
          cmd = "in = 1;";
          disp(cmd)
          eval(cmd);
          cmd = "bode(sys_mimo,wrange,out,in);";
          run_cmd;
          disp("\nNotice that this bode plot is the same as the plot from example 2.");
          prompt
          closeplot

        elseif( k1 == 2 )
          disp("")
          clc
          disp("\nDiscrete system bode analysis\n");
          disp("Display bode plots of a discrete SISO system (dbode)\n")
          disp("Example #1, Consider the following discrete transfer");
          disp(" function:\n");
          cmd = "sys1 = tf([0.00100502, -0.00099502], [1, -2, 1], 0.001);";
          disp(cmd);
          eval(cmd);
          cmd = "sysout(sys1)";
          disp(cmd);
          eval(cmd);
          disp("\nTo examine open loop zeros and poles of the system,");
          disp("use the command:\n")
          cmd = "sysout(sys1,""zp"");";
          run_cmd;
          disp("\nTo view the system's bode plots, execute the following");
          disp("command:\n")
          cmd = "bode(sys1);";
          run_cmd;
          disp("\nNotice (1) the plot label uses exp(jwT) for its title axis. This")
          disp("           allows the user to determine what kind of system was")
          disp("           used to generate the bode plot");
          disp("       (2) the system poles are both at z=1, (break frequency at")
          disp("           jwT = 0); pure integrator poles like this are discarded")
          disp("           by Octave when computing the plot frequency range.")

          disp("\nIf magnitude, phase, and frequency data are also desired,");
          disp(" perform the following command instead:\n");
          disp("[M,P,w]=dbode(num,den,T,wrange).\n Where:");
          disp("M => Bode magnitude response data");
          disp("P => Bode phase response data");
          disp("w => frequencies that M and P were evaluated at");
          disp("sys1 => system data structure")
          disp("T => sample period")
          disp("wrange => optional vector of frequencies")
          disp("          if wrange is entered in the argument list, the");
          disp("          system will be evaluated at these specific");
          disp("          frequencies\n");

          prompt
          disp("")
          clc
          disp("Example #2, Consider the following set of discrete poles and");
          disp("zeros:\n")
          cmd = "sys2 = zp([0.99258;0.99745],[0.99961;0.99242],1,0.001);";
          disp(cmd);
          eval(cmd);
          cmd = "sysout(sys2)";
          disp(cmd);
          eval(cmd);
          disp("\nTo view the system's bode plots, execute the following");
          disp("command:\n")
          cmd = "bode(sys2);";
          run_cmd;
          disp("Notice that the bode command is the same in both of the previous");
          disp("examples.  The bode command is also the same for the continuous case.");
          disp("The function, dbode, is no longer used.");

          prompt
          disp("")
          clc
          disp("\nExample #3, Now consider the following state space system:\n");
          cmd = "sys3 = ss([.857, .0011; 0, .99930],[1;1],[-.6318, .0057096],5.2, .001);";
          disp(cmd);
          eval(cmd);
          cmd = "sysout(sys3);";
          disp(cmd);
          eval(cmd);
          disp("\nTo view the system's bode plots, execute the following command:\n")
          cmd = "bode(sys3);";
          run_cmd;
          disp("\nAgain, notice that the bode command is the same regardless of the form");
          disp("of the system.");
          disp("\nSuppose the user is interested in the response of the system");
          disp("defined over the input frequency range of 1 - 1000 rad/s.\n");
          disp("First, a frequency vector is required.  It can be created");
          disp("with the command:\n");
          cmd = "wrange = logspace(log10(1),log10(1000),100);";
          disp(cmd);
          eval(cmd);
          disp("\nThis creates a logrithmetically scaled frequency vector with");
          disp("100 values between 1 and 1000 rad/s\n");
          disp("Then, the bode command includes wrange in the input arguments");
          disp("like this:");
          cmd = "bode(sys3,wrange);";
          run_cmd;
          prompt;

          disp("")
          clc
          disp("\nExample #4, We will now examine a MIMO state-space system.  Systems");
          disp("two and three will be grouped.");
          cmd = "[nn,nz] = sysdimensions(sys2);";
          disp(cmd); eval(cmd);
          cmd = "sys2 = syssetsignals(sys2,\"out\",\"y_sys2\");";
          disp(cmd); eval(cmd);
          cmd = "sys2 = syssetsignals(sys2,\"in\",\"u_sys2\");";
          disp(cmd); eval(cmd);
          cmd = "sys2 = syssetsignals(sys2,\"st\",__sysdefioname__(nn+nz,\"x_sys2\"));";
          disp(cmd); eval(cmd);
          cmd = "sys_mimo = sysgroup(sys2,sys3);";
          disp(cmd); eval(cmd);
          cmd = "sysout(sys_mimo);";
          disp(cmd);
          eval(cmd);
          disp("\nTo view the system's bode plots, execute the following command:\n")
          cmd = "bode(sys_mimo);";
          run_cmd;
          prompt

          disp("\nThe bode plot of a single channel is viewed as follows:")
          cmd = "wrange = [];";
          disp(cmd)
          eval(cmd);
          cmd = "out = 1;";
          disp(cmd)
          eval(cmd);
          cmd = "in = 1;";
          disp(cmd)
          eval(cmd);
          cmd = "bode(sys_mimo,wrange,out,in);";
          run_cmd;
          disp("\nNotice that this bode plot is the same as the plot from example 2.");
          prompt
          closeplot

        elseif( k1 == 3 )
          help bode
          prompt
        endif
      endwhile
    elseif (j == 2)
      k2 = 0;
      disp("");
      while (k2 != 4)
        disp("\n");
        help nyquist
        prompt;
        disp("")
        clc;

        k2 = menu("Nyquist analysis (Nyquist)",
                  "Continuous system nyquist analysis",
                  "Discrete system nyquist analysis",
                  "Mixed system nyquist analysis",
                  "Return to frdemo menu");

        if( k2 == 1 )
          disp("")
          clc
          disp("\nContinuous system nyquist analysis\n");
          disp("Display Nyquist plots of a SISO system (nyquist)\n")
          disp("Example #1, Consider the following transfer function:\n")
          cmd = "sys1 = tf(1, [1, 0.8, 1]);";
          disp(cmd);
          eval(cmd);
          disp("To examine the transfer function, use the command:");
          cmd = "sysout(sys1);";
          disp(cmd);
          eval(cmd);
          disp("\nTo examine the open loop zeros and poles, use the command:");
          cmd = "sysout(sys1,""zp"");";
          run_cmd;
          disp("\nTo view the system""s nyquist plot, execute the following");
          disp("command:\n")
          cmd = "nyquist(sys1);";
          run_cmd;
          disp("\nIf the real and imaginary parts of the response are desired,");
          disp("use the following command:");
          disp("command: [R,I,w]=nyquist(sys1);\n");
          disp("If the user desires to evaluate the response in a certain");
          disp("frequency range, he may do so by entering the following:");
          disp("command: [M,P,w]=nyquist(num,den,wrange).\n")
          disp("wrange is a vector of frequencies that spans the desired");
          disp("viewing range.\n");
          disp("This will be illustrated in the third nyquist example.\n")
          disp("Variable Description:\n")
          disp("R => real part of response")
          disp("I => imaginary part of response")
          disp("w => frequencies that the transfer function was evaluated at")
          disp("sys1 => system data structure")
          disp("wrange => optional vector of frequencies")
          disp("          if wrange is entered in the argument list, the");
          disp("          system will be evaluated at these specific");
          disp("          frequencies\n")
          prompt

          disp("")
          clc
          disp("Example #2, Consider the following set of poles and zeros:\n")
          cmd = "sys2 = zp([-1;-4],[-2+1.4142i;-2-1.4142i],1);";
          disp(cmd);
          eval(cmd);
          disp("\nTo examine the poles and zeros, use the command:");
          cmd = "sysout(sys2)";
          disp(cmd);
          eval(cmd);
          disp("\nTo view the system""s nyquist plot, execute the following");
          disp("command:\n")
          cmd = "nyquist(sys2);";
          run_cmd;
          prompt

          disp("")
          clc
          disp("\nExample #3, Consider the following state space system:\n")
          cmd = "sys3 = ss([0, 1, 0, 0; 0, 0, 1, 0; 0, 0, 0, 1; 0, 0, -20, -12],[0;0;0;1],[50, 100, 0, 0],0);";
          disp(cmd);
          eval(cmd);
          disp("\nTo examine the state-space system, use the command:");
          cmd = "sysout(sys3)";
          disp(cmd);
          eval(cmd);
          disp("\nTo examine the poles and zeros, use the command:");
          cmd = "sysout(sys3,""zp"")";
          run_cmd;
          disp("\nTo view the system""s nyquist plot, execute the following");
          disp("commands:\n")
          cmd = "nyquist(sys3);";
          run_cmd;
          prompt

          disp("Example #3 (continued), If the user wishes to evaluate the");
          disp("system response over a desired frequency range, he must first");
          disp("create a frequency vector.\n")
          disp("For example, suppose the user is interested in the response");
          disp("of the system defined above over input frequency range of");
          disp("3 - 100 rad/s.\n")
          disp("A frequency vector can be created using the command:\n");
          cmd = "wrange = logspace(log10(3),log10(100),100);";
          disp(cmd);
          eval(cmd);
          disp("\nNyquist can be run again using the frequency vector as");
          disp("follows:\n")
          cmd = "nyquist(sys3,wrange);";
          run_cmd;
          prompt

          disp("")
          clc
          disp("Example #4,  Nyquist can be used for MIMO systems if the system has");
          disp("an equal number of inputs and outputs.  Otherwise, nyquist returns");
          disp("an error.  To examine a MIMO system, systems 2 and 3 will be grouped");
          cmd = "[nn,nz] = sysdimensions(sys2);";
          disp(cmd); eval(cmd);
          cmd = "sys2 = syssetsignals(sys2,\"out\",\"y_sys2\");";
          disp(cmd); eval(cmd);
          cmd = "sys2 = syssetsignals(sys2,\"in\",\"u_sys2\");";
          disp(cmd); eval(cmd);
          cmd = "sys2 = syssetsignals(sys2,\"st\",__sysdefioname__(nn+nz,\"x_sys2\"));";
          disp(cmd); eval(cmd);
          cmd = "sys_mimo = sysgroup(sys2,sys3);";
          disp(cmd); eval(cmd);
          cmd = "sysout(sys_mimo);";
          disp(cmd);
          eval(cmd);
          disp("\nTo view the system's nyquist plot, execute the following command:\n")
          cmd = "nyquist(sys_mimo);";
          run_cmd;
          prompt
          disp("\nTo view the nyquist plots for selected  channels, the command form changes:")
          cmd = "nyquist(sys_mimo,[],1,1);";
          run_cmd;
          disp("\nNotice that this bode plot is the same as the plot from example 2.");
          prompt
          closeplot



        elseif( k2 == 2 )
          disp("")
          clc
          disp("\nDiscrete system nyquist analysis\n");
          disp("Display Nyquist plots of a discrete SISO system (nyquist)\n")
          disp("We will first define a sampling time, T");
          cmd = "T = 0.01;";
          disp(cmd);
          eval(cmd);
          disp("\nExample #1, Consider the following transfer function:\n")
          cmd = "sys1 = tf([2, -3.4, 1.5],[1, -1.6, 0.8],T);";
          disp(cmd);
          eval(cmd);
          disp("To examine the transfer function, use the command:");
          cmd = "sysout(sys1);";
          disp(cmd);
          eval(cmd);
          disp("\nTo examine the open loop zeros and poles, use the command:");
          cmd = "sysout(sys1,""zp"")";
          disp(cmd);
          eval(cmd);
          disp("\nTo view the system""s nyquist plot, execute the following");
          disp("command:")
          cmd = "nyquist(sys1);";
          run_cmd;
          disp("To change the range used for the frequency, a frequency");
          disp("is needed.  Suppose the user would like to examine the");
          disp("nyquist plot in the frequency range of 0.01 - 31.6 rad/s.");
          disp("\nThe frequency vector needed to do this is created with the");
          disp("command:");
          cmd = "wrange = logspace(-2,1.5,200);";
          disp(cmd);
          eval(cmd);
          disp("\nNyquist can be run again with this frequency vector");
          cmd = "nyquist(sys1,wrange);";
          run_cmd;
          disp("\nIf the real and imaginary parts of the response are desired,");
          disp("perform the following command:\n");
          disp("[R,I,w]=nyquist(sys,wrange)\n")
          disp("Variable Description:\n")
          disp("R => real part of response")
          disp("I => imaginary part of response")
          disp("w => frequencies that the transfer function was evaluated at")
          disp("sys => The system data structure");
          disp("wrange => optional vector of frequencies")
          disp("          if wrange is entered in the argument list, the");
          disp("          system will be evaluated at these specific");
          prompt

          disp("")
          clc
          disp("\nExample #2, Consider the following set of poles and zeros:\n")
          cmd = "sys2 = zp([0.98025 + 0.01397i; 0.98025 - 0.01397i],[0.96079;0.99005],1,T);";
          disp(cmd);
          eval(cmd);
          disp("\nTo examine the open loop zeros and poles, use the command:");
          cmd = "sysout(sys2)";
          disp(cmd);
          eval(cmd);
          disp("\nTo view the system's nyquist plot between the frequencies");
          disp("0.01 - 100 rad/s, execute the following commands:\n")
          cmd = "wrange = logspace(-2,2,100);";
          disp(cmd);
          eval(cmd);
          cmd = "nyquist(sys2,wrange);";
          run_cmd;
          prompt;

          disp("")
          clc
          disp("\nExample #3, Consider the following discrete state space");
          disp("system:\n");
          disp("This example will use the same system used in the third");
          disp("example in the continuous nyquist demo.  First, that system");
          disp("will have to be re-entered useing the following commands:\n");
          cmd = "sys3 = ss([0, 1, 0, 0; 0, 0, 1, 0; 0, 0, 0, 1; 0, 0, -20, -12],[0;0;0;1],[50, 100, 0, 0],0);";
          disp(cmd);
          eval(cmd);
          disp("\nTo examine the state-space system, use the command:");
          cmd = "sysout(sys3)";
          disp(cmd);
          eval(cmd);
          disp("\nTo examine the poles and zeros, use the command:");
          cmd = "sysout(sys3,""zp"")";
          disp(cmd);
          eval(cmd);
          disp("\nTo convert the system to discrete time, we need a sampling");
          disp("time which can be entered like this:");
          cmd = "T = 0.01";
          disp(cmd);
          eval(cmd);
          disp("\nNow the command, c2d, is used to convert the system from");
          disp("continuous to discrete time, with the following command");
          cmd = "dsys3 = c2d(sys3,T);";
          run_cmd;
          disp("\nTo examine the new discrete state-space system, use the");
          disp("command");
          cmd = "sysout(dsys3);";
          disp(cmd);
          eval(cmd);
          disp("\nTo examine the new discrete poles and zeros, use the command:");
          cmd = "sysout(dsys3,""zp"")";
          disp(cmd);
          eval(cmd);
          disp("\nTo view the system's nyquist plot, execute the following");
          disp("commands:\n");
          cmd = "axis ([-4, 2, -2.5, 2.5]);";
          disp(cmd); eval(cmd);
          cmd = "nyquist(dsys3);";
          run_cmd;
          disp("Notice that the asymptotes swamp out the behavior of the plot")
          disp("near the origin.  You may use interactive nyquist plots")
          disp("to \"zoom in\" on a plot as follows:")

          cmd = "atol = 1;";
          disp(cmd)
          eval(cmd)
          cmd = "nyquist(dsys3,[],[],[],atol);";
          run_cmd
          prompt


          disp("")
          clc
          disp("MIMO SYSTEM:  Nyquist cannot be used for discrete MIMO systems");
          disp("at this time.");
          ## cmd = "dsys_mimo = sysgroup(sys2,dsys3);";
          ## disp(cmd);
          ## eval(cmd);
          ## cmd = "sysout(dsys_mimo);";
          ## disp(cmd);
          ## eval(cmd);
          ## disp("\nTo view the system's nyquist plot, execute the following command:\n")
          ## cmd = "nyquist(dsys_mimo);";
          ## run_cmd;
          ## prompt
          ## disp("\nTo view the nyquist plots for selected  channels, the command form changes:")
          ## cmd = "nyquist(dsys_mimo,[],1,1);";
          ## run_cmd;
          ## disp("\nNotice that this bode plot is the same as the plot from example 2.");
          prompt
          closeplot


        elseif( k2 == 3 )
          disp("\nMixed system nyquist analysis\n");
          disp("Nyquist exits with an error if it is passed a ""mixed"" system (one")
          disp("with both continuous and discrete states).  Use c2d or d2c to")
          disp("convert the system to either pure digital or pure continuous form");
        endif
      endwhile
    elseif (j == 3)
      help nichols
      prompt
    endif
  endwhile

endfunction
