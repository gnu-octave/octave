# Copyright (C) 1996 Auburn University.  All Rights Reserved.
#
# This file is part of Octave. 
#
# Octave is free software; you can redistribute it and/or modify it 
# under the terms of the GNU General Public License as published by the 
# Free Software Foundation; either version 2, or (at your option) any 
# later version. 
# 
# Octave is distributed in the hope that it will be useful, but WITHOUT 
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or 
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License 
# for more details.
# 
# You should have received a copy of the GNU General Public License 
# along with Octave; see the file COPYING.  If not, write to the Free 
# Software Foundation, 59 Temple Place, Suite 330, Boston, MA 02111 USA. 
 
    cmd = "ballsys = margetsys(""disc"")";
    eval(cmd);
    
    disp("Design LQG controller");
    cmd = "sysout(ballsys)";
    run_cmd
    disp("add noise inputs to system...")
 
    disp("discrete system:")
    [nn,nz,mm,pp] = sysdimensions(ballsys);
    cmd = "ballsys = sysappend(ballsys,nz);";
    run_cmd 

    cmd = "sysout(ballsys)";
    run_cmd

    disp("Notice the two additional inputs, u_2, and u_3.  These are the ");
    disp("""entry points"" for the gaussian noise disturbance.");
    disp(" ");
    disp("We'll design the controller to use only position feedback:")

    cmd = "ballsys=sysprune(ballsys,1,[]);";
    run_cmd
    cmd = "sysout(ballsys)";
    run_cmd

    disp("Now design an LQG controller: Sigw: input noise")
    Sigw = eye(2)
    disp("Now design an LQG controller: Sigv: measurement noise")
    Sigv = eye(pp);

    disp("State and input penalties:")
    Q = eye(2)
    R = 1
    disp("Controlled input is input 1");

    cmd="Ksys = lqg(ballsys,Sigw,Sigv,Q,R,1);";
    run_cmd

    disp("sysout(Ksys);");
    sysout(Ksys);

    disp("\nGet rid of the disturbance inputs");
    cmd = "ballsys = sysprune(ballsys,1,1);"
    run_cmd;
    sysout(ballsys);
    sysout(ballsys,"zp");

    disp("\nGrouping the plant and the controller");
    cmd = "closed_loop = sysgroup(ballsys,Ksys);"
    run_cmd;
    sysout(closed_loop);

    disp("\nduplicating the plant input");
    cmd = "closed_loop = sysdup(closed_loop,[],1);"
    run_cmd;
    sysout(closed_loop);

#    disp("\nscaling the duplicated input by -1");
#    cmd = "closed_loop = sysscale(closed_loop,[],diag([1,1,1]));"
#    run_cmd;
#    sysout(closed_loop);

    disp("\nconnecting plant output to controller input and controller output");
    disp("to the duplicated plant input");
    cmd = "closed_loop = sysconnect(closed_loop,[1 2],[2 3]);"
    run_cmd;
    sysout(closed_loop);

    disp("\nkeeping only the original plant input and plant output");
    cmd = "closed_loop = sysprune(closed_loop,1,1);"
    run_cmd;
    sysout(closed_loop);

    sysout(closed_loop,"zp");

 
