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
 
page_screen_output = 1;
opt = 0;
QUITOPT = 7;
while (opt != QUITOPT)
  opt = menu("Marsyas interface update demo:", ...
	"run Marsyas on the magnetically suspended ball example", ...
        "load continuous time marsyas example system", ...
	"load discrete-time marsyas example system", ...
	"bode plot of loaded system (MIMO)", ...
        "bode plot of loaded system (SISO)", ...
	"Design example", ...
	"Quit");

  if(opt == 1)
    cmd = "system(""marsyas mag1d.mar"")";
    run_cmd
    cmd = "system(""marplot -i"")";
    run_cmd
  elseif(opt == 2)
    cmd = "ballsys = margetsys();";
    run_cmd;
    cmd = "sysout(ballsys);"
    run_cmd
  elseif(opt == 3)
    cmd = "ballsys = margetsys(""disc"");";
    run_cmd
    cmd = "sysout(ballsys);"
    run_cmd
  elseif(opt == 4)
    cmd = "bode(ballsys);";
    run_cmd
  elseif(opt == 5)
    cmd = "bode(ballsys,[],1,1);";
    run_cmd
  elseif(opt == 6)
    if(!exist("ballsys"))
      warning("You didn't load a system yet (option 2 or 3)");
    else
      disp("Design LQG controller");
      cmd = "sysout(ballsys)";
      run_cmd
      disp("add noise inputs to system...")
      if(ballsys.n)
        disp("continuous system:")
        cmd = "ballsys1 = sysappend(ballsys,eye(ballsys.n));";
      else
        disp("discrete system:")
        cmd = "ballsys1 = sysappend(ballsys,eye(ballsys.nz));";
      endif
      run_cmd
      cmd = "sysout(ballsys1)";
      run_cmd
      disp("Notice the two additional inputs, u_2, and u_3.  These are the ");
      disp("""entry points"" for the gaussian noise disturbance.");
      disp(" ");
      disp("We'll design the controller to use only position feedback:")
      cmd = "ballsys1=sysprune(ballsys1,1,[]);";
      run_cmd
      cmd = "sysout(ballsys1)";
      run_cmd
      disp("Now design an LQG controller: Sigw: input noise")
      Sigw = eye(2)
      disp("Now design an LQG controller: Sigv: measurement noise")
      Sigv = eye(rows(ballsys1.c))
      disp("State and input penalties:")
      Q = eye(2)
      R = 1
      disp("Controlled input is input 1");
      cmd="Ksys = lqg(ballsys1,Sigw,Sigv,Q,R,1);";
      run_cmd
      disp("sysout(Ksys);");
      sysout(Ksys);
      
      disp("marsyas conversion: output in scalar form:")
      cmd = "maroutsys(Ksys, ""ball_controller"",""scalar"");";
      run_cmd
      disp("here's the output file:")
      prompt
      system("more ball_controller.mar");
      
      disp("marsyas conversion: output in state space form: (default option;")
      disp("the ""ss"" in the command below is not needed)")
      cmd = "maroutsys(Ksys, ""ball_controller_ss"",""ss"");";
      run_cmd
      disp("here's the output file:")
      prompt
      system("more ball_controller_ss.mar");
      
      disp("marsyas conversion: output in transfer function form:")
      cmd = "maroutsys(Ksys, ""ball_controller_tf"",""tf"")"
      run_cmd
      disp("here's the output file:")
      prompt
      system("more ball_controller_tf.mar");
  
    endif
  endif
endwhile
