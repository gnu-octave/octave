## Copyright (C) 1996 Auburn University.  All rights reserved.
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
## @deftypefn {Function File} {} dgkfdemo ()
## Octave Controls toolbox demo: 
## @iftex
## @tex
## $ { \cal H }_2 $/$ { \cal H }_\infty $
## @end tex
## @end iftex
## @ifinfo
## H-2/H-infinity
## @end ifinfo
## options demos.
## @end deftypefn

## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>
## Created: June 1995

function dgkfdemo ()

  save_val = page_screen_output ();
  page_screen_output  (0);
  while (1)
    clc
    sel = 0;
    while (sel > 10 || sel < 1)
      sel = menu ("Octave H2/Hinfinity options demo",
                  "LQ regulator",
                  "LG state estimator",
                  "LQG optimal control design",
                  "H2 gain of a system",
                  "H2 optimal controller of a system",
                  "Hinf gain of a system",
                  "Hinf optimal controller of a SISO system",
                  "Hinf optimal controller of a MIMO system",
                  "Discrete-time Hinf optimal control by bilinear transform",
                  "Return to main demo menu");
    endwhile
    if (sel == 1)
      disp("Linear/Quadratic regulator design:")
      disp("Compute optimal state feedback via the lqr command...")
      help lqr
      disp(" ")
      disp("Example:")
      A = [0, 1; -2, -1]
      B = [0; 1]
      Q = [1, 0; 0, 0]
      R = 1
      disp("Q = state penalty matrix; R = input penalty matrix")
      prompt
      disp("Compute state feedback gain k, ARE solution P, and closed-loop")
      disp("poles as follows:");
      cmd = "[k, p, e] = lqr(A,B,Q,R)";
      run_cmd
      prompt
      disp("A similar approach can be used for LTI discrete-time systems")
      disp("by using the dlqr command in place of lqr (see LQG example).")
    elseif (sel == 2)
      disp("Linear/Gaussian estimator design:")
      disp("Compute optimal state estimator via the lqe command...")
      help lqe
      disp(" ")
      disp("Example:")
      A = [0, 1; -2, -1]
      disp("disturbance entry matrix G")
      G = eye(2)
      disp("Output measurement matrix C")
      C = [0, 1]
      SigW = [1, 0; 0, 1]
      SigV = 1
      disp("SigW = input disturbance intensity matrix;")
      disp("SigV = measurement noise intensity matrix")
      prompt
      disp("Compute estimator feedback gain k, ARE solution P, and estimator")
      disp("poles via the command: ")
      cmd = "[k, p, e] = lqe(A,G,C,SigW,SigV)";
      run_cmd
      disp("A similar approach can be used for LTI discrete-time systems")
      disp("by using the dlqe command in place of lqe (see LQG example).")
    elseif (sel == 3)
      disp("LQG optimal controller of a system:")
      disp("Input accepted as either A,B,C matrices or in system data structure form")
      disp("in both discrete and continuous time.")
      disp("Example 1: continuous time design:")
      prompt
      help lqg
      disp("Example system")
      A = [0, 1; .5, .5];
      B = [0; 2];
      G = eye(2)
      C = [1, 1];
      sys = ss(A, [B, G], C);
      sys = syssetsignals(sys,"in", ...
                       ["control input"; "disturbance 1"; "disturbance 2"]);
      sysout(sys)
      prompt
      disp("Filtering/estimator parameters:")
      SigW = eye(2)
      SigV = 1
      prompt
      disp("State space (LQR) parameters Q and R are:")
      Q = eye(2)
      R = 1
      cmd = "[K,Q1,P1,Ee,Er] = lqg(sys,SigW,SigV,Q,R,1);";
      run_cmd
      disp("Check: closed loop system A-matrix is")
      disp(" [A,      B*Cc]")
      disp(" [Bc*C,   Ac  ]")
      cmd = "[Ac, Bc, Cc] = sys2ss(K);";
      run_cmd
      cmd = "Acl = [A, B*Cc; Bc*C, Ac]";
      run_cmd
      disp("Check: poles of Acl:")
      Acl_poles = sortcom(eig(Acl))
      disp("Predicted poles from design = union(Er,Ee)")
      cmd = "pred_poles = sortcom([Er; Ee])";
      run_cmd
      disp("Example 2: discrete-time example")
      cmd1 = "Dsys = ss(A, [G, B], C, [0, 0, 0], 1);";
      cmd2 = "[K,Q1,P1,Ee,Er] = lqg(Dsys,SigW, SigV,Q,R);";
      disp("Run commands:")
      cmd = cmd1;
      run_cmd
      cmd = cmd2;
      run_cmd
      prompt
      disp("Check: closed loop system A-matrix is")
      disp(" [A,      B*Cc]")
      disp(" [Bc*C,   Ac  ]")
      [Ac,Bc,Cc] = sys2ss(K);
      Acl = [A, B*Cc; Bc*C, Ac]
      prompt
      disp("Check: poles of Acl:")
      Acl_poles = sortcom(eig(Acl))
      disp("Predicted poles from design = union(Er,Ee)")
      pred_poles = sortcom([Er;Ee])
    elseif (sel == 4)
      disp("H2 gain of a system: (Energy in impulse response)")
      disp("Example 1: Stable plant:")
      cmd = "A = [0, 1; -2, -1]; B = [0; 1]; C = [1, 0]; sys_poles = eig(A)";
      run_cmd
      disp("Put into Packed system form:")
      cmd = "Asys = ss(A,B,C);";
      run_cmd
      disp("Evaluate system 2-norm (impulse response energy):");
      cmd = "AsysH2 = h2norm(Asys)";
      run_cmd
      disp("Compare with a plot of the system impulse response:")
      tt = 0:0.1:20;
      for ii=1:length(tt)
        ht(ii) = C*expm(A*tt(ii))*B;
      endfor
      plot(tt,ht)
      title("impulse response of example plant")
      prompt
      disp("Example 2: unstable plant")
      cmd = "A = [0, 1; 2, 1]";
      eval(cmd);
      cmd = "B = [0; 1]";
      eval(cmd);
      cmd = "C = [1, 0]";
      eval(cmd);
      cmd = "sys_poles = eig(A)";
      run_cmd
      prompt
      disp("Put into system data structure form:")
      cmd="Bsys = ss(A,B,C);";
      run_cmd
      disp("Evaluate 2-norm:")
      cmd = "BsysH2 = h2norm(Bsys)";
      run_cmd
      disp(" ")
      prompt("NOTICE: program returns a value without an error signal.")
      disp("")

    elseif (sel == 5)
      disp("H2 optimal controller of a system: command = h2syn:")
      prompt
      help h2syn
      prompt
      disp("Example system: double integrator with output noise and")
      disp("input disturbance:")
      disp(" ");
      disp("       -------------------->y2");
      disp("       |   _________");
      disp("u(t)-->o-->| 1/s^2 |-->o-> y1");
      disp("       ^   ---------   ^");
      disp("       |               |");
      disp("      w1(t)           w2(t)");
      disp(" ")
      disp("w enters the system through B1, u through B2")
      disp("z = [y1; y2] is obtained through C1, y=y1 through C2");
      disp(" ")
      cmd = "A = [0, 1; 0, 0];  B1 = [0, 0; 1, 0]; B2 = [0; 1];";
      disp(cmd)
      eval(cmd);
      cmd = "C1 = [1, 0; 0, 0]; C2 = [1, 0];    D11 = zeros(2);";
      disp(cmd)
      eval(cmd);
      cmd = "D12 = [0; 1];  D21 = [0, 1];  D22 = 0; D = [D11, D12; D21, D22];";
      disp(cmd)
      eval(cmd);
      disp("Design objective: compute U(s)=K(s)Y1(s) to minimize the closed")
      disp("loop impulse response from w(t) =[w1; w2] to z(t) = [y1; y2]");
      prompt
      disp("First: pack system:")
      cmd="Asys = ss(A, [B1, B2], [C1; C2], D);";
      run_cmd
      disp("Open loop multivariable Bode plot: (will take a moment)")
      cmd="bode(Asys);";
      run_cmd
      prompt("Press a key to close plot and continue");
      closeplot
      disp("Controller design command: (only need 1st two output arguments)")
      cmd="[K,gain, Kc, Kf, Pc,  Pf] = h2syn(Asys,1,1);";
      run_cmd
      disp("Controller is:")
      cmd = "sysout(K)";
      run_cmd
      disp(["returned gain value is: ",num2str(gain)]);
      disp("Check: close the loop and then compute h2norm:")
      prompt
      cmd="K_loop = sysgroup(Asys,K);";
      run_cmd
      cmd = "Kcl = sysconnect(K_loop,[3,4],[4,3]);";
      run_cmd
      cmd = "Kcl = sysprune(Kcl,[1,2],[1,2]);";
      run_cmd
      cmd="gain_Kcl = h2norm(Kcl)";
      run_cmd
      cmd="gain_err = gain_Kcl - gain";
      run_cmd
      disp("Check: multivarible bode plot:")
      cmd="bode(Kcl);";
      run_cmd
      prompt
      disp("Related functions: is_dgkf, is_controllable, is_stabilizable,")
      disp("                is_observable, is_detectable")
    elseif (sel == 6)
      disp("Hinfinity gain of a system: (max gain over all j-omega)")
      disp("Example 1: Stable plant:")
      cmd = "A = [0, 1; -2, -1]; B = [0; 1]; C = [1, 0]; sys_poles = eig(A)";
      run_cmd
      disp("Pack into system format:")
      cmd = "Asys = ss(A,B,C);";
      run_cmd
      disp("The infinity norm must be computed iteratively by")
      disp("binary search.  For this example, we select tolerance tol = 0.01, ")
      disp("min gain gmin = 1e-2, max gain gmax=1e4.")
      disp("Search quits when upper bound <= (1+tol)*lower bound.")
      cmd = "tol = 0.01; gmin = 1e-2; gmax = 1e+4;";
      run_cmd
      cmd = "[AsysHinf,gmin,gmax] = hinfnorm(Asys,tol,gmin,gmax)"
      run_cmd
      disp("Check: look at max value of magntude Bode plot of Asys:");
      [M,P,w] = bode(Asys);
      xlabel("Omega")
      ylabel("|Asys(j omega)| ")
      grid();
      semilogx(w,M);
      disp(["Max magnitude is ",num2str(max(M)), ...
        ", compared with gmin=",num2str(gmin)," and gmax=", ...
        num2str(gmax),"."])
      prompt
      disp("Example 2: unstable plant")
      cmd = "A = [0, 1; 2, 1]; B = [0; 1]; C = [1, 0]; sys_poles = eig(A)";
      run_cmd
      disp("Pack into system format:")
      cmd = "Bsys = ss(A,B,C);";
      run_cmd
      disp("Evaluate with BsysH2 = hinfnorm(Bsys,tol,gmin,gmax)")
      BsysH2 = hinfnorm(Bsys,tol,gmin,gmax)
      disp(" ")
      disp("NOTICE: program returns a value without an error signal.")
      disp("")

    elseif (sel == 7)
      disp("Hinfinity optimal controller of a system: command = hinfsyn:")
      prompt
      help hinfsyn
      prompt
      disp("Example system: double integrator with output noise and")
      disp("input disturbance:")
      A = [0, 1; 0, 0]
      B1 = [0, 0; 1, 0]
      B2 = [0; 1]
      C1 = [1, 0; 0, 0]
      C2 = [1, 0]
      D11 = zeros(2);
      D12 = [0; 1];
      D21 = [0, 1];
      D22 = 0;
      D = [D11, D12; D21, D22]
      prompt
      disp("First: pack system:")
      cmd="Asys = ss(A, [B1, B2], [C1; C2], D);";
      run_cmd
      prompt
      disp("Open loop multivariable Bode plot: (will take a moment)")
      cmd="bode(Asys);";
      run_cmd
      prompt
      disp("Controller design command: (only need 1st two output arguments)")
      gmax = 1000
      gmin = 0.1
      gtol = 0.01
      cmd="[K,gain] = hinfsyn(Asys,1,1,gmin,gmax,gtol);";
      run_cmd
      disp("Check: close the loop and then compute h2norm:")
      prompt
      cmd="K_loop = sysgroup(Asys,K);";
      run_cmd
      cmd = "Kcl = sysconnect(K_loop,[3,4],[4,3]);";
      run_cmd
      cmd = "Kcl = sysprune(Kcl,[1,2],[1,2]);";
      run_cmd
      cmd="gain_Kcl = hinfnorm(Kcl)";
      run_cmd
      cmd="gain_err = gain_Kcl - gain";
      run_cmd
      disp("Check: multivarible bode plot:")
      cmd="bode(Kcl);";
      run_cmd
      prompt
      disp("Related functions: is_dgkf, is_controllable, is_stabilizable,")
      disp("                   is_observable, is_detectable, buildssic")
    elseif (sel == 8)
      disp("Hinfinity optimal controller of MIMO system: command = hinfsyn:")
      prompt
      help hinfsyn
      prompt
      disp("Example system: Boeing 707-321 airspeed/pitch angle control")
      disp(" ")
      hinfdemo
    elseif (sel == 9)
      disp("Discrete time H-infinity control via bilinear transform");
      prompt
      dhinfdemo
    elseif (sel == 10)
      return
    endif
    prompt
  endwhile
  page_screen_output (save_val);

endfunction
