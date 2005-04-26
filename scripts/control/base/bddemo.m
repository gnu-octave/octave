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
## @deftypefn {Function File} {} bddemo (@var{inputs})
## Octave Controls toolbox demo: Block Diagram Manipulations demo.
## @end deftypefn

## Author: David Clem
## Created: August 15, 1994
## Modified by A S Hodel Summer-Fall 1996

function bddemo ()

  sav_page = page_screen_output;
  page_screen_output = 1;

  while (1)
    clc
    k=0;
    while(k > 14 || k < 1)
      k = menu("Octave Block Diagram Manipulations Demo", ...
        "sysadd/syssub: F(s) = G(s) +/- H(s)", ...
        "sysappend: add new inputs/outputs", ...
        "syssetsignals: change names of inputs, outputs, and/or states", ...
        "sysconnect: connect specified system inputs/outputs", ...
        "syscont/sysdisc: extract the continuous (discrete) part of a system", ...
        "sysdup: duplicate specified inputs/outputs", ...
        "sysgroup: group two systems into a single system,", ...
        "sysmult: F(s) = G(s)*H(s) (series connection)", ...
        "sysprune: keep only specified inputs/outputs", ...
        "sysscale: scale inputs/outputs by specified gain matrices", ...
        "parallel: parallel connection of two systems", ...
        "buildssic: the combination of all", ...
        "Design examples:", ...
        "Return to main demo menu");
    endwhile
    if (k == 1)
      clc
      disp("sysadd: add two systems together")
      disp("syssub: subtract F = G - H")
      prompt
      help sysadd
      prompt
      help syssub
      prompt
      disp("Example #1, \n")
      cmd = "sys1 = tf([1 -1],[1 2 1]);";
      run_cmd
      cmd = "sys2 = tf([1 -1],[1 2 3]);";
      run_cmd
      disp("sys1=")
      sysout(sys1);
      prompt
      disp("sys2=")
      sysout(sys2);
      cmd = "sys_sum1 = sysadd(sys1,sys1);";
      run_cmd
      disp("This example adds sys1 to itself.")
      cmd = "sysout(sys_sum1)";
      run_cmd
      disp("Notice that the numerator is twice what it used to be.")
      prompt
      disp("Example 2:")
      cmd = "sys_sub1 = syssub(sys1,sys2);";
      run_cmd
      disp("Notice that sysadd (actually sysgroup, called by sysadd) lets you")
      disp("know that your two systems had identical names for their states,")
      disp("inputs and outputs.  The warning message is perfectly harmless,")
      disp("and is provided for user information only.")
      disp("sys_sub1=")
      sysout(sys_sub1)
      disp("Notice that, since the two transfer functions had different")
      disp("denominators, syssub converted the primary system type to ")
      disp("state space.  This is the typical behavior unless the")
      disp("the two systems are both SISO, they both have the same poles,")
      disp("and at least one of them has  tf for its primary system type.");
      prompt
    elseif (k == 2)
      disp("sysappend: add new inputs and/or outputs to a system")
      help sysappend
      prompt
      disp("Consider a double-integrator system:")
      sys = tf(1, [1, 0, 0]);
      sys=sysupdate(sys,"ss");
      sysout(sys,"ss");
      disp("We add a velocity disturbance input as follows:")
      cmd = "sys1=sysappend(sys,[1;0]);";
      run_cmd
      sysout(sys1,"ss");
      disp("Names of inputs can be included as follows:")
      cmd = "sys1=sysappend(sys,[1;0], [],[],[],\"Disturb\");";
      run_cmd
      disp("Notice that empty matrices can be listed for the D matrix if")
      disp("all entries are zeros.")
      disp(" ")
      disp("sys1 is thus:")
      sysout(sys1);
      prompt
    elseif (k == 3)
      disp("syssetsignals:")
      help syssetsignals
      disp("Example system");
      a = rand(3,3);
      b = rand(3,2);
      c = rand(2,3);
      sys = ss(a,b,c);
      sysout(sys);
      prompt
      disp("Change state names to larry, moe, and curly as follows:")
      cmd = 'sys = syssetsignals(sys,"st",{"larry","moe" , "curly"});';
      run_cmd
      disp("Indicate that output 2 is discrete-time:")
      cmd = 'sys = syssetsignals(sys,"yd",1,2);';
      run_cmd
      disp("Change output 2 name to \"Vir\"");
      cmd = "sys = syssetsignals(sys,\"out\",\"Vir\",2);";
      run_cmd
      disp("Resulting system is:")
      sysout(sys);
      prompt
    elseif (k == 4)
      help sysconnect
      prompt
      disp("********* N O T E *********")
      disp("sysconnect is demonstrated fully in the design examples (option 13)");
      prompt
    elseif (k == 5)
      disp("syscont and sysdisc: ")
      disp("Example block diagram 1:")
      disp("        ------------------     ---------------------");
      disp(" u_in ->| Discrete system |--->| Continuous system | ---> y_out");
      disp("        ------------------     ---------------------");
      sys1 = tf([1, 2],[1, 2, 1], 1,"u_in","y_disc");
      sys2 = tf([1, 0],[1, -3, -2],0,"c_in","y_out");
      sys = sysmult(sys2,sys1);
      disp("Consider the hybrid system")
      sysout(sys);
      prompt
      help syscont
      disp("The continuous part of the system can be extracted with syscont")
      cmd = "[csys,Acd,Ccd] = syscont(sys);";
      run_cmd
      disp("The resulting csys is")
      sysout(csys);
      disp("Notice that B is 0; there is no purely continuous path from the")
      disp("input to the output");
      prompt
      help sysdisc
      disp("The discrete part of the system can be extracted with sysdisc")
      cmd = "[dsys,Adc,Cdc] = sysdisc(sys)";
      run_cmd
      disp("The resulting dsys is")
      sysout(dsys);
      disp("sysdisc returns dsys=empty since sys has no discrete outputs.");
      prompt
      disp("Example block diagram 2:")
      sys1 = tf([1, 2],[1, 2, 1], 1,"u_in","y_disc");
      sys2 = tf([1, 0],[1, -3, -2],0,"c_in","y_out");
      disp("             ---------------------")
      disp(" u_in -->o-->| Discrete system   | --------> y_disc")
      disp("         ^   ---------------------    |")
      disp("         |                            | ");
      disp("         -----------------------------|---")
      disp("                                      |  |")
      disp("         ------------------------------  |")
      disp("         |                               |")
      disp("         v   ---------------------       |")
      disp(" c_in -->o-->| continuous system | --------> y_out")
      disp("             ---------------------")
      disp("repeat the above example with sys=")
      sys = sysgroup(sys1, sys2);
      sysout(sys)
      prompt
      sys = sysconnect(sys,[1, 2],[2, 1]);
      sysout(sys);
      cmd = "[csys,Acd,Bcd] = syscont(sys);";
      run_cmd
      cmd = "[dsys,Acd,Bcd] = sysdisc(sys);";
      run_cmd
      disp("csys is now")
      sysout(csys)
      disp("dsys is now")
      sysout(dsys);
      prompt
    elseif (k == 6)
      help sysdup
      prompt
      disp("********* N O T E *********")
      disp("sysdup is fully demonstrated in the design examples (option 13)")
      prompt
    elseif (k == 7)
      help sysgroup
      disp(" ")
      prompt
      disp("Example: combine two SISO systems together:")
      cmd = "sys_a=tf([1, 2],[3, 4]);";
      run_cmd
      cmd = "sys_b=tf([5, 6],[7, 8],1);";
      run_cmd
      cmd = "sys_g=sysgroup(sys_a,sys_b);";
      run_cmd
      disp("Notice that sysgroup warns you when you join a purely continuous")
      disp("system to a purely discrete system.  sysgroup also warns when")
      disp("you join two systems that have common state, input, or output names.")
      cmd = "sysout(sys_g)";
      run_cmd
      disp("Since the grouped system is a multiple-input multiple-output system,")
      disp("the output system is by default in state-space format.")
      disp(" ")
      disp("********* N O T E *********")
      disp("sysgroup is further demonstrated in the design examples (option 13)")
      prompt
    elseif (k == 8)
      help sysmult
      disp("sysmult performs a series connection of two systems.")
      disp("Example 1")
      disp(" ")
      disp("         ----------     ----------")
      disp("   u --->|  Bsys  |---->|  Asys  |---> y")
      disp("         ----------     ----------")
      disp(" ")
      Asys = tf(1,[1, 2, 1],0,"a_in","a_out");
      Bsys = tf([2, 3],[1, 3, 2],0,"b_in","b_out");
      disp("Asys=")
      sysout(Asys);
      disp("Bsys=");
      sysout(Bsys);
      cmd = "sys = sysmult(Asys,Bsys);";
      run_cmd
      disp("sys =")
      sysout(sys);
      disp("Notice that sysmult automatically transforms to state space")
      disp("internal representation.  This is to avoid numerical problems")
      disp("when multiplying polynomials");
      prompt
      disp("Example 2: same system, except that Bsys is discrete-time");
      Bsys = tf([2, 3],[1, 3, 2],1e-2,"b_in","b_out");
      sysout(Bsys);
      cmd = "sys = sysmult(Asys,Bsys);";
      run_cmd
      disp("sys =")
      sysout(sys);
      prompt
    elseif (k == 9)
      help sysprune
      prompt
      disp("********* N O T E *********")
      disp("sysprune is demonstrated in the design examples (option 13)");
      prompt
    elseif (k == 10)
      help sysscale
      disp(" ")
      prompt
      disp("********* N O T E *********")
      disp("See the design examples (option 13) for use of sysscale.")
      prompt
    elseif ( k == 11)
      help parallel
      disp("parallel operates by making a call to sysgroup and sysscale.")
      disp("Example:")
      sys1 = tf(1,[1, 1],0,"in1","out1");
      sys2 = tf(2,[1, 2],0,"in2","out2");
      disp("sys1=")
      sysout(sys1);
      disp("sys2=")
      sysout(sys2);
      cmd = "sysp = parallel(sys1,sys2);";
      run_cmd
      disp("sysp=")
      sysout(sysp);
      prompt
      disp("parallel can be used for multiple input systems as well:")

      in1 = {"u1.1","u1.2"};
      in2 = {"u2.1","u2.2"};
      out1 = {"y1.1","y1.2"};
      out2 = {"y2.1","y2.2"};

      sys1 = ss([-1, 0; 0, -2],eye(2),eye(2),[]);
      sys2 = ss([-2, 0; 0, -4],eye(2),eye(2),[]);

      sys1 = syssetsignals(sys1,"in",in1);
      sys1 = syssetsignals(sys1,"out",out1);

      sys2 = syssetsignals(sys2,"in",in2);
      sys2 = syssetsignals(sys2,"out",out2);

      disp("sys1=")
      sysout(sys1);
      disp("sys2=")
      sysout(sys2);
      cmd = "sysp = parallel(sys1,sys2);";
      run_cmd
      disp("sysp=")
      sysout(sysp);
      prompt
    elseif (k == 12)
      ## buildssic description
      disp(" ")
      disp("        ---------------------------------------")
      disp("                    b u i l d s s i c")
      disp("          (BUILD State Space InterConnections)")
      disp("        ---------------------------------------")
      disp(" ")
      disp("buildssic builds a single system from up to 8 systems.")
      disp("It's primary pupose is the forming of interconnections")
      disp("for H2/H_inf designs and the building of closed loop")
      disp("systems.")
      disp("The interconnections may be of arbitrary complexity.")
      disp("The use of buildssic is an alternative to sysgroup,")
      disp("sysadd/syssub, sysappend, sysconnect, sysdup, sysmult")
      disp("sysprune, sysscale, parallel etc.")
      disp("In contrast to these functions buildssic does not")
      disp("handle mixed continuous and discrete systems. However,")
      disp("discrete systems can be connected as long as their")
      disp("sampling times are identical. Another drawback: the")
      disp("names of input/output and state variables are clobbered.")
      disp("Of course, buildsysic is useful in combination with sysgroup,")
      disp("sysmult etc.")
      prompt
      disp("********* N O T E *********")
      disp("buildssic is demonstrated in the design examples (option 13)");
      prompt
    elseif (k == 13)
      disp("Design examples")
      disp("Packed system matrices may be connected and manipulated")
      disp("With the functions listed below:")
      disp("  sysdup: duplicate selected inputs/outputs")
      disp("  sysconnect: connect selected inputs/outputs")
      disp("  sysgroup: group two systems together")
      disp("  sysprune: prune a system to keep only selected inputs and outputs")
      disp("  sysscale:pre/post multiply a system by constant matrices")
      disp("  buildssic: connect systems with arbitrary complexity.")
      prompt
      disp("As a simple example, we will construct the system block ")
      disp("diagram shown below ")
      disp(" ")
      disp("         +          --------    --------");
      disp("  r(t) ---> (+) --->| K(s) |--->| P(s) | ----> y(t)");
      disp("            -^      --------    --------  |");
      disp("             |                            |");
      disp("             ------------------------------");
      disp(" ")
      disp("where P(s) is the plant, K(s) is the controller.")
      prompt
      disp("Simple example: P(s) is a first order lag, K(s) is a PI ")
      disp("controller")
      nump = 1;
      denp = [1, 1];
      disp("P(s)=")
      tfout(nump,denp)
      numk = [1, 1];
      denk = [1, 0];
      disp("\nK(s)=")
      tfout(numk,denk);
      prompt
      disp("We'll show three approaches.  ")
      P = tf(nump,denp,0,"plant input","plant output");
      K = tf(numk, denk,0,"controller input","controller output");

      meth = 0;
      while(meth != 5)
        disp("The first method consists of the following steps:")
        disp("   step 1: create systems P and K")
        disp("   step 2: group P and K together")
        disp("   step 3: create a summing junction")
        disp("   step 4: connect outputs to respective inputs")
        disp("   step 5: prune the desired i/o connections")
        disp("The second method is done as follows:")
        disp("   step 1: create systems P and K and a summing block S")
        disp("   step 2: connect P, K, and S in series")
        disp("   step 3: connect y to inverted summing connection")
        disp("   step 4: prune the desired i/o connections")
        disp("The third method uses buildssic:")
        disp("   step 1: GW = buildssic(...,K,P)")
        disp(" ")
        disp("Other design examples are in dgkfdemo (controldemo option 7)")
        disp(" ")
        meth = menu("Select design example method", ...
                "Method 1 ", ...
                "Method 1 (w/o algebraic loop warning)", ...
                "Method 2", ...
                "Method 3", ...
                "Exit design examples");
        if(meth == 1)
          disp(" * * * Method 1 * * *")
          disp(" ")
          disp("         +          --------    --------");
          disp("  r(t) ---> (+) --->| K(s) |--->| P(s) | ----> y(t)");
          disp("            -^      --------    --------  |");
          disp("             |                            |");
          disp("             ------------------------------");
          disp(" ")
          disp("Step 1: put plants in system format:");
          nump
          denp
          cmd =  "P = tf(nump,denp,0,""plant input"",""plant output"");";
          run_cmd
          disp("P=")
          sysout(P)
          prompt
          numk
          denk
          cmd = "K = tf(numk, denk,0,""controller input"",""controller output"");";
          run_cmd
          sysout(K)
          prompt
          disp("Step 2: group the systems together")
          cmd = "PK = sysgroup(P,K);";
          run_cmd
          disp("PK=")
          sysout(PK);
          prompt
          disp(" ")
          disp("                           y2   u1")
          disp("         +          --------    --------");
          disp("  r(t) ---> (+) --->| K(s) |--->| P(s) | ----> y(t)");
          disp("  u2        -^      --------    --------    |  y1");
          disp("          u3 |                              |");
          disp("             --------------------------------");
          disp(" ")
          disp("The controller has two inputs summed together, r(t)")
          disp("and the negative feedback of  y(t)")
          disp("Step 3a: duplicate controller input: (input 2 of PK)")
          prompt
          cmd = "PK = sysdup(PK,[],2);";
          run_cmd
          disp("PK=")
          sysout(PK);
          disp("Notice that PK now has three inputs (input 3 is a duplicate ");
          prompt("of input 2).  Press return to go on")
          disp("Step 3b: scale input 3 by -1")
          cmd = "PK = sysscale(PK,[],diag([1, 1, -1]));";
          run_cmd
          disp("PK=")
          sysout(PK);
          prompt
          disp("Step 4: connect:")
          disp("   y(t) (output 1) to the negative sum junction (input 3)")
          disp("   u(t) (output 2) to plant input (input 1)")
          disp("and prune extraneous inputs/outputs (retain input 2, output 1)")
          prompt
          out_connect = [1, 2]
          in_connect = [3, 1]
          cmd = "PK0 = sysconnect(PK,out_connect,in_connect);";
          run_cmd
          prompt
          disp("Notice that sysconnect detects the possibility of algebraic")
          disp("connections when connecting inputs.  Option 2 (Method 1 ")
          disp("without algebraic loops) shows how to avoid this warning")
          disp("by performing connections one at a time.")
          prompt
          disp("PK0=")
          sysout(PK0);
          disp("Notice that the connected inputs now have stars on their")
          disp("names.  This is how the Octave controls toolbox reminds you")
          disp("that the loop has been closed around these inputs.")
          prompt("Press return to prune extra inputs/outputs from system")
          disp("Only keep plant output (output 1) and r(t) (input 2)")
          cmd = "PK0 = sysprune(PK0,1,2);";
          run_cmd
          disp("PK0=")
          sysout(PK0);
          prompt
          disp("The resulting closed-loop transfer function is obtained as follows:")
          cmd = "[num,den] = sys2tf(PK0);";
          run_cmd
          prompt
          disp("Transfer function is now")
          tfout(num,den)
          disp("You can check this: Pk0=PK/(1+PK), as expected")
          prompt
        elseif(meth == 2)
          disp("Method 1 without algebraic loops")
          disp(" ")
          disp("                           y2   u1")
          disp("         +          --------    --------");
          disp("  r(t) ---> (+) --->| K(s) |--->| P(s) | ----> y(t)");
          disp("  u2        -^      --------    --------    |  y1");
          disp("          u3 |                              |");
          disp("             --------------------------------");
          disp(" ")
          disp("Recall that sysconnect checks for algebraic loops.  Although")
          disp("Design option 1 gets a warning message about a possible");
          disp("algebraic loop, such a loop does not exist.")
          disp("This can be seen by performing the connections one at a time");
          cmd = "PK = sysgroup(P,K);";
          run_cmd
          disp("PK=")
          sysout(PK);
          disp("Create an additial inverted input to the controller.")
          cmd = "PK = sysdup(PK,[],2);";
          run_cmd
          cmd = "PK = sysscale(PK,[],diag([1,1,-1]));";
          run_cmd
          disp("PK=")
          sysout(PK);
          disp("Connect controller to plant:")
          cmd = "PK0 = sysconnect(PK,2,1);";
          run_cmd
          disp("Plant output to negative control input")
          cmd = "PK0 = sysconnect(PK0,1,3);";
          run_cmd
          disp("Only keep plant output (output 1) and r(t) (input 2)")
          cmd = "PK0 = sysprune(PK0,1,2);";
          run_cmd
          disp("PK0=")
          sysout(PK0);
          prompt
          disp("The transfer function form of PK0 is:")
          sysout(PK0,"tf");
          prompt
        elseif(meth == 3)
          disp(" * * * Method 2 * * *")
          disp(" ")
          disp("         +          --------    --------");
          disp("  r(t) ---> (+) --->| K(s) |--->| P(s) | ----> y(t)");
          disp("            -^      --------    --------  |");
          disp("             |                            |");
          disp("             ------------------------------");
          disp(" ")
      disp("Step 1: We've already created systems P and K.  Create a sum ")
      disp("block as follows:")
      cmd = 'S = ss([],[],[],[1, -1],0,0,0,[],{"r(t)", "y(t)"},"e(t)");';
      run_cmd
      disp("(You may wish to look at help ss to see what the above does)");
      disp("S=")
      sysout(S)
      disp("notice that this is just a gain block that outputs e = r - y")
      prompt
      disp("Step 2: series connections of P, K, and S")
      cmd = "PKS = sysmult(P,sysmult(K,S));";
      run_cmd
      disp("PKS=")
      sysout(PKS)
      disp("Step 3: connect y to inverted input")
      cmd = "PKcl = sysconnect(PKS,1,2);";
      run_cmd
      disp("PKcl=")
      sysout(PKcl)
      disp("Step 4: prune desired inputs/outputs")
      cmd = "PKcl=sysprune(PKcl,1,1);";
      run_cmd
      disp("PKcl=")
      sysout(PKcl)
      prompt
        elseif(meth == 4)
          disp(" * * * Method 3 * * *")
          disp(" ")
          disp("         +          --------    --------");
          disp("  r(t) ---> (+) --->| K(s) |--->| P(s) | ----> y(t)");
          disp("            -^      --------    --------  |");
          disp("             |                            |");
          disp("             ------------------------------");
          disp(" ")
      disp("Step 1: We've already created systems P and K.")
      disp("        Let us call buildssic:")
      disp("   PKcl = buildssic([1, 2; 2, -1],[],[1],[2],P,K)")
      disp(" ")
      disp("                         ^      ^  ^   ^  ^ ^")
      disp("                         |      |  |   |  | |")
      disp("     Connection list ----+      |  |   |  | |")
      disp(" internal input list -----------+  |   |  | +-- controller")
      disp("         output list --------------+   |  |")
      disp("          input list ------------------+  +---- plant")
      disp(" ")
      disp(" Connection list: connect input 1 (P) with output 2 (K)")
      disp("                  connect input 2 (K) with neg. outp. 1 (P)")
      disp(" ")
      disp("  int. inp. list: do not append internal inputs")
      disp("                  (e.g. the internal input of K (r-y))")
      disp(" ")
      disp("     output list: the only output is 1 (P), positive")
      disp(" ")
      disp("      input list: the only input is 2 (K), positive")
      disp(" ")
      cmd = "PKcl = buildssic([1, 2; 2, -1],[],[1],[2],P,K);"
      run_cmd
      sysout(PKcl)
      prompt
      disp("The transfer function form of PKcl is:")
      sysout(PKcl,"tf");
      disp("You can check this: PKcl = PK / (1 + PK), as expected")
      prompt
      elseif(meth != 5)
        disp("invalid selection")
     endif
    endwhile

    elseif (k == 14)
      return
    endif
  endwhile
  implict_str_to_num_ok = str_sav;
  page_screen_output = sav_page;
endfunction
