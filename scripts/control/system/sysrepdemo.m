## Copyright (C) 1996, 1998 Auburn University.  All rights reserved.
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
## @deftypefn {Function File} {} sysrepdemo
## Tutorial for the use of the system data structure functions.
## @end deftypefn

## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>
## Created: June 1995
## Revised Aug 1995 for system data structure format

function sysrepdemo ()

  save_val = page_screen_output;
  page_screen_output = 1;

  disp("System representation demo:")
  num = [5, -1];
  denom = [1, -2, 6];
  a = b = c = [];
  syschoice = -1;
  ch_init = 2;
  ch_extract = ch_init+1;
  ch_update = ch_extract+1;
  ch_view = ch_update+1;
  ch_details = ch_view+1;
  ch_quit = ch_details+1;
  while(syschoice != ch_quit)
   disp(" ")
    syschoice = menu("Octave System Representation Menu", ...
      "General overview of system representation (DO THIS FIRST)", ...
      "Initialize a system (ss, tf, zp)", ...
      "Extract data from a system(sys2ss, sys2tf, sys2zp, etc.)", ...
      "Update internal representation (sysupdate)", ...
      "View the internal contents of a system (sysout)", ...
      "Details of internal representation", ...
      "Return to main menu");
    if(syschoice == 1)  # general overview
      disp("The Octave Control Systems Toolbox (OCST) was designed to")
      disp("provide a simple user interface to a powerful set of tools.")
      disp(" ")
      disp("               ----------")
      disp(" input(s) ---->| System | ---> output(s) ")
      disp("               ----------")
      disp(" ")
      disp("Like other computer-aided control system design tools, the OCST")
      disp("enables users to enter their descriptions of dynamic systems in ")
      disp("their preferred form (state space, transfer function, or ");
      disp("zero-pole format).  ");
      disp("The OCST stores system descriptions in a single variable data ");
      disp("structure that allows for continuous time, discrete-time, or mixed ");
      disp("(sampled-data) systems.  ");
      disp(" ");
      disp("This single variable description of dynamic systems greatly simplifies ");
      disp("both the code of the OCST as well as the user interface, since only")
      disp("one variable is passed per system, regardless of the  internal ")
      disp("representation used in the data structure.  As a result, the ");
      disp("likelihood of user error is greatly reduced when calling OCST")
      disp("functions.  Further, all OCST functions have been written to")
      disp("provide meaningful warning or error message to assist the user")
      disp("in correcting their programming errors while using the OCST.")
      disp("The details of the internal representation can be seen in ");
      disp(["menu option ",num2str(ch_details)]);
      disp("The data structure used in the OCST is called a \"system data structure.\"");
      disp("A system data structure is contstructed with one of:")
      disp("   fir (FIR transfer function to system)")
      disp("   ss (state space matrices to system)")
      disp("   tf (SISO transfer function to system)")
      disp("   zp (SISO zero/pole/leading coefficient to system)")
      disp(" ")
      disp(["These functions are discussed in in menu option ",num2str(ch_init)])
      disp("The data in a system may be extracted using ")
      disp("   sys2fir (FIR transfer function from system")
      disp("   sys2ss (state space matrices from system)")
      disp("   sys2tf (SISO transfer function from system)")
      disp("   sys2zp (SISO zero/pole/leading coefficient from system)")
      disp(" ")
      disp(["These functions are discussed in menu option ", ...
        num2str(ch_extract)]);
      disp("Other options discussed under this menu are updating the internal")
      disp("representation form of a system data structure with sysupdate and printing")
      disp("the description of a dynamic system to the screen with sysout.")
      disp(" ")
      disp("Once the user is familiar with these commands, the rest of the ")
      disp("OCST package will be quite easy to use.")
    elseif(syschoice == ch_init) % Initialize
      disp("Initialization of a system:");
      disp(" ");
      formopt = 0;
      while(formopt != 4)
      disp("Three data formats may be used to initialize a system:")
        formopt = menu("System data structure initialization menu", ...
                "State space form       (ss)", ...
                "Transfer function form (tf)", ...
                "zero-pole form         (zp)", ...
                "Return to System representation menu");
        if(formopt == 1)
          disp("State space representation of a system is based on the usual")
          disp("multi-variable differential equations")
          disp(" ")
          disp("  . ")
          disp("  x = A x + B u      -or -   x(k+1) = A x(k) + B u(k) ")
          disp("  y = C x + D u                y(k) = C x(k) + D u(k) ")
          disp(" ")
          disp("for matrices A, B, C, D of appropriate dimension.")
          disp(" ")
          ssopt = 0;
          ssquit = 5;
          while(ssopt < ssquit)
            ssopt = menu("State space initialization examples", ...
                "Double integrator example", ...
                "Double delay (discrete-time) example", ...
                "Summing junction (D-matrix only) example", ...
                "ss details (help ss)", ...
                "return to system initialization menu", ...
                "return to system representation main menu");
            if(ssopt == 1)
              disp("Example: construct a system representation of a")
              disp("double integrator via state-space form")
              cmd = "a = [0, 1; 0, 0];";
              run_cmd
              cmd = "b = [0; 1];";
              run_cmd
              cmd = "c = [1, 0];";
              run_cmd
              cmd = "sys = ss(a,b,c);";
              run_cmd
              disp("The state space form of the system is seen via sysout:")
              cmd = "sysout(sys)";
              run_cmd
              disp("Notice that the Octave controls  toolbox automatically")
              disp("assigns names to the states, inputs and outputs,")
              disp("and that the D matrix was filled in automatically.")
              disp("We verify that it's a double integrator via sysout:")
              cmd = "sysout(sys,""tf"")";
              run_cmd
              prompt
            elseif(ssopt == 2)
              disp("Example: discrete-time double-delay:")
              disp("This example is identical to the double-integrator,")
              disp("except that it is a discrete-time system, and so has")
              disp("a sampling interval.  We arbitrarily select T=1e-3.");
              cmd = "a = [0, 1; 0, 0];";
              run_cmd
              cmd = "b = [0; 1];";
              run_cmd
              cmd = "c = [1, 0];";
              run_cmd
              cmd = "sys=ss(a,b,c,[],1e-3);";
              run_cmd
              cmd = "sysout(sys)";
              run_cmd
              disp("Notice that the D matrix was filled in automatically.")
              disp("This is done if D is input as the empty matrix.")
              disp(" ")
              disp("Notice also that the output y_1 is labelled as a discrete")
              disp("output.  The OCST data structure keeps track of states")
              disp("and output signals that are produced by the discrete-time")
              disp("portion of a system.  Discrete states and outputs are ")
              disp("implemented as shown in the block diagram below:")
              disp(" ")
              disp(" ")
              disp("       _________   ________ x(kT)  ________________")
              disp("f(t)-->|sampler|-->| delay |----->|zero order hold| -->")
              disp("       ---------   --------        ----------------")
              disp(" ")
              disp("        ___________    _______________")
              disp("f(t)-->| sampler |-->|zero-order hold| --> y(discrete)")
              disp("        -----------    ---------------")
              disp(" ")
              disp("where f(t) is an input signal to either the output or the")
              disp(" discrete state.")
              disp(" ")
              disp("The OCST does not implement samplers on inputs to continuous")
              disp("time states (i.e., there are no samplers implicit in the B")
              disp("or D matrices unless there are corresponding discrete")
              disp("outputs or states.  The OCST provides warning messages when")
              disp("if this convention is violated.")
              prompt
            elseif(ssopt == 3)
              disp("A summing junction that computes e(t) = r(t) - y(t) may be");
              disp("constructed as follows:");
              disp("First, we set the matrix D:")
              cmd = "D = [1, -1];";
              run_cmd
              disp("ss allows the initialization of signal and state names")
              disp("(see option 4), so we initialize these as follows:")
              cmd = "inname = {\"r(t)\",\"y(t)\"};";
              run_cmd;
              cmd = "outname = \"e(t)\";";
              run_cmd
              disp("Since the system is continous time and without states,")
              disp("the ss inputs tsam, n, and nz are all zero:")
              cmd = "sys = ss([],[],[],D,0,0,0,[],inname,outname);";
              run_cmd
              disp("The resulting system is:")
              cmd = "sysout(sys)";
              run_cmd
              disp("A discrete-time summing block can be implemented by setting")
              disp("the sampling time positive:")
              cmd = "sys = ss([],[],[],D,1e-3,0,0,[],inname,outname);";
              run_cmd
              disp("The resulting system is:")
              cmd = "sysout(sys)";
              run_cmd
              prompt
            elseif(ssopt == 4)
              help ss
              disp(" ")
              disp(" ")
              disp("Notice that state-space form allows a single system to have")
              disp("both continuous and discrete-time states and to have both continuous")
              disp("and discrete-time outputs.  Since it's fairly easy to make an")
              disp("error when mixing systems of this form, the Octave controls")
              disp("toolbox attempts to print warning messages whenever something")
              disp("questionable occurs.")
            elseif(ssopt == 6)
              formopt = 4;              # return to main menu
            endif
          endwhile
        elseif(formopt == 2)
          tfopt = 0;
          while(tfopt < 5)
            tfopt = menu("Transfer function initialization menu", ...
                "Continuous time initialization" , ...
                "Discrete time initialization" , ...
                "User specified signal names" , ...
                "tf details (help tf)", ...
                "Return to system initialization menu", ...
                "Return to system representation main menu");
            if(tfopt == 1) # continuous time
              disp("A transfer function is represented by vectors of the")
              disp("coefficients of the numerator and denominator polynomials");
              disp(" ")
              disp("For example: the transfer function");
              disp(" ");
              num = [5, -1];
              denom = [1, -2, 6];
              tfout(num,denom);
              disp(" ")
              disp("is generated by the following commands:")
              cmd = "num = [5, -1]";
              run_cmd
              cmd = "denom = [1, -2, 6]";
              run_cmd
              cmd = "sys = tf(num,denom);";
              run_cmd
              disp("alternatively, the system can be generated in a single command:");
              cmd = "sys = tf([5, -1], [1, -2, 6]);";
              run_cmd
              disp("Notice the output of sys: it is an Octave data structure.")
              disp("The details of its member variables are explained under")
              disp("System Representation Menu option 5 (the details of system form)")
              disp(" ");
              disp("The data structure can be observed with the sysout command:")
              cmd = "sysout(sys)";
              run_cmd
              disp("Notice that Octave assigns names to inputs and outputs.")
              disp("The user may manually select input and output names; see option 3");
              prompt
            elseif(tfopt == 2) # discrete time
              disp("A transfer function is represented by vectors of the")
              disp("coefficients of the numerator and denominator polynomials");
              disp("Discrete-time transfer functions require ")
              disp("the additional parameter of a sampling period:")
              cmd = "sys=tf([5, -1], [1, 2, -6], 1e-3);";
              run_cmd
              cmd = "sysout(sys)";
              run_cmd
              disp("The OCST recognizes discrete-time transfer functions and")
              disp("accordingly prints them with the frequency domain variable z.");
              disp("Notice that Octave assigns names to inputs and outputs.")
              disp("The user may set input and output names; see option 3");
            elseif(tfopt == 3) # user specified names
              disp("The OCST requires all signals to have names.  The OCST assigned default");
              disp("names to the signals in the other examples.  We may initialize a transfer");
              disp("function with user-specified names as follows: Consider a simple ")
              disp("double-integrator model of aircraft roll dynamics with ")
              disp("input \"aileron angle\" and output \"theta\".  A ")
              disp("system for this model is generated by the command")
              cmd = "aircraft=tf(1, [1, 0, 0], 0,\"aileron angle\",\"theta\");";          run_cmd
              disp("The sampling  time parameter 0 indicates that the system")
              disp("is continuous time.  A positive sampling time indicates a")
              disp("discrete-time system (or sampled data system).")
              cmd = "sysout(aircraft)";
              run_cmd
              disp("Notice that the user-selected signal names are listed.")
              disp("These signal names are used in OCST plots and design functions.");
              disp("(Run the frequency response demo to see an example of the use of ");
              disp("signal names in plots.)")
              prompt
            elseif(tfopt == 4) # help
              help  tf
              prompt
            elseif(tfopt == 6) # return to main menu
              formopt = 4;
            endif
          endwhile
        elseif (formopt == 3)
          zpopt = 0;
          while(zpopt < 5)
            zpopt = menu("Zero-pole initialization menu", ...
                "Continuous time initialization" , ...
                "Discrete time initialization" , ...
                "User specified signal names" , ...
                "zp details (help zp)", ...
                "Return to system initialization menu", ...
                "Return to system representation main menu");
            if(zpopt == 1) # continuous time
              disp("A zero-pole form representation of a system includes vectors")
              disp("of the system poles and zeros and a scalar leading coefficient.");
              disp(" ")
              disp("For example: the transfer function");
              disp(" ");
              k = 5;
              num = [5, -1];
              denom = [1, -2, 6];
              zpout(num,denom,k);
              disp(" ")
              disp("is generated by the following commands:")
              cmd = "num = [5, -1]";
              run_cmd
              cmd = "denom = [1, -2, 6]";
              run_cmd
              cmd = "k = 5";
              run_cmd
              cmd = "sys = zp(num,denom,k);";
              run_cmd
              disp("alternatively, the system can be generated in a single command:");
              cmd = "sys = zp([5, -1],[1, -2, 6],5);";
              run_cmd
              disp("Notice the output of sys: it is an Octave data structure.")
              disp("The details of its member variables are explained under")
              disp("System Representation Menu option 5 (the details of system form)")
              disp(" ");
              disp("The data structure can be observed with the sysout command:")
              cmd = "sysout(sys)";
              run_cmd
              disp("Notice that Octave assigns names to inputs and outputs.")
              disp("The user may manually select input and output names; see option 3");
              prompt
            elseif(zpopt == 2) # discrete time
              disp("A zero-pole form representation of a system includes vectors")
              disp("of the system poles and zeros and a scalar leading coefficient.");
              disp(" ")
              disp("Discrete-time systems require the additional parameter of a sampling period:")
              cmd = "sys=zp([5, -1],[1, 2, -6],5,1e-3);";
              run_cmd
              cmd = "sysout(sys)";
              run_cmd
              disp("The OCST recognizes discrete-time transfer functions and")
              disp("accordingly prints them with the frequency domain variable z.");
              disp("Notice that Octave assigns names to inputs and outputs.")
              disp("The user may set input and output names; see option 3");
            elseif(zpopt == 3) # user specified names
              disp("The OCST requires all signals to have names.  The OCST assigned default");
              disp("names to the signals in the other examples.  We may initialize a transfer");
              disp("function with user-specified names as follows: Consider a simple ")
              disp("double-integrator model of aircraft roll dynamics with ")
              disp("input \"aileron angle\" and output \"theta\".  A ")
              disp("system for this model is generated by the command")
              cmd = "aircraft=zp([],[0, 0],1,0,\"aileron angle\",\"theta\");";            run_cmd
              disp("The sampling  time parameter 0 indicates that the system")
              disp("is continuous time.  A positive sampling time indicates a")
              disp("discrete-time system (or sampled data system).")
              cmd = "sysout(aircraft)";
              run_cmd
              disp("Notice that the user-selected signal names are listed.")
              disp("These signal names are used in OCST plots and design functions.");
              disp("(Run the frequency response demo to see an example of the use of ");
              disp("signal names in plots.)")
              prompt
            elseif(zpopt == 4) # help
              help  zp
              prompt
            elseif(zpopt == 6) # return to main menu
              formopt = 4;
            endif
          endwhile
        endif
      endwhile
    elseif(syschoice == ch_extract)  # extract system information
      disp("Extract information from a system data structure in a selected format:")
      disp("The actions of operations ss, tf, and zp are reversed by")
      disp("respective functions sys2ss, sys2tf, and sys2zp.  The latter two");
      disp("functions are applicable only to SISO systems.")
      formopt = 0;
      while(formopt != 8)
        formopt = menu("Extract system information", ...
                "in state space form       (sys2ss)", ...
                "in transfer function form (sys2tf)", ...
                "in zero pole form         (sys2zp)", ...
                "signal names       (sysgetsignals,syssetsignals)", ...
                "sampling time         (sysgettsam)", ...
                "signal dimensions  (sysdimensions)", ...
                "primary system type   (sysgettype)", ...
                "Return to system representation menu");
        if(formopt == 1)
          help sys2ss
        elseif(formopt == 2)
          help sys2tf
        elseif(formopt == 3)
          help sys2zp
        elseif(formopt == 4)
          help sysgetsignals
          cmd="sys=ss(rand(4),rand(4,2),rand(3,4));";
          run_cmd
          printf("Example: All signals names can be extracted by\n");
          cmd = "[Ast,Ain,Aout,Ayd] = sysgetsignals(sys)";
          run_cmd
          printf("Example: Input signal names can be extracted as\n");
          cmd = "Ain = sysgetsignals(sys,\"in\")";
          run_cmd
          printf("Example: The name of output signal 2 can be extracted as\n");
          cmd = "Aout = sysgetsignals(sys,\"out\",2)";
          run_cmd
          printf("\nNotice that Aout is returned as a cell array; the signal name\n");
          printf("itself is obtained by specifying the input parameter strflg\n");
          cmd = "Aout = sysgetsignals(sys,\"out\",2,1)";
          run_cmd
          prompt
          cmd = "help syssetsignals";
          run_cmd
          printf("Example: set input 2 name to \"motor voltage\"\n");
          cmd = "sys = syssetsignals(sys,\"in\",\"motor voltage\",2); sysout(sys)";
          run_cmd

          printf("Other syssetsignals demos are in the Block diagram demo program bddemo\n");
        elseif(formopt == 5)
          help sysgettsam
        elseif(formopt == 6)
          help sysdimensions
        elseif(formopt == 7)
          help sysgettype
        endif
        prompt
      endwhile
    elseif(syschoice== ch_update)
      disp("The OCST system data structure format will store a system in the same format")
      disp("as that with which it was initialized.  For example, consider the following:")
      cmd = "sys=zp([1, 2],[3, 4, 5],6)";
      run_cmd
      disp(" ")
      disp("Notice the internal variables in the structure include zer, pol, and k,")
      disp("the required variables for zero-pole form.  We can update the system")
      disp("to include state-space form as follows:")
      cmd = "sys = sysupdate(sys,\"ss\")";
      run_cmd
      disp(" ")
      disp("Now the sys data structure includes variables a, b, c, and d, as well")
      disp("the default state names stname.  sysupdate is usually used internally in")
      disp("the OCST, but can be used manually if desired.  A full description of")
      disp("sysupdate is as follows:")
      help sysupdate
      prompt
    elseif(syschoice == ch_view)
      disp("The sysout command can be used to view a system in any desired format.")
      disp("For example, consider the system created as follows:")
      cmd = "aircraft=zp(1,[0, 0],1,0,\"aileron angle\",\"theta\");";             run_cmd
      disp("The system may be viewed in its default format (zero-pole) as follows")
      cmd = "sysout(aircraft)";
      run_cmd
      disp(" ")
      disp("The system may be viewed in state-space or transfer function form as well:")
      cmd = "sysout(aircraft,\"ss\")";
      run_cmd
      cmd = "sysout(aircraft,\"tf\")";
      run_cmd
      disp("A complete description of sysout is below:")
      help sysout
      prompt
    elseif(syschoice == ch_details)
      packedform
    endif

  endwhile
  page_screen_output = save_val;
endfunction

