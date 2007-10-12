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
## @deftypefn {Function File} {} moddemo (@var{inputs})
## Octave Control toolbox demo: Model Manipulations demo.
## @end deftypefn

## Author: David Clem
## Created: August 15, 1994
## a s hodel: updated to reflect updated output order in ss2zp

function moddemo ()

  while (1)
    clc
    disp("Octave Model Manipulations Demo")
    disp("=======================================")
    disp("  1)  Perform continuous to discrete time conversion (c2d)")
    disp("  2)  Convert from state space to zero / pole form (ss2zp)")
    disp("      Convert from zero / pole to state space form (zp2ss)")
    disp("  3)  Convert from state space to transfer function form (ss2tf)")
    disp("      Convert from transfer function to state space form (tf2ss)")
    disp("  4)  Convert from transfer function to zero / pole form (tf2zp)")
    disp("      Convert from zero / pole to transfer function form (zp2tf)")
    disp("  5)  Return to main demo menu")
    disp(" ")
    k=6;
    while(k > 5 || k < 1)
      k = input("Please enter a number:");
    endwhile
    if (k == 1)
      clc
      disp("Perform continuous to discrete time conversion (c2d)\n")
      disp("Example #1, Consider the following continuous time state space system:\n")
      a=[0, 1; -25, -4]
      b=[0; 1]
      c=[1, 1]
      d=1
      prompt
      disp("\nTo convert this to a discrete time system (using a zero order hold),")
      disp("use the following commands:\n")
      cmd="sys=ss(a,b,c,d);";
      run_cmd
      cmd="dsys = c2d(sys,0.2);";
      run_cmd
      cmd="sysout(dsys);";
      run_cmd
      disp("Function check\n")
      disp("Check the poles of sys vs dsys:\n")
      cmd="[da,db]=sys2ss(dsys);";
      run_cmd
      cmd="lam = eig(a);";
      run_cmd
      disp("Discretize the continuous time eigenvalues using the matrix exponential:\n")
      disp("lambc = exp(lam*0.2)\n")
      lambc = exp(lam*0.2)
      disp("Check the eigenvalues of da\n")
      lambd = eig(da)
      disp("Calculate the difference between lambd and lambc:\n")
      cmd = "error = sort(lambd)-sort(lambc)\n";
      run_cmd
      disp("The error is on the order of roundoff noise, so we're o.k.")
      prompt
      clc
    elseif (k == 2)
      clc
      disp("Convert from state space to zero / pole form (ss2zp)\n")
      disp("Example #1, Consider the following state space system:\n")
      a=[0, 3, 1; -2, -4, 5; 5, 8, 2]
      b=[0; 5; 2.5]
      c=[6, -1.9, 2]
      d=[-20]
      prompt
      disp(" ")
      disp("\nTo find the poles and zeros of this sytstem, use the following command:\n")
      disp("\n[zer, pol] = ss2zp(a, b, c, d)\n")
      prompt
      disp("Results:\n")
      [zer, pol] = ss2zp(a, b, c, d)
      disp("Variable Description:\n")
      disp("zer, pol => zeros and poles of the state space system")
      disp("a, b, c, d => state space system\n")
      prompt
      clc
      disp("Convert from zero / pole to state space form (zp2ss)\n")
      disp("Example #1, Consider the following set of zeros and poles:\n")
      zer
      pol
      prompt
      disp("\nTo find an equivalent state space representation for this set of poles")
      disp("and zeros, use the following commands:\n")
      k=1
      disp("\n[na, nb, nc, nd] = zp2ss(zer, pol, k)\n")
      prompt
      disp("Results:\n")
      [na, nb, nc, nd] = zp2ss(zer, pol, k)
      disp("Variable Description:\n")
      disp("na, nb, nc, nd => state space system equivalent to zero / pole input")
      disp("zer, pol => zeros and poles of desired state space system")
      disp("k => gain associated with the zeros\n")
      prompt
      disp("Function check\n")
      disp("Are the eigenvalues of the origonal state space system the same as the")
      disp("eigenvalues of the newly constructed state space system ?\n")
      disp("Find the difference between the two sets of eigenvalues")
      disp("error = sort(eig(a)) - sort(eig(na))\n")
      error = sort(eig(a)) - sort(eig(na))
      prompt
      clc
    elseif (k == 3)
      clc
      disp("Convert from state space to transfer function (ss2tf)\n")
      disp("Example #1, Consider the following state space system:\n")
      a=[0, 1; -2, -3]
      b=[1; 1]
      c=[1, 9]
      d=[1]
      prompt
      disp("\nTo find an equivalent transfer function for this system, use")
      disp("the following command:\n")
      disp("[num, den] = ss2tf(a, b, c, d)\n")
      prompt
      disp("Results:\n")
      [num,den] = ss2tf(a, b, c, d)
      disp("Variable Description:\n")
      disp("num, den => numerator and denominator of transfer function that is")
      disp("            equivalent to the state space system")
      disp("a, b, c, d => state space system\n")
      prompt
      clc
      disp("Convert from transfer function to state space form (tf2ss)\n")
      disp("Example #1, Consider the following transfer function:\n")
      num
      den
      prompt
      disp("\nTo find an equivalent state space representation for this system, use")
      disp("the following command:\n")
      disp("[a, b, c, d] = tf2ss(num, den)\n")
      prompt
      disp("Results:\n")
      [a, b, c, d] = tf2ss(num, den)
      disp("Variable Description:\n")
      disp("a, b, c, d => state space system equivalent to transfer function input")
      disp("num, den => numerator and denominator of transfer function that is equivalent")
      disp("            to the state space system\n")
      prompt
      clc
    elseif (k == 4)
      clc
      disp("Convert from transfer function to zero / pole form (tf2zp)\n")
      disp("Example #1, Consider the following transfer function:\n")
      num=[1, 2, 3, 4, 5, ]
      den=[1, 2, 3, 4, 5, 6, 7]
      prompt
      disp("\nTo find the zeros and poles of this system, use the following command:\n")
      disp("[zer,pol] = tf2zp(num,den)\n")
      prompt
      disp("Results:\n")
      [zer,pol] = tf2zp(num,den)
      disp("Variable Description:\n")
      disp("zer,pol => zeros and poles of the transfer function")
      disp("num, den => numerator and denominator of transfer function\n")
      prompt
      clc
      disp("Convert from zero / pole to transfer function (zp2tf)\n")
      disp("Example #1, Consider the following set of zeros and poles:\n")
      zer
      pol
      prompt
      disp("\nTo find an equivalent transfer function representation for this set")
      disp("of poles and zeros, use the following commands:\n")
      k=1
      disp("\n[num, den] = zp2tf(zer, pol, k)\n")
      prompt
      disp("Results:\n")
      [num, den] = zp2tf(zer, pol, k)
      disp("Variable Description:\n")
      disp("[num, den] => transfer function representation of desired set of zeros")
      disp("              and poles")
      disp("a, b, c, d => state space system")
      disp("zer, pol => zeros and poles of desired state space system")
      disp("k => gain associated with the zeros\n")
      prompt
      clc
    elseif (k == 5)
      return
    endif
  endwhile
endfunction
