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
## @deftypefn {Function File} {} DEMOcontrol
## Octave Control Systems Toolbox demo/tutorial program.  The demo
## allows the user to select among several categories of @acronym{OCST} function:
## @example
## @group
## octave:1> DEMOcontrol
## O C T A V E    C O N T R O L   S Y S T E M S   T O O L B O X
## Octave Controls System Toolbox Demo
##
##   [ 1] System representation
##   [ 2] Block diagram manipulations
##   [ 3] Frequency response functions
##   [ 4] State space analysis functions
##   [ 5] Root locus functions
##   [ 6] LQG/H2/Hinfinity functions
##   [ 7] End
## @end group
## @end example
## Command examples are interactively run for users to observe the use
## of @acronym{OCST} functions.
## @end deftypefn
## @seealso{Demo Programs: bddemo.m, frdemo.m, analdemo.m,
## moddmeo.m, rldemo.m}

## Author: David Clem
## Created: August 15, 1994

function DEMOcontrol ()

  puts ("O C T A V E    C O N T R O L   S Y S T E M S   T O O L B O X");

  while (1)

    clc ();

    k = 0;

    while (k > 8 || k < 1),
      k = menu ("Octave Controls System Toolbox Demo",
                "System representation",
                "Block diagram manipulations",
                "Frequency response functions",
                "State space analysis functions",
                "System model manipulations",
                "Root locus functions",
                "LQG/H2/Hinfinity functions",
                "End");
    endwhile

    switch (k)
      case (1) sysrepdemo ();
      case (2) bddemo ();
      case (3) frdemo ();
      case (4) analdemo ();
      case (5) moddemo ();
      case (6) rldemo ();
      case (7) dgkfdemo ();
      case (8) return;
    endswitch

  endwhile

endfunction
