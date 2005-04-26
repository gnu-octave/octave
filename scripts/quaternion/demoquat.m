## Copyright (C) 1998 Auburn University.  All rights reserved.
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} demoquat ()
## Demonstrate the functions available for manipulating quaternions.
##
## Thanks to Mr. Charles Hall, Dr. Don Krupp and Dr. Larry Mullins at
## NASA's Marshall Space Flight Center for notes and instruction in
## use and conventions with quaternions.  - A. S. Hodel
## @end deftypefn

## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>
## Adapted-By: jwe

function opt = demoquat ()

  opt = 0;
  quitopt = 5;

  while (opt != quitopt)
    opt = menu ("Quaternion function demo (c) 1998 A. S. Hodel, a.s.hodel@eng.auburn.edu",
		"quaternion construction/data extraction",
		"simple quaternion functions",
		"transformation functions",
		"body-inertial frame demo",
		"Quit");

    switch(opt)

    case(1)
      printf("Quaternion construction/data extraction\n");
      help quaternion
      prompt
      cmd = "q = quaternion(1,2,3,4)";
      run_cmd
      disp("This format stores the i,j,k parts of the quaternion first;")
      disp("the real part is stored last.")
      prompt
      disp(" ")
      disp("i, j, and k are all square roots of -1; however they do not")
      disp("commute under multiplication (discussed further with the function")
      disp("qmult).  Therefore quaternions do not commute under multiplcation:")
      disp("    q1*q2 != q2*q1 (usually)")
      prompt

      disp("Quaternions as rotations: unit quaternion to represent")
      disp("rotation of 45 degrees about the vector [1 1 1]")
      cmd = "degrees = pi/180; q1 = quaternion([1 1 1],45*degrees)";
      run_cmd
      prompt
      cmd = "real_q = cos(45*degrees/2)";
      run_cmd
      printf("The real part of the quaternion q(4) is cos(theta/2).\n----\n\n");
      cmd = "imag_q = sin(45*degrees/2)*[1 1 1]/norm([1 1 1])"
      run_cmd
      disp("The imaginary part of the quaternion is sin(theta/2)*unit vector");
      disp("The constructed quaternion is a unit quaternion.");
      prompt
      disp("Can also extract both forms of the quaternion:")
      disp("Vector/angle form of 1i + 2j + 3k + 4:")
      cmd = "[vv,th] = quaternion(q)";
      run_cmd
      cmd = "vv_norm = norm(vv)";
      run_cmd
      disp("Returns the eigenaxis as a 3-d unit vector");
      disp("Check values: ")
      cmd = "th_deg = th*180/pi";
      run_cmd
      disp("")
      disp("This concludes the quaternion construction/extraction demo.");
      prompt

    case(2)
      printf("Simple quaternion functions\n");
      cmd = "help qconj";
      run_cmd
      cmd = "degrees = pi/180; q1 = quaternion([1 1 1],45*degrees)";
      run_cmd
      cmd = "q2 = qconj(q1)";
      run_cmd
      disp("The conjugate changes the sign of the complex part of the")
      printf("quaternion.\n\n");
      prompt
      printf("\n\n\nMultiplication of quaternions:\n");
      cmd = "help qmult";
      run_cmd
      cmd = "help qinv"
      run_cmd
      disp("Inverse quaternion: q*qi = qi*q = 1:")
      cmd = "q1i = qinv(q1)";
      run_cmd
      cmd = "one = qmult(q1,q1i)";
      run_cmd

      printf("Conclusion of simple quaternion functions");
      prompt

    case(3)
      printf("Transformation functions\n");
      disp("A problem with the discussion of coordinate transformations is that");
      disp("one must be clear on what is being transformed: does a rotation of");
      disp("theta degrees mean that you're rotating the VECTOR by theta degrees,");
      disp("also called the 'active convention,' or does it mean that you rotate ");
      disp("the COORDINATE FRAME by theta degrees, also called the 'passive convention,' ");
      disp("which is equivalent to rotating the VECTOR by (-theta) degrees.  The");
      disp("functions in this demo use the active convention.  I'll point out where");
      disp("this changes the code as the demo runs.");
      disp("    -- The author");
      prompt
      printf("\n\n");
      disp("Sequences of rotations:")
      printf("\n\nRotation of a vector by 90 degrees about the reference z axis\n");
      cmd = "qz = quaternion([0 0 1], pi/2);";
      disp(cmd) ; eval(cmd);
      printf("\n\nRotation of a vector by 90 degrees about the reference y axis\n");
      cmd="qy = quaternion([0 1 0], pi/2);";
      disp(cmd) ; eval(cmd);
      printf("\n\nRotation of a vector by 90 degrees about the reference x axis\n");
      cmd="qx = quaternion([1 0 0], pi/2);";
      run_cmd
      printf("\n\nSequence of three rotations: 90 degrees about x, then 90 degrees\n");
      disp("about y, then 90 degrees about z (all axes specified in the reference frame):");
      qchk = qmult(qz,qmult(qy,qx));
      cmd = "[vv,th] = quaternion(qchk), th_deg = th*180/pi";
      run_cmd
      disp("The sequence of the three rotations above is equivalent to a single rotation")
      disp("of 90 degrees about the y axis. Check:");
      cmd = "err = norm(qchk - qy)";
      run_cmd

      disp("Transformation of a quaternion by a quaternion:")
      disp("The three quaternions above were rotations specified about")
      disp("a single reference frame.  It is often convenient to specify the");
      disp("eigenaxis of a rotation in a different frame (e.g., when computing");
      disp("the transformation rotation in terms of the Euler angles yaw-pitch-roll).");
      cmd = "help qtrans";
      run_cmd
      disp("")
      disp("NOTE: If the passive convention is used, then the above");
      disp("formula changes to   v = qinv(q)*v*q  instead of ")
      disp("v = q*v*qinv(q).")
      prompt
      disp("")
      disp("Example: Vectors in Frame 2 are obtained by rotating them from ")
      disp("   from Frame 1 by 90 degrees about the x axis (quaternion qx)")
      disp("   A quaternion in Frame 2 rotates a vector by 90 degrees about")
      disp("   the Frame 2 y axis (quaternion qy).  The equivalent rotation")
      disp("   in the reference frame is:")
      cmd = "q_eq = qtrans(qy,qx); [vv,th] = quaternion(q_eq)";
      run_cmd
      disp("The rotation is equivalent to rotating about the reference z axis")
      disp("by 90 degrees (quaternion qz)")
      prompt

      disp("Transformation of a vector by a quaternion");
      cmd = "help qtransv";
      run_cmd

      disp("NOTE: the above formula changes if the passive quaternion ")
      disp("is used; the cross product term is subtracted instead of added.");
      prompt
      disp("Example: rotate the vector [1,1,1] by 90 degrees about the y axis");
      cmd = "vec_r = qtransv([1,1,1],qy)";
      run_cmd
      prompt
      disp("Equivalently, one may multiply by qtransvmat:")
      cmd = "help qtransvmat";
      run_cmd
      disp("NOTE: the passive quaternion convention would use the transpose")
      disp("(inverse) of the orthogonal matrix returned by qtransvmat.");
      prompt
      cmd = "vec_r_2 = qtransvmat(qy)*[1;1;1]; vec_err = norm(vec_r - vec_r_2)";
      run_cmd

      disp("")
      disp("The last transformation function is the derivative of a quaternion")
      disp("Given rotation rates about the reference x, y, and z axes.");
      cmd = "help qderivmat";
      run_cmd
      disp("")
      disp("Example:")
      disp("Frame is rotating about the z axis at 1 rad/s")
      cmd = "Omega = [0,0,1]; Dmat = qderivmat(Omega)";
      run_cmd
      disp("Notice that Dmat is skew symmetric, as it should be.")
      disp("expm(Dmat*t) is orthogonal, so that unit quaternions remain")
      disp("unit quaternions as the rotating frame precesses.");
      disp(" ")
      disp("This concludes the transformation demo.");
      prompt;

    case(4)
      printf("Body-inertial frame demo: Look at the source code for\n");
      printf("demoquat.m and qcoordinate_plot.m to see how it's done.\n");

      # i,j,k units
      iv = quaternion(1,0,0,0); jv = quaternion(0,1,0,0);
      kv = quaternion(0,0,1,0);

      # construct quaternion to desired view.
      degrees = pi/180; daz = 45*degrees; del = -30*degrees;
      qazimuth = quaternion([0,0,1],daz);
      qelevation = quaternion([cos(daz),sin(daz),0],del);
      qview = qmult(qelevation,qazimuth);

      # inertial frame i, j, k axes.
      iif = iv; jf = qtrans(jv,iv); kf = qtrans(kv,iv);

      # rotation steps
      th = 0:5:20; ov = ones(size(th)); myth = [th,max(th)*ov ; 0*ov,th];

      # construct yaw-pitch-roll cartoon
      for kk=1:length(myth(1,:))
	figure(kk)
	thy = myth(1,kk);
	thp = myth(2,kk);

	qyaw = quaternion([0,0,1],thy*pi/180);
	[jvy,th] = quaternion(qtrans(jf,qyaw));
	qpitch = quaternion(jvy(1:3),thp*pi/180);
	qb = qmult(qpitch, qyaw);
	qi = quaternion([1, 0, 0],180*degrees);

	printf("yaw=%8.4f, pitch=%8.4f, \n    qbi = (%8.4f)i + (%8.4e)j + (%8.4f)k + (%8.4f)\n",thy,thp, ...
	  qb(1), qb(2), qb(3), qb(4));
	[vv,th] = quaternion(qb);
	printf("      = (vector) = [%8.4f %8.4f %8.4f], th=%5.2f deg\n", ...
	  vv(1), vv(2), vv(3), th*180/pi);

	qb = qmult(qb,qi);
	title(sprintf("yaw=%5.2f deg, pitch=%5.2f deg",thy,thp))
	qcoordinate_plot(qi,qb,qview);
	# uncomment the next four lines to get eps output
	#__gnuplot_set__ terminal postscript eps 
	#eval(sprintf("__gnuplot_set__ output 'fig%d.eps'",kk));
	#replot
	#__gnuplot_set__ terminal x11
	#prompt
      endfor

    case(quitopt)
      printf ("Exiting quaternion demo\n");

    otherwise
      error ("invalid option %f", opt);

    endswitch    
  endwhile

endfunction
