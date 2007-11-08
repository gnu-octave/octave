## Copyright (C) 1998, 1999, 2000, 2005, 2007
##               Auburn University.  All rights reserved.
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
## @deftypefn {Function File} {} qcoordinate_plot (@var{qf}, @var{qb}, @var{qv})
## Plot in the current figure a set of coordinate axes as viewed from 
## the orientation specified by quaternion @var{qv}.  Inertial axes are
## also plotted:
##
## @table @var
## @item qf
## Quaternion from reference (x,y,z) to inertial.
## @item qb
## Quaternion from reference to body.
## @item qv
## Quaternion from reference to view angle.
## @end table
## @end deftypefn

## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>
## Adapted-By: jwe

function qcoordinate_plot (qf, qb, qv)

  if (nargin != 3 )
    print_usage ();
  endif

  degrees = pi / 180;
  d180 = 180 * degrees;

  ## construct coordinate transformation to view frame

  cm = qtransvmat(qv);

  p1 = [-1, -1,  1];
  p2 = [-1, -1, -1];
  p3 = [ 1, -1, -1];
  p4 = [ 1, -1,  1];
  p5 = [-1,  1,  1];
  p6 = [ 1,  1,  1];
  p7 = [ 1,  1, -1];
  p8 = [-1,  1, -1];

  ## outline positive quadrant

  box1 = cm * [p4; p6; p5; p6; p7]';

  ## outline rest of the box

  box2 = cm * [p7; p8; p5; p1; p4; p3; p7; p3; p2; p1; p2; p8]';

  ## compute inertial to body rotation eigenaxis
  ## qb = qbf*qf => qbf = qb/qf
  ##
  ## need to use inverse quaternion to rotate axes

  qbf = qinv (qmult (qb, qinv (qf)));

  [eaxv, th_eig] = quaternion (qbf);

  ## draw 1/3 circle in x-y plane around a unit z axis

  th = (0:-12:-120) * degrees * sign (th_eig);
  lth = length (th);

  cpts = [0, 0, 0.1*cos(th);
          0, 0, 0.1*sin(th);
          0, 1,   1*ones(1,lth)];

  ## rotate the 1/3 circle around eigenaxis of inertial to body rotation
  ## qez = qe/qz = rotation to get from z axis to eigenaxis.
  ## This rotates the 1/3 circle from x-y plane to the plane normal to
  ## eigenaxis

  qez = qmult (qbf, qinv (quaternion (0, 0, 1, 0)));
  eig_xm = qtransvmat (qez);
  cpts = cm*eig_xm * cpts;

  ## transform inertial and body quaternions to view coordinates (rotate
  ## by azimuth, elevation)

  qfm = qtransvmat (qf);
  qbm = qtransvmat (qf);

  qf = qmult (qv, qf);
  qb = qmult (qv, qb);

  ## get coordinate axes in inertial and reference frame

  jnk = qtransvmat (qf);
  ifv = jnk(:,1);
  jfv = jnk(:,2);
  kfv = jnk(:,3);

  jnk = qtransvmat (qb);
  ibv = jnk(:,1);
  jbv = jnk(:,2);
  kbv = jnk(:,3);

  axis ([-2, 2, -2, 2], "square");

  [vv, theta] = quaternion (qb);

  xlabel (sprintf ("rotate about eigenaxis %5.2f deg", th_eig/degrees));

  plot ([ibv(1), 0], [ibv(3), 0], "-@11;x (body);",
        [0, jbv(1)], [0, jbv(3)], "-@21;y (body);",
        [0, kbv(1)], [0, kbv(3)], "-@32;z (body);",
        [ifv(1), 0], [ifv(3), 0], "-@13;x (inertial);",
        [0, jfv(1)], [0, jfv(3)], "-@23;y (inertial);",
        [0, kfv(1)], [0, kfv(3)], "-@34;z (inertial);",
        cpts(1,:), cpts(3,:), ".-6 ;eigenaxis;",
        box2(1,:), box2(3,:), "-4;;",
        box1(1,:), box1(3,:), "-5;;");

endfunction
