# Copyright (C) 1996,1998 Auburn University.  All Rights Reserved
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
# Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

# I think that this m-file can be deleted
# a.s.hodel@eng.auburn.edu - 4 Dec. 1998

Ap = [0, 1;1960, 0];
Bp = [0;-6261];
Cp = [1, 0];
Dp = 0;

Gp = ss2sys(Ap,Bp,Cp,Dp,0,2,0,[],"delta_i","delta_y");
Gp = syssetsignals(Gp,"st","delta_x1",1);
Gp = syssetsignals(Gp,"st","delta_x2",2);

Ak = [-20, 1;-22160, -200];
Bk = [-20;-2160];
Ck = [-3.5074, -0.0319];
Dk = 0;

Gk = ss2sys(Ak,Bk,Ck,Dk,0,2,0,[],"y","i");
Gk = syssetsignals(Gk,"st","x1",1);
Gk = syssetsignals(Gk,"st","x2",2);

Gc = sysgroup(Gp,Gk);

Gc = sysdup(Gc,[],[1, 2]);
# Gc = sysscale(Gc,[],diag([1,1,1,1]));

Gc = sysconnect(Gc,[1, 2],[4, 3]);
Gc = sysprune(Gc,1,[1, 2]);

disp("after pruning, closed loop system is")
sysout(Gc)

# Gc = sysdup(Gc,[],2);
# Gc = sysconnect(Gc,1,3);
# Gc = sysprune(Gc,1,1);

is_stable(Gc)
Gca = sys2ss(Gc);
eig(Gca)

[Gpa,Gpb,Gpc,Gpd] = sys2ss(Gp);
[Gka,Gkb,Gkc,Gkd] = sys2ss(Gk);
Acl = [Gpa, -Gpb*Gkc; Gkb*Gpc, Gka]
eig(Acl)
