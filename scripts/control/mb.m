# $Revision: 1.1 $

Ap = [0 1;1960 0];
Bp = [0;-6261];
Cp = [1 0];
Dp = 0;

Gp = ss2sys(Ap,Bp,Cp,Dp,0,2,0,[],"delta_i","delta_y");
Gp = syschnames(Gp,"st",1,"delta_x1");
Gp = syschnames(Gp,"st",2,"delta_x2");

Ak = [-20 1;-22160 -200];
Bk = [-20;-2160];
Ck = [-3.5074 -0.0319];
Dk = 0;

Gk = ss2sys(Ak,Bk,Ck,Dk,0,2,0,[],"y","i");
Gk = syschnames(Gk,"st",1,"x1");
Gk = syschnames(Gk,"st",2,"x2");

Gc = sysgroup(Gp,Gk);

Gc = sysdup(Gc,[],[1 2]);
# Gc = sysscale(Gc,[],diag([1,1,1,1]));

Gc = sysconnect(Gc,[1 2],[4 3]);
Gc = sysprune(Gc,1,[1 2]);

disp("after pruning, closed loop system is")
sysout(Gc)

# Gc = sysdup(Gc,[],2);
# Gc = sysconnect(Gc,1,3);
# Gc = sysprune(Gc,1,1);

is_stable(Gc)
eig(Gc.a)

Acl = [Gp.a, -Gp.b*Gk.c; Gk.b*Gp.c, Gk.a]
eig(Acl)
