x=[0,.5,1];
v=[0, .520499877813047, .842700792949715];
all(abs(erf(x)-v)<1.e-10) &&  all(abs(erf(-x)+v)<1.e-10) && all(abs(erfc(x)+v-1)<1.e-10) && all(abs(erfinv(v)-x)<1.e-10)
