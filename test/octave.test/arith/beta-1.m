a=[1, 1.5, 2, 3];
b=[4, 3, 2, 1];
v1=beta(a,b);
v2=beta(b,a);
v3=gamma(a).*gamma(b)./gamma(a+b);
all(abs(v1-v2)<sqrt(eps)) && all(abs(v2-v3)<sqrt(eps))
