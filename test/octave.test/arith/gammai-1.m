a = [.5 .5 .5 .5 .5];
x = [0 1 2 3 4];
v1 = sqrt(pi)*erf(x)./gamma(a);
v2 = gammai(a,x.*x);
v3 = gammainc(x.*x,a);
all (abs(v1 - v2) < sqrt(eps)) && all (abs(v2 - v3) < sqrt(eps))
