a=[1, 1.5, 2, 3];
b=[4, 3, 2, 1];
v1=betainc(1,a,b);
v2=[1,1,1,1];
x = [.2, .4, .6, .8];
v3=betainc(x, a, b);
v4 = 1-betainc(1.-x, b, a);
all(abs(v1-v2)<sqrt(eps)) && all(abs(v3-v4)<sqrt(eps))
