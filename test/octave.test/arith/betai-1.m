a=[1, 1.5, 2, 3];
b=[4, 3, 2, 1];
v1=betai(a,b,1);
v2=[1,1,1,1];
x = [.2, .4, .6, .8];
v3=betai(a, b, x);
v4 = 1-betai(b, a, 1.-x);
all(abs(v1-v2)<sqrt(eps)) && all(abs(v3-v4)<sqrt(eps))
