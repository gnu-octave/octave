x = [.5, 1, 1.5, 2, 3, 4, 5];
v = [sqrt(pi), 1, .5*sqrt(pi), 1, 2, 6, 24];
all(abs(gamma(x) - v) < sqrt(eps)) && all(abs(lgamma(x) - log(v)) < sqrt(eps))

