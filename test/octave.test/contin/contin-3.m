whitespace_in_literal_matrix = "standard";
x = [1,2];
a = 1;
b = 2;
y = [a... # comments here ok
b];
all (y == x)
