x = -10:10;
y = x';
z = [y, y+10];
mean (x) == 0 && mean (y) == 0 && mean (z) == [0, 10]
