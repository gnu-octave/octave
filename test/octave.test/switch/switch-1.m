a = 1;
b = 2;
c = 3;

switch 0 case 1 x = a; case 2 x = b; otherwise x = c; endswitch
switch 1 case 1 y = a; case 2 y = b; otherwise y = c; endswitch
switch 2 case 1 z = a; case 2 z = b; otherwise z = c; endswitch
switch 3 case 1 p = a; case 2 p = b; otherwise p = c; endswitch

x == c && y == a && z == b && p == c
