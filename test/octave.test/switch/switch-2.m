a = 1;
b = 2;
c = 3;

x = zeros (1, 4);

k = 1;

for i = 0:3
  switch (i)
    case a
      x(k) = a;
    case b
      x(k) = b;
    otherwise
      x(k) = c;
  endswitch
  k++;
endfor

all (x == [3, 1, 2, 3])
