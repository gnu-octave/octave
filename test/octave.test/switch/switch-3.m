a = 1;
b = 2;
c = 3;

x = zeros (1, 4);

k = 1;

for i = 0:3
  switch (i)
    case a
      x(k) = a;
  endswitch
  k++;
endfor

all (x == [0, 1, 0, 0])
