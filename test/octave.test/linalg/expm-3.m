arg = diag ([6, 6, 6], 1);
result = [1, 6, 18, 36;
          0, 1,  6, 18;
          0, 0,  1,  6;
          0, 0,  0,  1];
all (all (expm (arg) == result))
