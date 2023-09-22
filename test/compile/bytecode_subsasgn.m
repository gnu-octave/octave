function bytecode_subsasgn ()
  A = [1 2; 3 4];
  A(1) = 3;
  __printf_assert__ ("%d ", A(1));

  A(1,2) = 5;
  __printf_assert__ ("%d ", A(1,2));

  A(:,1) = [9;8];
  __printf_assert__ ("%d ", A(:,1));

  A(:,:) = [11 12; 13 14];
  __printf_assert__ ("%d ", A(:,1));

  B = [1:10];
  B(7:end) = [77 88 99 1010];
  __printf_assert__ ("%d ", B);
  B(4:min (5, end)) = 987;
  __printf_assert__ ("%d ", B);

  % Subassign to a undefined variable
  C(3,2) = 13;
  __printf_assert__ ("%d ", C);
  __printf_assert__ ("%s ", class (C));
  __printf_assert__ ("%d ", size (C));

  % Subassign cells
  D = {1,2,3};
  D{1} = 4;
  __printf_assert__ ("%d ", D{:});
  __printf_assert__ ("%s ", class (D));
  __printf_assert__ ("%d ", size (D));

  D{2,3} = {6,7};
  dd = D{2,3};
  __printf_assert__ ("%d ", dd{:});
  __printf_assert__ ("%d ", size (D));

  E = {1,2,3};
  E(2:3) = {4,5};
  __printf_assert__ ("%d ", E{:});

  % Use cells as a subscript
  M = [1 2 3; 3 4 5; 5 6 7];
  s = {":", [1;2]};
  __printf_assert__ ("%d ", M(s{:}));
  M(s{:}) = 7;
  __printf_assert__ ("%d ", M(s{:}));

  % Assure that sources are not modified
  x = [1 2 3];
  y = x;
  y(2) = 3;
  __printf_assert__ ("%d %d ", x, y);

  x = {1 2 3};
  y = x;
  y{2} = 3;
  __printf_assert__ ("%d %d ", x{2}, y{2});

  % Chained assigns
  a.b.c.d = 2;
  __printf_assert__ ("%d ", a.b.c.d);
  a.("a").c.d = 3;
  __printf_assert__ ("%d ", a.a.c.d);
  q.w = {{1},{2}};
  __printf_assert__ ("%d ", q.w{1}{1});
  q.w{1} = {3};
  __printf_assert__ ("%d ", q.w{1}{1});

  z.x.c = [1 2 3];
  __printf_assert__ ("%d ", z.x.c);
  z.x.c(:) = 4;
  __printf_assert__ ("%d ", z.x.c);

  x = {[1 2 3], [4 5 6]; [8 9 10], [11 12 13]};
  q.y = {{}, {}; {} {}};
  q.y{1, 2} = x;
  q.y{1, 2}{2, 1} = 3;

  __printf_assert__ ("%d ", q.y{1, 2}{2, 1});
  __printf_assert__ ("%d ", q.y{1, 2}{1, 2});

  % += etc
  A = [1 2 3 4];
  A(2) += 3;
  __printf_assert__ ("%d ", A);
  A(3) -= 4;
  __printf_assert__ ("%d ", A);

  C = struct ();
  C.A = A;
  C.A(4) *= 2;
  __printf_assert__ ("%d ", C.A);

  % Chained assignments
  AA = BB.c = 3;
  __printf_assert__ ("%d ", AA);
  CC = DD.c.d = 3;
  __printf_assert__ ("%d ", DD.c.d);
  __printf_assert__ ("%d ", CC);
  EE.a = FF.c.d = 3;
  __printf_assert__ ("%d ", EE.a);
  GG = HH.a = II.a.b = JJ.a.b.c = 3;
  __printf_assert__ ("%d ", GG);
  __printf_assert__ ("%d ", HH.a);
  __printf_assert__ ("%d ", II.a.b);
  __printf_assert__ ("%d ", JJ.a.b.c);

  % cs-list in lhs using opcode SUBASSIGN_CHAINED
  idx = {1, 2};
  M = ones (2,2);
  M(idx{:}) *= 3;
  __printf_assert__ ("%d ", M);

  % SUBASSIGN_ID need to handle magic [] constant when chained (bug #64704)
  M1 = [1 2 3];
  M2 = [4 5 6];
  M3 = [7 8 9];

  M1(1) = M2(2) = M3(3) = [];
  assert (M1 == [2 3])
  assert (M2 == [4 6])
  assert (M3 == [7 8])

  M1 = {1 2 3};
  M2 = {4 5 6};
  M3 = {7 8 9};

  M1(1) = M2(2) = M3(3) = [];
  assert (cell2mat (M1) == [2 3])
  assert (cell2mat (M2) == [4 6])
  assert (cell2mat (M3) == [7 8])

  M1 = [1 2 3; 4 5 6];
  M2 = {4 5 6; 7 8 9};
  S3.a = "zxc";

  M1(1,2) = M2{2,1} = S3.a = 123;
  assert (M1 == [1 123 3; 4 5 6])
  assert (cell2mat (M2) == [4 5 6; 123 8 9])
  assert (S3.a == 123)
end
