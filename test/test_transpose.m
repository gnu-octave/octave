%% Automatically generated from DejaGNU files

%% test/octave.test/transpose/transpose-1.m
%!test
%! scalar = 2;
%! assert(scalar',2);

%% test/octave.test/transpose/transpose-2.m
%!test
%! range = 1:4;
%! assert(range',[1;2;3;4]);

%% test/octave.test/transpose/transpose-3.m
%!test
%! vector = [1;2;3;4];
%! assert(vector',[1,2,3,4]);

%% test/octave.test/transpose/transpose-4.m
%!test
%! matrix = [1,2;3,4];
%! assert(matrix',[1,3;2,4]);

