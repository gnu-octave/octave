function umfpack_demo
% UMFPACK DEMO
%
% A demo of UMFPACK for OCTAVE.
%
% See also umfpack, umfpack_make, umfpack_details, umfpack_report,
% and umfpack_simple.

% UMFPACK Version 4.3 (Jan. 16, 2004), Copyright (c) 2004 by Timothy A.
% Davis.  All Rights Reserved.  Type umfpack_details for License.

%-------------------------------------------------------------------------------
% get default control parameters
%-------------------------------------------------------------------------------

control = umfpack ;
fprintf ('\nEnter the printing level for UMFPACK''s output statistics:\n') ;
fprintf ('0: none, 1: errors only, 2: statistics, 4: print some of outputs\n') ;
c = input ('5: print all output [default is 1]: ') ;
if (isempty (c))
    c = 1 ;
end
control (1) = c ;

%-------------------------------------------------------------------------------
% solve a simple system
%-------------------------------------------------------------------------------

fprintf ('\n--------------------------------------------------------------\n') ;
fprintf ('Factor and solve a small system, Ax=b, using default parameters\n') ;
if (control (1) > 1)
    fprintf ('(except for verbose printing enabled)\n') ;
end

load west0067 ;
A = Problem.A ;
n = size (A, 1) ;
b = rand (n, 1) ;

fprintf ('Solving Ax=b via UMFPACK:\n') ;
[xu, info] = umfpack (A, '\\', b, control) ;
x = xu ;

fprintf ('Solving Ax=b via OCTAVE:\n') ;
xm = A\b ;
x = xm ;

fprintf ('Difference between UMFPACK and OCTAVE solution: %g\n', ...
    norm (xu - xm, Inf)) ;

%-------------------------------------------------------------------------------
% spy the results
%-------------------------------------------------------------------------------

figure (1) ;
clf

subplot (2,3,1)
title ('The matrix A') ;
spy (A)

subplot (2,3,2)
[P1, Q1, Fr, Ch, Info] = umfpack (A, 'symbolic') ;
title ('Supernodal column elimination tree') ;
%% Disable for now !!
%% treeplot (Fr (1:end-1,2)') ;

subplot (2,3,3)
title ('A, with initial row and column order') ;
spy (P1 * A * Q1)

subplot (2,3,4)
fprintf ('\n--------------------------------------------------------------\n') ;
fprintf ('\nFactorizing [L, U, P, Q, R] = umfpack (A)\n') ;
[L, U, P, Q, R] = umfpack (A) ;
title ('A, with final row/column order') ;
spy (P*A*Q)

fprintf ('\nP * (R\\A) * Q - L*U should be zero:\n') ;
fprintf ('norm (P*(R\\A)*Q - L*U, 1) = %g (exact) %g (estimated)\n', ...
    norm (P * (R\A) * Q - L*U, 1), lu_normest (P * (R\A) * Q,  L, U)) ;

fprintf ('\nSolution to Ax=b via UMFPACK factorization:\n') ;
fprintf ('x = Q * (U \\ (L \\ (P * (R \\ b))))\n') ;
xu = Q * (U \ (L \ (P * (R \ b)))) ;
x = xu ;

fprintf ('\nUMFPACK flop count: %d\n', luflop (L, U)) ;

subplot (2,3,5)
title ('UMFPACK LU factors') ;
spy (spones (L) + spones (U))

subplot (2,3,6)
fprintf ('\nFactorizing [L, U, P] = lu (A (:, q))\n') ;
try
    q = colamd (A) ;
catch
    fprintf ('\n *** colamd not found, using colmmd instead *** \n') ;
    q = colmmd (A) ;
end
[L, U, P] = lu (A (:,q)) ;
title ('OCTAVE LU factors') ;
spy (spones (L) + spones (U))

fprintf ('\nSolution to Ax=b via OCTAVE factorization:\n') ;
fprintf ('x = U \\ (L \\ (P * b)) ; x (q) = x ;\n') ;
xm = U \ (L \ (P * b)) ;
xm (q) = xm ;

fprintf ('Difference between UMFPACK and OCTAVE solution: %g\n', ...
    norm (xu - xm, Inf)) ;

fprintf ('\nOCTAVE LU flop count: %d\n', luflop (L, U)) ;

%-------------------------------------------------------------------------------
% solve A'x=b
%-------------------------------------------------------------------------------

fprintf ('\n--------------------------------------------------------------\n') ;
fprintf ('Solve A''x=b:\n') ;

fprintf ('Solving A''x=b via UMFPACK:\n') ;
[xu, info] = umfpack (b', '/', A, control) ;
xu = xu' ;

fprintf ('Solving A''x=b via OCTAVE:\n') ;
xm = (b'/A)' ;
x = xm ;

fprintf ('Difference between UMFPACK and OCTAVE solution: %g\n', ...
    norm (xu - xm, Inf)) ;

%-------------------------------------------------------------------------------
% factor A' and then solve Ax=b using the factors of A'
%-------------------------------------------------------------------------------

fprintf ('\n--------------------------------------------------------------\n') ;
fprintf ('Compute C = A'', and compute the LU factorization of C.\n') ;
fprintf ('Factorizing A'' can sometimes be better than factorizing A itself\n');
fprintf ('(less work and memory usage).  Solve C''x=b; the solution is the\n') ;
fprintf ('same as the solution to Ax=b for the original A.\n');

C = A' ;

% factorize C (P,Q) = L*U
[L, U, P, Q, R, info] = umfpack (C, control) ;

fprintf ('\nP * (R\\C) * Q - L*U should be zero:\n') ;
fprintf ('norm (P*(R\\C)*Q - L*U, 1) = %g (exact) %g (estimated)\n', ...
    norm (P * (R\C) * Q - L*U, 1), lu_normest (P * (R\C) * Q,  L, U)) ;

fprintf ('\nSolution to Ax=b via UMFPACK, using the factors of C:\n') ;
fprintf ('x = R \\ (P'' * (L'' \\ (U'' \\ (Q'' * b)))) ;\n') ;
xu = R \ (P' * (L' \ (U' \ (Q' * b)))) ;
x = xu ;

fprintf ('Solution to Ax=b via OCTAVE:\n') ;
xm = A\b ;
x = xm ;

fprintf ('Difference between UMFPACK and OCTAVE solution: %g\n', ...
    norm (xu - xm, Inf)) ;

%-------------------------------------------------------------------------------
% solve Ax=B
%-------------------------------------------------------------------------------

fprintf ('\n--------------------------------------------------------------\n') ;
fprintf ('\nSolve AX=B, where B is n-by-10, and sparse\n') ;
B = sprandn (n, 10, 0.05) ;
XU = umfpack_solve (A, '\\', B, control) ;
XM = A\B ;

fprintf ('Difference between UMFPACK and OCTAVE solution: %g\n', ...
    norm (XU - XM, Inf)) ;

fprintf ('\n--------------------------------------------------------------\n') ;
fprintf ('\nSolve AX=B, where B is n-by-10, and sparse, using umfpack_btf\n') ;
XU = umfpack_btf (A, B, control) ;

fprintf ('Difference between UMFPACK and OCTAVE solution: %g\n', ...
    norm (XU - XM, Inf)) ;

fprintf ('\n--------------------------------------------------------------\n') ;
fprintf ('\nSolve A''X=B, where B is n-by-10, and sparse\n') ;
XU = umfpack_solve (B', '/', A, control) ;
XM = B'/A ;

fprintf ('Difference between UMFPACK and OCTAVE solution: %g\n', ...
    norm (XU - XM, Inf)) ;
