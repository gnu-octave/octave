function amd_demo
% AMD DEMO
%
% A demo of AMD for OCTAVE.
%
% --------------------------------------------------------------------------
% AMD Version 1.1 (Jan. 21, 2004), Copyright (c) 2004 by Timothy A. Davis,
% Patrick R. Amestoy, and Iain S. Duff.  See ../README for License.
% email: davis@cise.ufl.edu    CISE Department, Univ. of Florida.
% web: http://www.cise.ufl.edu/research/sparse/amd
% --------------------------------------------------------------------------
%
% See also: amd

% Get the Harwell/Boeing can_24 matrix.  This is an example matrix from the
% MATLAB-accessible UF sparse matrix collection, and can be loaded into
% MATLAB with the statment "Problem = UFget ('HB/can_24')", after obtaining
% the UFget function and its supporting routines at
% http://www.cise.ufl.edu/sparse/mat .

load can_24.mat
A = Problem.A ;
n = size (A,1) ;

figure (1)
clf
hold off
subplot (2,1,1) ;
% remove the "_" from the name before printing it in the plot title
title (sprintf ('%s', strrep (Problem.name, '_', '-'))) ;
fprintf ('Matrix name:  %s\n', Problem.name) ;
fprintf ('Matrix title: %s\n', Problem.title) ;
spy (A)

% print the details during AMD ordering and SYMBFACT
control = amd ();
control (3) = 1;

% order the matrix.  Note that the Info argument is optional.
fprintf ('\nIf the next step fails, then you have\n') ;
fprintf ('not yet compiled the AMD mexFunction.\n') ;
[p, Info] = amd (A, control) ;

% order again, but this time print some statistics
[p, Info] = amd (A, [10 1 1]) ;

fprintf ('Permutation vector:\n') ;
fprintf (' %2d', p) ;
fprintf ('\n\n') ;

subplot (2,1,2) ;
title ('Permuted matrix') ;
spy (A (p,p))

% approximations from amd:
lnz2 = n + Info (10) ;
fl2 = n + Info (11) + 2 * Info (12) ;
fprintf ('\nResults from AMD''s approximate analysis:\n') ;
fprintf ('number of nonzeros in L (including diagonal):      %d\n', lnz2) ;
fprintf ('floating point operation count for chol (A (p,p)): %d\n\n', fl2) ;

