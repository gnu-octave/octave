% umfpack_simple:  a simple demo of UMFPACK
%
% UMFPACK Version 4.4, Copyright (c) 2005 by Timothy A. Davis.
% All Rights Reserved.  Type umfpack_details for License.
%
% UMFPACK License:
%
%     Your use or distribution of UMFPACK or any modified version of
%     UMFPACK implies that you agree to this License.
%
%     THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY
%     EXPRESSED OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.
%
%     Permission is hereby granted to use or copy this program, provided
%     that the Copyright, this License, and the Availability of the original
%     version is retained on all copies.  User documentation of any code that
%     uses UMFPACK or any modified version of UMFPACK code must cite the
%     Copyright, this License, the Availability note, and "Used by permission."
%     Permission to modify the code and to distribute modified code is granted,
%     provided the Copyright, this License, and the Availability note are
%     retained, and a notice that the code was modified is included.  This
%     software was developed with support from the National Science Foundation,
%     and is provided to you free of charge.
%
% Availability: http://www.cise.ufl.edu/research/sparse/umfpack
%
% See also: umfpack, umfpack_details

help umfpack_simple
i = input ('Hit enter to agree to the above License: ', 's') ;
if (~isempty (i))
    error ('terminating') ;
end

format short

A = [
 2  3  0  0  0
 3  0  4  0  6
 0 -1 -3  2  0
 0  0  1  0  0
 0  4  2  0  1
]

A = sparse (A) ;

b = [8 45 -3 3 19]'

fprintf ('Solution to Ax=b via UMFPACK:\n') ;
fprintf ('x1 = umfpack (A, ''\\'', b)\n') ;

x1 = umfpack (A, '\', b)

fprintf ('Solution to Ax=b via MATLAB:\n') ;
fprintf ('x2 = A\\b\n') ;

x2 = A\b

fprintf ('norm (x1-x2) should be small: %g\n', norm (x1-x2)) ;

fprintf ('Type ''umfpack_demo'' for a full demo of UMFPACK\n') ;
