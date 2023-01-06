########################################################################
##
## Copyright (C) 2014-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

## Don't alter the spacing in the command_test lines.  These are
## specifically testing for possible differences in things like
##   A(X) or A( X ) or A (X) or A ( X )

%!function command_test (varargin)
%!  assignin ('caller', 'cmd_out', ['|', sprintf('%s|', varargin{:})]);
%!endfunction

%!function gobble_command (varargin)
%!endfunction

## 0, 1, 2, 3 simple arguments
%!test
%! command_test
%! assert (cmd_out, '|');
%!test
%! command_test a
%! assert (cmd_out, '|a|');
%!test
%! command_test aa     b
%! assert (cmd_out, '|aa|b|');
%!test
%! command_test aaa  bb    c
%! assert (cmd_out, '|aaa|bb|c|');

## continuation
%!test
%! command_test a...
%!  bb ccc
%! assert (cmd_out, '|a|bb|ccc|');
%!test
%! command_test a ...
%!  bb ccc
%! assert (cmd_out, '|a|bb|ccc|');
%!test
%! command_test aa(...
%!  bb cc
%! assert (cmd_out, '|aa(|bb|cc|');
%!test
%! command_test aa(   ...
%!  bb cc
%! assert (cmd_out, '|aa(   |bb|cc|');

## comments
%!test
%! command_test aa bb cc%comment
%! assert (cmd_out, '|aa|bb|cc|');
%!test
%! command_test aa bb cc#comment
%! assert (cmd_out, '|aa|bb|cc|');
%!test
%! command_test aa bb cc   %comment
%! assert (cmd_out, '|aa|bb|cc|');
%!test
%! command_test aa bb cc   #comment
%! assert (cmd_out, '|aa|bb|cc|');
%!test
%! command_test aa bb cc(  %comment
%! assert (cmd_out, '|aa|bb|cc(  |');
%!test
%! command_test aa bb cc(  #comment
%! assert (cmd_out, '|aa|bb|cc(  |');

## semicolons and commas; multiple commands
%!test
%! command_test aa bb, gobble_command cc
%! assert (cmd_out, '|aa|bb|');
%!test
%! command_test aa bb ; gobble_command cc
%! assert (cmd_out, '|aa|bb|');
%!test
%! command_test aa bb ; command_test cc dd
%! assert (cmd_out, '|cc|dd|');
%!test
%! command_test aa bb
%!test
%! command_test cc dd
%! assert (cmd_out, '|cc|dd|');

## parenthesis matching
%!test
%! command_test aa(bb,cc,dd) ee(ff,gg) hh
%! assert (cmd_out, '|aa(bb,cc,dd)|ee(ff,gg)|hh|');
%!test
%! command_test aa([bb,cc)]
%! assert (cmd_out, '|aa([bb,cc)]|');
%!test
%! command_test aa(,@!$@"bb"'cc'
%! assert (cmd_out, '|aa(,@!$@"bb"''cc''|');
%!test
%! command_test aa(bb,cc,dd)
%! assert (cmd_out, '|aa(bb,cc,dd)|');
%!test
%! command_test aa( bb,cc,dd )
%! assert (cmd_out, '|aa( bb,cc,dd )|');
%!test
%! command_test aa (bb,cc,dd)
%! assert (cmd_out, '|aa|(bb,cc,dd)|');
%!test
%! command_test aa ( bb,cc,dd )
%! assert (cmd_out, '|aa|( bb,cc,dd )|');
%!test
%! command_test aa(bb, cc, dd)
%! assert (cmd_out, '|aa(bb, cc, dd)|');
%!test
%! command_test aa( bb, cc, dd )
%! assert (cmd_out, '|aa( bb, cc, dd )|');
%!test
%! command_test aa (bb, cc, dd)
%! assert (cmd_out, '|aa|(bb, cc, dd)|');
%!test
%! command_test aa ( bb, cc, dd )
%! assert (cmd_out, '|aa|( bb, cc, dd )|');

## single and double quotes
%!test
%! command_test "aa" 'bb' cc
%! assert (cmd_out, '|aa|bb|cc|');
%!test
%! command_test "aa"'bb'cc
%! assert (cmd_out, '|aabbcc|');
%!test
%! command_test aa'bb'"cc"
%! assert (cmd_out, '|aabbcc|');
%!test
%! command_test "aa"bb'cc'
%! assert (cmd_out, '|aabbcc|');

## CVX-inspired
%!test
%! command_test Z(n,n) hermitian toeplitz
%! assert (cmd_out, '|Z(n,n)|hermitian|toeplitz|');
%!test
%! command_test X( n, n ) symmetric
%! assert (cmd_out, '|X( n, n )|symmetric|');
%!test
%! command_test xw( nm-1, nv );
%! assert (cmd_out, '|xw( nm-1, nv )|');
%!test
%! command_test x( sx ) y( sx ) z( sx )
%! assert (cmd_out, '|x( sx )|y( sx )|z( sx )|');
%!test
%! command_test coeffs(deg+1) complex;
%! assert (cmd_out, '|coeffs(deg+1)|complex|');
%!test
%! command_test w( 1, npairs * nv ) v( 1, npairs * nv )
%! assert (cmd_out, '|w( 1, npairs * nv )|v( 1, npairs * nv )|');
%!test
%! command_test w(m,1)   % edge weights
%! assert (cmd_out, '|w(m,1)|');
%!test
%! command_test x2( size ( x ) )
%! assert (cmd_out, '|x2( size ( x ) )|');
