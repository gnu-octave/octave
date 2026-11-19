########################################################################
##
## Copyright (C) 2026 The Octave Project Developers
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

## A method of a classdef class, written three ways.
%!assert (! isempty (strfind (help ("PlainCls.zorkify"), "bravoword")))
%!assert (! isempty (strfind (help ("PlainCls/zorkify"), "bravoword")))
%!assert (! isempty (strfind (help ("PlainCls>zorkify"), "bravoword")))

## A property of a classdef class, written three ways.
%!assert (! isempty (strfind (help ("PlainCls.zcount"), "charlieword")))
%!assert (! isempty (strfind (help ("PlainCls/zcount"), "charlieword")))
%!assert (! isempty (strfind (help ("PlainCls>zcount"), "charlieword")))

## A class stored in an @ directory accepts the @ form as well.
%!assert (! isempty (strfind (help ("AtCls.ameth"), "echoword")))
%!assert (! isempty (strfind (help ("@AtCls/ameth"), "echoword")))
%!assert (! isempty (strfind (help ("AtCls/ameth"), "echoword")))
%!assert (! isempty (strfind (help ("AtCls>ameth"), "echoword")))

## An old-style class, whose methods are separate files, likewise.
%!assert (! isempty (strfind (help ("oldC.om"), "golfword")))
%!assert (! isempty (strfind (help ("@oldC/om"), "golfword")))
%!assert (! isempty (strfind (help ("oldC/om"), "golfword")))
%!assert (! isempty (strfind (help ("oldC>om"), "golfword")))

## A class in a namespace, method and property.
%!assert (! isempty (strfind (help ("pk.NsCls.m1"), "kiloword")))
%!assert (! isempty (strfind (help ("pk.NsCls/m1"), "kiloword")))
%!assert (! isempty (strfind (help ("pk.NsCls>m1"), "kiloword")))
%!assert (! isempty (strfind (help ("pk.NsCls.pcount"), "mikeword")))
%!assert (! isempty (strfind (help ("pk.NsCls>pcount"), "mikeword")))

## The local function of an ordinary file is reached by the greater-than sign,
## and only by it, because it is not a class member.  The doc string returned
## is the local function's own, not that of the file it lives in.
%!assert (! isempty (strfind (help ("subfun>helper"), "oscarword")))
%!assert (! isempty (strfind (help ("subfun"), "novemberword")))
%!error <not found> help ("subfun.helper")
%!error <not found> help ("subfun/helper")
%!error <not found> help ("subfun>nosuchlocalfunction")

## The @ form applies only to a class which really is in an @ directory.
%!error <not found> help ("@PlainCls/zorkify")

## Asking for help on a member must leave the class working.  A name of the
## form CLASS>MEMBER used to reparse the class file and discard every method
## the class had.
%!test
%! assert (zorkify (PlainCls ()), 0);
%! h = help ("PlainCls>zorkify");
%! assert (zorkify (PlainCls ()), 0);

## The same for a member which does not exist, which reached the same code.
%!test
%! assert (zorkify (PlainCls ()), 0);
%! fail ('help ("PlainCls>nosuchmember")');
%! assert (zorkify (PlainCls ()), 0);
