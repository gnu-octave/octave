########################################################################
##
## Copyright (C) 2017-2023 The Octave Project Developers
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

%!shared rv,cv,ndv,wm,tm,nd,lrv,lcv,lwm,ltm,lndv1,lndv2,lnd1,lnd2,rrv,rcv,rndv1,rndv2
%! rv = [1,2,3,4,5,6,7,8,9,10,11,12];
%! cv = rv';
%! ndv = reshape (rv, 1, 1, 12);
%! wm = reshape (rv, 2, 6);
%! tm = reshape (rv, 6, 2);
%! nd = reshape (rv, 2, 2, 3);
%! lrv = logical ([1,0,0,1,1,0,0,1,1,0,0,1]);
%! lcv = lrv';
%! lwm = reshape (lrv, 2, 6);
%! ltm = reshape (lrv, 6, 2);
%! lndv1 = reshape (lrv, 1, 1, 12);
%! lndv2 = reshape (lrv, 1, 1, 1, 12);
%! lnd1 = reshape (lrv, 2, 2, 3);
%! lnd2 = reshape (lrv, 2, 3, 2);
%! rrv = [1,4,5,8,9,12];
%! rcv = rrv';
%! rndv1 = reshape (rrv, 1, 1, 6);
%! rndv2 = reshape (rrv, 1, 1, 1, 6);
%!assert (rv(lrv), rrv)
%!assert (rv(lcv), rrv)
%!assert (rv(lwm), rrv)
%!assert (rv(ltm), rrv)
%!assert (rv(lndv1), rrv)
%!assert (rv(lndv2), rrv)
%!assert (rv(lnd1), rrv)
%!assert (rv(lnd2), rrv)
%!assert (cv(lrv), rcv)
%!assert (cv(lcv), rcv)
%!assert (cv(lwm), rcv)
%!assert (cv(ltm), rcv)
%!assert (cv(lndv1), rcv)
%!assert (cv(lndv2), rcv)
%!assert (cv(lnd1), rcv)
%!assert (cv(lnd2), rcv)
%!assert (ndv(lrv), rndv1)
%!assert (ndv(lcv), rndv1)
%!assert (ndv(lwm), rndv1)
%!assert (ndv(ltm), rndv1)
%!assert (ndv(lndv1), rndv1)
%!assert (ndv(lndv2), rndv1)
%!assert (ndv(lnd1), rndv1)
%!assert (ndv(lnd2), rndv1)
%!assert (wm(lrv), rrv)
%!assert (wm(lcv), rcv)
%!assert (wm(lwm), rcv)
%!assert (wm(ltm), rcv)
%!assert (wm(lndv1), rndv1)
%!assert (wm(lndv2), rndv2)
%!assert (wm(lnd1), rcv)
%!assert (wm(lnd2), rcv)
%!assert (tm(lrv), rrv)
%!assert (tm(lcv), rcv)
%!assert (tm(lwm), rcv)
%!assert (tm(ltm), rcv)
%!assert (tm(lndv1), rndv1)
%!assert (tm(lndv2), rndv2)
%!assert (tm(lnd1), rcv)
%!assert (tm(lnd2), rcv)
%!assert (nd(lrv), rrv)
%!assert (nd(lcv), rcv)
%!assert (nd(lwm), rcv)
%!assert (nd(ltm), rcv)
%!assert (nd(lndv1), rndv1)
%!assert (nd(lndv2), rndv2)
%!assert (nd(lnd1), rcv)
%!assert (nd(lnd2), rcv)

%!shared rv,cv,ndv,wm,tm,nd,lrv,lcv,lwm,ltm,lndv1,lndv2,lnd1,lnd2,rrv,rcv,rndv1,rndv2
%! rv = [1,2,3,4,5,6,7,8,9,10,11,12];
%! cv = rv';
%! ndv = reshape (rv, 1, 1, 12);
%! wm = reshape (rv, 2, 6);
%! tm = reshape (rv, 6, 2);
%! nd = reshape (rv, 2, 2, 3);
%! lrv = logical ([1,1,1,1,1,1,1,1,1,1,1,1]);
%! lcv = lrv';
%! lwm = reshape (lrv, 2, 6);
%! ltm = reshape (lrv, 6, 2);
%! lndv1 = reshape (lrv, 1, 1, 12);
%! lndv2 = reshape (lrv, 1, 1, 1, 12);
%! lnd1 = reshape (lrv, 2, 2, 3);
%! lnd2 = reshape (lrv, 2, 3, 2);
%! rrv = [1,2,3,4,5,6,7,8,9,10,11,12];
%! rcv = rrv';
%! rndv1 = reshape (rrv, 1, 1, 12);
%! rndv2 = reshape (rrv, 1, 1, 1, 12);
%!assert (rv(lrv), rrv)
%!assert (rv(lcv), rrv)
%!assert (rv(lwm), rrv)
%!assert (rv(ltm), rrv)
%!assert (rv(lndv1), rrv)
%!assert (rv(lndv2), rrv)
%!assert (rv(lnd1), rrv)
%!assert (rv(lnd2), rrv)
%!assert (cv(lrv), rcv)
%!assert (cv(lcv), rcv)
%!assert (cv(lwm), rcv)
%!assert (cv(ltm), rcv)
%!assert (cv(lndv1), rcv)
%!assert (cv(lndv2), rcv)
%!assert (cv(lnd1), rcv)
%!assert (cv(lnd2), rcv)
%!assert (ndv(lrv), rndv1)
%!assert (ndv(lcv), rndv1)
%!assert (ndv(lwm), rndv1)
%!assert (ndv(ltm), rndv1)
%!assert (ndv(lndv1), rndv1)
%!assert (ndv(lndv2), rndv1)
%!assert (ndv(lnd1), rndv1)
%!assert (ndv(lnd2), rndv1)
%!assert (wm(lrv), rrv)
%!assert (wm(lcv), rcv)
%!assert (wm(lwm), rcv)
%!assert (wm(ltm), rcv)
%!assert (wm(lndv1), rndv1)
%!assert (wm(lndv2), rndv2)
%!assert (wm(lnd1), rcv)
%!assert (wm(lnd2), rcv)
%!assert (tm(lrv), rrv)
%!assert (tm(lcv), rcv)
%!assert (tm(lwm), rcv)
%!assert (tm(ltm), rcv)
%!assert (tm(lndv1), rndv1)
%!assert (tm(lndv2), rndv2)
%!assert (tm(lnd1), rcv)
%!assert (tm(lnd2), rcv)
%!assert (nd(lrv), rrv)
%!assert (nd(lcv), rcv)
%!assert (nd(lwm), rcv)
%!assert (nd(ltm), rcv)
%!assert (nd(lndv1), rndv1)
%!assert (nd(lndv2), rndv2)
%!assert (nd(lnd1), rcv)
%!assert (nd(lnd2), rcv)

%!shared rv,cv,ndv,wm,tm,nd,irv,icv,iwm,itm,indv1,indv2,ind1,ind2,rrv,rcv,rndv1,rndv2
%! rv = [1,2,3,4,5,6,7,8,9,10,11,12];
%! cv = rv';
%! ndv = reshape (rv, 1, 1, 12);
%! wm = reshape (rv, 2, 6);
%! tm = reshape (rv, 6, 2);
%! nd = reshape (rv, 2, 2, 3);
%! irv = [1,3,5,7,9,11];
%! icv = irv';
%! iwm = reshape (irv, 2, 3);
%! itm = reshape (irv, 3, 2);
%! indv1 = reshape (irv, 1, 1, 6);
%! indv2 = reshape (irv, 1, 1, 1, 6);
%! ind1 = reshape (irv, 2, 1, 3);
%! ind2 = reshape (irv, 1, 3, 2);
%! rrv = [1,3,5,7,9,11];
%! rcv = rrv';
%! rndv1 = reshape (rrv, 1, 1, 6);
%! rndv2 = reshape (rrv, 1, 1, 1, 6);
%!assert (rv(irv), rrv)
%!assert (rv(icv), rrv)
%!assert (rv(iwm), iwm)
%!assert (rv(itm), itm)
%!assert (rv(indv1), rrv)
%!assert (rv(indv2), rrv)
%!assert (rv(ind1), ind1)
%!assert (rv(ind2), ind2)
%!assert (cv(irv), rcv)
%!assert (cv(icv), rcv)
%!assert (cv(iwm), iwm)
%!assert (cv(itm), itm)
%!assert (cv(indv1), rcv)
%!assert (cv(indv2), rcv)
%!assert (cv(ind1), ind1)
%!assert (cv(ind2), ind2)
%!assert (ndv(irv), rndv1)
%!assert (ndv(icv), rndv1)
%!assert (ndv(iwm), iwm)
%!assert (ndv(itm), itm)
%!assert (ndv(indv1), rndv1)
%!assert (ndv(indv2), rndv1)
%!assert (ndv(ind1), ind1)
%!assert (ndv(ind2), ind2)
%!assert (wm(irv), rrv)
%!assert (wm(icv), rcv)
%!assert (wm(iwm), iwm)
%!assert (wm(itm), itm)
%!assert (wm(indv1), rndv1)
%!assert (wm(indv2), rndv2)
%!assert (wm(ind1), ind1)
%!assert (wm(ind2), ind2)
%!assert (tm(irv), rrv)
%!assert (tm(icv), rcv)
%!assert (tm(iwm), iwm)
%!assert (tm(itm), itm)
%!assert (tm(indv1), rndv1)
%!assert (tm(indv2), rndv2)
%!assert (tm(ind1), ind1)
%!assert (tm(ind2), ind2)
%!assert (nd(irv), rrv)
%!assert (nd(icv), rcv)
%!assert (nd(iwm), iwm)
%!assert (nd(itm), itm)
%!assert (nd(indv1), rndv1)
%!assert (nd(indv2), rndv2)
%!assert (nd(ind1), ind1)
%!assert (nd(ind2), ind2)

%!shared rv,cv,ndv,wm,tm,nd,irv,icv,iwm,itm,indv1,indv2,ind1,ind2,rrv,rcv,rndv1,rndv2
%! rv = [1,2,3,4,5,6,7,8,9,10,11,12];
%! cv = rv';
%! ndv = reshape (rv, 1, 1, 12);
%! wm = reshape (rv, 2, 6);
%! tm = reshape (rv, 6, 2);
%! nd = reshape (rv, 2, 2, 3);
%! irv = [1,2,3,4,5,6,7,8,9,10,11,12];
%! icv = irv';
%! iwm = reshape (irv, 2, 6);
%! itm = reshape (irv, 6, 2);
%! indv1 = reshape (irv, 1, 1, 12);
%! indv2 = reshape (irv, 1, 1, 1, 12);
%! ind1 = reshape (irv, 2, 2, 3);
%! ind2 = reshape (irv, 2, 3, 2);
%! rrv = [1,2,3,4,5,6,7,8,9,10,11,12];
%! rcv = rrv';
%! rndv1 = reshape (rrv, 1, 1, 12);
%! rndv2 = reshape (rrv, 1, 1, 1, 12);
%!assert (rv(irv), rrv)
%!assert (rv(icv), rrv)
%!assert (rv(iwm), iwm)
%!assert (rv(itm), itm)
%!assert (rv(indv1), rrv)
%!assert (rv(indv2), rrv)
%!assert (rv(ind1), ind1)
%!assert (rv(ind2), ind2)
%!assert (cv(irv), rcv)
%!assert (cv(icv), rcv)
%!assert (cv(iwm), iwm)
%!assert (cv(itm), itm)
%!assert (cv(indv1), rcv)
%!assert (cv(indv2), rcv)
%!assert (cv(ind1), ind1)
%!assert (cv(ind2), ind2)
%!assert (ndv(irv), rndv1)
%!assert (ndv(icv), rndv1)
%!assert (ndv(iwm), iwm)
%!assert (ndv(itm), itm)
%!assert (ndv(indv1), rndv1)
%!assert (ndv(indv2), rndv1)
%!assert (ndv(ind1), ind1)
%!assert (ndv(ind2), ind2)
%!assert (wm(irv), rrv)
%!assert (wm(icv), rcv)
%!assert (wm(iwm), iwm)
%!assert (wm(itm), itm)
%!assert (wm(indv1), rndv1)
%!assert (wm(indv2), rndv2)
%!assert (wm(ind1), ind1)
%!assert (wm(ind2), ind2)
%!assert (tm(irv), rrv)
%!assert (tm(icv), rcv)
%!assert (tm(iwm), iwm)
%!assert (tm(itm), itm)
%!assert (tm(indv1), rndv1)
%!assert (tm(indv2), rndv2)
%!assert (tm(ind1), ind1)
%!assert (tm(ind2), ind2)
%!assert (nd(irv), rrv)
%!assert (nd(icv), rcv)
%!assert (nd(iwm), iwm)
%!assert (nd(itm), itm)
%!assert (nd(indv1), rndv1)
%!assert (nd(indv2), rndv2)
%!assert (nd(ind1), ind1)
%!assert (nd(ind2), ind2)
