# Copyright (C) 1993, 1998 A. Scottedward Hodel
#
# This file is part of Octave.
#
# Octave is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2, or (at your option) any
# later version.
#
# Octave is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License
# along with Octave; see the file COPYING.  If not, write to the Free
# Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

function QQ = krygetq(HH,alpha,kb)
# function QQ = krygetq(HH,alpha,kb)
# used internally by krylovb
# construct last kb columns of orthogonal transform returned by qrhouse
# Inputs:
#   HH,alpha: set of nh Householder reflections set returned by qrhouse
#   kb: desired number of columns
# Outputs: 
#   QQ: n x kb orthogonal matrix; basis of orthogonal subspace provided
#      by final kb reflections in HH.
#   Note: due to details in qrhouse and krylovb, QQ is *not* necessarily 
#      the last kb columns defined by HH.  span(QQ) is orthogonal to the
#      subspace spanned by the first (nh-kb) reflections in HH.

# Written by A. S. Hodel 1993--1995
# $Revision$
# $Log$

[n,m] = size(HH);
kb1 = m+1-kb;

# construct permuted identity on the right rows:
# since qrhouse removes zero rows, we've got to check the rows to which
# the current householder reflections actually reflected their columns
Hs = HH(:,kb1:m);
if(is_vector(Hs))
  nzidx = find(Hs' != 0);
else
  nzidx = find(max(abs(Hs')) != 0);
endif
nzidx = nzidx(1:kb);
QQ = zeros(n,kb);
QQ(nzidx,1:kb) = eye(kb);

# back out QQ matrix 
for i=m:-1:1
 hv = HH(:,i);
 av = alpha(i);
 QQ = QQ-av*hv*(hv'*QQ);
end;

endfunction
