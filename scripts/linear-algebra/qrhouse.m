# Copyright (C) 1992, 1998 A. Scottedward Hodel
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

function [hv,alph,kb] = qrhouse(VV,eps1)
# function [hv,alph,kb] = qrhouse(VV,eps1)
# construct orthogonal basis of span(VV) with Householder vectors
# Q R = VV; Q may be obtained via routine krygetq; R is upper triangular
#   if all rows of VV are nonzero; otherwise it's a permuted uppert
#   triangular matrix with zero rows matching those of VV.
# Inputs: 
#   VV: matrix
#   eps1: zero threshhold; default: 0.  Used to check if a column
#     of reduced R is already upper triangular; entries with magnitude
#     <= eps1 are considered to be 0.
# Outputs:
#   hv: matrix of householder reflection vectors as returned by housh
#   alpha: vector of householder reflection values as returned by housh
#   kb: computed rank of matrix
# qrhouse is used in krylovb for block Arnoldi iteration
#
# Reference: Golub and Van Loan, MATRIX COMPUTATIONS, 2nd ed.

# Written by A. S. Hodel, 1992
# $Revision: 1.2 $
# $Log: qrhouse.m,v $
# Revision 1.2  1998/08/26 21:08:29  hodelas
# Fixed bug in controllability analysis; code isolates zero rows of
# input matrices; correctly checks zero threshhold
#

if(nargin < 2)
  usage("[hv,alph,kb] = qrhouse(VV,eps1)");
endif

# extract only those rows of VV that are nonzero
if(is_vector(VV))	nzidx = find(abs(VV') > 0);
else			nzidx = find(max(abs(VV')) > 0);
endif

VVlen = rows(VV);	# remember original size

if(is_vector(VV))	VV = VV(nzidx);
else			VV = VV(nzidx,:);
endif

[Vr,Vc] = size(VV);	nits   = min(Vr,Vc);
kb      = 0;		kbnext = kb+1;
for ii = 1:nits;
  kb = kbnext;		# select column number of hv to build
  hh = VV(:,ii);	# extract next column of VV; ignore items 1:(ii-1).
  idx = kb-1;		# for zeroing
  if(max(abs(hh(kb:Vr))) > eps1)
    [hv(kb:Vr,kb),alph(kb),z] = housh(hh(kb:Vr),1,0);
    if(kb>1)
      hv(1:idx,kb) = 0;			# zero top of hv for safety
    endif
    VV = VV - alph(kb)*hv(:,kb)*(hv(:,kb)'*VV);
    kbnext = kb+1;
  else
    kb = kb-1;
  end
endfor
if(kb <=0)
  hv = [];
  alph = [];
else
  hvs = hv(:,1:kb);	# remove extraneous vectors, expand to original size
  hv = zeros(VVlen,kb);
  hv(nzidx,:) = hvs;
  alph = alph(1:kb);
end
endfunction
