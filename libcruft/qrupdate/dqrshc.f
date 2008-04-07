c Copyright (C) 2008  VZLU Prague, a.s., Czech Republic
c 
c Author: Jaroslav Hajek <highegg@gmail.com>
c 
c This source is free software; you can redistribute it and/or modify
c it under the terms of the GNU General Public License as published by
c the Free Software Foundation; either version 2 of the License, or
c (at your option) any later version.
c 
c This program is distributed in the hope that it will be useful,
c but WITHOUT ANY WARRANTY; without even the implied warranty of
c MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c GNU General Public License for more details.
c 
c You should have received a copy of the GNU General Public License
c along with this software; see the file COPYING.  If not, see
c <http://www.gnu.org/licenses/>.
c 
      subroutine dqrshc(m,n,k,Q,R,i,j)
c purpose:      updates a QR factorization after circular shift of
c               columns.      
c               i.e., given an m-by-k orthogonal matrix Q, an k-by-n
c               upper trapezoidal matrix R and index j in the range 
c               1:n+1, this subroutine updates the matrix Q -> Q1 and 
c               R -> R1 so that Q1 is again unitary, R1 upper trapezoidal, 
c               and 
c               Q1*R1 = A(:,p), where A = Q*R and p is the permutation
c               [1:i-1,shift(i:j,-1),j+1:n] if i < j  or
c               [1:j-1,shift(j:i,+1),i+1:n] if j > i.
c               if m == 0, the matrix Q is ignored.
c               (real version)
c arguments:
c m (in)        number of rows of the matrix Q, or 0 if Q is not needed.
c n (in)        number of columns of the matrix R.
c k (in)        number of columns of Q, and rows of R.
c Q (io)        on entry, the (orthogonal) matrix Q.
c               on exit, the updated matrix Q1
c R (io)        on entry, the upper trapezoidal m-by-n matrix R.
c               on exit, the updated matrix R1.
c i (in)        the first index determining the range (see above)
c j (in)        the second index determining the range (see above)
c
      integer m,n,k,i,j
      double precision Q(m,k),R(k,n)
      external dswap,dqhqr
      double precision w
      integer l,jj,kk,info

c quick return if possible
      if (k <= 0 .or. n <= 1) return
      info = 0
      if (m /= 0 .and. k > m) then
        info = 3
      else if (i < 1 .or. i > n) then
        info = 6
      else if (j < 1 .or. j > n) then
        info = 7
      end if
      if (info /= 0) then
        call xerbla('DQRSHC',info)
      end if

      if (i < j) then
c shift columns
        do l = i,j-1
          call dswap(min(k,l+1),R(1,l),1,R(1,l+1),1)
        end do
c retriangularize
        if (i < k) then
          kk = min(k,j)
          if (m > 0) then
            call dqhqr(m,n+1-i,kk+1-i,Q(1,i),m,R(i,i),k)
          else
            call dqhqr(0,n+1-i,kk+1-i,Q,1,R(i,i),k)
          endif
        end if
      else if (j < i) then
c shift columns
        do l = i,j+1,-1
          call dswap(min(k,i),R(1,l),1,R(1,l-1),1)
        end do
c retriangularize
        if (j < k) then
          jj = min(j+1,n)
          kk = min(k,i)
          if (m > 0) then
            call dqrqhu(m,n-j,kk+1-j,Q(1,j),m,R(j,jj),k,R(j,j),w)
          else
            call dqrqhu(0,n-j,kk+1-j,Q,1,R(j,jj),k,R(j,j),w)
          end if
          R(j,j) = w
          do jj = j+1,kk
            R(jj,j) = 0
          end do
        end if
      end if
      end
