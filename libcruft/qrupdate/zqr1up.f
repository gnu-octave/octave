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
      subroutine zqr1up(m,n,k,Q,R,u,v)
c purpose:      updates a QR factorization after rank-1 modification
c               i.e., given a m-by-k unitary Q and m-by-n upper 
c               trapezoidal R, an m-vector u and n-vector v, 
c               this subroutine updates Q -> Q1 and R -> R1 so that
c               Q1*R1 = Q*R + Q*Q'u*v', and Q1 is again unitary
c               and R1 upper trapezoidal.
c               (complex version)
c arguments:
c m (in)        number of rows of the matrix Q.
c n (in)        number of columns of the matrix R.
c k (in)        number of columns of Q, and rows of R. k <= m.
c Q (io)        on entry, the unitary m-by-k matrix Q.
c               on exit, the updated matrix Q1.
c R (io)        on entry, the upper trapezoidal m-by-n matrix R.
c               on exit, the updated matrix R1.
c u (in)        the left m-vector.
c v (in)        the right n-vector.
c
      integer m,n,k
      double complex Q(m,k),R(k,n),u(m),v(n)
      double complex w
      external zqrqhv,zqhqr
      integer i
c quick return if possible      
      if (m <= 0 .or. n <= 0) return
c eliminate tail of Q'*u
      call zqrqhv(m,n,k,Q,m,R,m,u,w)
c update R      
      do i = 1,n
        R(1,i) = R(1,i) + w*conjg(v(i))
      end do
c retriangularize R
      call zqhqr(m,n,k,Q,m,R,k)
      end 
