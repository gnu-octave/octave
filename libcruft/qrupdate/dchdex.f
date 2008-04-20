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

      subroutine dchdex(n,R,R1,j)
c purpose:      given an upper triangular matrix R that is a Cholesky
c               factor of a symmetric positive definite matrix A, i.e.
c               A = R'*R, this subroutine updates R -> R1 so that
c               R1'*R1 = A(jj,jj), where jj = [1:j-1,j+1:n+1].
c               (real version)
c arguments:
c n (in)        the order of matrix R
c R (in)        the original upper trapezoidal matrix R
c R1 (out)      the updated matrix R1
c j (in)        the position of the deleted row/column
      integer n,j,info
      double precision R(n,n),R1(n-1,n-1)
      double precision Qdum,c,s,rr
      external xerbla,dlacpy,dqhqr,dlartg

c quick return if possible
      if (n == 1) return

c check arguments      
      info = 0
      if (n <= 0) then
        info = 1
      else if (j < 1 .or. j > n) then
        info = 4
      end if
      if (info /= 0) then
        call xerbla('DCHDEX',info)
      end if

c setup the new matrix R1
      if (j > 1) then
        call dlacpy('0',n-1,j-1,R(1,1),n,R1(1,1),n-1)
      end if
      if (j < n) then
        call dlacpy('0',n-1,n-j,R(1,j+1),n,R1(1,j),n-1)
        call dqhqr(0,n-j,n-j,Qdum,1,R1(j,j),n-1)
c eliminate R(n,n)      
        call dlartg(R1(n-1,n-1),R(n,n),c,s,rr)
        R1(n-1,n-1) = rr
      endif
      end
