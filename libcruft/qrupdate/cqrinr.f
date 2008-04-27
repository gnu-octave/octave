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
      subroutine cqrinr(m,n,Q,Q1,R,R1,j,x)
c purpose:      updates a QR factorization after inserting a new
c               row.      
c               i.e., given an m-by-m unitary matrix Q, an m-by-n
c               upper trapezoidal matrix R and index j in the range 
c               1:m+1, this subroutine forms the (m+1)-by-(m+1) matrix 
c               Q1 and an (m+1)-by-n matrix R1 so that Q1 is again 
c               unitary, R1 upper trapezoidal, and 
c               Q1*R1 = [A(1:j-1,:); x; A(j:m,:)], where A = Q*R.
c               (complex version)
c arguments:
c m (in)        number of rows of the matrix R. 
c n (in)        number of columns of the matrix R
c Q (in)        the orthogonal matrix Q
c Q1 (out)      the updated matrix Q1
c R (in)        the upper trapezoidal matrix R
c R1 (out)      the updated matrix R1
c j (in)        the position of the new row in R1
c x (in)        the row being added
c
      integer m,n,j
      complex Q(m,m),Q1(m+1,m+1),R(m,n),R1(m+1,n),x(n)
      external xerbla,clacpy,ccopy,cqhqr
      integer i
c check arguments      
      info = 0
      if (n < 0) then
        info = 2
      else if (j < 1 .or. j > m+1) then
        info = 7
      end if
      if (info /= 0) then
        call xerbla('CQRINR',info)
      end if
c setup the new matrix Q1
c permute the columns of Q1 and rows of R1 so that c the new row ends 
c up being the topmost row.      
      if (j > 1) then
        call clacpy('0',j-1,m,Q(1,1),m,Q1(1,2),m+1)
      end if
      if (j <= m) then
        call clacpy('0',m-j+1,m,Q(j,1),m,Q1(j+1,2),m+1)
      end if
c zero the rest of Q1      
      do i = 1,m+1
        Q1(i,1) = 0e0
        Q1(j,i) = 0e0
      end do
      Q1(j,1) = 1e0
c setup the new matrix R1
      call ccopy(n,x,1,R1(1,1),m+1)
      call clacpy('0',m,n,R(1,1),m,R1(2,1),m+1)
c rotate to form proper QR      
      call cqhqr(m+1,n,m+1,Q1,m+1,R1,m+1)
      end 
