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
      subroutine sqrinc(m,n,k,Q,R,R1,j,x)
c purpose:      updates a QR factorization after inserting a new
c               column.      
c               i.e., given an m-by-k orthogonal matrix Q, an m-by-n
c               upper trapezoidal matrix R and index j in the range 
c               1:n+1, this subroutine updates the matrix Q -> Q1 and 
c               forms an m-by-(n+1) matrix R1 so that Q1 is again
c               orthogonal, R1 upper trapezoidal, and 
c               Q1*R1 = [A(:,1:j-1); Q*Q'*x; A(:,j:n-1)], where A = Q*R.
c               (real version)
c arguments:
c m (in)        number of rows of the matrix Q.
c n (in)        number of columns of the matrix R.
c k (in)        number of columns of Q, and rows of R. k <= m.
c Q (io)        on entry, the orthogonal matrix Q.
c               on exit, the updated matrix Q1
c R (in)        the original upper trapezoidal matrix R
c R1 (out)      the updated matrix R1
c j (in)        the position of the new column in R1
c x (in)        the column being inserted
c
      integer m,n,k,j
      real Q(m,k),R(k,n),R1(k,n+1),x(m)


      real w
      external xerbla,scopy,sqrqhv,sgemv
      integer info,i,jj
c quick return if possible      
      if (m <= 0) return
c check arguments      
      info = 0
      if (n < 0) then
        info = 2
      else if (j < 1 .or. j > n+1) then
        info = 6
      end if
      if (info /= 0) then
        call xerbla('SQRINC',info)
      end if
c copy leading portion of R 
      call scopy(k*(j-1),R,1,R1,1)
      if (j <= n) then
        call scopy(k*(n+1-j),R(1,j),1,R1(1,j+1),1)
      end if
      call sgemv('T',m,min(k,j-1),1e0,Q,m,x,1,0e0,R1(1,j),1)
      if (j < k) then
c eliminate tail, updating Q(:,j:k) and R1(j:k,j+1:n+1)
        jj = min(j,n)+1
        call sqrqhv(m,n+1-j,k-j+1,Q(1,j),m,R1(j,jj),m,x,w)
c assemble inserted column        
        R1(j,j) = w
        do i = j+1,k
          R1(i,j) = 0e0
        end do 
      end if
      end 
