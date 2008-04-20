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
      subroutine zqrdec(m,n,k,Q,R,R1,j)
c purpose:      updates a QR factorization after deleting
c               a column.      
c               i.e., given an m-by-k unitary matrix Q, an k-by-n
c               upper trapezoidal matrix R and index j in the range 
c               1:n+1, this subroutine updates the matrix Q -> Q1 and 
c               forms an m-by-(n-1) matrix R1 so that Q1 remains
c               unitary, R1 is upper trapezoidal, and 
c               Q1*R1 = [A(:,1:j-1) A(:,j+1:n)], where A = Q*R.
c               (complex version)
c arguments:
c m (in)        number of rows of the matrix Q.
c n (in)        number of columns of the matrix R.
c k (in)        number of columns of Q, and rows of R.
c Q (io)        on entry, the unitary m-by-k matrix Q.
c               on exit, the updated matrix Q1.
c R (in)        the original upper trapezoidal matrix R.
c R1 (out)      the updated matrix R1.
c j (in)        the position of the deleted column in R.
c               1 <= j <= n.
c
      integer m,n,k,j
      double complex Q(m,k),R(k,n),R1(k,n-1)
      external xerbla,zcopy,zqhqr
      integer info
c quick return if possible      
      if (m <= 0 .or. k <= 0 .or. n == 1) return
c check arguments      
      info = 0
      if (n < 1) then
        info = 2
      else if (j < 1 .or. j > n) then
        info = 7
      end if
      if (info /= 0) then
        call xerbla('ZQRDEC',info)
      end if
c copy leading portion
      call zcopy(k*(j-1),R,1,R1,1)
      if (j < n) then
c copy trailing portion of R        
        call zcopy(k*(n-j),R(1,j+1),1,R1(1,j),1)
c if necessary, retriangularize R1(j:k,j:n-1) and update Q(:,j:k)
        if (j < k) then
          call zqhqr(m,n-j,k-j+1,Q(1,j),m,R1(j,j),k)
        end if
      end if
      end 
