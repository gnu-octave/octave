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
      subroutine zqrder(m,n,Q,Q1,R,R1,j)
c purpose:      updates a QR factorization after deleting a row.      
c               i.e., given an m-by-m unitary matrix Q, an m-by-n
c               upper trapezoidal matrix R and index j in the range 
c               1:m, this subroutine forms the (m-1)-by-(m-1) matrix 
c               Q1 and an (m-1)-by-n matrix R1 so that Q1 is again 
c               unitary, R1 upper trapezoidal, and 
c               Q1*R1 = [A(1:j-1,:); A(j+1:m,:)], where A = Q*R.
c               (complex version)
c               
c arguments:
c m (in)        number of rows of the matrix R. 
c n (in)        number of columns of the matrix R
c Q (in)        the unitary matrix Q
c Q1 (out)      the updated matrix Q1
c R (in)        the upper trapezoidal matrix R
c R1 (out)      the updated matrix R1
c j (in)        the position of the new row in R1
c
      integer m,n,j
      double complex Q(m,m),Q1(m-1,m-1),R(m,n),R1(m-1,n)
      double precision c
      double complex s,rr,w
      external xerbla,zlacpy,zcopy,zlartg,zrot,zdscal,zaxpy
      integer i
c quick return if possible      
      if (m == 1) return
c check arguments      
      info = 0
      if (m < 1) then
        info = 1
      else if (j < 1 .or. j > n) then
        info = 7
      end if
      if (info /= 0) then
        call xerbla('ZQRDER',info)
      end if
c setup the new matrix Q1
c permute the columns of Q and rows of R so that the deleted row ends 
c up being the topmost row.      
      if (j > 1) then
        call zlacpy('0',j-1,m-1,Q(1,2),m,Q1(1,1),m-1)
      end if
      if (j < m) then
        call zlacpy('0',m-j,m-1,Q(j+1,2),m,Q1(j,1),m-1)
      end if
c setup the new matrix R1
      call zlacpy('0',m-1,n,R(2,1),m,R1(1,1),m-1)
c eliminate Q(j,2:m)
      w = Q(j,m)
      do i = m-1,2,-1
        call zlartg(Q(j,i),w,c,s,rr)
        w = rr
c apply rotation to rows of R1
        if (i <= n) then
          call zrot(n-i+1,R1(i-1,i),m-1,R1(i,i),m-1,c,conjg(s))
        end if
c apply rotation to columns of Q1
        call zrot(m-1,Q1(1,i-1),1,Q1(1,i),1,c,s)
      end do
c the last iteration is special, as we don't have the first row of
c R and first column of Q
      call zlartg(Q(j,1),w,c,s,rr)
      w = rr
      call zdscal(n,c,R1(1,1),m-1)
      call zaxpy(n,-s,R(1,1),m,R1(1,1),m-1)
c apply rotation to columns of Q1
      call zdscal(m-1,c,Q1(1,1),1)
      if (j > 1) then
        call zaxpy(j-1,-conjg(s),Q(1,1),1,Q1(1,1),1)
      end if
      if (j < m) then
        call zaxpy(m-j,-conjg(s),Q(j+1,1),1,Q1(j,1),1)
      end if
      end 
