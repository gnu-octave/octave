      subroutine qzit(nm,n,a,b,eps1,matz,z,ierr)
c
      integer i,j,k,l,n,en,k1,k2,ld,ll,l1,na,nm,ish,itn,its,km1,lm1,
     x        enm2,ierr,lor1,enorn
      double precision a(nm,n),b(nm,n),z(nm,n)
      double precision r,s,t,a1,a2,a3,ep,sh,u1,u2,u3,v1,v2,v3,ani,a11,
     x       a12,a21,a22,a33,a34,a43,a44,bni,b11,b12,b22,b33,b34,
     x       b44,epsa,epsb,eps1,anorm,bnorm,epslon
      logical matz,notlas
c
c     this subroutine is the second step of the qz algorithm
c     for solving generalized matrix eigenvalue problems,
c     siam j. numer. anal. 10, 241-256(1973) by moler and stewart,
c     as modified in technical note nasa tn d-7305(1973) by ward.
c
c     this subroutine accepts a pair of real matrices, one of them
c     in upper hessenberg form and the other in upper triangular form.
c     it reduces the hessenberg matrix to quasi-triangular form using
c     orthogonal transformations while maintaining the triangular form
c     of the other matrix.  it is usually preceded by  qzhes  and
c     followed by  qzval  and, possibly,  qzvec.
c
c     on input
c
c        nm must be set to the row dimension of two-dimensional
c          array parameters as declared in the calling program
c          dimension statement.
c
c        n is the order of the matrices.
c
c        a contains a real upper hessenberg matrix.
c
c        b contains a real upper triangular matrix.
c
c        eps1 is a tolerance used to determine negligible elements.
c          eps1 = 0.0 (or negative) may be input, in which case an
c          element will be neglected only if it is less than roundoff
c          error times the norm of its matrix.  if the input eps1 is
c          positive, then an element will be considered negligible
c          if it is less than eps1 times the norm of its matrix.  a
c          positive value of eps1 may result in faster execution,
c          but less accurate results.
c
c        matz should be set to .true. if the right hand transformations
c          are to be accumulated for later use in computing
c          eigenvectors, and to .false. otherwise.
c
c        z contains, if matz has been set to .true., the
c          transformation matrix produced in the reduction
c          by  qzhes, if performed, or else the identity matrix.
c          if matz has been set to .false., z is not referenced.
c
c     on output
c
c        a has been reduced to quasi-triangular form.  the elements
c          below the first subdiagonal are still zero and no two
c          consecutive subdiagonal elements are nonzero.
c
c        b is still in upper triangular form, although its elements
c          have been altered.  the location b(n,1) is used to store
c          eps1 times the norm of b for later use by  qzval  and  qzvec.
c
c        z contains the product of the right hand transformations
c          (for both steps) if matz has been set to .true..
c
c        ierr is set to
c          zero       for normal return,
c          j          if the limit of 30*n iterations is exhausted
c                     while the j-th eigenvalue is being sought.
c
c     questions and comments should be directed to burton s. garbow,
c     mathematics and computer science div, argonne national laboratory
c
c     this version dated august 1983.
c
c     ------------------------------------------------------------------
c
      ierr = 0
c     .......... compute epsa,epsb ..........
      anorm = 0.0d0
      bnorm = 0.0d0
c
      do 30 i = 1, n
         ani = 0.0d0
         if (i .ne. 1) ani = dabs(a(i,i-1))
         bni = 0.0d0
c
         do 20 j = i, n
            ani = ani + dabs(a(i,j))
            bni = bni + dabs(b(i,j))
   20    continue
c
         if (ani .gt. anorm) anorm = ani
         if (bni .gt. bnorm) bnorm = bni
   30 continue
c
      if (anorm .eq. 0.0d0) anorm = 1.0d0
      if (bnorm .eq. 0.0d0) bnorm = 1.0d0
      ep = eps1
      if (ep .gt. 0.0d0) go to 50
c     .......... use roundoff level if eps1 is zero ..........
      ep = epslon(1.0d0)
   50 epsa = ep * anorm
      epsb = ep * bnorm
c     .......... reduce a to quasi-triangular form, while
c                keeping b triangular ..........
      lor1 = 1
      enorn = n
      en = n
      itn = 30*n
c     .......... begin qz step ..........
   60 if (en .le. 2) go to 1001
      if (.not. matz) enorn = en
      its = 0
      na = en - 1
      enm2 = na - 1
   70 ish = 2
c     .......... check for convergence or reducibility.
c                for l=en step -1 until 1 do -- ..........
      do 80 ll = 1, en
         lm1 = en - ll
         l = lm1 + 1
         if (l .eq. 1) go to 95
         if (dabs(a(l,lm1)) .le. epsa) go to 90
   80 continue
c
   90 a(l,lm1) = 0.0d0
      if (l .lt. na) go to 95
c     .......... 1-by-1 or 2-by-2 block isolated ..........
      en = lm1
      go to 60
c     .......... check for small top of b ..........
   95 ld = l
  100 l1 = l + 1
      b11 = b(l,l)
      if (dabs(b11) .gt. epsb) go to 120
      b(l,l) = 0.0d0
      s = dabs(a(l,l)) + dabs(a(l1,l))
      u1 = a(l,l) / s
      u2 = a(l1,l) / s
      r = dsign(dsqrt(u1*u1+u2*u2),u1)
      v1 = -(u1 + r) / r
      v2 = -u2 / r
      u2 = v2 / v1
c
      do 110 j = l, enorn
         t = a(l,j) + u2 * a(l1,j)
         a(l,j) = a(l,j) + t * v1
         a(l1,j) = a(l1,j) + t * v2
         t = b(l,j) + u2 * b(l1,j)
         b(l,j) = b(l,j) + t * v1
         b(l1,j) = b(l1,j) + t * v2
  110 continue
c
      if (l .ne. 1) a(l,lm1) = -a(l,lm1)
      lm1 = l
      l = l1
      go to 90
  120 a11 = a(l,l) / b11
      a21 = a(l1,l) / b11
      if (ish .eq. 1) go to 140
c     .......... iteration strategy ..........
      if (itn .eq. 0) go to 1000
      if (its .eq. 10) go to 155
c     .......... determine type of shift ..........
      b22 = b(l1,l1)
      if (dabs(b22) .lt. epsb) b22 = epsb
      b33 = b(na,na)
      if (dabs(b33) .lt. epsb) b33 = epsb
      b44 = b(en,en)
      if (dabs(b44) .lt. epsb) b44 = epsb
      a33 = a(na,na) / b33
      a34 = a(na,en) / b44
      a43 = a(en,na) / b33
      a44 = a(en,en) / b44
      b34 = b(na,en) / b44
      t = 0.5d0 * (a43 * b34 - a33 - a44)
      r = t * t + a34 * a43 - a33 * a44
      if (r .lt. 0.0d0) go to 150
c     .......... determine single shift zeroth column of a ..........
      ish = 1
      r = dsqrt(r)
      sh = -t + r
      s = -t - r
      if (dabs(s-a44) .lt. dabs(sh-a44)) sh = s
c     .......... look for two consecutive small
c                sub-diagonal elements of a.
c                for l=en-2 step -1 until ld do -- ..........
      do 130 ll = ld, enm2
         l = enm2 + ld - ll
         if (l .eq. ld) go to 140
         lm1 = l - 1
         l1 = l + 1
         t = a(l,l)
         if (dabs(b(l,l)) .gt. epsb) t = t - sh * b(l,l)
         if (dabs(a(l,lm1)) .le. dabs(t/a(l1,l)) * epsa) go to 100
  130 continue
c
  140 a1 = a11 - sh
      a2 = a21
      if (l .ne. ld) a(l,lm1) = -a(l,lm1)
      go to 160
c     .......... determine double shift zeroth column of a ..........
  150 a12 = a(l,l1) / b22
      a22 = a(l1,l1) / b22
      b12 = b(l,l1) / b22
      a1 = ((a33 - a11) * (a44 - a11) - a34 * a43 + a43 * b34 * a11)
     x     / a21 + a12 - a11 * b12
      a2 = (a22 - a11) - a21 * b12 - (a33 - a11) - (a44 - a11)
     x     + a43 * b34
      a3 = a(l1+1,l1) / b22
      go to 160
c     .......... ad hoc shift ..........
  155 a1 = 0.0d0
      a2 = 1.0d0
      a3 = 1.1605d0
  160 its = its + 1
      itn = itn - 1
      if (.not. matz) lor1 = ld
c     .......... main loop ..........
      do 260 k = l, na
         notlas = k .ne. na .and. ish .eq. 2
         k1 = k + 1
         k2 = k + 2
         km1 = max0(k-1,l)
         ll = min0(en,k1+ish)
         if (notlas) go to 190
c     .......... zero a(k+1,k-1) ..........
         if (k .eq. l) go to 170
         a1 = a(k,km1)
         a2 = a(k1,km1)
  170    s = dabs(a1) + dabs(a2)
         if (s .eq. 0.0d0) go to 70
         u1 = a1 / s
         u2 = a2 / s
         r = dsign(dsqrt(u1*u1+u2*u2),u1)
         v1 = -(u1 + r) / r
         v2 = -u2 / r
         u2 = v2 / v1
c
         do 180 j = km1, enorn
            t = a(k,j) + u2 * a(k1,j)
            a(k,j) = a(k,j) + t * v1
            a(k1,j) = a(k1,j) + t * v2
            t = b(k,j) + u2 * b(k1,j)
            b(k,j) = b(k,j) + t * v1
            b(k1,j) = b(k1,j) + t * v2
  180    continue
c
         if (k .ne. l) a(k1,km1) = 0.0d0
         go to 240
c     .......... zero a(k+1,k-1) and a(k+2,k-1) ..........
  190    if (k .eq. l) go to 200
         a1 = a(k,km1)
         a2 = a(k1,km1)
         a3 = a(k2,km1)
  200    s = dabs(a1) + dabs(a2) + dabs(a3)
         if (s .eq. 0.0d0) go to 260
         u1 = a1 / s
         u2 = a2 / s
         u3 = a3 / s
         r = dsign(dsqrt(u1*u1+u2*u2+u3*u3),u1)
         v1 = -(u1 + r) / r
         v2 = -u2 / r
         v3 = -u3 / r
         u2 = v2 / v1
         u3 = v3 / v1
c
         do 210 j = km1, enorn
            t = a(k,j) + u2 * a(k1,j) + u3 * a(k2,j)
            a(k,j) = a(k,j) + t * v1
            a(k1,j) = a(k1,j) + t * v2
            a(k2,j) = a(k2,j) + t * v3
            t = b(k,j) + u2 * b(k1,j) + u3 * b(k2,j)
            b(k,j) = b(k,j) + t * v1
            b(k1,j) = b(k1,j) + t * v2
            b(k2,j) = b(k2,j) + t * v3
  210    continue
c
         if (k .eq. l) go to 220
         a(k1,km1) = 0.0d0
         a(k2,km1) = 0.0d0
c     .......... zero b(k+2,k+1) and b(k+2,k) ..........
  220    s = dabs(b(k2,k2)) + dabs(b(k2,k1)) + dabs(b(k2,k))
         if (s .eq. 0.0d0) go to 240
         u1 = b(k2,k2) / s
         u2 = b(k2,k1) / s
         u3 = b(k2,k) / s
         r = dsign(dsqrt(u1*u1+u2*u2+u3*u3),u1)
         v1 = -(u1 + r) / r
         v2 = -u2 / r
         v3 = -u3 / r
         u2 = v2 / v1
         u3 = v3 / v1
c
         do 230 i = lor1, ll
            t = a(i,k2) + u2 * a(i,k1) + u3 * a(i,k)
            a(i,k2) = a(i,k2) + t * v1
            a(i,k1) = a(i,k1) + t * v2
            a(i,k) = a(i,k) + t * v3
            t = b(i,k2) + u2 * b(i,k1) + u3 * b(i,k)
            b(i,k2) = b(i,k2) + t * v1
            b(i,k1) = b(i,k1) + t * v2
            b(i,k) = b(i,k) + t * v3
  230    continue
c
         b(k2,k) = 0.0d0
         b(k2,k1) = 0.0d0
         if (.not. matz) go to 240
c
         do 235 i = 1, n
            t = z(i,k2) + u2 * z(i,k1) + u3 * z(i,k)
            z(i,k2) = z(i,k2) + t * v1
            z(i,k1) = z(i,k1) + t * v2
            z(i,k) = z(i,k) + t * v3
  235    continue
c     .......... zero b(k+1,k) ..........
  240    s = dabs(b(k1,k1)) + dabs(b(k1,k))
         if (s .eq. 0.0d0) go to 260
         u1 = b(k1,k1) / s
         u2 = b(k1,k) / s
         r = dsign(dsqrt(u1*u1+u2*u2),u1)
         v1 = -(u1 + r) / r
         v2 = -u2 / r
         u2 = v2 / v1
c
         do 250 i = lor1, ll
            t = a(i,k1) + u2 * a(i,k)
            a(i,k1) = a(i,k1) + t * v1
            a(i,k) = a(i,k) + t * v2
            t = b(i,k1) + u2 * b(i,k)
            b(i,k1) = b(i,k1) + t * v1
            b(i,k) = b(i,k) + t * v2
  250    continue
c
         b(k1,k) = 0.0d0
         if (.not. matz) go to 260
c
         do 255 i = 1, n
            t = z(i,k1) + u2 * z(i,k)
            z(i,k1) = z(i,k1) + t * v1
            z(i,k) = z(i,k) + t * v2
  255    continue
c
  260 continue
c     .......... end qz step ..........
      go to 70
c     .......... set error -- all eigenvalues have not
c                converged after 30*n iterations ..........
 1000 ierr = en
c     .......... save epsb for use by qzval and qzvec ..........
 1001 if (n .gt. 1) b(n,1) = epsb
      return
      end
