      subroutine qzval(nm,n,a,b,alfr,alfi,beta,matz,z)
c
      integer i,j,n,en,na,nm,nn,isw
      double precision a(nm,n),b(nm,n),alfr(n),alfi(n),beta(n),z(nm,n)
      double precision c,d,e,r,s,t,an,a1,a2,bn,cq,cz,di,dr,ei,ti,tr,u1,
     x       u2,v1,v2,a1i,a11,a12,a2i,a21,a22,b11,b12,b22,sqi,sqr,
     x       ssi,ssr,szi,szr,a11i,a11r,a12i,a12r,a22i,a22r,epsb
      logical matz
c
c     this subroutine is the third step of the qz algorithm
c     for solving generalized matrix eigenvalue problems,
c     siam j. numer. anal. 10, 241-256(1973) by moler and stewart.
c
c     this subroutine accepts a pair of real matrices, one of them
c     in quasi-triangular form and the other in upper triangular form.
c     it reduces the quasi-triangular matrix further, so that any
c     remaining 2-by-2 blocks correspond to pairs of complex
c     eigenvalues, and returns quantities whose ratios give the
c     generalized eigenvalues.  it is usually preceded by  qzhes
c     and  qzit  and may be followed by  qzvec.
c
c     on input
c
c        nm must be set to the row dimension of two-dimensional
c          array parameters as declared in the calling program
c          dimension statement.
c
c        n is the order of the matrices.
c
c        a contains a real upper quasi-triangular matrix.
c
c        b contains a real upper triangular matrix.  in addition,
c          location b(n,1) contains the tolerance quantity (epsb)
c          computed and saved in  qzit.
c
c        matz should be set to .true. if the right hand transformations
c          are to be accumulated for later use in computing
c          eigenvectors, and to .false. otherwise.
c
c        z contains, if matz has been set to .true., the
c          transformation matrix produced in the reductions by qzhes
c          and qzit, if performed, or else the identity matrix.
c          if matz has been set to .false., z is not referenced.
c
c     on output
c
c        a has been reduced further to a quasi-triangular matrix
c          in which all nonzero subdiagonal elements correspond to
c          pairs of complex eigenvalues.
c
c        b is still in upper triangular form, although its elements
c          have been altered.  b(n,1) is unaltered.
c
c        alfr and alfi contain the real and imaginary parts of the
c          diagonal elements of the triangular matrix that would be
c          obtained if a were reduced completely to triangular form
c          by unitary transformations.  non-zero values of alfi occur
c          in pairs, the first member positive and the second negative.
c
c        beta contains the diagonal elements of the corresponding b,
c          normalized to be real and non-negative.  the generalized
c          eigenvalues are then the ratios ((alfr+i*alfi)/beta).
c
c        z contains the product of the right hand transformations
c          (for all three steps) if matz has been set to .true.
c
c     questions and comments should be directed to burton s. garbow,
c     mathematics and computer science div, argonne national laboratory
c
c     this version dated august 1983.
c
c     ------------------------------------------------------------------
c
      epsb = b(n,1)
      isw = 1
c     .......... find eigenvalues of quasi-triangular matrices.
c                for en=n step -1 until 1 do -- ..........
      do 510 nn = 1, n
         en = n + 1 - nn
         na = en - 1
         if (isw .eq. 2) go to 505
         if (en .eq. 1) go to 410
         if (a(en,na) .ne. 0.0d0) go to 420
c     .......... 1-by-1 block, one real root ..........
  410    alfr(en) = a(en,en)
         if (b(en,en) .lt. 0.0d0) alfr(en) = -alfr(en)
         beta(en) = dabs(b(en,en))
         alfi(en) = 0.0d0
         go to 510
c     .......... 2-by-2 block ..........
  420    if (dabs(b(na,na)) .le. epsb) go to 455
         if (dabs(b(en,en)) .gt. epsb) go to 430
         a1 = a(en,en)
         a2 = a(en,na)
         bn = 0.0d0
         go to 435
  430    an = dabs(a(na,na)) + dabs(a(na,en)) + dabs(a(en,na))
     x      + dabs(a(en,en))
         bn = dabs(b(na,na)) + dabs(b(na,en)) + dabs(b(en,en))
         a11 = a(na,na) / an
         a12 = a(na,en) / an
         a21 = a(en,na) / an
         a22 = a(en,en) / an
         b11 = b(na,na) / bn
         b12 = b(na,en) / bn
         b22 = b(en,en) / bn
         e = a11 / b11
         ei = a22 / b22
         s = a21 / (b11 * b22)
         t = (a22 - e * b22) / b22
         if (dabs(e) .le. dabs(ei)) go to 431
         e = ei
         t = (a11 - e * b11) / b11
  431    c = 0.5d0 * (t - s * b12)
         d = c * c + s * (a12 - e * b12)
         if (d .lt. 0.0d0) go to 480
c     .......... two real roots.
c                zero both a(en,na) and b(en,na) ..........
         e = e + (c + dsign(dsqrt(d),c))
         a11 = a11 - e * b11
         a12 = a12 - e * b12
         a22 = a22 - e * b22
         if (dabs(a11) + dabs(a12) .lt.
     x       dabs(a21) + dabs(a22)) go to 432
         a1 = a12
         a2 = a11
         go to 435
  432    a1 = a22
         a2 = a21
c     .......... choose and apply real z ..........
  435    s = dabs(a1) + dabs(a2)
         u1 = a1 / s
         u2 = a2 / s
         r = dsign(dsqrt(u1*u1+u2*u2),u1)
         v1 = -(u1 + r) / r
         v2 = -u2 / r
         u2 = v2 / v1
c
         do 440 i = 1, en
            t = a(i,en) + u2 * a(i,na)
            a(i,en) = a(i,en) + t * v1
            a(i,na) = a(i,na) + t * v2
            t = b(i,en) + u2 * b(i,na)
            b(i,en) = b(i,en) + t * v1
            b(i,na) = b(i,na) + t * v2
  440    continue
c
         if (.not. matz) go to 450
c
         do 445 i = 1, n
            t = z(i,en) + u2 * z(i,na)
            z(i,en) = z(i,en) + t * v1
            z(i,na) = z(i,na) + t * v2
  445    continue
c
  450    if (bn .eq. 0.0d0) go to 475
         if (an .lt. dabs(e) * bn) go to 455
         a1 = b(na,na)
         a2 = b(en,na)
         go to 460
  455    a1 = a(na,na)
         a2 = a(en,na)
c     .......... choose and apply real q ..........
  460    s = dabs(a1) + dabs(a2)
         if (s .eq. 0.0d0) go to 475
         u1 = a1 / s
         u2 = a2 / s
         r = dsign(dsqrt(u1*u1+u2*u2),u1)
         v1 = -(u1 + r) / r
         v2 = -u2 / r
         u2 = v2 / v1
c
         do 470 j = na, n
            t = a(na,j) + u2 * a(en,j)
            a(na,j) = a(na,j) + t * v1
            a(en,j) = a(en,j) + t * v2
            t = b(na,j) + u2 * b(en,j)
            b(na,j) = b(na,j) + t * v1
            b(en,j) = b(en,j) + t * v2
  470    continue
c
  475    a(en,na) = 0.0d0
         b(en,na) = 0.0d0
         alfr(na) = a(na,na)
         alfr(en) = a(en,en)
         if (b(na,na) .lt. 0.0d0) alfr(na) = -alfr(na)
         if (b(en,en) .lt. 0.0d0) alfr(en) = -alfr(en)
         beta(na) = dabs(b(na,na))
         beta(en) = dabs(b(en,en))
         alfi(en) = 0.0d0
         alfi(na) = 0.0d0
         go to 505
c     .......... two complex roots ..........
  480    e = e + c
         ei = dsqrt(-d)
         a11r = a11 - e * b11
         a11i = ei * b11
         a12r = a12 - e * b12
         a12i = ei * b12
         a22r = a22 - e * b22
         a22i = ei * b22
         if (dabs(a11r) + dabs(a11i) + dabs(a12r) + dabs(a12i) .lt.
     x       dabs(a21) + dabs(a22r) + dabs(a22i)) go to 482
         a1 = a12r
         a1i = a12i
         a2 = -a11r
         a2i = -a11i
         go to 485
  482    a1 = a22r
         a1i = a22i
         a2 = -a21
         a2i = 0.0d0
c     .......... choose complex z ..........
  485    cz = dsqrt(a1*a1+a1i*a1i)
         if (cz .eq. 0.0d0) go to 487
         szr = (a1 * a2 + a1i * a2i) / cz
         szi = (a1 * a2i - a1i * a2) / cz
         r = dsqrt(cz*cz+szr*szr+szi*szi)
         cz = cz / r
         szr = szr / r
         szi = szi / r
         go to 490
  487    szr = 1.0d0
         szi = 0.0d0
  490    if (an .lt. (dabs(e) + ei) * bn) go to 492
         a1 = cz * b11 + szr * b12
         a1i = szi * b12
         a2 = szr * b22
         a2i = szi * b22
         go to 495
  492    a1 = cz * a11 + szr * a12
         a1i = szi * a12
         a2 = cz * a21 + szr * a22
         a2i = szi * a22
c     .......... choose complex q ..........
  495    cq = dsqrt(a1*a1+a1i*a1i)
         if (cq .eq. 0.0d0) go to 497
         sqr = (a1 * a2 + a1i * a2i) / cq
         sqi = (a1 * a2i - a1i * a2) / cq
         r = dsqrt(cq*cq+sqr*sqr+sqi*sqi)
         cq = cq / r
         sqr = sqr / r
         sqi = sqi / r
         go to 500
  497    sqr = 1.0d0
         sqi = 0.0d0
c     .......... compute diagonal elements that would result
c                if transformations were applied ..........
  500    ssr = sqr * szr + sqi * szi
         ssi = sqr * szi - sqi * szr
         i = 1
         tr = cq * cz * a11 + cq * szr * a12 + sqr * cz * a21
     x      + ssr * a22
         ti = cq * szi * a12 - sqi * cz * a21 + ssi * a22
         dr = cq * cz * b11 + cq * szr * b12 + ssr * b22
         di = cq * szi * b12 + ssi * b22
         go to 503
  502    i = 2
         tr = ssr * a11 - sqr * cz * a12 - cq * szr * a21
     x      + cq * cz * a22
         ti = -ssi * a11 - sqi * cz * a12 + cq * szi * a21
         dr = ssr * b11 - sqr * cz * b12 + cq * cz * b22
         di = -ssi * b11 - sqi * cz * b12
  503    t = ti * dr - tr * di
         j = na
         if (t .lt. 0.0d0) j = en
         r = dsqrt(dr*dr+di*di)
         beta(j) = bn * r
         alfr(j) = an * (tr * dr + ti * di) / r
         alfi(j) = an * t / r
         if (i .eq. 1) go to 502
  505    isw = 3 - isw
  510 continue
      b(n,1) = epsb
c
      return
      end
