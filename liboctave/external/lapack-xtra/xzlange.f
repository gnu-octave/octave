      subroutine xzlange (norm, m, n, a, lda, work, retval)
      character norm
      integer lda, m, n
      double complex a (lda, *)
      double precision work (*), zlange, retval
      external zlange
      retval = zlange (norm, m, n, a, lda, work)
      return
      end
