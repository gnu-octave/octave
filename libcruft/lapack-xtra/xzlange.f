      subroutine xzlange (norm, m, n, a, lda, work, retval)
      character norm
      integer lda, m, n
      double precision work (*), zlange, retval
      complex*16 a (lda, *)
      retval = zlange (norm, m, n, a, lda, work)
      return
      end
