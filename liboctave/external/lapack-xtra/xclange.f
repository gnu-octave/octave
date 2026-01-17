      subroutine xclange (norm, m, n, a, lda, work, retval)
      character norm
      integer lda, m, n
      complex a (lda, *)
      real work (*), clange, retval
      external clange
      retval = clange (norm, m, n, a, lda, work)
      return
      end
