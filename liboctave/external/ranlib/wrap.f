      subroutine dgennor (av, sd, result)
      double precision av, sd, result
      real gennor
      external gennor
      result = gennor (real (av), real (sd))
      return
      end
      subroutine dgenunf (low, high, result)
      double precision low, high, result
      real genunf
      external genunf
      result = genunf (real (low), real (high))
      return
      end
      subroutine dgenexp (av, result)
      double precision av, result
      real genexp
      external genexp
      result = genexp (real (av))
      return
      end
      subroutine dgengam (a, r, result)
      double precision a, r, result
      real gengam
      external gengam
      result = gengam (real (a), real (r))
      return
      end
      subroutine dignpoi (mu, result)
      double precision mu, result
      integer*4 ignpoi
      external ignpoi
      result = ignpoi (real (mu))
      return
      end
