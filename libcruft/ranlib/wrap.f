      subroutine dgennor (av, sd, result)
      double precision av, sd, result
      result = gennor (real (av), real (sd))
      return
      end
      subroutine dgenunf (low, high, result)
      double precision low, high, result
      result = genunf (real (low), real (high))
      return
      end
