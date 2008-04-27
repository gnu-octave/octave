      subroutine xsacosh (x, result)
      external acosh
      real x, result, dacosh
      result = acosh (x)
      return
      end
