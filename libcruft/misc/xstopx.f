      subroutine xstopx (string)
      character *(*) string
      integer slen
      slen = len (string)
      if (slen .eq. 0) goto 9999
      if (slen .eq. 1 .and. string(1:1) .eq. ' ') goto 9999
      write (*, *) string
 9999 call dostop ()
      end
