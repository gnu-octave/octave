      subroutine xstopx (string)
      character *(*) string
      integer slen
      slen = len (string)
      if (slen .eq. 1 .and. string(1:1) .eq. ' ') then
        slen = 0
      endif
 9999 call dostop (string, slen)
      end
