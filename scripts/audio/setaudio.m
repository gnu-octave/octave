function setaudio (w_type, value)
  
# usage: setaudio ([w_type [, value]])
#
# executes the shell command `mixer [w_type [, value]]'

# Written by AW (Andreas.Weingessel@ci.tuwien.ac.at) on Oct 5, 1994
# Updated by AW on Nov 3, 1994
# Copyright Department of Probability Theory and Statistics TU Wien

  if (nargin == 0)
    system ("mixer");
  elseif (nargin == 1)
    system (sprintf ("mixer %s", w_type));
  elseif (nargin == 2)
    system (sprintf ("mixer %s %d", w_type, value));
  else
    usage ("setaudio ([w_type [, value]])");
  endif
  
endfunction
