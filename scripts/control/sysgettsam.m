function T = sysgettsam(sys)
# T = sysgettsam(sys)
# return the sampling time of the system

# $Revision: 1.3 $
# $Log: sysdimensions.m,v $
# Revision 1.3  1997/03/10 21:35:13  scotte
# added debugging code (commented out)
#
# Revision 1.2  1997/03/10 20:42:27  scotte
# added warning message about nargout
#

if(!is_struct(sys))
  usage("T = sysgettsam(sys)");
endif

T = sys.tsam;

endfunction
