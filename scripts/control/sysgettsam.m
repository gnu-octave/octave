function T = sysgettsam(sys)
# T = sysgettsam(sys)
# return the sampling time of the system

# $Revision: 2.0.0.0 $

if(!is_struct(sys))
  usage("T = sysgettsam(sys)");
endif

T = sys.tsam;

endfunction
