function idxvec = sysidx(sys,sigtype,signamelist)
# idxvec = sysidx(sys,sigtype,signamelist)
# return indices of signals with specified signal names
# inputs:
#   sys:         OCST system data structure
#   sigtype:     signal type to be selected: "in", "out", "st"
#   signamelist: list of desired signal names
# outputs:
#   idxvec: vector of signal indices (appropriate for use with sysprune)

if(nargin != 3)
  usage("idxvec = sysidx(sys,sigtype,signamelist)");
elseif(!is_struct(sys))
  error("sys must be a system data structure");
elseif(!isstr(sigtype))
  error("sigtype must be a string");
elseif(rows(sigtype) != 1)
  error("sigtype (%d x %d) must be a single string", ...
    rows(sigtype),columns(sigtype));
end

# extract correct set of signal names values
[idxvec,msg] = listidx( list("in","out","st","yd"), sigtype);
if(msg)
  error("Illegal sigtype=%s",sigtype);
endif

syssiglist = sysgetsignals(sys,sigtype);
[idxvec,msg] = listidx(syssiglist,signamelist);
if(length(msg))
  error("sysidx(sigtype=%s): %s",sigtype, strrep(msg,"strlist","signamelist"));
end

endfunction
