# rdepend.make -- rules for remaking the dependencies.
# 
# Have to use -M, not -MM, since we use <kpathsea/...> instead of
# "kpathsea/..." in the sources.  But that means we have to remove the
# directory prefixes and all the system include files.
# And <kpathsea/paths.h> is generated, not part of the distribution.
# 
# And, there's no need for any installer/user to ever run this, it can
# only cause trouble. So comment it out in the distribution.
# (It doesn't work when the source and build directories are different.)
##ifndef c_auto_h_dir
##c_auto_h_dir = .
##endif
##ifdef HOSTNAME
##depend depend.make:: $(c_auto_h_dir)/c-auto.h \
##  $(top_srcdir)/../make/rdepend.make 
##	$(CC) -M $(ALL_CPPFLAGS) -I$(c_auto_h_dir) *.c \
##	  | sed -e 's,\(\.\./\)\+kpathsea/,$$(kpathsea_srcdir)/,g' \
##	        -e 's,$$(kpathsea_srcdir)/paths.h,$$(kpathsea_dir)/paths.h,g' \
##	        -e 's,/usr[^ ]* ,,g' \
##	        -e 's,/usr[^ ]*$$,,g' \
##	        -e 's,dvi2xx.o,dvilj.o dvilj2p.o dvilj4.o dvilj4l.o,' \
##	  | grep -v '^ *\\$$' \
##	  >depend.make
### If kpathsea, we're making .lo library objects instead of .o's.
##	pwd | grep -v kpathsea >/dev/null \
##	  || (sed -e 's/\.o:/.lo:/' -e 's/kpsewhich.lo:/kpsewhich.o:/' \
##	      <depend.make >depend-tmp.make; mv depend-tmp.make depend.make)
##.PHONY: depend
##endif

# Let's stick a rule for TAGS here, just in case someone wants them.
# (We don't put them in the distributions, to keep them smaller.)
TAGS: *.c *.h
	pwd | grep kpathsea >/dev/null && append=../kpathsea/TAGS; \
	  etags $$append *.[ch]
	

# Prevent GNU make 3.[59,63) from overflowing arg limit on system V.
.NOEXPORT:

# End of rdepend.make.
