# dist.make -- making distribution tar files.
top_distdir = $(distname)-$(version)
top_files = ChangeLog Makefile.in README configure.in configure install-sh \
  acklibtool.m4 config.guess config.sub klibtool $(HOME)/bin/mkdirchain
distdir = $(top_distdir)/$(distname)
kpathsea_distdir = ../$(distname)/$(top_distdir)/kpathsea
ln_files = AUTHORS ChangeLog INSTALL NEWS README *.in *.h *.c \
  configure *.make .gdbinit stamp-auto

dist_rm_predicate = -name depend.make -o -name Makefile
dist: all depend pre-dist-$(distname)
	rm -rf $(top_distdir)*
	mkdir -p $(distdir)
	cd .. && make Makefile ./configure
	cd .. && ln $(top_files) $(distname)/$(top_distdir)
	cp $(txinfo)/dir $(top_distdir)
	-ln $(ln_files) $(distdir)
	ln $(program_files) $(distdir)
	cd $(kpathsea_dir); $(MAKE) distdir=$(kpathsea_distdir) \
	  ln_files='$(ln_files)' distdir
	cp -pr ../make ../etc $(top_distdir)
	ln -s $(gnu)/share/autoconf/acsite.m4 $(top_distdir)/etc/autoconf
	rm -rf $(top_distdir)/make/RCS
	ungnumake `find $(top_distdir) -name Makefile.in -o -name \*.make`
# Remove the extra files our patterns got us.
	cd $(top_distdir); rm -f */c-auto.h
	find $(top_distdir) \( $(dist_rm_predicate) \) -exec rm '{}' \;
	find $(top_distdir) -name \.*texi -exec egrep -ni '	| ::|xx[^}]' \;
	$(MAKE) post-dist-$(distname)
	cd $(distdir); add-version $(version) $(version_files)
	cd $(distdir); test ! -r *.info || touch *.info*
	chmod -R a+rwX $(top_distdir)
	GZIP=-9 tar chzf $(top_distdir).tar.gz $(top_distdir)
	rm -rf $(top_distdir)

# End of dist.make.
