# config.make -- autoconf rules to remake the Makefile, c-auto.h, etc.

config.status: $(srcdir)/configure
	$(SHELL) $(srcdir)/configure --no-create $(enablemaintflag)

Makefile: $(srcdir)/Makefile.in config.status $(top_srcdir)/../make/*.make
	$(SHELL) config.status

# This rule isn't used for the top-level Makefile, but it doesn't hurt.
# We don't depend on config.status because configure always rewrites
# config.status, even when it doesn't change. Thus it might be newer
# than c-auto.h when we don't need to remake the latter.
c-auto.h: stamp-auto
stamp-auto: $(srcdir)/c-auto.in
	$(SHELL) config.status
	date >stamp-auto


# End of config.make.
