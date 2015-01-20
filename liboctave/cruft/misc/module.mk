EXTRA_DIST += \
  misc/module.mk \
  misc/d1mach-tst.for

libcruft_la_SOURCES += \
  misc/blaswrap.c \
  misc/cquit.c \
  misc/d1mach.f \
  misc/f77-extern.cc \
  misc/f77-fcn.c \
  misc/i1mach.f \
  misc/lo-error.c \
  misc/quit.cc \
  misc/r1mach.f

octinclude_HEADERS += \
  misc/f77-fcn.h \
  misc/lo-error.h \
  misc/quit.h
