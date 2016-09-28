CRUFT_SOURCES += \
  liboctave/cruft/misc/blaswrap.c \
  liboctave/cruft/misc/cquit.c \
  liboctave/cruft/misc/d1mach.f \
  liboctave/cruft/misc/f77-extern.cc \
  liboctave/cruft/misc/f77-fcn.c \
  liboctave/cruft/misc/i1mach.f \
  liboctave/cruft/misc/lo-error.c \
  liboctave/cruft/misc/quit.cc \
  liboctave/cruft/misc/r1mach.f

CRUFT_INC += \
  liboctave/cruft/misc/f77-fcn.h \
  liboctave/cruft/misc/lo-error.h \
  liboctave/cruft/misc/quit.h

liboctave_EXTRA_DIST += \
  liboctave/cruft/misc/d1mach-tst.for
