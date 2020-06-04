%canon_reldir%_EXTRA_DIST =

%canon_reldir%_CLEANFILES =
%canon_reldir%_DISTCLEANFILES =
%canon_reldir%_MAINTAINERCLEANFILES =

%canon_reldir%_SRC = \
  %reldir%/@FIRfilter/FIRfilter.m \
  %reldir%/@FIRfilter/FIRfilter_aggregation.m \
  %reldir%/@FIRfilter/display.m \
  %reldir%/@FIRfilter/subsasgn.m \
  %reldir%/@FIRfilter/subsref.m \
  %reldir%/@polynomial/disp.m \
  %reldir%/@polynomial/double.m \
  %reldir%/@polynomial/end.m \
  %reldir%/@polynomial/get.m \
  %reldir%/@polynomial/mtimes.m \
  %reldir%/@polynomial/numel.m \
  %reldir%/@polynomial/plot.m \
  %reldir%/@polynomial/polynomial.m \
  %reldir%/@polynomial/polynomial_superiorto.m \
  %reldir%/@polynomial/polyval.m \
  %reldir%/@polynomial/roots.m \
  %reldir%/@polynomial/set.m \
  %reldir%/@polynomial/subsasgn.m \
  %reldir%/@polynomial/subsref.m \
  %reldir%/addtwomatrices.cc \
  %reldir%/celldemo.cc \
  %reldir%/embedded.cc \
  %reldir%/fortrandemo.cc \
  %reldir%/fortransub.f \
  %reldir%/funcdemo.cc \
  %reldir%/globaldemo.cc \
  %reldir%/helloworld.cc \
  %reldir%/make_int.cc \
  %reldir%/mex_demo.c \
  %reldir%/mycell.c \
  %reldir%/myfeval.c \
  %reldir%/myfevalf.f \
  %reldir%/myfunc.c \
  %reldir%/myhello.c \
  %reldir%/mypow2.c \
  %reldir%/myprop.c \
  %reldir%/myset.c \
  %reldir%/mysparse.c \
  %reldir%/mystring.c \
  %reldir%/mystruct.c \
  %reldir%/oct_demo.cc \
  %reldir%/oregonator.cc \
  %reldir%/oregonator.m \
  %reldir%/paramdemo.cc \
  %reldir%/polynomial2.m \
  %reldir%/standalone.cc \
  %reldir%/standalonebuiltin.cc \
  %reldir%/stringdemo.cc \
  %reldir%/structdemo.cc \
  %reldir%/unwinddemo.cc

%canon_reldir%_EXTRA_DIST += \
  $(%canon_reldir%_SRC)

EXTRA_DIST += $(%canon_reldir%_EXTRA_DIST)

CLEANFILES += $(%canon_reldir%_CLEANFILES)
DISTCLEANFILES += $(%canon_reldir%_DISTCLEANFILES)
MAINTAINERCLEANFILES += $(%canon_reldir%_MAINTAINERCLEANFILES)

examples-clean:
	rm -f $(%canon_reldir%_CLEANFILES)

examples-distclean: examples-clean
	rm -f $(%canon_reldir%_DISTCLEANFILES)

examples-maintainer-clean: examples-distclean
	rm -f $(%canon_reldir%_MAINTAINERCLEANFILES)
