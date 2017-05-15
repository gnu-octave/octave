%canon_reldir%_EXTRA_DIST =

%canon_reldir%_CLEANFILES =
%canon_reldir%_DISTCLEANFILES =
%canon_reldir%_MAINTAINERCLEANFILES =

%canon_reldir%_data_SRC = \
   %reldir%/data/penny.mat

octdata_DATA += \
  $(%canon_reldir%_data_SRC)

%canon_reldir%_code_SRC = \
  %reldir%/code/@FIRfilter/FIRfilter.m \
  %reldir%/code/@FIRfilter/FIRfilter_aggregation.m \
  %reldir%/code/@FIRfilter/display.m \
  %reldir%/code/@FIRfilter/subsasgn.m \
  %reldir%/code/@FIRfilter/subsref.m \
  %reldir%/code/@polynomial/disp.m \
  %reldir%/code/@polynomial/double.m \
  %reldir%/code/@polynomial/end.m \
  %reldir%/code/@polynomial/get.m \
  %reldir%/code/@polynomial/mtimes.m \
  %reldir%/code/@polynomial/numel.m \
  %reldir%/code/@polynomial/plot.m \
  %reldir%/code/@polynomial/polynomial.m \
  %reldir%/code/@polynomial/polynomial_superiorto.m \
  %reldir%/code/@polynomial/polyval.m \
  %reldir%/code/@polynomial/roots.m \
  %reldir%/code/@polynomial/set.m \
  %reldir%/code/@polynomial/subsasgn.m \
  %reldir%/code/@polynomial/subsref.m \
  %reldir%/code/addtwomatrices.cc \
  %reldir%/code/celldemo.cc \
  %reldir%/code/embedded.cc \
  %reldir%/code/fortrandemo.cc \
  %reldir%/code/fortransub.f \
  %reldir%/code/funcdemo.cc \
  %reldir%/code/globaldemo.cc \
  %reldir%/code/helloworld.cc \
  %reldir%/code/make_int.cc \
  %reldir%/code/mex_demo.c \
  %reldir%/code/mycell.c \
  %reldir%/code/myfeval.c \
  %reldir%/code/myfevalf.f \
  %reldir%/code/myfunc.c \
  %reldir%/code/myhello.c \
  %reldir%/code/mypow2.c \
  %reldir%/code/myprop.c \
  %reldir%/code/myset.c \
  %reldir%/code/mysparse.c \
  %reldir%/code/mystring.c \
  %reldir%/code/mystruct.c \
  %reldir%/code/oct_demo.cc \
  %reldir%/code/oregonator.cc \
  %reldir%/code/oregonator.m \
  %reldir%/code/paramdemo.cc \
  %reldir%/code/polynomial2.m \
  %reldir%/code/standalone.cc \
  %reldir%/code/standalonebuiltin.cc \
  %reldir%/code/stringdemo.cc \
  %reldir%/code/structdemo.cc \
  %reldir%/code/unwinddemo.cc

%canon_reldir%_EXTRA_DIST += \
  $(%canon_reldir%_data_SRC) \
  $(%canon_reldir%_code_SRC) \
  %reldir%/code/COPYING \
  %reldir%/module.mk

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
