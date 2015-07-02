examples_data_SRC = \
   examples/data/penny.mat

octdata_DATA += \
  $(examples_data_SRC)

EXTRA_DIST += \
  $(examples_data_SRC) \
  examples/code/@FIRfilter/FIRfilter.m \
  examples/code/@FIRfilter/FIRfilter_aggregation.m \
  examples/code/@FIRfilter/display.m \
  examples/code/@FIRfilter/subsasgn.m \
  examples/code/@FIRfilter/subsref.m \
  examples/code/@polynomial/display.m \
  examples/code/@polynomial/double.m \
  examples/code/@polynomial/end.m \
  examples/code/@polynomial/get.m \
  examples/code/@polynomial/mtimes.m \
  examples/code/@polynomial/numel.m \
  examples/code/@polynomial/plot.m \
  examples/code/@polynomial/polynomial.m \
  examples/code/@polynomial/polynomial_superiorto.m \
  examples/code/@polynomial/polyval.m \
  examples/code/@polynomial/roots.m \
  examples/code/@polynomial/set.m \
  examples/code/@polynomial/subsasgn.m \
  examples/code/@polynomial/subsref.m \
  examples/code/addtwomatrices.cc \
  examples/code/celldemo.cc \
  examples/code/embedded.cc \
  examples/code/fortrandemo.cc \
  examples/code/fortransub.f \
  examples/code/funcdemo.cc \
  examples/code/globaldemo.cc \
  examples/code/helloworld.cc \
  examples/code/make_int.cc \
  examples/code/mex_demo.c \
  examples/code/mycell.c \
  examples/code/myfeval.c \
  examples/code/myfevalf.f \
  examples/code/myfunc.c \
  examples/code/myhello.c \
  examples/code/mypow2.c \
  examples/code/myprop.c \
  examples/code/myset.c \
  examples/code/mysparse.c \
  examples/code/mystring.c \
  examples/code/mystruct.c \
  examples/code/oct_demo.cc \
  examples/code/oregonator.cc \
  examples/code/oregonator.m \
  examples/code/paramdemo.cc \
  examples/code/standalone.cc \
  examples/code/standalonebuiltin.cc \
  examples/code/stringdemo.cc \
  examples/code/structdemo.cc \
  examples/code/unwinddemo.cc \
  examples/module.mk
