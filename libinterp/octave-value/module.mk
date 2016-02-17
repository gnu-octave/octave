OV_INTTYPE_INC = \
  libinterp/octave-value/ov-base-int.h \
  libinterp/octave-value/ov-base-int.cc \
  libinterp/octave-value/ov-int-traits.h \
  libinterp/octave-value/ov-int16.h \
  libinterp/octave-value/ov-int32.h \
  libinterp/octave-value/ov-int64.h \
  libinterp/octave-value/ov-int8.h \
  libinterp/octave-value/ov-intx.h \
  libinterp/octave-value/ov-uint16.h \
  libinterp/octave-value/ov-uint32.h \
  libinterp/octave-value/ov-uint64.h \
  libinterp/octave-value/ov-uint8.h

OV_SPARSE_INC = \
  libinterp/octave-value/ov-base-sparse.h \
  libinterp/octave-value/ov-bool-sparse.h \
  libinterp/octave-value/ov-cx-sparse.h \
  libinterp/octave-value/ov-re-sparse.h

OCTAVE_VALUE_INC = \
  libinterp/octave-value/ov-base-diag.h \
  libinterp/octave-value/ov-base-diag.cc \
  libinterp/octave-value/ov-base-mat.h \
  libinterp/octave-value/ov-base-mat.cc \
  libinterp/octave-value/ov-base-scalar.h \
  libinterp/octave-value/ov-base-scalar.cc \
  libinterp/octave-value/ov-base.h \
  libinterp/octave-value/ov-bool-mat.h \
  libinterp/octave-value/ov-bool.h \
  libinterp/octave-value/ov-builtin.h \
  libinterp/octave-value/ov-cell.h \
  libinterp/octave-value/ov-ch-mat.h \
  libinterp/octave-value/ov-class.h \
  libinterp/octave-value/ov-classdef.h \
  libinterp/octave-value/ov-colon.h \
  libinterp/octave-value/ov-complex.h \
  libinterp/octave-value/ov-cs-list.h \
  libinterp/octave-value/ov-cx-diag.h \
  libinterp/octave-value/ov-cx-mat.h \
  libinterp/octave-value/ov-dld-fcn.h \
  libinterp/octave-value/ov-fcn-handle.h \
  libinterp/octave-value/ov-fcn-inline.h \
  libinterp/octave-value/ov-fcn.h \
  libinterp/octave-value/ov-float.h \
  libinterp/octave-value/ov-flt-complex.h \
  libinterp/octave-value/ov-flt-cx-diag.h \
  libinterp/octave-value/ov-flt-cx-mat.h \
  libinterp/octave-value/ov-flt-re-diag.h \
  libinterp/octave-value/ov-flt-re-mat.h \
  libinterp/octave-value/ov-java.h \
  libinterp/octave-value/ov-lazy-idx.h \
  libinterp/octave-value/ov-mex-fcn.h \
  libinterp/octave-value/ov-null-mat.h \
  libinterp/octave-value/ov-oncleanup.h \
  libinterp/octave-value/ov-perm.h \
  libinterp/octave-value/ov-range.h \
  libinterp/octave-value/ov-re-diag.h \
  libinterp/octave-value/ov-re-mat.h \
  libinterp/octave-value/ov-scalar.h \
  libinterp/octave-value/ov-str-mat.h \
  libinterp/octave-value/ov-struct.h \
  libinterp/octave-value/ov-type-conv.h \
  libinterp/octave-value/ov-typeinfo.h \
  libinterp/octave-value/ov-usr-fcn.h \
  libinterp/octave-value/ov.h \
  libinterp/octave-value/ovl.h \
  $(OV_INTTYPE_INC) \
  $(OV_SPARSE_INC)

OV_INTTYPE_SRC = \
  libinterp/octave-value/ov-int16.cc \
  libinterp/octave-value/ov-int32.cc \
  libinterp/octave-value/ov-int64.cc \
  libinterp/octave-value/ov-int8.cc \
  libinterp/octave-value/ov-uint16.cc \
  libinterp/octave-value/ov-uint32.cc \
  libinterp/octave-value/ov-uint64.cc \
  libinterp/octave-value/ov-uint8.cc

OV_SPARSE_SRC = \
  libinterp/octave-value/ov-base-sparse.cc \
  libinterp/octave-value/ov-bool-sparse.cc \
  libinterp/octave-value/ov-cx-sparse.cc \
  libinterp/octave-value/ov-re-sparse.cc

OCTAVE_VALUE_SRC = \
  libinterp/octave-value/ov-base.cc \
  libinterp/octave-value/ov-bool-mat.cc \
  libinterp/octave-value/ov-bool.cc \
  libinterp/octave-value/ov-builtin.cc \
  libinterp/octave-value/ov-cell.cc \
  libinterp/octave-value/ov-ch-mat.cc \
  libinterp/octave-value/ov-class.cc \
  libinterp/octave-value/ov-classdef.cc \
  libinterp/octave-value/ov-colon.cc \
  libinterp/octave-value/ov-complex.cc \
  libinterp/octave-value/ov-cs-list.cc \
  libinterp/octave-value/ov-cx-diag.cc \
  libinterp/octave-value/ov-cx-mat.cc \
  libinterp/octave-value/ov-dld-fcn.cc \
  libinterp/octave-value/ov-fcn-handle.cc \
  libinterp/octave-value/ov-fcn-inline.cc \
  libinterp/octave-value/ov-fcn.cc \
  libinterp/octave-value/ov-float.cc \
  libinterp/octave-value/ov-flt-complex.cc \
  libinterp/octave-value/ov-flt-cx-diag.cc \
  libinterp/octave-value/ov-flt-cx-mat.cc \
  libinterp/octave-value/ov-flt-re-diag.cc \
  libinterp/octave-value/ov-flt-re-mat.cc \
  libinterp/octave-value/ov-java.cc \
  libinterp/octave-value/ov-lazy-idx.cc \
  libinterp/octave-value/ov-mex-fcn.cc \
  libinterp/octave-value/ov-null-mat.cc \
  libinterp/octave-value/ov-oncleanup.cc \
  libinterp/octave-value/ov-perm.cc \
  libinterp/octave-value/ov-range.cc \
  libinterp/octave-value/ov-re-diag.cc \
  libinterp/octave-value/ov-re-mat.cc \
  libinterp/octave-value/ov-scalar.cc \
  libinterp/octave-value/ov-str-mat.cc \
  libinterp/octave-value/ov-struct.cc \
  libinterp/octave-value/ov-typeinfo.cc \
  libinterp/octave-value/ov-usr-fcn.cc \
  libinterp/octave-value/ov.cc \
  libinterp/octave-value/ovl.cc \
  $(OV_INTTYPE_SRC) \
  $(OV_SPARSE_SRC)

OV_JAVA_DF = \
  libinterp/octave-value/ov.df \
  libinterp/octave-value/ov-class.df \
  libinterp/octave-value/ov-java.df \
  libinterp/octave-value/ov-typeinfo.df

## Special rules for Java .df files so that not all .df files are built with
## JAVA_CPPFLAGS
$(OV_JAVA_DF) : libinterp/octave-value/%.df : libinterp/octave-value/%.cc $(GENERATED_MAKE_BUILTINS_INCS) octave-config.h
	$(AM_V_GEN)rm -f $@-t $@-t1 $@ && \
	$(CXXCPP) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) \
	  $(libinterp_octave_value_liboctave_value_la_CPPFLAGS) $(JAVA_CPPFLAGS) $(CPPFLAGS) \
	  $(libinterp_octave_value_liboctave_value_la_CXXFLAGS) $(CXXFLAGS) \
	  -DMAKE_BUILTINS $< > $@-t1 && \
	$(SHELL) $(srcdir)/libinterp/mkdefs $(srcdir)/libinterp $< < $@-t1 > $@-t && \
	mv $@-t $@ && \
	rm -f $@-t1

noinst_LTLIBRARIES += libinterp/octave-value/liboctave-value.la

libinterp_octave_value_liboctave_value_la_SOURCES = $(OCTAVE_VALUE_SRC)

## FIXME: maybe it would be better to limit the JAVA flags to
## the compile commands for ov-java.cc?  Does JAVA_LIBS need to be
## added to LIBOCTINTERP_LINK_DEPS (see libinterp/link-deps.mk)?
## Should we have a separate set of JAVA_LDFLAGS?

libinterp_octave_value_liboctave_value_la_CPPFLAGS = \
  $(libinterp_liboctinterp_la_CPPFLAGS) \
  $(HDF5_CPPFLAGS) \
  $(JAVA_CPPFLAGS)

libinterp_octave_value_liboctave_value_la_CFLAGS = $(AM_CFLAGS) $(WARN_CFLAGS)

libinterp_octave_value_liboctave_value_la_CXXFLAGS = $(AM_CXXFLAGS) $(WARN_CXXFLAGS)

libinterp_octave_value_liboctave_value_la_LIBADD = $(JAVA_LIBS)
