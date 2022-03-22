OV_INTTYPE_INC = \
  %reldir%/ov-base-int.h \
  %reldir%/ov-int-traits.h \
  %reldir%/ov-int16.h \
  %reldir%/ov-int32.h \
  %reldir%/ov-int64.h \
  %reldir%/ov-int8.h \
  %reldir%/ov-intx.h \
  %reldir%/ov-uint16.h \
  %reldir%/ov-uint32.h \
  %reldir%/ov-uint64.h \
  %reldir%/ov-uint8.h

OV_SPARSE_INC = \
  %reldir%/ov-base-sparse.h \
  %reldir%/ov-bool-sparse.h \
  %reldir%/ov-cx-sparse.h \
  %reldir%/ov-re-sparse.h

OCTAVE_VALUE_INC = \
  %reldir%/cdef-class.h \
  %reldir%/cdef-fwd.h \
  %reldir%/cdef-manager.h \
  %reldir%/cdef-method.h \
  %reldir%/cdef-object.h \
  %reldir%/cdef-package.h \
  %reldir%/cdef-property.h \
  %reldir%/cdef-utils.h \
  %reldir%/ov-base-diag.h \
  %reldir%/ov-base-mat.h \
  %reldir%/ov-base-scalar.h \
  %reldir%/ov-base.h \
  %reldir%/ov-bool-mat.h \
  %reldir%/ov-bool.h \
  %reldir%/ov-builtin.h \
  %reldir%/ov-cell.h \
  %reldir%/ov-ch-mat.h \
  %reldir%/ov-class.h \
  %reldir%/ov-classdef.h \
  %reldir%/ov-colon.h \
  %reldir%/ov-complex.h \
  %reldir%/ov-cs-list.h \
  %reldir%/ov-cx-diag.h \
  %reldir%/ov-cx-mat.h \
  %reldir%/ov-dld-fcn.h \
  %reldir%/ov-fcn-handle.h \
  %reldir%/ov-fcn.h \
  %reldir%/ov-float.h \
  %reldir%/ov-flt-complex.h \
  %reldir%/ov-flt-cx-diag.h \
  %reldir%/ov-flt-cx-mat.h \
  %reldir%/ov-flt-re-diag.h \
  %reldir%/ov-flt-re-mat.h \
  %reldir%/ov-java.h \
  %reldir%/ov-lazy-idx.h \
  %reldir%/ov-legacy-range.h \
  %reldir%/ov-magic-int.h \
  %reldir%/ov-mex-fcn.h \
  %reldir%/ov-null-mat.h \
  %reldir%/ov-oncleanup.h \
  %reldir%/ov-perm.h \
  %reldir%/ov-range-traits.h \
  %reldir%/ov-range.h \
  %reldir%/ov-re-diag.h \
  %reldir%/ov-re-mat.h \
  %reldir%/ov-scalar.h \
  %reldir%/ov-str-mat.h \
  %reldir%/ov-struct.h \
  %reldir%/ov-typeinfo.h \
  %reldir%/ov-usr-fcn.h \
  %reldir%/ov.h \
  %reldir%/ovl.h \
  $(OV_INTTYPE_INC) \
  $(OV_SPARSE_INC)

OV_INTTYPE_SRC = \
  %reldir%/ov-int16.cc \
  %reldir%/ov-int32.cc \
  %reldir%/ov-int64.cc \
  %reldir%/ov-int8.cc \
  %reldir%/ov-uint16.cc \
  %reldir%/ov-uint32.cc \
  %reldir%/ov-uint64.cc \
  %reldir%/ov-uint8.cc

OV_SPARSE_SRC = \
  %reldir%/ov-bool-sparse.cc \
  %reldir%/ov-cx-sparse.cc \
  %reldir%/ov-re-sparse.cc

OCTAVE_VALUE_SRC = \
  %reldir%/cdef-class.cc \
  %reldir%/cdef-manager.cc \
  %reldir%/cdef-method.cc \
  %reldir%/cdef-object.cc \
  %reldir%/cdef-package.cc \
  %reldir%/cdef-property.cc \
  %reldir%/cdef-utils.cc \
  %reldir%/ov-base.cc \
  %reldir%/ov-bool-mat.cc \
  %reldir%/ov-bool.cc \
  %reldir%/ov-builtin.cc \
  %reldir%/ov-cell.cc \
  %reldir%/ov-ch-mat.cc \
  %reldir%/ov-class.cc \
  %reldir%/ov-classdef.cc \
  %reldir%/ov-colon.cc \
  %reldir%/ov-complex.cc \
  %reldir%/ov-cs-list.cc \
  %reldir%/ov-cx-diag.cc \
  %reldir%/ov-cx-mat.cc \
  %reldir%/ov-dld-fcn.cc \
  %reldir%/ov-fcn-handle.cc \
  %reldir%/ov-fcn.cc \
  %reldir%/ov-float.cc \
  %reldir%/ov-flt-complex.cc \
  %reldir%/ov-flt-cx-diag.cc \
  %reldir%/ov-flt-cx-mat.cc \
  %reldir%/ov-flt-re-diag.cc \
  %reldir%/ov-flt-re-mat.cc \
  %reldir%/ov-java.cc \
  %reldir%/ov-lazy-idx.cc \
  %reldir%/ov-legacy-range.cc \
  %reldir%/ov-magic-int.cc \
  %reldir%/ov-mex-fcn.cc \
  %reldir%/ov-null-mat.cc \
  %reldir%/ov-oncleanup.cc \
  %reldir%/ov-perm.cc \
  %reldir%/ov-range.cc \
  %reldir%/ov-re-diag.cc \
  %reldir%/ov-re-mat.cc \
  %reldir%/ov-scalar.cc \
  %reldir%/ov-str-mat.cc \
  %reldir%/ov-struct.cc \
  %reldir%/ov-typeinfo.cc \
  %reldir%/ov-usr-fcn.cc \
  %reldir%/ov.cc \
  %reldir%/ovl.cc \
  $(OV_INTTYPE_SRC) \
  $(OV_SPARSE_SRC)

## These source files contain template definitions that are included
## in other files.  They are not supposed to be compiled separately,
## so they should not appear in the OCTAVE_VALUE_SRC list.

libinterp_EXTRA_DIST += \
  %reldir%/ov-base-diag.cc \
  %reldir%/ov-base-int.cc \
  %reldir%/ov-base-mat.cc \
  %reldir%/ov-base-scalar.cc \
  %reldir%/ov-base-sparse.cc

noinst_LTLIBRARIES += %reldir%/liboctave-value.la

%canon_reldir%_liboctave_value_la_SOURCES = $(OCTAVE_VALUE_SRC)

## FIXME: maybe it would be better to limit the JAVA flags to
## the compile commands for ov-java.cc?  Does JAVA_LIBS need to be
## added to LIBOCTINTERP_LINK_DEPS (see libinterp/link-deps.mk)?
## Should we have a separate set of JAVA_LDFLAGS?

%canon_reldir%_liboctave_value_la_CPPFLAGS = \
  $(libinterp_liboctinterp_la_CPPFLAGS) \
  $(HDF5_CPPFLAGS) \
  $(JAVA_CPPFLAGS)

%canon_reldir%_liboctave_value_la_LIBADD = $(JAVA_LIBS)
