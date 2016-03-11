export AWK
export GREP
export FIND
export SED
export SHELL
export PERL

version := ${OCTAVE_VERSION}
api_version := ${OCTAVE_API_VERSION}

## AM_LIBTOOLFLAGS = --silent

AM_LFLAGS = @LFLAGS@

AM_YFLAGS = -dv

# FIXME: This seems unnecessary as of 3/10/2016.
# Commenting out definition and re-configuring made no change to Makefile
SHLLINKEXT =

# Fortran compiler flags.

AM_FFLAGS = @FFLAGS@

# C compiler flags.

AM_CFLAGS = ${XTRA_CFLAGS}

# ifeq (${INCLUDE_DEPS},no)
#   omit_deps = true;
# endif

# C++ compiler flags.

AM_CXXFLAGS = ${XTRA_CXXFLAGS}

FFTW_XCPPFLAGS = @FFTW_XCPPFLAGS@
FFTW_XLDFLAGS = @FFTW_XLDFLAGS@
FFTW_XLIBS = @FFTW_XLIBS@

SPARSE_XCPPFLAGS = @SPARSE_XCPPFLAGS@
SPARSE_XLDFLAGS = @SPARSE_XLDFLAGS@
SPARSE_XLIBS = @SPARSE_XLIBS@

GNULIB_LINK_DEPS = @GNULIB_LINK_DEPS@

LIBOCTAVE_LINK_DEPS = @LIBOCTAVE_LINK_DEPS@
LIBOCTAVE_LINK_OPTS = @LIBOCTAVE_LINK_OPTS@

LIBOCTINTERP_LINK_DEPS = @LIBOCTINTERP_LINK_DEPS@
LIBOCTINTERP_LINK_OPTS = @LIBOCTINTERP_LINK_OPTS@

OCTAVE_LINK_DEPS = @OCTAVE_LINK_DEPS@
OCTAVE_LINK_OPTS = @OCTAVE_LINK_OPTS@

OCT_LINK_DEPS = @OCT_LINK_DEPS@
OCT_LINK_OPTS = @OCT_LINK_OPTS@

LIBOCTGUI_LINK_DEPS = @LIBOCTGUI_LINK_DEPS@
LIBOCTGUI_LINK_OPTS = @LIBOCTGUI_LINK_OPTS@

OCTAVE_GUI_LINK_DEPS = @OCTAVE_GUI_LINK_DEPS@
OCTAVE_GUI_LINK_OPTS = @OCTAVE_GUI_LINK_OPTS@

# The arguments passed to configure.

CONFIG_SUBDIRS = @subdirs@

null =
ldpreloadsep = ${null}@ldpreloadsep@${null}

# ==================== Octave-specific Makefile rules ====================

# The following pattern rules and the substitution functions require
# GNU make.  If you don't have it, get it!

define simple_move_if_change_rule
  if [ -s $@-t ]; then \
    ${SHELL} ${top_srcdir}/build-aux/move-if-change $@-t $@; \
  else \
    echo "$@-t is empty!" 1>&2; \
    rm -f $@-t; \
    exit 1; \
  fi
endef

define cp_update_rule
  if [ "x${srcdir}" != "x." ] && [ -f ${srcdir}/$@ ] && [ ! -f $@ ]; then \
    cp ${srcdir}/$@ $@; \
    touch -r ${srcdir}/$@ $@; \
  fi
endef

define simple-filter-rule
  rm -f $@-t $@ && \
  ${SHELL} $(1) < $< > $@-t && \
  mv $@-t $@
endef

define subst-bison-api-decls
  case "${BISON_API_PREFIX_DECL_STYLE}" in \
    *api*) \
      case "${BISON_API_PREFIX_DECL_STYLE}" in \
       *brace*) \
         api_prefix_decl='%define api.prefix {$(1)}'; ;; \
       *) \
         api_prefix_decl='%define api.prefix "$(1)"'; ;; \
       esac; \
      ;; \
    *name*) \
      case "${BISON_API_PREFIX_DECL_STYLE}" in \
        *brace*) \
          api_prefix_decl='%name-prefix {$(1)}'; ;; \
        *) \
          api_prefix_decl='%name-prefix="$(1)"'; ;; \
      esac; \
    ;; \
  esac; \
  case "${BISON_PUSH_PULL_DECL_STYLE}" in \
    *quote*) quote='"' ;; \
    *) quote="" ;; \
  esac; \
  case "${BISON_PUSH_PULL_DECL_STYLE}" in \
    *dash*) push_pull_decl="%define api.push-pull $${quote}both$${quote}"; ;; \
    *underscore*) push_pull_decl="%define api.push_pull $${quote}both$${quote}"; ;; \
  esac; \
  ${SED} -e "s/%PUSH_PULL_DECL%/$$push_pull_decl/" \
         -e "s/%API_PREFIX_DECL%/$$api_prefix_decl/" $< > $@-t && \
  mv $@-t $@
endef

define gdbinit_install_rule
  if [ -f $@ ]; then \
    echo "refusing to overwrite $@ with newer version from $<" 1>&2; \
  else \
    echo "Installing $@ from version at $<" ; \
    cp $< $@; \
  fi
endef

define test-file-commands
  rm -f $@-t $@ && \
  ( echo "## DO NOT EDIT!  Generated automatically from $(<F) by Make."; \
    $(GREP) '^%!' $< \
  ) > $@-t && \
  mv $@-t $@
endef

%.cc-tst : %.cc
	$(AM_V_GEN)$(test-file-commands)

%.yy-tst : %.yy
	$(AM_V_GEN)$(test-file-commands)

%.ll-tst : %.ll
	$(AM_V_GEN)$(test-file-commands)
