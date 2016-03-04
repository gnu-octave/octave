export AWK
export GREP
export FIND
export SED
export SHELL
export PERL

version = ${OCTAVE_VERSION}
api_version = ${OCTAVE_API_VERSION}

## AM_LIBTOOLFLAGS = --silent

AM_LFLAGS = @LFLAGS@

AM_YFLAGS = -dv

SHLLINKEXT=

# Fortran compiler flags.

AM_FFLAGS = @FFLAGS@

# C compiler flags.

AM_CFLAGS = ${XTRA_CFLAGS}

# ifeq (${INCLUDE_DEPS},no)
#   omit_deps = true;
# endif

# C++ compiler flags.

AM_CXXFLAGS = ${XTRA_CXXFLAGS}

FFTW_XCPPFLAGS = ${FFTW3_CPPFLAGS} ${FFTW3F_CPPFLAGS}
FFTW_XLDFLAGS = ${FFTW3_LDFLAGS} ${FFTW3F_LDFLAGS}
FFTW_XLIBS = ${FFTW3_LIBS} ${FFTW3F_LIBS}

SPARSE_XCPPFLAGS = \
  ${CHOLMOD_CPPFLAGS} ${UMFPACK_CPPFLAGS} \
  ${AMD_CPPFLAGS} ${CAMD_CPPFLAGS} ${COLAMD_CPPFLAGS} \
  ${CCOLAMD_CPPFLAGS} ${CXSPARSE_CPPFLAGS}

SPARSE_XLDFLAGS = \
  ${CHOLMOD_LDFLAGS} ${UMFPACK_LDFLAGS} \
  ${AMD_LDFLAGS} ${CAMD_LDFLAGS} ${COLAMD_LDFLAGS} \
  ${CCOLAMD_LDFLAGS} ${CXSPARSE_LDFLAGS}

## Order matters, at least on some systems (Cygwin, for example).
SPARSE_XLIBS = \
  ${CHOLMOD_LIBS} ${UMFPACK_LIBS} \
  ${AMD_LIBS} ${CAMD_LIBS} ${COLAMD_LIBS} \
  ${CCOLAMD_LIBS} ${CXSPARSE_LIBS}

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
