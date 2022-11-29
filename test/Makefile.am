BUILT_SOURCES =
DIRSTAMP_FILES =
EXTRA_DIST =
CLEANFILES =
DISTCLEANFILES =
MAINTAINERCLEANFILES =
TEST_FILES =
noinst_TEST_FILES =

octave_dirstamp = $(am__leading_dot)octave-dirstamp

DIRSTAMP_FILES += $(octave_dirstamp)

$(DIRSTAMP_FILES):
	$(AM_V_GEN)$(MKDIR_P) $(@D) && \
	: > $@

TEST_FILES += \
  fntests.m \
  args.tst \
  bug-31371.tst \
  bug-40117.tst \
  bug-45969.tst \
  bug-45972.tst \
  bug-46330.tst \
  bug-49904.tst \
  bug-53579.tst \
  bug-53599.tst \
  bug-54490.tst \
  bug-55308.tst \
  bug-55321.tst \
  bug-55322.tst \
  bug-59950.tst \
  bug-61201.tst \
  colormaps.tst \
  command.tst \
  complex.tst \
  deprecate-props.tst \
  diag-perm.tst \
  error.tst \
  eval-catch.tst \
  eval-command.tst \
  for.tst \
  func.tst \
  global.tst \
  if.tst \
  index.tst \
  inline-fcn.tst \
  integer.tst \
  io.tst \
  leftdiv.tst \
  line-continue.tst \
  logical-index.tst \
  null-assign.tst \
  parser.tst \
  prefer.tst \
  range.tst \
  recursion.tst \
  return.tst \
  single-index.tst \
  slice.tst \
  sparse-assign.tst \
  struct.tst \
  switch.tst \
  system.tst \
  transpose.tst \
  try.tst \
  unwind.tst \
  while.tst

include bug-35448/module.mk
include bug-35881/module.mk
include bug-36025/module.mk
include bug-38236/module.mk
include bug-38691/module.mk
include bug-41723/module.mk
include bug-44940/module.mk
include bug-45351/module.mk
include bug-46660/module.mk
include bug-47680/module.mk
include bug-49379/module.mk
include bug-50014/module.mk
include bug-50035/module.mk
include bug-50716/module.mk
include bug-50831/module.mk
include bug-51192/module.mk
include bug-51532/module.mk
include bug-51534/module.mk
include bug-51599/module.mk
include bug-52075/module.mk
include bug-52722/module.mk
include bug-52851/module.mk
include bug-53027/module.mk
include bug-53468/module.mk
include bug-53956/module.mk
include bug-54995/module.mk
include bug-55758/module.mk
include bug-56068/module.mk
include bug-58572/module.mk
include bug-58593/module.mk
include bug-59451/module.mk
include bug-59617/module.mk
include bug-59661/module.mk
include bug-59704/module.mk
include bug-59937/module.mk
include bug-60237/module.mk
include bug-60882/module.mk
include bug-61105/module.mk
include bug-61191/module.mk
include class-concat/module.mk
include classdef/module.mk
include classdef-multiple-inheritance/module.mk
include classes/module.mk
include colon-op/module.mk
include ctor-vs-method/module.mk
include fcn-handle/module.mk
include file-encoding/module.mk
include json/module.mk
include jupyter-notebook/module.mk
include local-functions/module.mk
include mex/module.mk
include nest/module.mk
include private-functions/module.mk
include publish/module.mk
include pkg/module.mk

define run-octave-tests
  ( $(SHELL) ../run-octave $(RUN_OCTAVE_OPTIONS) $(1) --no-init-file --silent --no-history -p $(abs_top_builddir)/test/mex $(abs_top_srcdir)/test/fntests.m $(abs_top_srcdir)/test ) && \
  if $(AM_V_P); then \
    echo ""; \
    if [ -f fntests.log ]; then \
      echo "Contents of test/fntests.log:"; \
      echo ""; \
      $(AWK) -f $(srcdir)/show-failures.awk fntests.log; \
    else \
      echo "fntests.log is missing!"; \
    fi; \
  fi
endef

check-local: $(GENERATED_TEST_FILES) $(MEX_TEST_FUNCTIONS) | $(OCTAVE_INTERPRETER_TARGETS) $(octave_dirstamp)
	$(AM_V_at)$(call run-octave-tests)

COVERAGE_DIR = coverage
COVERAGE_INFO = $(COVERAGE_DIR)/$(PACKAGE).info

## FIXME: To get something useful out of 'make coverage', you should use gcc
## and configure with compiler flags set to '-g --coverage'.  Adding the
## --coverage option to either WARN_CXXFLAGS or XTRA_CXXFLAGS resulted in
## link errors, so some work still needed to get a '--enable-coverage-flags'
## option working.

coverage: all
	lcov --directory . --zerocounters
	$(MAKE) $(AM_MAKEFLAGS) check
	$(MKDIR_P) $(COVERAGE_DIR)
	lcov --directory . --capture --output-file $(COVERAGE_INFO)
	genhtml --output-directory $(COVERAGE_DIR) $(COVERAGE_INFO)
	@echo ""
	@echo "Code coverage report successfully built.  Open the file"
	@echo ""
	@echo "   $(abs_top_builddir)/$(COVERAGE_DIR)/index.html"
	@echo ""
	@echo "in a web browser to view the results."
	@echo ""
.PHONY: coverage

conv.tst: mk-conv-tst.sh | $(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(SHELL) $(srcdir)/mk-conv-tst.sh > $@-t && \
	mv $@-t $@

sparse.tst: mk-sparse-tst.sh | $(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(SHELL) $(srcdir)/mk-sparse-tst.sh > $@-t && \
	mv $@-t $@

GENERATED_BC_OVERLOADS_DIRS := \
  $(shell $(SHELL) $(srcdir)/mk-bc-overloads-tst.sh $(abs_top_builddir)/test --list-dirs)

GENERATED_BC_OVERLOADS_FILES := \
  $(shell $(SHELL) $(srcdir)/mk-bc-overloads-tst.sh $(abs_top_builddir)/test --list-files)

$(GENERATED_BC_OVERLOADS_FILES): mk-bc-overloads-tst-stamp

.bc-overload-tests-stamp: mk-bc-overloads-tst.sh bc-overloads-expected | $(octave_dirstamp)
	$(AM_V_GEN)rm -f $@ && \
	$(SHELL) $(srcdir)/mk-bc-overloads-tst.sh $(abs_top_builddir)/test $(srcdir)/bc-overloads-expected && \
	touch $@

GENERATED_TEST_FILES = \
  conv.tst \
  sparse.tst \
  .bc-overload-tests-stamp

fixedtestsdir := $(octtestsdir)/fixed

TEST_INST_FILES = \
  conv.tst \
  sparse.tst \
  $(GENERATED_BC_OVERLOADS_FILES) \
  $(filter-out fntests.m, $(TEST_FILES))

install-data-local: install-test-files

uninstall-local: uninstall-test-files

install-test-files:
	for f in $(TEST_INST_FILES); do \
	  if test -f "$$f"; then d=; else d="$(srcdir)/"; fi; \
	  $(MKDIR_P) $(DESTDIR)$(fixedtestsdir)/`echo $$f | $(SED) 's,/*[^/]*$$,,'`; \
	  $(INSTALL_DATA) $$d$$f $(DESTDIR)$(fixedtestsdir)/$$f; \
	done
.PHONY: install-test-files

uninstall-test-files:
	for f in $(TEST_INST_FILES); do \
	  rm -f $(DESTDIR)$(fixedtestsdir)/$$f; \
	done
.PHONY: uninstall-test-files

BUILT_SOURCES += \
  $(GENERATED_TEST_FILES) \
  $(DIRSTAMP_FILES)

EXTRA_DIST += \
  bc-overloads-expected \
  mk-bc-overloads-tst.sh \
  mk-conv-tst.sh \
  mk-sparse-tst.sh \
  mk_bc_overloads_expected.m \
  show-failures.awk \
  $(addprefix $(srcdir)/, $(TEST_FILES)) \
  $(addprefix $(srcdir)/, $(noinst_TEST_FILES)) \
  $(MEX_TEST_SRC)

CLEANFILES += \
  $(GENERATED_BC_OVERLOADS_FILES) \
  $(GENERATED_TEST_FILES)

DISTCLEANFILES += \
  fntests.log \
  $(DIRSTAMP_FILES)

clean-local: test-clean

test-clean:
	rm -f $(CLEANFILES)
	rm -rf $(GENERATED_BC_OVERLOADS_DIRS)
	rm -rf $(COVERAGE_DIR)
	rm -rf $(MEX_TEST_FUNCTIONS)

test-distclean: test-clean
	rm -f $(DISTCLEANFILES)

test-maintainer-clean: test-distclean
	rm -f $(MAINTAINERCLEANFILES)
