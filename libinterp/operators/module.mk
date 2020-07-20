LIBINTERP_OPERATORS_SRC = \
  %reldir%/op-b-b.cc \
  %reldir%/op-b-bm.cc \
  %reldir%/op-b-sbm.cc \
  %reldir%/op-bm-b.cc \
  %reldir%/op-bm-bm.cc \
  %reldir%/op-bm-sbm.cc \
  %reldir%/op-cdm-cdm.cc \
  %reldir%/op-cdm-cm.cc \
  %reldir%/op-cdm-cs.cc \
  %reldir%/op-cdm-dm.cc \
  %reldir%/op-cdm-m.cc \
  %reldir%/op-cdm-s.cc \
  %reldir%/op-cell.cc \
  %reldir%/op-chm.cc \
  %reldir%/op-class.cc \
  %reldir%/op-cm-cdm.cc \
  %reldir%/op-cm-cm.cc \
  %reldir%/op-cm-cs.cc \
  %reldir%/op-cm-dm.cc \
  %reldir%/op-cm-m.cc \
  %reldir%/op-cm-pm.cc \
  %reldir%/op-cm-s.cc \
  %reldir%/op-cm-scm.cc \
  %reldir%/op-cm-sm.cc \
  %reldir%/op-cs-cm.cc \
  %reldir%/op-cs-cs.cc \
  %reldir%/op-cs-m.cc \
  %reldir%/op-cs-s.cc \
  %reldir%/op-cs-scm.cc \
  %reldir%/op-cs-sm.cc \
  %reldir%/op-dm-cdm.cc \
  %reldir%/op-dm-cm.cc \
  %reldir%/op-dm-cs.cc \
  %reldir%/op-dm-dm.cc \
  %reldir%/op-dm-m.cc \
  %reldir%/op-dm-s.cc \
  %reldir%/op-dm-scm.cc \
  %reldir%/op-dm-sm.cc \
  %reldir%/op-fcdm-fcdm.cc \
  %reldir%/op-fcdm-fcm.cc \
  %reldir%/op-fcdm-fcs.cc \
  %reldir%/op-fcdm-fdm.cc \
  %reldir%/op-fcdm-fm.cc \
  %reldir%/op-fcdm-fs.cc \
  %reldir%/op-fcm-fcdm.cc \
  %reldir%/op-fcm-fcm.cc \
  %reldir%/op-fcm-fcs.cc \
  %reldir%/op-fcm-fdm.cc \
  %reldir%/op-fcm-fm.cc \
  %reldir%/op-fcm-fs.cc \
  %reldir%/op-fcm-pm.cc \
  %reldir%/op-fcn.cc \
  %reldir%/op-fcs-fcm.cc \
  %reldir%/op-fcs-fcs.cc \
  %reldir%/op-fcs-fm.cc \
  %reldir%/op-fcs-fs.cc \
  %reldir%/op-fdm-fcdm.cc \
  %reldir%/op-fdm-fcm.cc \
  %reldir%/op-fdm-fcs.cc \
  %reldir%/op-fdm-fdm.cc \
  %reldir%/op-fdm-fm.cc \
  %reldir%/op-fdm-fs.cc \
  %reldir%/op-fm-fcdm.cc \
  %reldir%/op-fm-fcm.cc \
  %reldir%/op-fm-fcs.cc \
  %reldir%/op-fm-fdm.cc \
  %reldir%/op-fm-fm.cc \
  %reldir%/op-fm-fs.cc \
  %reldir%/op-fm-pm.cc \
  %reldir%/op-fs-fcm.cc \
  %reldir%/op-fs-fcs.cc \
  %reldir%/op-fs-fm.cc \
  %reldir%/op-fs-fs.cc \
  %reldir%/op-i16-i16.cc \
  %reldir%/op-i32-i32.cc \
  %reldir%/op-i64-i64.cc \
  %reldir%/op-i8-i8.cc \
  %reldir%/op-int-concat.cc \
  %reldir%/op-m-cdm.cc \
  %reldir%/op-m-cm.cc \
  %reldir%/op-m-cs.cc \
  %reldir%/op-m-dm.cc \
  %reldir%/op-m-m.cc \
  %reldir%/op-m-pm.cc \
  %reldir%/op-m-s.cc \
  %reldir%/op-m-scm.cc \
  %reldir%/op-m-sm.cc \
  %reldir%/op-mi.cc \
  %reldir%/op-pm-cm.cc \
  %reldir%/op-pm-fcm.cc \
  %reldir%/op-pm-fm.cc \
  %reldir%/op-pm-m.cc \
  %reldir%/op-pm-pm.cc \
  %reldir%/op-pm-scm.cc \
  %reldir%/op-pm-sm.cc \
  %reldir%/op-range.cc \
  %reldir%/op-s-cm.cc \
  %reldir%/op-s-cs.cc \
  %reldir%/op-s-m.cc \
  %reldir%/op-s-s.cc \
  %reldir%/op-s-scm.cc \
  %reldir%/op-s-sm.cc \
  %reldir%/op-sbm-b.cc \
  %reldir%/op-sbm-bm.cc \
  %reldir%/op-sbm-sbm.cc \
  %reldir%/op-scm-cm.cc \
  %reldir%/op-scm-cs.cc \
  %reldir%/op-scm-m.cc \
  %reldir%/op-scm-s.cc \
  %reldir%/op-scm-scm.cc \
  %reldir%/op-scm-sm.cc \
  %reldir%/op-sm-cm.cc \
  %reldir%/op-sm-cs.cc \
  %reldir%/op-sm-m.cc \
  %reldir%/op-sm-s.cc \
  %reldir%/op-sm-scm.cc \
  %reldir%/op-sm-sm.cc \
  %reldir%/op-str-m.cc \
  %reldir%/op-str-s.cc \
  %reldir%/op-str-str.cc \
  %reldir%/op-struct.cc \
  %reldir%/op-ui16-ui16.cc \
  %reldir%/op-ui32-ui32.cc \
  %reldir%/op-ui64-ui64.cc \
  %reldir%/op-ui8-ui8.cc

LIBINTERP_OPERATORS_INC = \
  %reldir%/ops.h

## These look like included header files to Autotools build process
NOINSTALL_LIBINTERP_OPERATORS_INC = \
  %reldir%/op-dm-template.cc \
  %reldir%/op-dms-template.cc \
  %reldir%/op-int.h \
  %reldir%/op-pm-template.cc

libinterp_EXTRA_DIST += \
  %reldir%/mk-ops.sh

## Special rules for sources which must be built before rest of compilation.
%reldir%/ops.cc: $(LIBINTERP_OPERATORS_SRC) %reldir%/mk-ops.sh
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(SHELL) $(srcdir)/%reldir%/mk-ops.sh $(LIBINTERP_OPERATORS_SRC) > $@-t && \
	mv $@-t $@

