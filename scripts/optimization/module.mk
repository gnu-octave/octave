FCN_FILE_DIRS += \
  %reldir% \
  %reldir%/private

%canon_reldir%_PRIVATE_FCN_FILES = \
  %reldir%/private/__fdjac__.m

%canon_reldir%_FCN_FILES = \
  %reldir%/.oct-config \
  %reldir%/__all_opts__.m \
  %reldir%/fminbnd.m \
  %reldir%/fminsearch.m \
  %reldir%/fminunc.m \
  %reldir%/fsolve.m \
  %reldir%/fzero.m \
  %reldir%/glpk.m \
  %reldir%/humps.m \
  %reldir%/lsqnonneg.m \
  %reldir%/optimget.m \
  %reldir%/optimset.m \
  %reldir%/pqpnonneg.m \
  %reldir%/qp.m \
  %reldir%/sqp.m

%canon_reldir%dir = $(fcnfiledir)/optimization

%canon_reldir%_DATA = $(%canon_reldir%_FCN_FILES)

%canon_reldir%_privatedir = $(fcnfiledir)/optimization/private

%canon_reldir%_private_DATA = $(%canon_reldir%_PRIVATE_FCN_FILES)

FCN_FILES += \
  $(%canon_reldir%_FCN_FILES) \
  $(%canon_reldir%_PRIVATE_FCN_FILES)

PKG_ADD_FILES += %reldir%/PKG_ADD

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)
