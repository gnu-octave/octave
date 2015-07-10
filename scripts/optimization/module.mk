FCN_FILE_DIRS += scripts/optimization

scripts_optimization_PRIVATE_FCN_FILES = \
  scripts/optimization/private/__fdjac__.m

scripts_optimization_FCN_FILES = \
  scripts/optimization/__all_opts__.m \
  scripts/optimization/fminbnd.m \
  scripts/optimization/fminsearch.m \
  scripts/optimization/fminunc.m \
  scripts/optimization/fsolve.m \
  scripts/optimization/fzero.m \
  scripts/optimization/glpk.m \
  scripts/optimization/lsqnonneg.m \
  scripts/optimization/optimget.m \
  scripts/optimization/optimset.m \
  scripts/optimization/pqpnonneg.m \
  scripts/optimization/qp.m \
  scripts/optimization/sqp.m \
  $(scripts_optimization_PRIVATE_FCN_FILES)

FCN_FILES += $(scripts_optimization_FCN_FILES)

PKG_ADD_FILES += scripts/optimization/PKG_ADD

DIRSTAMP_FILES += scripts/optimization/$(octave_dirstamp)
