FCN_FILE_DIRS += scripts/linear-algebra

scripts_linear_algebra_FCN_FILES = \
  scripts/linear-algebra/bandwidth.m \
  scripts/linear-algebra/commutation_matrix.m \
  scripts/linear-algebra/cond.m \
  scripts/linear-algebra/condest.m \
  scripts/linear-algebra/condeig.m \
  scripts/linear-algebra/cross.m \
  scripts/linear-algebra/duplication_matrix.m \
  scripts/linear-algebra/expm.m \
  scripts/linear-algebra/housh.m \
  scripts/linear-algebra/isbanded.m \
  scripts/linear-algebra/isdefinite.m \
  scripts/linear-algebra/isdiag.m \
  scripts/linear-algebra/ishermitian.m \
  scripts/linear-algebra/issymmetric.m \
  scripts/linear-algebra/istril.m \
  scripts/linear-algebra/istriu.m \
  scripts/linear-algebra/krylov.m \
  scripts/linear-algebra/linsolve.m \
  scripts/linear-algebra/logm.m \
  scripts/linear-algebra/normest.m \
  scripts/linear-algebra/null.m \
  scripts/linear-algebra/onenormest.m \
  scripts/linear-algebra/orth.m \
  scripts/linear-algebra/planerot.m \
  scripts/linear-algebra/qzhess.m \
  scripts/linear-algebra/rank.m \
  scripts/linear-algebra/rref.m \
  scripts/linear-algebra/subspace.m \
  scripts/linear-algebra/trace.m \
  scripts/linear-algebra/vech.m

scripts_linear_algebradir = $(fcnfiledir)/linear-algebra

scripts_linear_algebra_DATA = $(scripts_linear_algebra_FCN_FILES)

FCN_FILES += $(scripts_linear_algebra_FCN_FILES)

PKG_ADD_FILES += scripts/linear-algebra/PKG_ADD

DIRSTAMP_FILES += scripts/linear-algebra/$(octave_dirstamp)
