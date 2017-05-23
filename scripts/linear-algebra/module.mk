FCN_FILE_DIRS += scripts/linear-algebra

scripts_linear_algebra_FCN_FILES = \
  %reldir%/bandwidth.m \
  %reldir%/commutation_matrix.m \
  %reldir%/cond.m \
  %reldir%/condeig.m \
  %reldir%/condest.m \
  %reldir%/cross.m \
  %reldir%/duplication_matrix.m \
  %reldir%/expm.m \
  %reldir%/housh.m \
  %reldir%/isbanded.m \
  %reldir%/isdefinite.m \
  %reldir%/isdiag.m \
  %reldir%/ishermitian.m \
  %reldir%/issymmetric.m \
  %reldir%/istril.m \
  %reldir%/istriu.m \
  %reldir%/krylov.m \
  %reldir%/linsolve.m \
  %reldir%/logm.m \
  %reldir%/normest.m \
  %reldir%/normest1.m \
  %reldir%/null.m \
  %reldir%/orth.m \
  %reldir%/planerot.m \
  %reldir%/qzhess.m \
  %reldir%/rank.m \
  %reldir%/rref.m \
  %reldir%/subspace.m \
  %reldir%/trace.m \
  %reldir%/vech.m

%canon_reldir%dir = $(fcnfiledir)/linear-algebra

scripts_linear_algebra_DATA = $(scripts_linear_algebra_FCN_FILES)

FCN_FILES += $(scripts_linear_algebra_FCN_FILES)

PKG_ADD_FILES += %reldir%/PKG_ADD

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)
