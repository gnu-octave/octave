FCN_FILE_DIRS += optimization

optimization_FCN_FILES = \
  optimization/fzero.m \
  optimization/__fdjac__.m \
  optimization/__dogleg__.m \
  optimization/__doglegm__.m \
  optimization/fsolve.m \
  optimization/fminunc.m \
  optimization/glpk.m \
  optimization/glpkmex.m \
  optimization/lsqnonneg.m \
  optimization/pqpnonneg.m \
  optimization/optimset.m \
  optimization/optimget.m \
  optimization/__all_opts__.m \
  optimization/qp.m \
  optimization/sqp.m

FCN_FILES += $(optimization_FCN_FILES)

PKG_ADD_FILES += optimization/PKG_ADD
