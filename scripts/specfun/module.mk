FCN_FILE_DIRS += scripts/specfun

scripts_specfun_FCN_FILES = \
  scripts/specfun/bessel.m \
  scripts/specfun/beta.m \
  scripts/specfun/betaln.m \
  scripts/specfun/ellipke.m \
  scripts/specfun/expint.m \
  scripts/specfun/factor.m \
  scripts/specfun/factorial.m \
  scripts/specfun/isprime.m \
  scripts/specfun/lcm.m \
  scripts/specfun/legendre.m \
  scripts/specfun/nchoosek.m \
  scripts/specfun/nthroot.m \
  scripts/specfun/perms.m \
  scripts/specfun/pow2.m \
  scripts/specfun/primes.m \
  scripts/specfun/reallog.m \
  scripts/specfun/realpow.m \
  scripts/specfun/realsqrt.m

FCN_FILES += $(scripts_specfun_FCN_FILES)

PKG_ADD_FILES += scripts/specfun/PKG_ADD

DIRSTAMP_FILES += scripts/specfun/$(octave_dirstamp)
