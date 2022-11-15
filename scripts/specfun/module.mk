FCN_FILE_DIRS += %reldir%

%canon_reldir%_FCN_FILES = \
  %reldir%/.oct-config \
  %reldir%/beta.m \
  %reldir%/betainc.m \
  %reldir%/betaincinv.m \
  %reldir%/betaln.m \
  %reldir%/cosint.m \
  %reldir%/ellipke.m \
  %reldir%/expint.m \
  %reldir%/factor.m \
  %reldir%/factorial.m \
  %reldir%/gammainc.m \
  %reldir%/gammaincinv.m \
  %reldir%/isprime.m \
  %reldir%/lcm.m \
  %reldir%/legendre.m \
  %reldir%/nchoosek.m \
  %reldir%/nthroot.m \
  %reldir%/perms.m \
  %reldir%/primes.m \
  %reldir%/reallog.m \
  %reldir%/realpow.m \
  %reldir%/realsqrt.m \
  %reldir%/sinint.m

%canon_reldir%dir = $(fcnfiledir)/specfun

%canon_reldir%_DATA = $(%canon_reldir%_FCN_FILES)

FCN_FILES += $(%canon_reldir%_FCN_FILES)

PKG_ADD_FILES += %reldir%/PKG_ADD

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)
