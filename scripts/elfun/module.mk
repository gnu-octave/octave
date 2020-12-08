FCN_FILE_DIRS += %reldir%

%canon_reldir%_FCN_FILES = \
  %reldir%/.oct-config \
  %reldir%/acosd.m \
  %reldir%/acot.m \
  %reldir%/acotd.m \
  %reldir%/acoth.m \
  %reldir%/acsc.m \
  %reldir%/acscd.m \
  %reldir%/acsch.m \
  %reldir%/asec.m \
  %reldir%/asecd.m \
  %reldir%/asech.m \
  %reldir%/asind.m \
  %reldir%/atan2d.m \
  %reldir%/atand.m \
  %reldir%/cosd.m \
  %reldir%/cospi.m \
  %reldir%/cot.m \
  %reldir%/cotd.m \
  %reldir%/coth.m \
  %reldir%/csc.m \
  %reldir%/cscd.m \
  %reldir%/csch.m \
  %reldir%/sec.m \
  %reldir%/secd.m \
  %reldir%/sech.m \
  %reldir%/sind.m \
  %reldir%/sinpi.m \
  %reldir%/tand.m

%canon_reldir%dir = $(fcnfiledir)/elfun

%canon_reldir%_DATA = $(%canon_reldir%_FCN_FILES)

FCN_FILES += $(%canon_reldir%_FCN_FILES)

PKG_ADD_FILES += %reldir%/PKG_ADD

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)
