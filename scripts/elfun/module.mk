FCN_FILE_DIRS += scripts/elfun

scripts_elfun_FCN_FILES = \
  scripts/elfun/acosd.m \
  scripts/elfun/acot.m \
  scripts/elfun/acotd.m \
  scripts/elfun/acoth.m \
  scripts/elfun/acsc.m \
  scripts/elfun/acscd.m \
  scripts/elfun/acsch.m \
  scripts/elfun/asec.m \
  scripts/elfun/asecd.m \
  scripts/elfun/asech.m \
  scripts/elfun/asind.m \
  scripts/elfun/atan2d.m \
  scripts/elfun/atand.m \
  scripts/elfun/cosd.m \
  scripts/elfun/cot.m \
  scripts/elfun/cotd.m \
  scripts/elfun/coth.m \
  scripts/elfun/csc.m \
  scripts/elfun/cscd.m \
  scripts/elfun/csch.m \
  scripts/elfun/sec.m \
  scripts/elfun/secd.m \
  scripts/elfun/sech.m \
  scripts/elfun/sind.m \
  scripts/elfun/tand.m

scripts_elfundir = $(fcnfiledir)/elfun

scripts_elfun_DATA = $(scripts_elfun_FCN_FILES)

FCN_FILES += $(scripts_elfun_FCN_FILES)

PKG_ADD_FILES += scripts/elfun/PKG_ADD

DIRSTAMP_FILES += scripts/elfun/$(octave_dirstamp)
