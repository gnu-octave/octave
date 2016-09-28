FCN_FILE_DIRS += scripts/strings

scripts_strings_FCN_FILES = \
  scripts/strings/base2dec.m \
  scripts/strings/bin2dec.m \
  scripts/strings/blanks.m \
  scripts/strings/cstrcat.m \
  scripts/strings/deblank.m \
  scripts/strings/dec2base.m \
  scripts/strings/dec2bin.m \
  scripts/strings/dec2hex.m \
  scripts/strings/findstr.m \
  scripts/strings/hex2dec.m \
  scripts/strings/index.m \
  scripts/strings/isletter.m \
  scripts/strings/isstrprop.m \
  scripts/strings/mat2str.m \
  scripts/strings/ostrsplit.m \
  scripts/strings/regexptranslate.m \
  scripts/strings/rindex.m \
  scripts/strings/str2num.m \
  scripts/strings/strcat.m \
  scripts/strings/strchr.m \
  scripts/strings/strjoin.m \
  scripts/strings/strjust.m \
  scripts/strings/strmatch.m \
  scripts/strings/strsplit.m \
  scripts/strings/strtok.m \
  scripts/strings/strtrim.m \
  scripts/strings/strtrunc.m \
  scripts/strings/substr.m \
  scripts/strings/untabify.m \
  scripts/strings/validatestring.m

scripts_stringsdir = $(fcnfiledir)/strings

scripts_strings_DATA = $(scripts_strings_FCN_FILES)

FCN_FILES += $(scripts_strings_FCN_FILES)

PKG_ADD_FILES += scripts/strings/PKG_ADD

DIRSTAMP_FILES += scripts/strings/$(octave_dirstamp)
