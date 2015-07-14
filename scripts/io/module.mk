FCN_FILE_DIRS += scripts/io

scripts_io_FCN_FILES = \
  scripts/io/beep.m \
  scripts/io/csvread.m \
  scripts/io/csvwrite.m \
  scripts/io/dlmwrite.m \
  scripts/io/fileread.m \
  scripts/io/importdata.m \
  scripts/io/is_valid_file_id.m \
  scripts/io/strread.m \
  scripts/io/textscan.m \
  scripts/io/textread.m

scripts_iodir = $(fcnfiledir)/io

scripts_io_DATA = $(scripts_io_FCN_FILES)

FCN_FILES += $(scripts_io_FCN_FILES)

PKG_ADD_FILES += scripts/io/PKG_ADD

DIRSTAMP_FILES += scripts/io/$(octave_dirstamp)
