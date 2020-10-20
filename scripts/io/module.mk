FCN_FILE_DIRS += %reldir%

%canon_reldir%_FCN_FILES = \
  %reldir%/.oct-config \
  %reldir%/beep.m \
  %reldir%/csvread.m \
  %reldir%/csvwrite.m \
  %reldir%/dlmwrite.m \
  %reldir%/fileread.m \
  %reldir%/importdata.m \
  %reldir%/is_valid_file_id.m

%canon_reldir%dir = $(fcnfiledir)/io

%canon_reldir%_DATA = $(%canon_reldir%_FCN_FILES)

FCN_FILES += $(%canon_reldir%_FCN_FILES)

PKG_ADD_FILES += %reldir%/PKG_ADD

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)
