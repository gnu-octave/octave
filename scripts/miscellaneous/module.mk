FCN_FILE_DIRS += \
  scripts/miscellaneous \
  %reldir%/private

%canon_reldir%_PRIVATE_FCN_FILES = \
  %reldir%/private/__w2mpth__.m \
  %reldir%/private/display_info_file.m

%canon_reldir%_FCN_FILES = \
  %reldir%/bug_report.m \
  %reldir%/bunzip2.m \
  %reldir%/cast.m \
  %reldir%/citation.m \
  %reldir%/compare_versions.m \
  %reldir%/computer.m \
  %reldir%/copyfile.m \
  %reldir%/debug.m \
  %reldir%/delete.m \
  %reldir%/desktop.m \
  %reldir%/dir.m \
  %reldir%/dos.m \
  %reldir%/edit.m \
  %reldir%/fact.m \
  %reldir%/fileattrib.m \
  %reldir%/fileparts.m \
  %reldir%/fullfile.m \
  %reldir%/genvarname.m \
  %reldir%/getappdata.m \
  %reldir%/getfield.m \
  %reldir%/gunzip.m \
  %reldir%/info.m \
  %reldir%/inputname.m \
  %reldir%/isappdata.m \
  %reldir%/isdeployed.m \
  %reldir%/ismac.m \
  %reldir%/ispc.m \
  %reldir%/isunix.m \
  %reldir%/license.m \
  %reldir%/list_primes.m \
  %reldir%/ls.m \
  %reldir%/ls_command.m \
  %reldir%/menu.m \
  %reldir%/mex.m \
  %reldir%/mexext.m \
  %reldir%/mkdir.m \
  %reldir%/mkoctfile.m \
  %reldir%/movefile.m \
  %reldir%/namelengthmax.m \
  %reldir%/news.m \
  %reldir%/open.m \
  %reldir%/orderfields.m \
  %reldir%/pack.m \
  %reldir%/parseparams.m \
  %reldir%/perl.m \
  %reldir%/python.m \
  %reldir%/recycle.m \
  %reldir%/rmappdata.m \
  %reldir%/run.m \
  %reldir%/setappdata.m \
  %reldir%/setfield.m \
  %reldir%/substruct.m \
  %reldir%/swapbytes.m \
  %reldir%/symvar.m \
  %reldir%/tar.m \
  %reldir%/tempdir.m \
  %reldir%/tmpnam.m \
  %reldir%/unix.m \
  %reldir%/unpack.m \
  %reldir%/untar.m \
  %reldir%/unzip.m \
  %reldir%/ver.m \
  %reldir%/version.m \
  %reldir%/what.m \
  %reldir%/xor.m \
  %reldir%/zip.m

%canon_reldir%dir = $(fcnfiledir)/miscellaneous

%canon_reldir%_DATA = $(%canon_reldir%_FCN_FILES)

%canon_reldir%_privatedir = $(fcnfiledir)/miscellaneous/private

%canon_reldir%_private_DATA = $(%canon_reldir%_PRIVATE_FCN_FILES)

FCN_FILES += \
  $(%canon_reldir%_FCN_FILES) \
  $(%canon_reldir%_PRIVATE_FCN_FILES)

PKG_ADD_FILES += %reldir%/PKG_ADD

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)
