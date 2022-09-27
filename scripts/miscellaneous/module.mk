FCN_FILE_DIRS += \
  %reldir% \
  %reldir%/private

%canon_reldir%_PRIVATE_FCN_FILES = \
  %reldir%/private/__memoize__.m \
  %reldir%/private/__publish_html_output__.m \
  %reldir%/private/__publish_latex_output__.m \
  %reldir%/private/__w2mpth__.m \
  %reldir%/private/display_info_file.m \
  %reldir%/private/tar_is_bsd.m

%canon_reldir%_FCN_FILES = \
  %reldir%/.oct-config \
  %reldir%/bug_report.m \
  %reldir%/bunzip2.m \
  %reldir%/cast.m \
  %reldir%/citation.m \
  %reldir%/clearAllMemoizedCaches.m \
  %reldir%/clearvars.m \
  %reldir%/compare_versions.m \
  %reldir%/computer.m \
  %reldir%/copyfile.m \
  %reldir%/delete.m \
  %reldir%/dir.m \
  %reldir%/dos.m \
  %reldir%/edit.m \
  %reldir%/fieldnames.m \
  %reldir%/fileattrib.m \
  %reldir%/fileparts.m \
  %reldir%/fullfile.m \
  %reldir%/getfield.m \
  %reldir%/grabcode.m \
  %reldir%/gunzip.m \
  %reldir%/info.m \
  %reldir%/inputParser.m \
  %reldir%/inputname.m \
  %reldir%/isdeployed.m \
  %reldir%/isfile.m \
  %reldir%/isfolder.m \
  %reldir%/ismac.m \
  %reldir%/ismethod.m \
  %reldir%/ispc.m \
  %reldir%/isunix.m \
  %reldir%/jupyter_notebook.m \
  %reldir%/license.m \
  %reldir%/list_primes.m \
  %reldir%/loadobj.m \
  %reldir%/ls.m \
  %reldir%/ls_command.m \
  %reldir%/memoize.m \
  %reldir%/memory.m \
  %reldir%/menu.m \
  %reldir%/methods.m \
  %reldir%/mex.m \
  %reldir%/mexext.m \
  %reldir%/mkdir.m \
  %reldir%/mkoctfile.m \
  %reldir%/movefile.m \
  %reldir%/mustBeFinite.m \
  %reldir%/mustBeGreaterThan.m \
  %reldir%/mustBeGreaterThanOrEqual.m \
  %reldir%/mustBeInteger.m \
  %reldir%/mustBeLessThan.m \
  %reldir%/mustBeLessThanOrEqual.m \
  %reldir%/mustBeMember.m \
  %reldir%/mustBeNegative.m \
  %reldir%/mustBeNonempty.m \
  %reldir%/mustBeNonNan.m \
  %reldir%/mustBeNonnegative.m \
  %reldir%/mustBeNonpositive.m \
  %reldir%/mustBeNonsparse.m \
  %reldir%/mustBeNonzero.m \
  %reldir%/mustBeNumeric.m \
  %reldir%/mustBeNumericOrLogical.m \
  %reldir%/mustBePositive.m \
  %reldir%/mustBeReal.m \
  %reldir%/namedargs2cell.m \
  %reldir%/namelengthmax.m \
  %reldir%/nargchk.m \
  %reldir%/narginchk.m \
  %reldir%/nargoutchk.m \
  %reldir%/news.m \
  %reldir%/nthargout.m \
  %reldir%/open.m \
  %reldir%/orderfields.m \
  %reldir%/pack.m \
  %reldir%/parseparams.m \
  %reldir%/perl.m \
  %reldir%/publish.m \
  %reldir%/python.m \
  %reldir%/recycle.m \
  %reldir%/run.m \
  %reldir%/saveobj.m \
  %reldir%/setfield.m \
  %reldir%/substruct.m \
  %reldir%/swapbytes.m \
  %reldir%/symvar.m \
  %reldir%/tar.m \
  %reldir%/unix.m \
  %reldir%/unpack.m \
  %reldir%/untar.m \
  %reldir%/unzip.m \
  %reldir%/validateattributes.m \
  %reldir%/ver.m \
  %reldir%/verLessThan.m \
  %reldir%/version.m \
  %reldir%/what.m \
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
