FCN_FILE_DIRS += \
  scripts/general \
  %reldir%/private

%canon_reldir%_PRIVATE_FCN_FILES = \
  %reldir%/private/__isequal__.m \
  %reldir%/private/__publish_html_output__.m \
  %reldir%/private/__publish_latex_output__.m \
  %reldir%/private/__splinen__.m

%canon_reldir%_FCN_FILES = \
  %reldir%/accumarray.m \
  %reldir%/accumdim.m \
  %reldir%/bincoeff.m \
  %reldir%/bitcmp.m \
  %reldir%/bitget.m \
  %reldir%/bitset.m \
  %reldir%/blkdiag.m \
  %reldir%/cart2pol.m \
  %reldir%/cart2sph.m \
  %reldir%/cell2mat.m \
  %reldir%/celldisp.m \
  %reldir%/chop.m \
  %reldir%/circshift.m \
  %reldir%/common_size.m \
  %reldir%/cplxpair.m \
  %reldir%/cumtrapz.m \
  %reldir%/curl.m \
  %reldir%/dblquad.m \
  %reldir%/deal.m \
  %reldir%/deg2rad.m \
  %reldir%/del2.m \
  %reldir%/divergence.m \
  %reldir%/fieldnames.m \
  %reldir%/flip.m \
  %reldir%/flipdim.m \
  %reldir%/fliplr.m \
  %reldir%/flipud.m \
  %reldir%/grabcode.m \
  %reldir%/gradient.m \
  %reldir%/idivide.m \
  %reldir%/inputParser.m \
  %reldir%/int2str.m \
  %reldir%/interp1.m \
  %reldir%/interp2.m \
  %reldir%/interp3.m \
  %reldir%/interpft.m \
  %reldir%/interpn.m \
  %reldir%/isdir.m \
  %reldir%/isequal.m \
  %reldir%/isequaln.m \
  %reldir%/loadobj.m \
  %reldir%/logspace.m \
  %reldir%/methods.m \
  %reldir%/nargchk.m \
  %reldir%/narginchk.m \
  %reldir%/nargoutchk.m \
  %reldir%/nextpow2.m \
  %reldir%/nthargout.m \
  %reldir%/num2str.m \
  %reldir%/pol2cart.m \
  %reldir%/polyarea.m \
  %reldir%/postpad.m \
  %reldir%/prepad.m \
  %reldir%/publish.m \
  %reldir%/quadgk.m \
  %reldir%/quadl.m \
  %reldir%/quadv.m \
  %reldir%/rad2deg.m \
  %reldir%/randi.m \
  %reldir%/rat.m \
  %reldir%/repmat.m \
  %reldir%/rot90.m \
  %reldir%/rotdim.m \
  %reldir%/saveobj.m \
  %reldir%/shift.m \
  %reldir%/shiftdim.m \
  %reldir%/sortrows.m \
  %reldir%/sph2cart.m \
  %reldir%/structfun.m \
  %reldir%/subsindex.m \
  %reldir%/trapz.m \
  %reldir%/triplequad.m \
  %reldir%/validateattributes.m

%canon_reldir%dir = $(fcnfiledir)/general

%canon_reldir%_DATA = $(%canon_reldir%_FCN_FILES)

%canon_reldir%_privatedir = $(fcnfiledir)/general/private

%canon_reldir%_private_DATA = $(%canon_reldir%_PRIVATE_FCN_FILES)

FCN_FILES += \
  $(%canon_reldir%_FCN_FILES) \
  $(%canon_reldir%_PRIVATE_FCN_FILES)

PKG_ADD_FILES += %reldir%/PKG_ADD

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)
