FCN_FILE_DIRS += \
  scripts/general \
  scripts/general/private

scripts_general_PRIVATE_FCN_FILES = \
  scripts/general/private/__isequal__.m \
  scripts/general/private/__splinen__.m

scripts_general_FCN_FILES = \
  scripts/general/accumarray.m \
  scripts/general/accumdim.m \
  scripts/general/bincoeff.m \
  scripts/general/bitcmp.m \
  scripts/general/bitget.m \
  scripts/general/bitset.m \
  scripts/general/blkdiag.m \
  scripts/general/cart2pol.m \
  scripts/general/cart2sph.m \
  scripts/general/cell2mat.m \
  scripts/general/celldisp.m \
  scripts/general/chop.m \
  scripts/general/circshift.m \
  scripts/general/common_size.m \
  scripts/general/cplxpair.m \
  scripts/general/cumtrapz.m \
  scripts/general/curl.m \
  scripts/general/dblquad.m \
  scripts/general/deal.m \
  scripts/general/del2.m \
  scripts/general/display.m \
  scripts/general/divergence.m \
  scripts/general/fieldnames.m \
  scripts/general/flip.m \
  scripts/general/flipdim.m \
  scripts/general/fliplr.m \
  scripts/general/flipud.m \
  scripts/general/gradient.m \
  scripts/general/idivide.m \
  scripts/general/inputParser.m \
  scripts/general/int2str.m \
  scripts/general/interp1.m \
  scripts/general/interp2.m \
  scripts/general/interp3.m \
  scripts/general/interpft.m \
  scripts/general/interpn.m \
  scripts/general/isdir.m \
  scripts/general/isequal.m \
  scripts/general/isequaln.m \
  scripts/general/loadobj.m \
  scripts/general/logspace.m \
  scripts/general/methods.m \
  scripts/general/nargchk.m \
  scripts/general/narginchk.m \
  scripts/general/nargoutchk.m \
  scripts/general/nextpow2.m \
  scripts/general/nthargout.m \
  scripts/general/num2str.m \
  scripts/general/pol2cart.m \
  scripts/general/polyarea.m \
  scripts/general/postpad.m \
  scripts/general/prepad.m \
  scripts/general/profexplore.m \
  scripts/general/profile.m \
  scripts/general/profshow.m \
  scripts/general/quadgk.m \
  scripts/general/quadl.m \
  scripts/general/quadv.m \
  scripts/general/randi.m \
  scripts/general/rat.m \
  scripts/general/repmat.m \
  scripts/general/rot90.m \
  scripts/general/rotdim.m \
  scripts/general/saveobj.m \
  scripts/general/shift.m \
  scripts/general/shiftdim.m \
  scripts/general/sortrows.m \
  scripts/general/sph2cart.m \
  scripts/general/structfun.m \
  scripts/general/subsindex.m \
  scripts/general/trapz.m \
  scripts/general/triplequad.m \
  scripts/general/validateattributes.m

scripts_generaldir = $(fcnfiledir)/general

scripts_general_DATA = $(scripts_general_FCN_FILES)

scripts_general_privatedir = $(fcnfiledir)/general/private

scripts_general_private_DATA = $(scripts_general_PRIVATE_FCN_FILES)

FCN_FILES += \
  $(scripts_general_FCN_FILES) \
  $(scripts_general_PRIVATE_FCN_FILES)

PKG_ADD_FILES += scripts/general/PKG_ADD

DIRSTAMP_FILES += scripts/general/$(octave_dirstamp)
