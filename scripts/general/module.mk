FCN_FILE_DIRS += general

general_PRIVATE_FCN_FILES = \
  general/private/__isequal__.m \
  general/private/__splinen__.m

general_FCN_FILES = \
  general/accumarray.m \
  general/accumdim.m \
  general/bicubic.m \
  general/bitcmp.m \
  general/bitget.m \
  general/bitset.m \
  general/blkdiag.m \
  general/cart2pol.m \
  general/cart2sph.m \
  general/cell2mat.m \
  general/celldisp.m \
  general/chop.m \
  general/circshift.m \
  general/colon.m \
  general/common_size.m \
  general/cplxpair.m \
  general/cumtrapz.m \
  general/curl.m \
  general/dblquad.m \
  general/deal.m \
  general/del2.m \
  general/display.m \
  general/divergence.m \
  general/flipdim.m \
  general/fliplr.m \
  general/flipud.m \
  general/genvarname.m \
  general/gradient.m \
  general/idivide.m \
  general/int2str.m \
  general/interp1.m \
  general/interp1q.m \
  general/interp2.m \
  general/interp3.m \
  general/interpn.m \
  general/interpft.m \
  general/isa.m \
  general/iscolumn.m \
  general/isdir.m \
  general/isequal.m \
  general/isequalwithequalnans.m \
  general/isrow.m \
  general/isscalar.m \
  general/issquare.m \
  general/isvector.m \
  general/loadobj.m \
  general/logspace.m \
  general/nargchk.m \
  general/narginchk.m \
  general/nargoutchk.m \
  general/nthargout.m \
  general/nextpow2.m \
  general/num2str.m \
  general/pol2cart.m \
  general/polyarea.m \
  general/postpad.m \
  general/prepad.m \
  general/profexplore.m \
  general/profile.m \
  general/profshow.m \
  general/quadgk.m \
  general/quadl.m \
  general/quadv.m \
  general/randi.m \
  general/rat.m \
  general/repmat.m \
  general/rot90.m \
  general/rotdim.m \
  general/saveobj.m \
  general/shift.m \
  general/shiftdim.m \
  general/sortrows.m \
  general/sph2cart.m \
  general/structfun.m \
  general/subsindex.m \
  general/triplequad.m \
  general/trapz.m \
  $(general_PRIVATE_FCN_FILES)

FCN_FILES += $(general_FCN_FILES)

PKG_ADD_FILES += general/PKG_ADD

DIRSTAMP_FILES += general/$(octave_dirstamp)
