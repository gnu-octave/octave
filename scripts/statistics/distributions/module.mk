FCN_FILE_DIRS += scripts/statistics/distributions

%canon_reldir%_FCN_FILES = \
  %reldir%/betacdf.m \
  %reldir%/betainv.m \
  %reldir%/betapdf.m \
  %reldir%/betarnd.m \
  %reldir%/binocdf.m \
  %reldir%/binoinv.m \
  %reldir%/binopdf.m \
  %reldir%/binornd.m \
  %reldir%/cauchy_cdf.m \
  %reldir%/cauchy_inv.m \
  %reldir%/cauchy_pdf.m \
  %reldir%/cauchy_rnd.m \
  %reldir%/chi2cdf.m \
  %reldir%/chi2inv.m \
  %reldir%/chi2pdf.m \
  %reldir%/chi2rnd.m \
  %reldir%/discrete_cdf.m \
  %reldir%/discrete_inv.m \
  %reldir%/discrete_pdf.m \
  %reldir%/discrete_rnd.m \
  %reldir%/empirical_cdf.m \
  %reldir%/empirical_inv.m \
  %reldir%/empirical_pdf.m \
  %reldir%/empirical_rnd.m \
  %reldir%/expcdf.m \
  %reldir%/expinv.m \
  %reldir%/exppdf.m \
  %reldir%/exprnd.m \
  %reldir%/fcdf.m \
  %reldir%/finv.m \
  %reldir%/fpdf.m \
  %reldir%/frnd.m \
  %reldir%/gamcdf.m \
  %reldir%/gaminv.m \
  %reldir%/gampdf.m \
  %reldir%/gamrnd.m \
  %reldir%/geocdf.m \
  %reldir%/geoinv.m \
  %reldir%/geopdf.m \
  %reldir%/geornd.m \
  %reldir%/hygecdf.m \
  %reldir%/hygeinv.m \
  %reldir%/hygepdf.m \
  %reldir%/hygernd.m \
  %reldir%/kolmogorov_smirnov_cdf.m \
  %reldir%/laplace_cdf.m \
  %reldir%/laplace_inv.m \
  %reldir%/laplace_pdf.m \
  %reldir%/laplace_rnd.m \
  %reldir%/logistic_cdf.m \
  %reldir%/logistic_inv.m \
  %reldir%/logistic_pdf.m \
  %reldir%/logistic_rnd.m \
  %reldir%/logncdf.m \
  %reldir%/logninv.m \
  %reldir%/lognpdf.m \
  %reldir%/lognrnd.m \
  %reldir%/nbincdf.m \
  %reldir%/nbininv.m \
  %reldir%/nbinpdf.m \
  %reldir%/nbinrnd.m \
  %reldir%/normcdf.m \
  %reldir%/norminv.m \
  %reldir%/normpdf.m \
  %reldir%/normrnd.m \
  %reldir%/poisscdf.m \
  %reldir%/poissinv.m \
  %reldir%/poisspdf.m \
  %reldir%/poissrnd.m \
  %reldir%/stdnormal_cdf.m \
  %reldir%/stdnormal_inv.m \
  %reldir%/stdnormal_pdf.m \
  %reldir%/stdnormal_rnd.m \
  %reldir%/tcdf.m \
  %reldir%/tinv.m \
  %reldir%/tpdf.m \
  %reldir%/trnd.m \
  %reldir%/unidcdf.m \
  %reldir%/unidinv.m \
  %reldir%/unidpdf.m \
  %reldir%/unidrnd.m \
  %reldir%/unifcdf.m \
  %reldir%/unifinv.m \
  %reldir%/unifpdf.m \
  %reldir%/unifrnd.m \
  %reldir%/wblcdf.m \
  %reldir%/wblinv.m \
  %reldir%/wblpdf.m \
  %reldir%/wblrnd.m \
  %reldir%/wienrnd.m

%canon_reldir%dir = $(fcnfiledir)/statistics/distributions

%canon_reldir%_DATA = $(%canon_reldir%_FCN_FILES)

FCN_FILES += $(%canon_reldir%_FCN_FILES)

PKG_ADD_FILES += %reldir%/PKG_ADD

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)
