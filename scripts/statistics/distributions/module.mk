FCN_FILE_DIRS += scripts/statistics/distributions

scripts_statistics_distributions_FCN_FILES = \
  scripts/statistics/distributions/betacdf.m \
  scripts/statistics/distributions/betainv.m \
  scripts/statistics/distributions/betapdf.m \
  scripts/statistics/distributions/betarnd.m \
  scripts/statistics/distributions/binocdf.m \
  scripts/statistics/distributions/binoinv.m \
  scripts/statistics/distributions/binopdf.m \
  scripts/statistics/distributions/binornd.m \
  scripts/statistics/distributions/cauchy_cdf.m \
  scripts/statistics/distributions/cauchy_inv.m \
  scripts/statistics/distributions/cauchy_pdf.m \
  scripts/statistics/distributions/cauchy_rnd.m \
  scripts/statistics/distributions/chi2cdf.m \
  scripts/statistics/distributions/chi2inv.m \
  scripts/statistics/distributions/chi2pdf.m \
  scripts/statistics/distributions/chi2rnd.m \
  scripts/statistics/distributions/discrete_cdf.m \
  scripts/statistics/distributions/discrete_inv.m \
  scripts/statistics/distributions/discrete_pdf.m \
  scripts/statistics/distributions/discrete_rnd.m \
  scripts/statistics/distributions/empirical_cdf.m \
  scripts/statistics/distributions/empirical_inv.m \
  scripts/statistics/distributions/empirical_pdf.m \
  scripts/statistics/distributions/empirical_rnd.m \
  scripts/statistics/distributions/expcdf.m \
  scripts/statistics/distributions/expinv.m \
  scripts/statistics/distributions/exppdf.m \
  scripts/statistics/distributions/exprnd.m \
  scripts/statistics/distributions/fcdf.m \
  scripts/statistics/distributions/finv.m \
  scripts/statistics/distributions/fpdf.m \
  scripts/statistics/distributions/frnd.m \
  scripts/statistics/distributions/gamcdf.m \
  scripts/statistics/distributions/gaminv.m \
  scripts/statistics/distributions/gampdf.m \
  scripts/statistics/distributions/gamrnd.m \
  scripts/statistics/distributions/geocdf.m \
  scripts/statistics/distributions/geoinv.m \
  scripts/statistics/distributions/geopdf.m \
  scripts/statistics/distributions/geornd.m \
  scripts/statistics/distributions/hygecdf.m \
  scripts/statistics/distributions/hygeinv.m \
  scripts/statistics/distributions/hygepdf.m \
  scripts/statistics/distributions/hygernd.m \
  scripts/statistics/distributions/kolmogorov_smirnov_cdf.m \
  scripts/statistics/distributions/laplace_cdf.m \
  scripts/statistics/distributions/laplace_inv.m \
  scripts/statistics/distributions/laplace_pdf.m \
  scripts/statistics/distributions/laplace_rnd.m \
  scripts/statistics/distributions/logistic_cdf.m \
  scripts/statistics/distributions/logistic_inv.m \
  scripts/statistics/distributions/logistic_pdf.m \
  scripts/statistics/distributions/logistic_rnd.m \
  scripts/statistics/distributions/logncdf.m \
  scripts/statistics/distributions/logninv.m \
  scripts/statistics/distributions/lognpdf.m \
  scripts/statistics/distributions/lognrnd.m \
  scripts/statistics/distributions/nbincdf.m \
  scripts/statistics/distributions/nbininv.m \
  scripts/statistics/distributions/nbinpdf.m \
  scripts/statistics/distributions/nbinrnd.m \
  scripts/statistics/distributions/normcdf.m \
  scripts/statistics/distributions/norminv.m \
  scripts/statistics/distributions/normpdf.m \
  scripts/statistics/distributions/normrnd.m \
  scripts/statistics/distributions/poisscdf.m \
  scripts/statistics/distributions/poissinv.m \
  scripts/statistics/distributions/poisspdf.m \
  scripts/statistics/distributions/poissrnd.m \
  scripts/statistics/distributions/stdnormal_cdf.m \
  scripts/statistics/distributions/stdnormal_inv.m \
  scripts/statistics/distributions/stdnormal_pdf.m \
  scripts/statistics/distributions/stdnormal_rnd.m \
  scripts/statistics/distributions/tcdf.m \
  scripts/statistics/distributions/tinv.m \
  scripts/statistics/distributions/tpdf.m \
  scripts/statistics/distributions/trnd.m \
  scripts/statistics/distributions/unidrnd.m \
  scripts/statistics/distributions/unidcdf.m \
  scripts/statistics/distributions/unidinv.m \
  scripts/statistics/distributions/unidpdf.m \
  scripts/statistics/distributions/unifrnd.m \
  scripts/statistics/distributions/unifcdf.m \
  scripts/statistics/distributions/unifinv.m \
  scripts/statistics/distributions/unifpdf.m \
  scripts/statistics/distributions/wblcdf.m \
  scripts/statistics/distributions/wblinv.m \
  scripts/statistics/distributions/wblpdf.m \
  scripts/statistics/distributions/wblrnd.m \
  scripts/statistics/distributions/wienrnd.m

FCN_FILES += $(scripts_statistics_distributions_FCN_FILES)

PKG_ADD_FILES += scripts/statistics/distributions/PKG_ADD

DIRSTAMP_FILES += scripts/statistics/distributions/$(octave_dirstamp)
