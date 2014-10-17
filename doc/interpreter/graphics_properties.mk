GRAPH_PROP_TEXI_SRC= \
  plot-axesproperties.texi \
  plot-figureproperties.texi \
  plot-imageproperties.texi \
  plot-lineproperties.texi \
  plot-patchproperties.texi \
  plot-rootproperties.texi \
  plot-surfaceproperties.texi \
  plot-textproperties.texi


plot-axesproperties.texi: genpropdoc.m
	$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "genpropdoc ('axes', '$@');" || { rm -f $@; exit 1; }

plot-figureproperties.texi: genpropdoc.m
	$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "genpropdoc ('figure', '$@');" || { rm -f $@; exit 1; }

plot-imageproperties.texi: genpropdoc.m
	$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "genpropdoc ('image', '$@');" || { rm -f $@; exit 1; }

plot-lineproperties.texi: genpropdoc.m
	$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "genpropdoc ('line', '$@');" || { rm -f $@; exit 1; }

plot-patchproperties.texi: genpropdoc.m
	$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "genpropdoc ('patch', '$@');" || { rm -f $@; exit 1; }

plot-rootproperties.texi: genpropdoc.m
	$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "genpropdoc ('root', '$@');" || { rm -f $@; exit 1; }

plot-surfaceproperties.texi: genpropdoc.m
	$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "genpropdoc ('surface', '$@');" || { rm -f $@; exit 1; }

plot-textproperties.texi: genpropdoc.m
	$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "genpropdoc ('text', '$@');" || { rm -f $@; exit 1; }
