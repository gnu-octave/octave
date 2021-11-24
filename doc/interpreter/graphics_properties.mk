GRAPH_PROP_TEXI_SRC= \
  interpreter/plot-axesproperties.texi \
  interpreter/plot-figureproperties.texi \
  interpreter/plot-imageproperties.texi \
  interpreter/plot-lineproperties.texi \
  interpreter/plot-patchproperties.texi \
  interpreter/plot-rootproperties.texi \
  interpreter/plot-scatterproperties.texi \
  interpreter/plot-surfaceproperties.texi \
  interpreter/plot-textproperties.texi

define gen-propdoc-texi
  rm -f $@-t $@ && \
  $(top_builddir)/run-octave -disable-asan --norc --silent --no-history --path $(srcdir)/interpreter --eval "genpropdoc ('$(1)');" > $@-t && \
  mv $@-t $@
endef

interpreter/plot-axesproperties.texi: interpreter/genpropdoc.m
	$(AM_V_GEN)$(call gen-propdoc-texi,axes)

interpreter/plot-figureproperties.texi: interpreter/genpropdoc.m
	$(AM_V_GEN)$(call gen-propdoc-texi,figure)

interpreter/plot-imageproperties.texi: interpreter/genpropdoc.m
	$(AM_V_GEN)$(call gen-propdoc-texi,image)

interpreter/plot-lineproperties.texi: interpreter/genpropdoc.m
	$(AM_V_GEN)$(call gen-propdoc-texi,line)

interpreter/plot-patchproperties.texi: interpreter/genpropdoc.m
	$(AM_V_GEN)$(call gen-propdoc-texi,patch)

interpreter/plot-rootproperties.texi: interpreter/genpropdoc.m
	$(AM_V_GEN)$(call gen-propdoc-texi,root)

interpreter/plot-surfaceproperties.texi: interpreter/genpropdoc.m
	$(AM_V_GEN)$(call gen-propdoc-texi,surface)

interpreter/plot-scatterproperties.texi: interpreter/genpropdoc.m
	$(AM_V_GEN)$(call gen-propdoc-texi,scatter)

interpreter/plot-textproperties.texi: interpreter/genpropdoc.m
	$(AM_V_GEN)$(call gen-propdoc-texi,text)
