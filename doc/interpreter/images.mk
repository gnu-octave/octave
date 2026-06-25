if AMCOND_BUILD_DOCS

## Command that runs Octave m-file $1 to create image name $2 with format $3
define gen-image
  $(SHELL) run-octave -disable-asan --norc --no-history --quiet \
	  --path $(abs_top_srcdir)/doc/interpreter \
	  --eval "$(1) ('doc/interpreter/', '$(2)', '$(3)');"
endef

GEOMETRYIMAGES_SRC = geometryimages.m
DOC_IMAGES_SRC += %reldir%/$(GEOMETRYIMAGES_SRC)
GEOMETRYIMAGES_PLOTS = $(addprefix %reldir%/,voronoi triplot griddata convhull delaunay inpolygon)

## NOTE: 4 repeated instantiations could be generated with GNU Make foreach and
## eval statements, but it would difficult to understand.
GEOMETRYIMAGES_EPS = $(addsuffix .eps,$(GEOMETRYIMAGES_PLOTS))
BUILT_DOC_IMAGES_EPS += $(GEOMETRYIMAGES_EPS)
$(GEOMETRYIMAGES_EPS): %reldir%/%.eps : %reldir%/$(GEOMETRYIMAGES_SRC)
	$(AM_V_GEN)$(call gen-image,geometryimages,$*,eps)

GEOMETRYIMAGES_PDF = $(addsuffix .pdf,$(GEOMETRYIMAGES_PLOTS))
BUILT_DOC_IMAGES_PDF += $(GEOMETRYIMAGES_PDF)
$(GEOMETRYIMAGES_PDF): %reldir%/%.pdf : %reldir%/$(GEOMETRYIMAGES_SRC)
	$(AM_V_GEN)$(call gen-image,geometryimages,$*,pdf)

GEOMETRYIMAGES_PNG = $(addsuffix .png,$(GEOMETRYIMAGES_PLOTS))
BUILT_DOC_IMAGES_PNG += $(GEOMETRYIMAGES_PNG)
$(GEOMETRYIMAGES_PNG): %reldir%/%.png : %reldir%/$(GEOMETRYIMAGES_SRC)
	$(AM_V_GEN)$(call gen-image,geometryimages,$*,png)

GEOMETRYIMAGES_TXT = $(addsuffix .txt,$(GEOMETRYIMAGES_PLOTS))
BUILT_DOC_IMAGES_TXT += $(GEOMETRYIMAGES_TXT)
$(GEOMETRYIMAGES_TXT): %reldir%/%.txt : %reldir%/$(GEOMETRYIMAGES_SRC)
	$(AM_V_GEN)$(call gen-image,geometryimages,$*,txt)


INTERPIMAGES_SRC = interpimages.m
DOC_IMAGES_SRC += %reldir%/$(INTERPIMAGES_SRC)
INTERPIMAGES_PLOTS = $(addprefix %reldir%/,interpft interpn interpderiv1 interpderiv2)

INTERPIMAGES_EPS = $(addsuffix .eps,$(INTERPIMAGES_PLOTS))
BUILT_DOC_IMAGES_EPS += $(INTERPIMAGES_EPS)
$(INTERPIMAGES_EPS): %reldir%/%.eps : %reldir%/$(INTERPIMAGES_SRC)
	$(AM_V_GEN)$(call gen-image,interpimages,$*,eps)

INTERPIMAGES_PDF = $(addsuffix .pdf,$(INTERPIMAGES_PLOTS))
BUILT_DOC_IMAGES_PDF += $(INTERPIMAGES_PDF)
$(INTERPIMAGES_PDF): %reldir%/%.pdf : %reldir%/$(INTERPIMAGES_SRC)
	$(AM_V_GEN)$(call gen-image,interpimages,$*,pdf)

INTERPIMAGES_PNG = $(addsuffix .png,$(INTERPIMAGES_PLOTS))
BUILT_DOC_IMAGES_PNG += $(INTERPIMAGES_PNG)
$(INTERPIMAGES_PNG): %reldir%/%.png : %reldir%/$(INTERPIMAGES_SRC)
	$(AM_V_GEN)$(call gen-image,interpimages,$*,png)

INTERPIMAGES_TXT = $(addsuffix .txt,$(INTERPIMAGES_PLOTS))
BUILT_DOC_IMAGES_TXT += $(INTERPIMAGES_TXT)
$(INTERPIMAGES_TXT): %reldir%/%.txt : %reldir%/$(INTERPIMAGES_SRC)
	$(AM_V_GEN)$(call gen-image,interpimages,$*,txt)


PLOTIMAGES_SRC = plotimages.m
DOC_IMAGES_SRC += %reldir%/$(PLOTIMAGES_SRC)
PLOTIMAGES_PLOTS = $(addprefix %reldir%/,plot hist errorbar polar mesh plot3 extended precisiondate)

PLOTIMAGES_EPS = $(addsuffix .eps,$(PLOTIMAGES_PLOTS))
BUILT_DOC_IMAGES_EPS += $(PLOTIMAGES_EPS)
$(PLOTIMAGES_EPS): %reldir%/%.eps : %reldir%/$(PLOTIMAGES_SRC)
	$(AM_V_GEN)$(call gen-image,plotimages,$*,eps)

PLOTIMAGES_PDF = $(addsuffix .pdf,$(PLOTIMAGES_PLOTS))
BUILT_DOC_IMAGES_PDF += $(PLOTIMAGES_PDF)
$(PLOTIMAGES_PDF): %reldir%/%.pdf : %reldir%/$(PLOTIMAGES_SRC)
	$(AM_V_GEN)$(call gen-image,plotimages,$*,pdf)

PLOTIMAGES_PNG = $(addsuffix .png,$(PLOTIMAGES_PLOTS))
BUILT_DOC_IMAGES_PNG += $(PLOTIMAGES_PNG)
$(PLOTIMAGES_PNG): %reldir%/%.png : %reldir%/$(PLOTIMAGES_SRC)
	$(AM_V_GEN)$(call gen-image,plotimages,$*,png)

PLOTIMAGES_TXT = $(addsuffix .txt,$(PLOTIMAGES_PLOTS))
BUILT_DOC_IMAGES_TXT += $(PLOTIMAGES_TXT)
$(PLOTIMAGES_TXT): %reldir%/%.txt : %reldir%/$(PLOTIMAGES_SRC)
	$(AM_V_GEN)$(call gen-image,plotimages,$*,txt)


SPARSEIMAGES_SRC = sparseimages.m
DOC_IMAGES_SRC += %reldir%/$(SPARSEIMAGES_SRC)
SPARSEIMAGES_PLOTS = $(addprefix %reldir%/,gplot grid spmatrix spchol spcholperm)

SPARSEIMAGES_EPS = $(addsuffix .eps,$(SPARSEIMAGES_PLOTS))
BUILT_DOC_IMAGES_EPS += $(SPARSEIMAGES_EPS)
$(SPARSEIMAGES_EPS): %reldir%/%.eps : %reldir%/$(SPARSEIMAGES_SRC)
	$(AM_V_GEN)$(call gen-image,sparseimages,$*,eps)

SPARSEIMAGES_PDF = $(addsuffix .pdf,$(SPARSEIMAGES_PLOTS))
BUILT_DOC_IMAGES_PDF += $(SPARSEIMAGES_PDF)
$(SPARSEIMAGES_PDF): %reldir%/%.pdf : %reldir%/$(SPARSEIMAGES_SRC)
	$(AM_V_GEN)$(call gen-image,sparseimages,$*,pdf)

SPARSEIMAGES_PNG = $(addsuffix .png,$(SPARSEIMAGES_PLOTS))
BUILT_DOC_IMAGES_PNG += $(SPARSEIMAGES_PNG)
$(SPARSEIMAGES_PNG): %reldir%/%.png : %reldir%/$(SPARSEIMAGES_SRC)
	$(AM_V_GEN)$(call gen-image,sparseimages,$*,png)

SPARSEIMAGES_TXT = $(addsuffix .txt,$(SPARSEIMAGES_PLOTS))
BUILT_DOC_IMAGES_TXT += $(SPARSEIMAGES_TXT)
$(SPARSEIMAGES_TXT): %reldir%/%.txt : %reldir%/$(SPARSEIMAGES_SRC)
	$(AM_V_GEN)$(call gen-image,sparseimages,$*,txt)


SPLINEIMAGES_SRC = splineimages.m
DOC_IMAGES_SRC += %reldir%/$(SPLINEIMAGES_SRC)
SPLINEIMAGES_PLOTS = $(addprefix %reldir%/,splinefit1 splinefit2 splinefit3 splinefit4 splinefit6)

SPLINEIMAGES_EPS = $(addsuffix .eps,$(SPLINEIMAGES_PLOTS))
BUILT_DOC_IMAGES_EPS += $(SPLINEIMAGES_EPS)
$(SPLINEIMAGES_EPS): %reldir%/%.eps : %reldir%/$(SPLINEIMAGES_SRC)
	$(AM_V_GEN)$(call gen-image,splineimages,$*,eps)

SPLINEIMAGES_PDF = $(addsuffix .pdf,$(SPLINEIMAGES_PLOTS))
BUILT_DOC_IMAGES_PDF += $(SPLINEIMAGES_PDF)
$(SPLINEIMAGES_PDF): %reldir%/%.pdf : %reldir%/$(SPLINEIMAGES_SRC)
	$(AM_V_GEN)$(call gen-image,splineimages,$*,pdf)

SPLINEIMAGES_PNG = $(addsuffix .png,$(SPLINEIMAGES_PLOTS))
BUILT_DOC_IMAGES_PNG += $(SPLINEIMAGES_PNG)
$(SPLINEIMAGES_PNG): %reldir%/%.png : %reldir%/$(SPLINEIMAGES_SRC)
	$(AM_V_GEN)$(call gen-image,splineimages,$*,png)

SPLINEIMAGES_TXT = $(addsuffix .txt,$(SPLINEIMAGES_PLOTS))
BUILT_DOC_IMAGES_TXT += $(SPLINEIMAGES_TXT)
$(SPLINEIMAGES_TXT): %reldir%/%.txt : %reldir%/$(SPLINEIMAGES_SRC)
	$(AM_V_GEN)$(call gen-image,splineimages,$*,txt)

endif

