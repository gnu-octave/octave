FCN_FILE_DIRS += scripts/java

scripts_java_FCN_FILES = \
  scripts/java/java_get.m \
  scripts/java/java_set.m \
  scripts/java/javaArray.m \
  scripts/java/javaaddpath.m \
  scripts/java/javachk.m \
  scripts/java/javaclasspath.m \
  scripts/java/javamem.m \
  scripts/java/javarmpath.m \
  scripts/java/usejava.m

if AMCOND_HAVE_JAVA
scripts_java_JAR_FILES = scripts/java/octave.jar

JAR_FILES += $(scripts_java_JAR_FILES)
endif

org_octave_dir = org/octave

if AMCOND_HAVE_JAVA
JAVA_SRC = \
  $(org_octave_dir)/ClassHelper.java \
  $(org_octave_dir)/OctClassLoader.java \
  $(org_octave_dir)/Octave.java \
  $(org_octave_dir)/OctaveReference.java \
  $(org_octave_dir)/Matrix.java \
  $(org_octave_dir)/JDialogBox.java \
  $(org_octave_dir)/DlgListener.java \
  $(org_octave_dir)/TeXtranslator.java \
  $(org_octave_dir)/TeXcode.java
endif

JAVA_CLASSES = $(JAVA_SRC:.java=.class)

JAVA_IMAGES = \
  $(org_octave_dir)/images/question.png \
  $(org_octave_dir)/images/error.png \
  $(org_octave_dir)/images/warning.png \
  $(org_octave_dir)/images/information.png \
  $(org_octave_dir)/images/octave.png

scripts_java_JAVA_SRC = $(addprefix scripts/java/, $(JAVA_SRC))

scripts_java_JAVA_CLASSES = $(addprefix scripts/java/, $(JAVA_CLASSES))

scripts_java_JAVA_IMAGES = $(addprefix scripts/java/, $(JAVA_IMAGES))

srcdir_scripts_java_JAVA_IMAGES = $(addprefix $(srcdir)/scripts/java/, $(JAVA_IMAGES))

$(scripts_java_JAVA_CLASSES) : %.class : %.java scripts/java/$(octave_dirstamp)
	$(AM_V_GEN)$(MKDIR_P) scripts/java/$(org_octave_dir) && \
	( cd $(srcdir)/scripts/java; "$(JAVAC)" -source 1.3 -target 1.3 -d $(abs_top_builddir)/scripts/java $(org_octave_dir)/$(<F) )

scripts/java/images.stamp: $(srcdir_scripts_java_JAVA_IMAGES)
	$(AM_V_GEN)if [ "x$(srcdir)" != "x." ]; then \
	  $(MKDIR_P) scripts/java/$(org_octave_dir)/images; \
	  cp $(srcdir_scripts_java_JAVA_IMAGES) scripts/java/$(org_octave_dir)/images; \
	fi && \
	touch $@

if AMCOND_HAVE_JAVA
scripts/java/octave.jar: scripts/java/images.stamp $(scripts_java_JAVA_CLASSES)
	$(AM_V_GEN)rm -f $@-t $@ && \
	( cd scripts/java; \
	  "$(JAR)" cf octave.jar-t $(JAVA_CLASSES) $(JAVA_IMAGES) ) && \
	mv $@-t $@
endif

EXTRA_DIST += \
  $(scripts_java_JAR_FILES) \
  $(scripts_java_JAVA_SRC) \
  $(scripts_java_JAVA_IMAGES)

CLEANFILES += \
  $(scripts_java_JAR_FILES) \
  $(scripts_java_JAVA_CLASSES)

DISTCLEANFILES += scripts/java/images.stamp

scripts_javadir = $(fcnfiledir)/java

scripts_java_DATA = \
  $(scripts_java_FCN_FILES) \
  $(scripts_java_JAR_FILES)

FCN_FILES += $(scripts_java_FCN_FILES)

PKG_ADD_FILES += scripts/java/PKG_ADD

DIRSTAMP_FILES += scripts/java/$(octave_dirstamp)
