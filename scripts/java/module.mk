FCN_FILE_DIRS += scripts/java

%canon_reldir%_FCN_FILES = \
  %reldir%/javaArray.m \
  %reldir%/java_get.m \
  %reldir%/java_set.m \
  %reldir%/javaaddpath.m \
  %reldir%/javachk.m \
  %reldir%/javaclasspath.m \
  %reldir%/javamem.m \
  %reldir%/javarmpath.m \
  %reldir%/usejava.m

if AMCOND_HAVE_JAVA
%canon_reldir%_JAR_FILES = %reldir%/octave.jar

JAR_FILES += $(%canon_reldir%_JAR_FILES)
endif

org_octave_dir = org/octave

if AMCOND_HAVE_JAVA
JAVA_SRC = \
  $(org_octave_dir)/ClassHelper.java \
  $(org_octave_dir)/Matrix.java \
  $(org_octave_dir)/OctClassLoader.java \
  $(org_octave_dir)/Octave.java \
  $(org_octave_dir)/OctaveReference.java
endif

JAVA_CLASSES = $(JAVA_SRC:.java=.class)

%canon_reldir%_JAVA_SRC = $(addprefix %reldir%/, $(JAVA_SRC))

%canon_reldir%_JAVA_CLASSES = $(addprefix %reldir%/, $(JAVA_CLASSES))

$(%canon_reldir%_JAVA_CLASSES) : %.class : %.java | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)$(MKDIR_P) %reldir%/$(org_octave_dir) && \
		( cd $(srcdir)/scripts/java; "$(JAVAC)" -source 1.3 -target 1.3 -Xlint:-options -d $(abs_top_builddir)/scripts/java $(org_octave_dir)/$(<F) )

if AMCOND_HAVE_JAVA
%reldir%/octave.jar: $(%canon_reldir%_JAVA_CLASSES)
	$(AM_V_GEN)rm -f $@-t $@ && \
	( cd scripts/java; \
	  "$(JAR)" cf octave.jar-t $(JAVA_CLASSES) ) && \
	mv $@-t $@
endif

%canon_reldir%dir = $(fcnfiledir)/java

%canon_reldir%_DATA = \
  $(%canon_reldir%_FCN_FILES) \
  $(%canon_reldir%_JAR_FILES)

FCN_FILES += $(%canon_reldir%_FCN_FILES)

PKG_ADD_FILES += %reldir%/PKG_ADD

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)

scripts_EXTRA_DIST += \
  $(%canon_reldir%_JAR_FILES) \
  $(%canon_reldir%_JAVA_SRC)

scripts_CLEANFILES += \
  $(%canon_reldir%_JAR_FILES) \
  $(%canon_reldir%_JAVA_CLASSES)
