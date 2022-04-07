FCN_FILE_DIRS += %reldir%

%canon_reldir%_FCN_FILES = \
  %reldir%/.oct-config \
  %reldir%/__vectorize__.m \
  %reldir%/findstr.m \
  %reldir%/flipdim.m \
  %reldir%/genvarname.m \
  %reldir%/isdir.m \
  %reldir%/isequalwithequalnans.m \
  %reldir%/isstr.m \
  %reldir%/maxNumCompThreads.m \
  %reldir%/setstr.m \
  %reldir%/strmatch.m \
  %reldir%/strread.m \
  %reldir%/textread.m \
  %reldir%/vectorize.m

## include %reldir%/@inline/module.mk
## The include above fails because Automake cannot process the '@' character.
## As a work around, the contents of %reldir%/@inline/module.mk are placed directly
## in this module.mk file.
scripts_EXTRA_DIST += %reldir%/@inline/module.mk
######################## include %reldir%/@inline/module.mk ########################
FCN_FILE_DIRS += %reldir%/@inline

%canon_reldir%_@inline_FCN_FILES = \
  %reldir%/@inline/argnames.m \
  %reldir%/@inline/cat.m \
  %reldir%/@inline/char.m \
  %reldir%/@inline/disp.m \
  %reldir%/@inline/exist.m \
  %reldir%/@inline/feval.m \
  %reldir%/@inline/formula.m \
  %reldir%/@inline/horzcat.m \
  %reldir%/@inline/inline.m \
  %reldir%/@inline/nargin.m \
  %reldir%/@inline/nargout.m \
  %reldir%/@inline/subsref.m \
  %reldir%/@inline/symvar.m \
  %reldir%/@inline/vectorize.m \
  %reldir%/@inline/vertcat.m

%canon_reldir%_@inlinedir = $(fcnfiledir)/legacy/@inline

%canon_reldir%_@inline_DATA = $(%canon_reldir%_@inline_FCN_FILES)

FCN_FILES += $(%canon_reldir%_@inline_FCN_FILES)

PKG_ADD_FILES += %reldir%/@inline/PKG_ADD

DIRSTAMP_FILES += %reldir%/@inline/$(octave_dirstamp)
####################### end include %reldir%/@inline/module.mk #####################

%canon_reldir%dir = $(fcnfiledir)/legacy

%canon_reldir%_DATA = $(%canon_reldir%_FCN_FILES)

FCN_FILES += $(%canon_reldir%_FCN_FILES)

PKG_ADD_FILES += %reldir%/PKG_ADD

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)
