%canon_reldir%_EXTRA_DIST =

include %reldir%/code/module.mk
include %reldir%/data/module.mk

%canon_reldir%_EXTRA_DIST += \
  %reldir%/code/COPYING

EXTRA_DIST += $(%canon_reldir%_EXTRA_DIST)
