%canon_reldir%_EXTRA_DIST =

%canon_reldir%_SRC = \
   %reldir%/penny.mat

octdata_DATA += \
  $(%canon_reldir%_SRC)

%canon_reldir%_EXTRA_DIST += \
  $(%canon_reldir%_SRC)

EXTRA_DIST += $(%canon_reldir%_EXTRA_DIST)
