%canon_reldir%_EXTRA_DIST =

%canon_reldir%_DAT = \
  %reldir%/penny.mat \
  %reldir%/west0479.mat

%canon_reldir%_EXTRA_DIST += \
  $(%canon_reldir%_DAT) \
  %reldir%/README \
  %reldir%/west0479.mtx

octdata_DATA += \
  $(%canon_reldir%_DAT)

EXTRA_DIST += $(%canon_reldir%_EXTRA_DIST)
