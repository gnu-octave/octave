FCN_FILE_DIRS += %reldir%

%canon_reldir%_FCN_FILES = \
  %reldir%/.oct-config \
  %reldir%/addtodate.m \
  %reldir%/asctime.m \
  %reldir%/calendar.m \
  %reldir%/clock.m \
  %reldir%/ctime.m \
  %reldir%/date.m \
  %reldir%/datenum.m \
  %reldir%/datestr.m \
  %reldir%/datevec.m \
  %reldir%/eomday.m \
  %reldir%/etime.m \
  %reldir%/is_leap_year.m \
  %reldir%/now.m \
  %reldir%/weekday.m

%canon_reldir%dir = $(fcnfiledir)/time

%canon_reldir%_DATA = $(%canon_reldir%_FCN_FILES)

FCN_FILES += $(%canon_reldir%_FCN_FILES)

PKG_ADD_FILES += %reldir%/PKG_ADD

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)
