FCN_FILE_DIRS += scripts/time

scripts_time_FCN_FILES = \
  scripts/time/addtodate.m \
  scripts/time/asctime.m \
  scripts/time/calendar.m \
  scripts/time/clock.m \
  scripts/time/ctime.m \
  scripts/time/date.m \
  scripts/time/datenum.m \
  scripts/time/datestr.m \
  scripts/time/datevec.m \
  scripts/time/eomday.m \
  scripts/time/etime.m \
  scripts/time/is_leap_year.m \
  scripts/time/now.m \
  scripts/time/weekday.m

FCN_FILES += $(scripts_time_FCN_FILES)

PKG_ADD_FILES += scripts/time/PKG_ADD

DIRSTAMP_FILES += scripts/time/$(octave_dirstamp)
