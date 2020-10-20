FCN_FILE_DIRS += \
  %reldir% \
  %reldir%/private

%canon_reldir%_PRIVATE_FCN_FILES = \
  %reldir%/private/AbsRel_norm.m \
  %reldir%/private/check_default_input.m \
  %reldir%/private/integrate_adaptive.m \
  %reldir%/private/kahan.m \
  %reldir%/private/ode_event_handler.m \
  %reldir%/private/odedefaults.m \
  %reldir%/private/odemergeopts.m \
  %reldir%/private/runge_kutta_23.m \
  %reldir%/private/runge_kutta_23s.m \
  %reldir%/private/runge_kutta_45_dorpri.m \
  %reldir%/private/runge_kutta_interpolate.m \
  %reldir%/private/starting_stepsize.m

%canon_reldir%_FCN_FILES =  \
  %reldir%/.oct-config \
  %reldir%/decic.m \
  %reldir%/ode15i.m \
  %reldir%/ode15s.m \
  %reldir%/ode23.m \
  %reldir%/ode23s.m \
  %reldir%/ode45.m \
  %reldir%/odeget.m \
  %reldir%/odeplot.m \
  %reldir%/odeset.m

%canon_reldir%dir = $(fcnfiledir)/ode

%canon_reldir%_DATA = $(%canon_reldir%_FCN_FILES)

%canon_reldir%_privatedir = $(fcnfiledir)/ode/private

%canon_reldir%_private_DATA = $(%canon_reldir%_PRIVATE_FCN_FILES)

FCN_FILES += \
  $(%canon_reldir%_FCN_FILES) \
  $(%canon_reldir%_PRIVATE_FCN_FILES)

PKG_ADD_FILES += %reldir%/PKG_ADD

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)
