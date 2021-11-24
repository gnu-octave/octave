FCN_FILE_DIRS += \
  %reldir% \
  %reldir%/private

%canon_reldir%_PRIVATE_FCN_FILES = \
  %reldir%/private/build.m \
  %reldir%/private/configure_make.m \
  %reldir%/private/default_prefix.m \
  %reldir%/private/describe.m \
  %reldir%/private/dirempty.m \
  %reldir%/private/expand_rel_paths.m \
  %reldir%/private/standardize_paths.m \
  %reldir%/private/get_description.m \
  %reldir%/private/get_forge_download.m \
  %reldir%/private/get_forge_pkg.m \
  %reldir%/private/get_inverse_dependencies.m \
  %reldir%/private/get_unsatisfied_deps.m \
  %reldir%/private/getarch.m \
  %reldir%/private/getarchdir.m \
  %reldir%/private/install.m \
  %reldir%/private/installed_packages.m \
  %reldir%/private/list_forge_packages.m \
  %reldir%/private/load_packages.m \
  %reldir%/private/load_packages_and_dependencies.m \
  %reldir%/private/make_rel_paths.m \
  %reldir%/private/rebuild.m \
  %reldir%/private/save_order.m \
  %reldir%/private/uninstall.m \
  %reldir%/private/unload_packages.m

%canon_reldir%_FCN_FILES = \
  %reldir%/.oct-config \
  %reldir%/pkg.m

%canon_reldir%dir = $(fcnfiledir)/pkg

%canon_reldir%_DATA = $(%canon_reldir%_FCN_FILES)

%canon_reldir%_privatedir = $(fcnfiledir)/pkg/private

%canon_reldir%_private_DATA = $(%canon_reldir%_PRIVATE_FCN_FILES)

FCN_FILES += \
  $(%canon_reldir%_FCN_FILES) \
  $(%canon_reldir%_PRIVATE_FCN_FILES)

PKG_ADD_FILES += %reldir%/PKG_ADD

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)
