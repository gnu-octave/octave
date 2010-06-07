FCN_FILE_DIRS += pkg

pkg_FCN_FILES = \
  pkg/pkg.m \
  pkg/get_forge_pkg.m

FCN_FILES += $(pkg_FCN_FILES)

PKG_ADD_FILES += pkg/PKG_ADD

DIRSTAMP_FILES += pkg/$(octave_dirstamp)
