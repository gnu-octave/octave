FCN_FILE_DIRS += pkg

pkg_PRIVATE_FCN_FILES = \
  pkg/private/get_forge_pkg.m

pkg_FCN_FILES = \
  pkg/pkg.m \
  $(pkg_PRIVATE_FCN_FILES)

FCN_FILES += $(pkg_FCN_FILES)

PKG_ADD_FILES += pkg/PKG_ADD

DIRSTAMP_FILES += pkg/$(octave_dirstamp)
