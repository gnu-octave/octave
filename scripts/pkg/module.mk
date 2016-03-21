FCN_FILE_DIRS += \
  scripts/pkg \
  scripts/pkg/private

scripts_pkg_PRIVATE_FCN_FILES = \
  scripts/pkg/private/build.m \
  scripts/pkg/private/configure_make.m \
  scripts/pkg/private/copy_built_files.m \
  scripts/pkg/private/copy_files.m \
  scripts/pkg/private/create_pkgadddel.m \
  scripts/pkg/private/default_prefix.m \
  scripts/pkg/private/describe.m \
  scripts/pkg/private/dirempty.m \
  scripts/pkg/private/extract_pkg.m \
  scripts/pkg/private/finish_installation.m \
  scripts/pkg/private/fix_depends.m \
  scripts/pkg/private/fix_version.m \
  scripts/pkg/private/generate_lookfor_cache.m \
  scripts/pkg/private/get_description.m \
  scripts/pkg/private/get_forge_download.m \
  scripts/pkg/private/get_forge_pkg.m \
  scripts/pkg/private/getarch.m \
  scripts/pkg/private/getarchdir.m \
  scripts/pkg/private/getarchprefix.m \
  scripts/pkg/private/get_unsatisfied_deps.m \
  scripts/pkg/private/install.m \
  scripts/pkg/private/installed_packages.m \
  scripts/pkg/private/is_architecture_dependent.m \
  scripts/pkg/private/list_forge_packages.m \
  scripts/pkg/private/load_package_dirs.m \
  scripts/pkg/private/load_packages.m \
  scripts/pkg/private/load_packages_and_dependencies.m \
  scripts/pkg/private/packinfo_copy_file.m \
  scripts/pkg/private/parse_pkg_idx.m \
  scripts/pkg/private/prepare_installation.m \
  scripts/pkg/private/print_package_description.m \
  scripts/pkg/private/rebuild.m \
  scripts/pkg/private/save_order.m \
  scripts/pkg/private/shell.m \
  scripts/pkg/private/uninstall.m \
  scripts/pkg/private/unload_packages.m \
  scripts/pkg/private/verify_directory.m \
  scripts/pkg/private/write_index.m

scripts_pkg_FCN_FILES = scripts/pkg/pkg.m

scripts_pkgdir = $(fcnfiledir)/pkg

scripts_pkg_DATA = $(scripts_pkg_FCN_FILES)

scripts_pkg_privatedir = $(fcnfiledir)/pkg/private

scripts_pkg_private_DATA = $(scripts_pkg_PRIVATE_FCN_FILES)

FCN_FILES += \
  $(scripts_pkg_FCN_FILES) \
  $(scripts_pkg_PRIVATE_FCN_FILES)

PKG_ADD_FILES += scripts/pkg/PKG_ADD

DIRSTAMP_FILES += scripts/pkg/$(octave_dirstamp)
