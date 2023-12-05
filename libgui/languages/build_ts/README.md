# Updating language files for the Octave GUI

## Why updating and when?

Before a new release of Octave, the language files  `libgui/languages/*.ts` have to be updated with changed or new strings in the source files in order to add translations afterwards.

## How to update the files (*.ts)

 The update is carried out by the Qt-tool `lupdate` which collect strings in source files found in the search path(s) provided as input argument to `lupdate`. The tool `lupdate`is called from the script

- `libgui/languages/build_ts/update_ts_files`

which also determines the required search paths for each language.

Besides source folders within `libgui`, the search path is extended by some Qt and Qsci files, collected in

- `libgui/build_ts/octave-qt` and
- `libgui/build_ts/octave-qsci`

respectively, whenever there are no translation files for the language from Qt or QScintilla.

The Following steps are required in order to update the language files `libgui/languages/*.ts`:

1. Change into `libgui/languages`
2. Start the update by calling `build_ts/update_ts_files`
3. Select the language files that should be updated

## Updating Qt and Qsci used for the language files

From time to time, like, e.g., when a new major version of Qt is available, the source files in

- `libgui/build_ts/octave-qt` and
- `libgui/build_ts/octave-qsci`

should also be updated. These source are scanned for strings for language where no translation is provided by Qt and/or QScintilla. The sources are update with the following steps:

1. Download the Qt sources (example for Qt6):
    1. `$ git clone git://code.qt.io/qt/qt5.git qt6`
    2. `$ cd qt6`
    3. `$ git switch 6.6`
    4. `$ perl init-repository`
2. [Download the latest QScintilla](https://riverbankcomputing.com/software/qscintilla/download) and unpack he archive to the desired location
3. Change into `libgui/languages/build_ts`
4. Call `./update_external_sources  path_qt  path_qsci` where `path_qt` and `path_qsci` are the absolute or relative paths to the Qt and QScintilla sources respectively.
