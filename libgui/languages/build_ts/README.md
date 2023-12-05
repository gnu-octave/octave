# Updating language files for the Octave GUI

## Why updating and when?

Before a new release of Octave, the language files  `libgui/languages/*.ts` have to be updated with changed or new strings in the source files in order to add translations afterwards.

## How to update the files

 The update is carried out by the Qt-tool `lupdate` which collect strings in source files found in the search path(s) provided as input argument to `lupdate`. The tool `lupdate`is called from the script

- `libgui/languages/build_ts/update_ts_files`

which also determines the required search paths for each language.

Besides source folders within `libgui`, the search path is extended by some qt and qsci files, collected in `libgui/build_ts/octave-qt` and `libgui/build_ts/octave-qsci` respectively, whenever there are no translation files for the language from Qt or QsciScintilla.

The Following steps are required in order to update the language files `libgui/languages/*.ts`:

1. Change into `libgui/languages`
2. Start the update by calling `build_ts/update_ts_files`
3. Select the language files that should be updated
