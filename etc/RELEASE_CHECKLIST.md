# `VERSION`.1 Release Checklist #

**Timeline** (tentative)

* YYYY-MM-DD 🚀 Release Kick-off ⚽ (default branch merged to stable)
* YYYY-MM-DD 🛠️ 1st release candidate **`VERSION`.0.90** on
  <https://alpha.gnu.org/gnu/octave/>
* YYYY-MM-DD 🏁 Final Release 🎉 **`VERSION`.1.0** on
  <https://ftp.gnu.org/gnu/octave/>

Please use `<strike> </strike>` to mark items below as done.

## 🚀 Kick-off ⚽ ##

------------------------------------------------------------

### ⚙️ Update stable and default branch to new version numbers ###

Completion Date:

* Merge default onto stable to become the current stable release (see
  instructions in `etc/HACKING.md`).
* Bump version numbers in `configure.ac` on stable branch (see
  `etc/HACKING.md`).
* Bump version numbers in `configure.ac` on default branch (see
  `etc/HACKING.md`).
* Create new `etc/NEWS.VERSION+1.md` file on default branch by copying
  `etc/NEWS.VERSION.md` and then removing text so that it is a template file
  with headings only.

### ⚙️ Update gnulib to latest version ###

Completion Date:

**Must occur first** as it could resolve existing, or create new, bug reports.
You should run `./bootstrap` in the source tree after updating to the new
gnulib version.

### 📢 Call for bug reports ###

Completion Date:

* Put out a general call for reports on [Octave Discourse](https://octave.discourse.group/) for all outstanding, but unreported, known bugs.
* Create Savannah `Release` tag `VERSION.0.90` on bug tracker for 1st release
  candidate.
* Mark items that should be fixed for the release with this tag.
* Update Savannah `Planned Release` and `Fixed Release` tags by adding
  `VERSION.1.0 (current stable)` and `VERSION+1.1.0 (current default)`.
* Review bug overview at <https://octave.space/savannah/>.

### 📢 Update GUI translation files ###

Completion Date:

* Call for updates ([Octave Discourse](https://octave.discourse.group/)) that
  might change GUI strings.
* String freeze date:
* Update language translation files (`*.ts`).  See instructions in
  `libgui/languages/build_ts/README.md`.
* Create bug report on Savannah as a centralized location for uploading files
  and tracking status of translations.
* Call for translations of GUI strings on [Octave Discourse Maintainers](https://octave.discourse.group/c/maintainers/7), CC-ing the translators (see [list of translators](https://hg.savannah.gnu.org/hgweb/octave/file/tip/libgui/languages/translators)).
* Collect translation files on Savannah bug report and push to Mercurial.

### ⚖️ GPL License activities ###

Completion Date:

* Update copyright statements for all source controlled files.

    Command to update copyright notifications

    `hg locate | xargs sed -i 's/Copyright (C) \([0-9][0-9][0-9][0-9]\)-2023 The Octave Project Developers/Copyright (C) \1-2024 The Octave Project Developers/; s/Copyright (C) 2023 The Octave Project Developers/Copyright (C) 2023-2024 The Octave Project Developers/'`

    Command to check results before checking in

    `hg diff | grep ^[-+][^-+] | sed 's/[0-9][0-9][0-9][0-9]-/YYYY-/' | sort -u`

* Update dates in any other locations (`CITATION`, MXE `README.html` file).
* Add any new contributors to `doc/interpreter/contributors.in` who wish to be
  mentioned (don't add them without permission).

### ✅ Style-check code base ###

Completion Date:

This will produce lots of whitespace changes, but no behavior changes. **Must
occur after patches have been added**, since whitespace changes can prevent
patches from applying.

* [m-file style check](https://wiki.octave.org/Octave_style_guide)
* [C++ style check](https://wiki.octave.org/C%2B%2B_style_guide)

### 📖 Review documentation ###

Completion Date:

* Grammar check documentation (See `doc/interpreter/doccheck/README`).
* Spell check documentation (`make spellcheck`).
* Verify no functions missing from manual
  (`make doc/interpreter/undocumented_list`).
* Verify deprecated functions removed from manual (`*.txi`) and from "see also"
  links.
* Verify all formats (Info, HTML, PDF) build correctly.
* Review `etc/NEWS.VERSION.md` for any features which should be announced.
* Review `__unimplemented__.m` for the latest changes to
  [Octave Forge](https://wiki.octave.org/Octave_Forge) packages and new Matlab
  functions.
* Update `installer-files/README.html` in MXE Octave with version highlights.

## 🔃 Repeat until all bugs are resolved ##

------------------------------------------------------------

Completion Date of first iteration:

### ⚠️ Current state at Savannah ###

Bug overview at <https://octave.space/savannah/>.

### ✅ `make check` ###

* Verify `make check` is passing on all [buildbot combinations of OS and compilers](http://buildbot.octave.org:8010/#/waterfall) and [GitHub CI runners](https://github.com/gnu-octave/octave/actions).  Also check [test suite runs on "freshly brewed Octave for Windows"](https://github.com/gnu-octave/octave-buildbot/actions) (Kai's buildbots on octave.space).
* Use software tools to check quality of Octave code.
  * Check for memory leaks by configuring with
    `--enable-address-sanitizer-flags` and compiling with
    `-g -O0 -fno-optimize-sibling-calls` in `CFLAGS`, `CXXFLAGS`, and
    `LDFLAGS`.
  * Check for bad memory accesses by compiling with
    `-g -O0 -fsanitize=undefined -fno-omit-frame-pointer
    -fno-optimize-sibling-calls` in `CFLAGS`, `CXXFLAGS`, and `LDFLAGS`.
  * Update static code analysis results.
      * For `clang`, do `scan-build make -j<N> all` and then `scan-view`.
      * See [PVS static analyzer - 5.0 Release](https://wiki.octave.org/PVS_static_analyzer_-_5.0_Release).
  * Use other tools such as `cppcheck`, etc.
* Start discussion on [Octave Discourse Maintainers](https://octave.discourse.group/c/maintainers/8) about which failing tests that must be fixed and which can be declared **WON'T FIX**.

### 🛠️ Create new release candidate ###

* Ensure correct version information (see "Release Numbering" in
  `/etc/HACKING.md`).
* Create `hg tag` in repository with release candidate version number
  (`rc-MAJOR-MINOR-PATCH`).
* Verify `make dist` works.
* Verify `make distcheck` passes.
* Create [Windows Installer](https://wiki.octave.org/Windows_Installer).
* Upload release candidates to <https://alpha.gnu.org/gnu/octave/>.
* Check [Windows Installer](https://wiki.octave.org/Windows_Installer)
  (executable and zip formats) against false positive detection at
  [virustotal.com](https://virustotal.com/).
* Add release candidate version to Savannah bug tracker.
* Announce release candidate to
  [Octave Discourse](https://octave.discourse.group/).

## 🏁 Final Release 🎉 ##

------------------------------------------------------------

### 📃 Update version information ###

Completion Date:

* Ensure correct version information in `configure.ac` (see "Release Numbering"
  in `/etc/HACKING.md`)
  * Set `OCTAVE_RELEASE_DATE` to the current date.
  * Set the year in `OCTAVE_COPYRIGHT` to the current year.
* Create `hg tag` in repository with release candidate version number
  (`release-VERSION-1-0`).
* Update `etc/NEWS.VERSION.md` (final release date in Summary header).
* Update `CITATION` (version, year, URL) if not already done as part of "GPL
  License activities.
* Update `etc/icons/org.octave.Octave.appdata.xml` (version number and release
  date).
* Upload all tarballs to <https://ftp.gnu.org/gnu/octave/>.
* Update web site files: `NEWS-VERSION.html`, `index.in`, `news.in`, and
  `download.in`.
* Update <https://wiki.octave.org/Release_History> page.
* Update Savannah bug tracker `Release` field to have new release number.
* Update Savannah bug tracker: **OPEN** bugs marked as **WON'T FIX** should be
  marked as **CONFIRMED** (or more appropriate) for the final release.
* Hide release candidate versions for `Release` field on Savannah.

### 📢 Announce final release ###

Completion Date:

* Octave mailing-lists (yes, still) <help@octave.org> and <info-gnu@gnu.org>.
* Octave web site (<https://hg.octave.org/web-octave>)
* Steps documented in this
  [changeset](https://hg.octave.org/web-octave/rev/fe59d0118a2b).
* Upload documentation (manual HTML + PDF `octave.org/doc` and version in
  `octave.org/.htaccess`, Doxygen `octave.org/doxygen`)
* This wiki
* Template:Release = 7.3.0
* Template:Release Date = November 2, 2022
* Template:Release Year = 2022

### ☑️ Post-Release ###

Completion Date:

* Remove all deprecated functions scheduled for deletion on default
  branch.  These functions are tagged with `OCTAVE_DEPRECATED` in C++ or are
  m-files located in the directory `scripts/deprecated`.  Check file
  `etc/NEWS.VERSION-2.md` for list of features that have been deprecated.

