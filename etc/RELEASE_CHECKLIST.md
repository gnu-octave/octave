# `VERSION`.1 Release Checklist #

**Timeline** (tentative)

* YYYY-MM-DD 🚀 Release Kick-off ⚽ (default branch merged to stable)
* YYYY-MM-DD 🛠️ 1st release candidate **`VERSION`.0.90** on <https://alpha.gnu.org/gnu/octave/>
* YYYY-MM-DD 🏁 Final Release 🎉 **`VERSION`.1.0** on <https://ftp.gnu.org/gnu/octave/>

Please use `<strike> </strike>` to mark items below as done.

## 🚀 Kick-off ⚽ ##

------------------------------------------------------------

### ⚙️ Update gnulib to latest version ###

Completion Date: 

**Must occur first** as it could resolve existing, or create new, bug reports.  You should run `./bootstrap` in the source tree after updating to the new gnulib version.

### 📢 Call for bug reports ###

Completion Date:

* Put out a general call for reports on [Octave Discourse](https://octave.discourse.group/) for all outstanding unreported known bugs.
* Create Savannah tag `VERSION.0.90` on bug tracker for 1st release candidate.
* Mark items that should be fixed for the release with this tag.
* Bug overview at <https://octave.space/savannah/>.

### 📢 Call for translations ###

Completion Date:

* Call for updates ([Octave Discourse](https://octave.discourse.group/)) that might change GUI strings.
* String freeze date:
* Update language translation files (`*.ts`) using scripts from Torsten.
* Create bug report on Savannah as a centralized location for uploading files and tracking status of translations.
* Call for translations of GUI strings on [Octave Discourse Maintainers](https://octave.discourse.group/c/maintainers/7), CC-ing the translators (see [list of translators](https://hg.savannah.gnu.org/hgweb/octave/file/tip/libgui/languages/translators)).
* Collect translation files on Savannah bug report and push to Mercurial.

### ⚖️ GPL License activities ###

Completion Date:

* Update copyright statements for all source controlled files.
* Update dates in any other locations (launch message, citation, MXE files, etc.).
* Add any new contributors to `doc/interpreter/contributors.in` who wish to be mentioned (don't add them without permission).

### ✅ Style-check code base ###

Completion Date:

This will produce lots of whitespace changes, but no behavior changes. **Must occur after patches have been added**, since whitespace changes can prevent patches from applying.

* [m-file style check](https://wiki.octave.org/Octave_style_guide)
* [C++ style check](https://wiki.octave.org/C%2B%2B_style_guide)

### 📖 Review documentation ###

Completion Date:

* Grammar check documentation (See `doc/interpreter/doccheck/README`).
* Spell check documentation (`make spellcheck`).
* Verify no functions missing from manual (`make doc/interpreter/undocumented_list`).
* Verify deprecated functions removed from "see also" links.
* Verify all formats (Info, HTML, PDF) build correctly.
* Review `etc/NEWS.VERSION.md` for any features which should be announced.
* Review `__unimplemented__.m` for the latest changes to [Octave Forge](https://wiki.octave.org/Octave_Forge) packages and new Matlab functions.
* Update major version number in "`@subtitle Edition XXX`" in `octave.texi`.
* Update `installer-files/README.html` in MXE Octave with version highlights.

## 🔃 Repeat until all bugs are resolved ##

------------------------------------------------------------

Completion Date of first iteration:

### ⚠️ Current state at Savannah ###

Bug overview at <https://octave.space/savannah/>.

### ✅ `make check` ###

* Verify `make check` is passing on all [buildbot combinations of OS and compilers](http://buildbot.octave.org:8010/#/waterfall) and [GitHub CI runners](https://github.com/gnu-octave/octave/actions).  Also check [test suite runs on "freshly brewed Octave for Windows"](https://github.com/gnu-octave/octave-buildbot/actions) (Kai's buildbots on octave.space).
* Use software tools to check quality of Octave code.
  * Check for memory leaks by compiling with `-fsanitize=undefined`, `--enable-address-sanitizer-flags`.
  * Update static code analysis results.  See [PVS static analyzer - 5.0 Release](https://wiki.octave.org/PVS_static_analyzer_-_5.0_Release).
  * Use other tools such as `cppcheck`, etc.
* Start discussion on [Octave Discourse Maintainers](https://octave.discourse.group/c/maintainers/8) about which failing tests that must be fixed and which can be declared **WON'T FIX**.

### 🛠️ Create new release candidate ###

* Ensure correct version information.
* Create hg tag in repository with release candidate version number.
* Verify `make distcheck` passes.
* Verify `make dist` works.
* Create [Windows Installer](https://wiki.octave.org/Windows_Installer).
* Upload release candidates.
* Check [Windows Installer](https://wiki.octave.org/Windows_Installer) (executable and zip formats) against false positive detection at [virustotal.com](https://virustotal.com/).
* Add release candidate version to Savannah bug tracker.
* Announce release candidate to [Octave Discourse](https://octave.discourse.group/).

## 🏁 Final Release 🎉 ##

------------------------------------------------------------

### 📃 Update version information ###

Completion Date:

* Ensure correct version information.
* Create hg tag in repository with release version number.
* Update `etc/NEWS.VERSION.md` (final release date).
* Update `CITATION` (version, year, URL).
* Update `etc/icons/org.octave.Octave.appdata.xml` (version number and release date).
* Update Savannah bug tracker version info.
* Update Savannah bug tracker: **OPEN** bugs marked as **WON'T FIX** should be marked as **CONFIRMED** (or more appropriate) for the final release.
* Remove release candidate versions from Savannah.

### 📢 Announce final release ###

Completion Date:

* Octave mailing-lists
* Octave web site (<https://hg.octave.org/web-octave>)
* Steps documented in this [changeset](https://hg.octave.org/web-octave/rev/fe59d0118a2b).
* Upload documentation (manual HTML + PDF `octave.org/doc` and version in `octave.org/.htaccess`, Doxygen `octave.org/doxygen`)
* This wiki
* Template:Release = 7.3.0
* Template:Release Date = November 2, 2022
* Template:Release Year = 2022

### ☑️ Post-Release ###

Completion Date:

* Merge default onto stable to become the current stable release.
* Ensure correct version information.
* Remove all deprecated functions (either tagged with `OCTAVE_DEPRECATED` in C++ or in the directory `scripts/deprecated` for m-files) scheduled for deletion in "default" branch.  Check file `etc/NEWS.VERSION-2.md` for list of features that have been deprecated.
* Create new `etc/NEWS.VERSION+1.md` file by copying `etc/NEWS.VERSION.md` and then removing text so that it is a template file with headings only.
