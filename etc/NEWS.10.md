Summary of important user-visible changes for version 10 (yyyy-mm-dd):
---------------------------------------------------------------------

### General improvements

- Three short form aliases have been added for long form options when starting
`octave`.
  * `-e CODE` for `--eval CODE`
  * `-g` for `--gui`
  * `-G` for `--no-gui`

- Three long form options have been introduced for clarity.
  * `--no-init-user` : Don't read user configuration files ~/.octaverc or
    .octaverc files at startup.
  * `--no-init-site` : Don't read site-wide configuration files at startup.
  * `--no-init-all` : Don't read any configuration files at startup.

### Graphical User Interface

### Graphics backend

- `polar` plots now include the center tick mark value, typically 0, in
the 'rtick' parameter when the plot is created.  Subsequent modifications
to 'rtick' by the function `rticks` will only include the center tick mark
value if it is specified.

### Matlab compatibility

- `height` and `width` are now aliases for the `rows` and `columns` functions.

- All colormaps now default to a size of 256 colors. (The previous default
size was 64.

### Alphabetical list of new functions added in Octave 10

* `rticklabels`
* `tticklabels`

### Deprecated functions, properties, and operators

The following functions and properties have been deprecated in Octave 10
and will be removed from Octave 12 (or whatever version is the second
major release after 10):

- Functions

        Function               | Replacement
        -----------------------|------------------

- Core

        Function                              | Replacement
        --------------------------------------|------------------
        symbol_record_rep::mark_as_variable   | symbol_record_rep::mark_variable
        symbol_record_rep::unmark_as_variable | symbol_record_rep::unmark_variable
        symbol_record::mark_as_variable       | symbol_record::mark_variable
        symbol_record::unmark_as_variable     | symbol_record::unmark_variable
        interpreter::verbose                  | interpreter::init_trace
        cmdline_options::verbose              | cmdline_options::init_trace

  - A new method `rwdata ()` provides direct read/write access (a pointer) to the
data in a liboctave `Array` object (or its derived classes such as `Matrix`).
Historically, the method `fortran_vec ()` provided this access, but the name
is unclear, and it is recommended to use `rwdata ()` in any new code.  There
are no plans to deprecate `fortran_vec`.

  - The `--verbose`,`-V` command-line option has been deprecated.  Use
`--init-trace` to print the configuration files executed during initialization.

The following features were deprecated in Octave 8 and have been removed
from Octave 10.

- Functions

        Function               | Replacement
        -----------------------|------------------
        shift                  | circshift
        sparse_auto_mutate     | none (see below)


Summary of bugs fixed for version 10.1.0 (yyyy-mm-dd):
----------------------------------------------------

### Old release news

- [Octave 9.x](etc/NEWS.9.md)
- [Octave 8.x](etc/NEWS.8.md)
- [Octave 7.x](etc/NEWS.7.md)
- [Octave 6.x](etc/NEWS.6.md)
- [Octave 5.x](etc/NEWS.5.md)
- [Octave 4.x](etc/NEWS.4)
- [Octave 3.x](etc/NEWS.3)
- [Octave 2.x](etc/NEWS.2)
- [Octave 1.x](etc/NEWS.1)
