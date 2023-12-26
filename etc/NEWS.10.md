Summary of important user-visible changes for version 10 (yyyy-mm-dd):
---------------------------------------------------------------------

### General improvements

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
