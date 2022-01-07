Summary of important user-visible changes for version 8 (yyyy-mm-dd):
---------------------------------------------------------------------

### General improvements

- Octave's libraries are now built using symbol visibility by default.
That means that less symbols are exported from these libraries.
Configure with `--disable-lib-visibility-flags` to export all symbols
(as in previous versions).


### Graphical User Interface


### Graphics backend


### Matlab compatibility


### Alphabetical list of new functions added in Octave 8


### Deprecated functions, properties, and operators

The following functions and properties have been deprecated in Octave 8
and will be removed from Octave 10 (or whatever version is the second
major release after 8):

- Functions

  Function               | Replacement
  -----------------------|------------------
  `shift`                | `circshift`


The following functions were deprecated in Octave 6 and have been removed
from Octave 8.

- Functions

  Function               | Replacement
  -----------------------|------------------
  `runtests`             | `oruntests`

- The environment variable used by `mkoctfile` for linker flags is now
  `LDFLAGS` rather than `LFLAGS`.  `LFLAGS` was deprecated in Octave 6,
  and will be removed in a future version of Octave.

### Old release news

- [Octave 7.x](etc/NEWS.7)
- [Octave 6.x](etc/NEWS.6)
- [Octave 5.x](etc/NEWS.5)
- [Octave 4.x](etc/NEWS.4)
- [Octave 3.x](etc/NEWS.3)
- [Octave 2.x](etc/NEWS.2)
- [Octave 1.x](etc/NEWS.1)
