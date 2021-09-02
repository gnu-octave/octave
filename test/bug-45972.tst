%!test <*45972>
%! ascii_filename = tempname ();
%! binary_filename = tempname ();
%! f = @ (x, y, varargin) x + y + varargin{1};
%! unwind_protect
%!   save ("-text", ascii_filename, "f");
%!   save ("-binary", binary_filename, "f");
%!   ascii = load (ascii_filename);
%!   binary = load (binary_filename);
%!   assert (f (1, 2, 3), ascii.f (1, 2, 3));
%!   assert (f (1, 2, 3), binary.f (1, 2, 3));
%! unwind_protect_cleanup
%!   unlink (ascii_filename);
%!   unlink (binary_filename);
%! end_unwind_protect

%!testif HAVE_HDF5 <*45972>
%! hdf5_filename = tempname ();
%! f = @ (x, y, varargin) x + y + varargin{1};
%! unwind_protect
%!   save ("-hdf5", hdf5_filename, "f");
%!   hdf5 = load (hdf5_filename);
%!   assert (f (1, 2, 3), hdf5.f (1, 2, 3));
%! unwind_protect_cleanup
%!   unlink (hdf5_filename);
%! end_unwind_protect
