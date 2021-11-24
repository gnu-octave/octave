%!test <*45969>
%! text_filename = tempname ();
%! binary_filename = tempname ();
%! a = 2;
%! b = 10;
%! c = 20;
%! f1 = @ (f, x) f (x) + a;
%! f2 = @ (y) f1 (@ (z) z^2 + b * y, y) + c;
%! f2_arg = 5;
%! save_default_options ("-text", "local");
%! unwind_protect
%!   save (text_filename, "f2");
%!   save ("-binary", binary_filename, "f2");
%!   text = load (text_filename);
%!   binary = load (binary_filename);
%!   assert (f2 (f2_arg), text.f2 (f2_arg));
%!   assert (f2 (f2_arg), binary.f2 (f2_arg));
%! unwind_protect_cleanup
%!   unlink (text_filename);
%!   unlink (binary_filename);
%! end_unwind_protect

%!testif HAVE_HDF5 <*45969>
%! hdf5_filename = tempname ();
%! a = 2;
%! b = 10;
%! c = 20;
%! f1 = @ (f, x) f (x) + a;
%! f2 = @ (y) f1 (@ (z) z^2 + b * y, y) + c;
%! f2_arg = 5;
%! save_default_options ("-text", "local");
%! unwind_protect
%!   save ("-hdf5", hdf5_filename, "f2");
%!   hdf5 = load (hdf5_filename);
%!   assert (f2 (f2_arg), hdf5.f2 (f2_arg));
%! unwind_protect_cleanup
%!   unlink (hdf5_filename);
%! end_unwind_protect
