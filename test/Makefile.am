# Makefile for Octave's test directory
#
# Copyright (C) 1994-2012 John W. Eaton
#
# This file is part of Octave.
# 
# Octave is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 3 of the License, or (at
# your option) any later version.
# 
# Octave is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
# 
# You should have received a copy of the GNU General Public License
# along with Octave; see the file COPYING.  If not, see
# <http://www.gnu.org/licenses/>.

include $(top_srcdir)/build-aux/common.mk

FCN_FILES = \
  fntests.m \
  test_args.m \
  test_bug_31371.m \
  test_diag_perm.m \
  test_error.m \
  test_eval-catch.m \
  test_for.m \
  test_func.m \
  test_global.m \
  test_if.m \
  test_index.m \
  test_io.m \
  test_line_continue.m \
  test_logical_index.m \
  test_null_assign.m \
  test_parser.m \
  test_prefer.m \
  test_range.m \
  test_recursion.m \
  test_return.m \
  test_slice.m \
  test_struct.m \
  test_switch.m \
  test_system.m \
  test_transpose.m \
  test_try.m \
  test_unwind.m \
  test_while.m

include bug-35448/module.mk
include bug-36025/module.mk
include bug-38236/module.mk
include classes/module.mk
include class-concat/module.mk
include ctor-vs-method/module.mk
include fcn-handle-derived-resolution/module.mk
include nest/module.mk

check: test_sparse.m test_bc_overloads.m
	$(top_builddir)/run-octave --norc --silent --no-history $(srcdir)/fntests.m $(srcdir)

test_sparse.m: build_sparse_tests.sh
	$(srcdir)/build_sparse_tests.sh

test_bc_overloads.m: build_bc_overload_tests.sh bc_overloads_expected
	$(srcdir)/build_bc_overload_tests.sh $(srcdir)/bc_overloads_expected

EXTRA_DIST = \
  build_sparse_tests.sh \
  build_bc_overload_tests.sh \
  bc_overloads_expected \
  build_bc_overloads_expected.m \
  $(FCN_FILES)

CLEANFILES = \
  test_sparse.m \
  test_bc_overloads.m \
  tbcover.m

DISTCLEANFILES = \
  fntests.log
        
clean-local:
	-rm -rf @double @logical @int8 @uint64 @int64 @cell @single @int32 \
	@uint8 @uint16 @uint32 @struct @int16 @function_handle @char
