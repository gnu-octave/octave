#-------------------------------------------------------------------------------
# UMFPACK Makefile for the UMFPACK MATLAB mexFunction (old "make" only)
#-------------------------------------------------------------------------------

# UMFPACK Version 4.3 (Jan. 16, 2004), Copyright (c) 2004 by Timothy A.
# Davis.  All Rights Reserved.  See ../README for License.

# This is a very ugly Makefile, and is only provided for those who do not
# have GNU make.  Note that it is not used if you have GNU make.  It ignores
# dependency checking and just compiles everything.  It was created
# automatically, via make -n using the GNUmakefile.  That way, I don't have
# maintain two Makefiles.

all: umfpack luflop

include ../Make/Make.include

MKOCT = mkoctfile $(CONFIG) -DNRECIPROCAL -I/usr/include/atlas -I../Include -I../Source -I../../AMD/Include -I../../AMD/Source 

OCT_SPARSE_INC = -I../../../
OCTUMFPACK_LIB = umfpack_octave.o

umfpack:
	$(MKOCT) -DDINT -c ../Source/umf_analyze.c
	$(MV) -f ../Source/umf_analyze.o umf_m_analyze.o
	$(MKOCT) -DDINT -c ../Source/umf_apply_order.c
	$(MV) -f ../Source/umf_apply_order.o umf_m_apply_order.o
	$(MKOCT) -DDINT -c ../Source/umf_colamd.c
	$(MV) -f ../Source/umf_colamd.o umf_m_colamd.o
	$(MKOCT) -DDINT -c ../Source/umf_free.c
	$(MV) -f ../Source/umf_free.o umf_m_free.o
	$(MKOCT) -DDINT -c ../Source/umf_fsize.c
	$(MV) -f ../Source/umf_fsize.o umf_m_fsize.o
	$(MKOCT) -DDINT -c ../Source/umf_is_permutation.c
	$(MV) -f ../Source/umf_is_permutation.o umf_m_is_permutation.o
	$(MKOCT) -DDINT -c ../Source/umf_malloc.c
	$(MV) -f ../Source/umf_malloc.o umf_m_malloc.o
	$(MKOCT) -DDINT -c ../Source/umf_realloc.c
	$(MV) -f ../Source/umf_realloc.o umf_m_realloc.o
	$(MKOCT) -DDINT -c ../Source/umf_report_perm.c
	$(MV) -f ../Source/umf_report_perm.o umf_m_report_perm.o
	$(MKOCT) -DDINT -c ../Source/umf_singletons.c
	$(MV) -f ../Source/umf_singletons.o umf_m_singletons.o
	$(MKOCT) -DDINT -DCONJUGATE_SOLVE -c ../Source/umf_ltsolve.c
	$(MV) -f ../Source/umf_ltsolve.o umf_od_lhsolve.o
	$(MKOCT) -DDINT -DCONJUGATE_SOLVE -c ../Source/umf_utsolve.c
	$(MV) -f ../Source/umf_utsolve.o umf_od_uhsolve.o
	$(MKOCT) -DDINT -DDO_MAP -c ../Source/umf_triplet.c
	$(MV) -f ../Source/umf_triplet.o umf_od_triplet_map_nox.o
	$(MKOCT) -DDINT -DDO_VALUES -c ../Source/umf_triplet.c
	$(MV) -f ../Source/umf_triplet.o umf_od_triplet_nomap_x.o
	$(MKOCT) -DDINT -c ../Source/umf_triplet.c
	$(MV) -f ../Source/umf_triplet.o umf_od_triplet_nomap_nox.o
	$(MKOCT) -DDINT -DDO_MAP -DDO_VALUES -c ../Source/umf_triplet.c
	$(MV) -f ../Source/umf_triplet.o umf_od_triplet_map_x.o
	$(MKOCT) -DDINT -DFIXQ -c ../Source/umf_assemble.c
	$(MV) -f ../Source/umf_assemble.o umf_od_assemble_fixq.o
	$(MKOCT) -DDINT -DDROP -c ../Source/umf_store_lu.c
	$(MV) -f ../Source/umf_store_lu.o umf_od_store_lu_drop.o
	$(MKOCT) -DDINT -c ../Source/umf_assemble.c
	$(MV) -f ../Source/umf_assemble.o umf_od_assemble.o
	$(MKOCT) -DDINT -c ../Source/umf_blas3_update.c
	$(MV) -f ../Source/umf_blas3_update.o umf_od_blas3_update.o
	$(MKOCT) -DDINT -c ../Source/umf_build_tuples.c
	$(MV) -f ../Source/umf_build_tuples.o umf_od_build_tuples.o
	$(MKOCT) -DDINT -c ../Source/umf_create_element.c
	$(MV) -f ../Source/umf_create_element.o umf_od_create_element.o
	$(MKOCT) -DDINT -c ../Source/umf_dump.c
	$(MV) -f ../Source/umf_dump.o umf_od_dump.o
	$(MKOCT) -DDINT -c ../Source/umf_extend_front.c
	$(MV) -f ../Source/umf_extend_front.o umf_od_extend_front.o
	$(MKOCT) -DDINT -c ../Source/umf_garbage_collection.c
	$(MV) -f ../Source/umf_garbage_collection.o umf_od_garbage_collection.o
	$(MKOCT) -DDINT -c ../Source/umf_get_memory.c
	$(MV) -f ../Source/umf_get_memory.o umf_od_get_memory.o
	$(MKOCT) -DDINT -c ../Source/umf_init_front.c
	$(MV) -f ../Source/umf_init_front.o umf_od_init_front.o
	$(MKOCT) -DDINT -c ../Source/umf_kernel.c
	$(MV) -f ../Source/umf_kernel.o umf_od_kernel.o
	$(MKOCT) -DDINT -c ../Source/umf_kernel_init.c
	$(MV) -f ../Source/umf_kernel_init.o umf_od_kernel_init.o
	$(MKOCT) -DDINT -c ../Source/umf_kernel_wrapup.c
	$(MV) -f ../Source/umf_kernel_wrapup.o umf_od_kernel_wrapup.o
	$(MKOCT) -DDINT -c ../Source/umf_local_search.c
	$(MV) -f ../Source/umf_local_search.o umf_od_local_search.o
	$(MKOCT) -DDINT -c ../Source/umf_lsolve.c
	$(MV) -f ../Source/umf_lsolve.o umf_od_lsolve.o
	$(MKOCT) -DDINT -c ../Source/umf_ltsolve.c
	$(MV) -f ../Source/umf_ltsolve.o umf_od_ltsolve.o
	$(MKOCT) -DDINT -c ../Source/umf_mem_alloc_element.c
	$(MV) -f ../Source/umf_mem_alloc_element.o umf_od_mem_alloc_element.o
	$(MKOCT) -DDINT -c ../Source/umf_mem_alloc_head_block.c
	$(MV) -f ../Source/umf_mem_alloc_head_block.o umf_od_mem_alloc_head_block.o
	$(MKOCT) -DDINT -c ../Source/umf_mem_alloc_tail_block.c
	$(MV) -f ../Source/umf_mem_alloc_tail_block.o umf_od_mem_alloc_tail_block.o
	$(MKOCT) -DDINT -c ../Source/umf_mem_free_tail_block.c
	$(MV) -f ../Source/umf_mem_free_tail_block.o umf_od_mem_free_tail_block.o
	$(MKOCT) -DDINT -c ../Source/umf_mem_init_memoryspace.c
	$(MV) -f ../Source/umf_mem_init_memoryspace.o umf_od_mem_init_memoryspace.o
	$(MKOCT) -DDINT -c ../Source/umf_report_vector.c
	$(MV) -f ../Source/umf_report_vector.o umf_od_report_vector.o
	$(MKOCT) -DDINT -c ../Source/umf_row_search.c
	$(MV) -f ../Source/umf_row_search.o umf_od_row_search.o
	$(MKOCT) -DDINT -c ../Source/umf_scale_column.c
	$(MV) -f ../Source/umf_scale_column.o umf_od_scale_column.o
	$(MKOCT) -DDINT -c ../Source/umf_set_stats.c
	$(MV) -f ../Source/umf_set_stats.o umf_od_set_stats.o
	$(MKOCT) -DDINT -c ../Source/umf_solve.c
	$(MV) -f ../Source/umf_solve.o umf_od_solve.o
	$(MKOCT) -DDINT -c ../Source/umf_symbolic_usage.c
	$(MV) -f ../Source/umf_symbolic_usage.o umf_od_symbolic_usage.o
	$(MKOCT) -DDINT -c ../Source/umf_transpose.c
	$(MV) -f ../Source/umf_transpose.o umf_od_transpose.o
	$(MKOCT) -DDINT -c ../Source/umf_tuple_lengths.c
	$(MV) -f ../Source/umf_tuple_lengths.o umf_od_tuple_lengths.o
	$(MKOCT) -DDINT -c ../Source/umf_usolve.c
	$(MV) -f ../Source/umf_usolve.o umf_od_usolve.o
	$(MKOCT) -DDINT -c ../Source/umf_utsolve.c
	$(MV) -f ../Source/umf_utsolve.o umf_od_utsolve.o
	$(MKOCT) -DDINT -c ../Source/umf_valid_numeric.c
	$(MV) -f ../Source/umf_valid_numeric.o umf_od_valid_numeric.o
	$(MKOCT) -DDINT -c ../Source/umf_valid_symbolic.c
	$(MV) -f ../Source/umf_valid_symbolic.o umf_od_valid_symbolic.o
	$(MKOCT) -DDINT -c ../Source/umf_grow_front.c
	$(MV) -f ../Source/umf_grow_front.o umf_od_grow_front.o
	$(MKOCT) -DDINT -c ../Source/umf_start_front.c
	$(MV) -f ../Source/umf_start_front.o umf_od_start_front.o
	$(MKOCT) -DDINT -c ../Source/umf_2by2.c
	$(MV) -f ../Source/umf_2by2.o umf_od_2by2.o
	$(MKOCT) -DDINT -c ../Source/umf_store_lu.c
	$(MV) -f ../Source/umf_store_lu.o umf_od_store_lu.o
	$(MKOCT) -DDINT -c ../Source/umf_scale.c
	$(MV) -f ../Source/umf_scale.o umf_od_scale.o
	$(MKOCT) -DDINT -DWSOLVE -c ../Source/umfpack_solve.c
	$(MV) -f ../Source/umfpack_solve.o umfpack_od_wsolve.o
	$(MKOCT) -DDINT -c ../Source/umfpack_col_to_triplet.c
	$(MV) -f ../Source/umfpack_col_to_triplet.o umfpack_od_col_to_triplet.o
	$(MKOCT) -DDINT -c ../Source/umfpack_defaults.c
	$(MV) -f ../Source/umfpack_defaults.o umfpack_od_defaults.o
	$(MKOCT) -DDINT -c ../Source/umfpack_free_numeric.c
	$(MV) -f ../Source/umfpack_free_numeric.o umfpack_od_free_numeric.o
	$(MKOCT) -DDINT -c ../Source/umfpack_free_symbolic.c
	$(MV) -f ../Source/umfpack_free_symbolic.o umfpack_od_free_symbolic.o
	$(MKOCT) -DDINT -c ../Source/umfpack_get_numeric.c
	$(MV) -f ../Source/umfpack_get_numeric.o umfpack_od_get_numeric.o
	$(MKOCT) -DDINT -c ../Source/umfpack_get_lunz.c
	$(MV) -f ../Source/umfpack_get_lunz.o umfpack_od_get_lunz.o
	$(MKOCT) -DDINT -c ../Source/umfpack_get_symbolic.c
	$(MV) -f ../Source/umfpack_get_symbolic.o umfpack_od_get_symbolic.o
	$(MKOCT) -DDINT -c ../Source/umfpack_get_determinant.c
	$(MV) -f ../Source/umfpack_get_determinant.o umfpack_od_get_determinant.o
	$(MKOCT) -DDINT -c ../Source/umfpack_numeric.c
	$(MV) -f ../Source/umfpack_numeric.o umfpack_od_numeric.o
	$(MKOCT) -DDINT -c ../Source/umfpack_qsymbolic.c
	$(MV) -f ../Source/umfpack_qsymbolic.o umfpack_od_qsymbolic.o
	$(MKOCT) -DDINT -c ../Source/umfpack_report_control.c
	$(MV) -f ../Source/umfpack_report_control.o umfpack_od_report_control.o
	$(MKOCT) -DDINT -c ../Source/umfpack_report_info.c
	$(MV) -f ../Source/umfpack_report_info.o umfpack_od_report_info.o
	$(MKOCT) -DDINT -c ../Source/umfpack_report_matrix.c
	$(MV) -f ../Source/umfpack_report_matrix.o umfpack_od_report_matrix.o
	$(MKOCT) -DDINT -c ../Source/umfpack_report_numeric.c
	$(MV) -f ../Source/umfpack_report_numeric.o umfpack_od_report_numeric.o
	$(MKOCT) -DDINT -c ../Source/umfpack_report_perm.c
	$(MV) -f ../Source/umfpack_report_perm.o umfpack_od_report_perm.o
	$(MKOCT) -DDINT -c ../Source/umfpack_report_status.c
	$(MV) -f ../Source/umfpack_report_status.o umfpack_od_report_status.o
	$(MKOCT) -DDINT -c ../Source/umfpack_report_symbolic.c
	$(MV) -f ../Source/umfpack_report_symbolic.o umfpack_od_report_symbolic.o
	$(MKOCT) -DDINT -c ../Source/umfpack_report_triplet.c
	$(MV) -f ../Source/umfpack_report_triplet.o umfpack_od_report_triplet.o
	$(MKOCT) -DDINT -c ../Source/umfpack_report_vector.c
	$(MV) -f ../Source/umfpack_report_vector.o umfpack_od_report_vector.o
	$(MKOCT) -DDINT -c ../Source/umfpack_solve.c
	$(MV) -f ../Source/umfpack_solve.o umfpack_od_solve.o
	$(MKOCT) -DDINT -c ../Source/umfpack_symbolic.c
	$(MV) -f ../Source/umfpack_symbolic.o umfpack_od_symbolic.o
	$(MKOCT) -DDINT -c ../Source/umfpack_transpose.c
	$(MV) -f ../Source/umfpack_transpose.o umfpack_od_transpose.o
	$(MKOCT) -DDINT -c ../Source/umfpack_triplet_to_col.c
	$(MV) -f ../Source/umfpack_triplet_to_col.o umfpack_od_triplet_to_col.o
	$(MKOCT) -DDINT -c ../Source/umfpack_scale.c
	$(MV) -f ../Source/umfpack_scale.o umfpack_od_scale.o
	$(MKOCT) -DDINT -c ../Source/umfpack_load_numeric.c
	$(MV) -f ../Source/umfpack_load_numeric.o umfpack_od_load_numeric.o
	$(MKOCT) -DDINT -c ../Source/umfpack_save_numeric.c
	$(MV) -f ../Source/umfpack_save_numeric.o umfpack_od_save_numeric.o
	$(MKOCT) -DDINT -c ../Source/umfpack_load_symbolic.c
	$(MV) -f ../Source/umfpack_load_symbolic.o umfpack_od_load_symbolic.o
	$(MKOCT) -DDINT -c ../Source/umfpack_save_symbolic.c
	$(MV) -f ../Source/umfpack_save_symbolic.o umfpack_od_save_symbolic.o
	$(MKOCT) -DZINT -DCONJUGATE_SOLVE -c ../Source/umf_ltsolve.c
	$(MV) -f ../Source/umf_ltsolve.o umf_oz_lhsolve.o
	$(MKOCT) -DZINT -DCONJUGATE_SOLVE -c ../Source/umf_utsolve.c
	$(MV) -f ../Source/umf_utsolve.o umf_oz_uhsolve.o
	$(MKOCT) -DZINT -DDO_MAP -c ../Source/umf_triplet.c
	$(MV) -f ../Source/umf_triplet.o umf_oz_triplet_map_nox.o
	$(MKOCT) -DZINT -DDO_VALUES -c ../Source/umf_triplet.c
	$(MV) -f ../Source/umf_triplet.o umf_oz_triplet_nomap_x.o
	$(MKOCT) -DZINT -c ../Source/umf_triplet.c
	$(MV) -f ../Source/umf_triplet.o umf_oz_triplet_nomap_nox.o
	$(MKOCT) -DZINT -DDO_MAP -DDO_VALUES -c ../Source/umf_triplet.c
	$(MV) -f ../Source/umf_triplet.o umf_oz_triplet_map_x.o
	$(MKOCT) -DZINT -DFIXQ -c ../Source/umf_assemble.c
	$(MV) -f ../Source/umf_assemble.o umf_oz_assemble_fixq.o
	$(MKOCT) -DZINT -DDROP -c ../Source/umf_store_lu.c
	$(MV) -f ../Source/umf_store_lu.o umf_oz_store_lu_drop.o
	$(MKOCT) -DZINT -c ../Source/umf_assemble.c
	$(MV) -f ../Source/umf_assemble.o umf_oz_assemble.o
	$(MKOCT) -DZINT -c ../Source/umf_blas3_update.c
	$(MV) -f ../Source/umf_blas3_update.o umf_oz_blas3_update.o
	$(MKOCT) -DZINT -c ../Source/umf_build_tuples.c
	$(MV) -f ../Source/umf_build_tuples.o umf_oz_build_tuples.o
	$(MKOCT) -DZINT -c ../Source/umf_create_element.c
	$(MV) -f ../Source/umf_create_element.o umf_oz_create_element.o
	$(MKOCT) -DZINT -c ../Source/umf_dump.c
	$(MV) -f ../Source/umf_dump.o umf_oz_dump.o
	$(MKOCT) -DZINT -c ../Source/umf_extend_front.c
	$(MV) -f ../Source/umf_extend_front.o umf_oz_extend_front.o
	$(MKOCT) -DZINT -c ../Source/umf_garbage_collection.c
	$(MV) -f ../Source/umf_garbage_collection.o umf_oz_garbage_collection.o
	$(MKOCT) -DZINT -c ../Source/umf_get_memory.c
	$(MV) -f ../Source/umf_get_memory.o umf_oz_get_memory.o
	$(MKOCT) -DZINT -c ../Source/umf_init_front.c
	$(MV) -f ../Source/umf_init_front.o umf_oz_init_front.o
	$(MKOCT) -DZINT -c ../Source/umf_kernel.c
	$(MV) -f ../Source/umf_kernel.o umf_oz_kernel.o
	$(MKOCT) -DZINT -c ../Source/umf_kernel_init.c
	$(MV) -f ../Source/umf_kernel_init.o umf_oz_kernel_init.o
	$(MKOCT) -DZINT -c ../Source/umf_kernel_wrapup.c
	$(MV) -f ../Source/umf_kernel_wrapup.o umf_oz_kernel_wrapup.o
	$(MKOCT) -DZINT -c ../Source/umf_local_search.c
	$(MV) -f ../Source/umf_local_search.o umf_oz_local_search.o
	$(MKOCT) -DZINT -c ../Source/umf_lsolve.c
	$(MV) -f ../Source/umf_lsolve.o umf_oz_lsolve.o
	$(MKOCT) -DZINT -c ../Source/umf_ltsolve.c
	$(MV) -f ../Source/umf_ltsolve.o umf_oz_ltsolve.o
	$(MKOCT) -DZINT -c ../Source/umf_mem_alloc_element.c
	$(MV) -f ../Source/umf_mem_alloc_element.o umf_oz_mem_alloc_element.o
	$(MKOCT) -DZINT -c ../Source/umf_mem_alloc_head_block.c
	$(MV) -f ../Source/umf_mem_alloc_head_block.o umf_oz_mem_alloc_head_block.o
	$(MKOCT) -DZINT -c ../Source/umf_mem_alloc_tail_block.c
	$(MV) -f ../Source/umf_mem_alloc_tail_block.o umf_oz_mem_alloc_tail_block.o
	$(MKOCT) -DZINT -c ../Source/umf_mem_free_tail_block.c
	$(MV) -f ../Source/umf_mem_free_tail_block.o umf_oz_mem_free_tail_block.o
	$(MKOCT) -DZINT -c ../Source/umf_mem_init_memoryspace.c
	$(MV) -f ../Source/umf_mem_init_memoryspace.o umf_oz_mem_init_memoryspace.o
	$(MKOCT) -DZINT -c ../Source/umf_report_vector.c
	$(MV) -f ../Source/umf_report_vector.o umf_oz_report_vector.o
	$(MKOCT) -DZINT -c ../Source/umf_row_search.c
	$(MV) -f ../Source/umf_row_search.o umf_oz_row_search.o
	$(MKOCT) -DZINT -c ../Source/umf_scale_column.c
	$(MV) -f ../Source/umf_scale_column.o umf_oz_scale_column.o
	$(MKOCT) -DZINT -c ../Source/umf_set_stats.c
	$(MV) -f ../Source/umf_set_stats.o umf_oz_set_stats.o
	$(MKOCT) -DZINT -c ../Source/umf_solve.c
	$(MV) -f ../Source/umf_solve.o umf_oz_solve.o
	$(MKOCT) -DZINT -c ../Source/umf_symbolic_usage.c
	$(MV) -f ../Source/umf_symbolic_usage.o umf_oz_symbolic_usage.o
	$(MKOCT) -DZINT -c ../Source/umf_transpose.c
	$(MV) -f ../Source/umf_transpose.o umf_oz_transpose.o
	$(MKOCT) -DZINT -c ../Source/umf_tuple_lengths.c
	$(MV) -f ../Source/umf_tuple_lengths.o umf_oz_tuple_lengths.o
	$(MKOCT) -DZINT -c ../Source/umf_usolve.c
	$(MV) -f ../Source/umf_usolve.o umf_oz_usolve.o
	$(MKOCT) -DZINT -c ../Source/umf_utsolve.c
	$(MV) -f ../Source/umf_utsolve.o umf_oz_utsolve.o
	$(MKOCT) -DZINT -c ../Source/umf_valid_numeric.c
	$(MV) -f ../Source/umf_valid_numeric.o umf_oz_valid_numeric.o
	$(MKOCT) -DZINT -c ../Source/umf_valid_symbolic.c
	$(MV) -f ../Source/umf_valid_symbolic.o umf_oz_valid_symbolic.o
	$(MKOCT) -DZINT -c ../Source/umf_grow_front.c
	$(MV) -f ../Source/umf_grow_front.o umf_oz_grow_front.o
	$(MKOCT) -DZINT -c ../Source/umf_start_front.c
	$(MV) -f ../Source/umf_start_front.o umf_oz_start_front.o
	$(MKOCT) -DZINT -c ../Source/umf_2by2.c
	$(MV) -f ../Source/umf_2by2.o umf_oz_2by2.o
	$(MKOCT) -DZINT -c ../Source/umf_store_lu.c
	$(MV) -f ../Source/umf_store_lu.o umf_oz_store_lu.o
	$(MKOCT) -DZINT -c ../Source/umf_scale.c
	$(MV) -f ../Source/umf_scale.o umf_oz_scale.o
	$(MKOCT) -DZINT -DWSOLVE -c ../Source/umfpack_solve.c
	$(MV) -f ../Source/umfpack_solve.o umfpack_oz_wsolve.o
	$(MKOCT) -DZINT -c ../Source/umfpack_col_to_triplet.c
	$(MV) -f ../Source/umfpack_col_to_triplet.o umfpack_oz_col_to_triplet.o
	$(MKOCT) -DZINT -c ../Source/umfpack_defaults.c
	$(MV) -f ../Source/umfpack_defaults.o umfpack_oz_defaults.o
	$(MKOCT) -DZINT -c ../Source/umfpack_free_numeric.c
	$(MV) -f ../Source/umfpack_free_numeric.o umfpack_oz_free_numeric.o
	$(MKOCT) -DZINT -c ../Source/umfpack_free_symbolic.c
	$(MV) -f ../Source/umfpack_free_symbolic.o umfpack_oz_free_symbolic.o
	$(MKOCT) -DZINT -c ../Source/umfpack_get_numeric.c
	$(MV) -f ../Source/umfpack_get_numeric.o umfpack_oz_get_numeric.o
	$(MKOCT) -DZINT -c ../Source/umfpack_get_lunz.c
	$(MV) -f ../Source/umfpack_get_lunz.o umfpack_oz_get_lunz.o
	$(MKOCT) -DZINT -c ../Source/umfpack_get_symbolic.c
	$(MV) -f ../Source/umfpack_get_symbolic.o umfpack_oz_get_symbolic.o
	$(MKOCT) -DZINT -c ../Source/umfpack_get_determinant.c
	$(MV) -f ../Source/umfpack_get_determinant.o umfpack_oz_get_determinant.o
	$(MKOCT) -DZINT -c ../Source/umfpack_numeric.c
	$(MV) -f ../Source/umfpack_numeric.o umfpack_oz_numeric.o
	$(MKOCT) -DZINT -c ../Source/umfpack_qsymbolic.c
	$(MV) -f ../Source/umfpack_qsymbolic.o umfpack_oz_qsymbolic.o
	$(MKOCT) -DZINT -c ../Source/umfpack_report_control.c
	$(MV) -f ../Source/umfpack_report_control.o umfpack_oz_report_control.o
	$(MKOCT) -DZINT -c ../Source/umfpack_report_info.c
	$(MV) -f ../Source/umfpack_report_info.o umfpack_oz_report_info.o
	$(MKOCT) -DZINT -c ../Source/umfpack_report_matrix.c
	$(MV) -f ../Source/umfpack_report_matrix.o umfpack_oz_report_matrix.o
	$(MKOCT) -DZINT -c ../Source/umfpack_report_numeric.c
	$(MV) -f ../Source/umfpack_report_numeric.o umfpack_oz_report_numeric.o
	$(MKOCT) -DZINT -c ../Source/umfpack_report_perm.c
	$(MV) -f ../Source/umfpack_report_perm.o umfpack_oz_report_perm.o
	$(MKOCT) -DZINT -c ../Source/umfpack_report_status.c
	$(MV) -f ../Source/umfpack_report_status.o umfpack_oz_report_status.o
	$(MKOCT) -DZINT -c ../Source/umfpack_report_symbolic.c
	$(MV) -f ../Source/umfpack_report_symbolic.o umfpack_oz_report_symbolic.o
	$(MKOCT) -DZINT -c ../Source/umfpack_report_triplet.c
	$(MV) -f ../Source/umfpack_report_triplet.o umfpack_oz_report_triplet.o
	$(MKOCT) -DZINT -c ../Source/umfpack_report_vector.c
	$(MV) -f ../Source/umfpack_report_vector.o umfpack_oz_report_vector.o
	$(MKOCT) -DZINT -c ../Source/umfpack_solve.c
	$(MV) -f ../Source/umfpack_solve.o umfpack_oz_solve.o
	$(MKOCT) -DZINT -c ../Source/umfpack_symbolic.c
	$(MV) -f ../Source/umfpack_symbolic.o umfpack_oz_symbolic.o
	$(MKOCT) -DZINT -c ../Source/umfpack_transpose.c
	$(MV) -f ../Source/umfpack_transpose.o umfpack_oz_transpose.o
	$(MKOCT) -DZINT -c ../Source/umfpack_triplet_to_col.c
	$(MV) -f ../Source/umfpack_triplet_to_col.o umfpack_oz_triplet_to_col.o
	$(MKOCT) -DZINT -c ../Source/umfpack_scale.c
	$(MV) -f ../Source/umfpack_scale.o umfpack_oz_scale.o
	$(MKOCT) -DZINT -c ../Source/umfpack_load_numeric.c
	$(MV) -f ../Source/umfpack_load_numeric.o umfpack_oz_load_numeric.o
	$(MKOCT) -DZINT -c ../Source/umfpack_save_numeric.c
	$(MV) -f ../Source/umfpack_save_numeric.o umfpack_oz_save_numeric.o
	$(MKOCT) -DZINT -c ../Source/umfpack_load_symbolic.c
	$(MV) -f ../Source/umfpack_load_symbolic.o umfpack_oz_load_symbolic.o
	$(MKOCT) -DZINT -c ../Source/umfpack_save_symbolic.c
	$(MV) -f ../Source/umfpack_save_symbolic.o umfpack_oz_save_symbolic.o
	$(MKOCT) -c ../Source/umfpack_timer.c
	$(MV) -f ../Source/umfpack_timer.o umfpack_m_timer.o
	$(MKOCT) -c ../Source/umfpack_tictoc.c
	$(MV) -f ../Source/umfpack_tictoc.o umfpack_m_tictoc.o
	$(MKOCT) -DDINT -c ../../AMD/Source/amd_aat.c
	$(MV) -f ../../AMD/Source/amd_aat.o amd_m_aat.o
	$(MKOCT) -DDINT -c ../../AMD/Source/amd_1.c
	$(MV) -f ../../AMD/Source/amd_1.o amd_m_1.o
	$(MKOCT) -DDINT -c ../../AMD/Source/amd_2.c
	$(MV) -f ../../AMD/Source/amd_2.o amd_m_2.o
	$(MKOCT) -DDINT -c ../../AMD/Source/amd_dump.c
	$(MV) -f ../../AMD/Source/amd_dump.o amd_m_dump.o
	$(MKOCT) -DDINT -c ../../AMD/Source/amd_postorder.c
	$(MV) -f ../../AMD/Source/amd_postorder.o amd_m_postorder.o
	$(MKOCT) -DDINT -c ../../AMD/Source/amd_post_tree.c
	$(MV) -f ../../AMD/Source/amd_post_tree.o amd_m_post_tree.o
	$(MKOCT) -DDINT -c ../../AMD/Source/amd_defaults.c
	$(MV) -f ../../AMD/Source/amd_defaults.o amd_m_defaults.o
	$(MKOCT) -DDINT -c ../../AMD/Source/amd_order.c
	$(MV) -f ../../AMD/Source/amd_order.o amd_m_order.o
	$(MKOCT) -DDINT -c ../../AMD/Source/amd_control.c
	$(MV) -f ../../AMD/Source/amd_control.o amd_m_control.o
	$(MKOCT) -DDINT -c ../../AMD/Source/amd_info.c
	$(MV) -f ../../AMD/Source/amd_info.o amd_m_info.o
	$(MKOCT) -DDINT -c ../../AMD/Source/amd_valid.c
	$(MV) -f ../../AMD/Source/amd_valid.o amd_m_valid.o
	$(MKOCT) -o umfpack.oct $(OCT_SPARSE_INC) umfpack.cc \
	    umf_o_analyze.o umf_o_apply_order.o umf_o_colamd.o umf_o_free.o \
	    umf_o_fsize.o umf_o_is_permutation.o umf_o_malloc.o \
	    umf_o_realloc.o umf_o_report_perm.o umf_o_singletons.o \
	    umf_od_lhsolve.o umf_od_uhsolve.o umf_od_triplet_map_nox.o \
	    umf_od_triplet_nomap_x.o umf_od_triplet_nomap_nox.o \
	    umf_od_triplet_map_x.o umf_od_assemble_fixq.o \
	    umf_od_store_lu_drop.o umf_od_assemble.o umf_od_blas3_update.o \
	    umf_od_build_tuples.o umf_od_create_element.o umf_od_dump.o \
	    umf_od_extend_front.o umf_od_garbage_collection.o \
	    umf_od_get_memory.o umf_od_init_front.o umf_od_kernel.o \
	    umf_od_kernel_init.o umf_od_kernel_wrapup.o umf_od_local_search.o \
	    umf_od_lsolve.o umf_od_ltsolve.o umf_od_mem_alloc_element.o \
	    umf_od_mem_alloc_head_block.o umf_od_mem_alloc_tail_block.o \
	    umf_od_mem_free_tail_block.o umf_od_mem_init_memoryspace.o \
	    umf_od_report_vector.o umf_od_row_search.o umf_od_scale_column.o \
	    umf_od_set_stats.o umf_od_solve.o umf_od_symbolic_usage.o \
	    umf_od_transpose.o umf_od_tuple_lengths.o umf_od_usolve.o \
	    umf_od_utsolve.o umf_od_valid_numeric.o umf_od_valid_symbolic.o \
	    umf_od_grow_front.o umf_od_start_front.o umf_od_2by2.o \
	    umf_od_store_lu.o umf_od_scale.o umfpack_od_wsolve.o \
	    umfpack_od_col_to_triplet.o umfpack_od_defaults.o \
	    umfpack_od_free_numeric.o umfpack_od_free_symbolic.o \
	    umfpack_od_get_numeric.o umfpack_od_get_lunz.o \
	    umfpack_od_get_symbolic.o umfpack_od_numeric.o \
	    umfpack_od_qsymbolic.o umfpack_od_report_control.o \
	    umfpack_od_report_info.o umfpack_od_report_matrix.o \
	    umfpack_od_report_numeric.o umfpack_od_report_perm.o \
	    umfpack_od_report_status.o umfpack_od_report_symbolic.o \
	    umfpack_od_report_triplet.o umfpack_od_report_vector.o \
	    umfpack_od_solve.o umfpack_od_symbolic.o umfpack_od_transpose.o \
	    umfpack_od_triplet_to_col.o umfpack_od_scale.o \
	    umfpack_od_load_numeric.o umfpack_od_save_numeric.o \
	    umfpack_od_load_symbolic.o umfpack_od_save_symbolic.o \
	    umf_oz_lhsolve.o umf_oz_uhsolve.o umf_oz_triplet_map_nox.o \
	    umf_oz_triplet_nomap_x.o umf_oz_triplet_nomap_nox.o \
	    umf_oz_triplet_map_x.o umf_oz_assemble_fixq.o \
	    umf_oz_store_lu_drop.o umf_oz_assemble.o umf_oz_blas3_update.o \
	    umf_oz_build_tuples.o umf_oz_create_element.o umf_oz_dump.o \
	    umf_oz_extend_front.o umf_oz_garbage_collection.o \
	    umf_oz_get_memory.o umf_oz_init_front.o umf_oz_kernel.o \
	    umf_oz_kernel_init.o umf_oz_kernel_wrapup.o umf_oz_local_search.o \
	    umf_oz_lsolve.o umf_oz_ltsolve.o umf_oz_mem_alloc_element.o \
	    umf_oz_mem_alloc_head_block.o umf_oz_mem_alloc_tail_block.o \
	    umf_oz_mem_free_tail_block.o umf_oz_mem_init_memoryspace.o \
	    umf_oz_report_vector.o umf_oz_row_search.o umf_oz_scale_column.o \
	    umf_oz_set_stats.o umf_oz_solve.o umf_oz_symbolic_usage.o \
	    umf_oz_transpose.o umf_oz_tuple_lengths.o umf_oz_usolve.o \
	    umf_oz_utsolve.o umf_oz_valid_numeric.o umf_oz_valid_symbolic.o \
	    umf_oz_grow_front.o umf_oz_start_front.o umf_oz_2by2.o \
	    umf_oz_store_lu.o umf_oz_scale.o umfpack_oz_wsolve.o \
	    umfpack_oz_col_to_triplet.o umfpack_oz_defaults.o \
	    umfpack_oz_free_numeric.o umfpack_oz_free_symbolic.o \
	    umfpack_oz_get_numeric.o umfpack_oz_get_lunz.o \
	    umfpack_oz_get_symbolic.o umfpack_oz_numeric.o \
	    umfpack_oz_qsymbolic.o umfpack_oz_report_control.o \
	    umfpack_oz_report_info.o umfpack_oz_report_matrix.o \
	    umfpack_oz_report_numeric.o umfpack_oz_report_perm.o \
	    umfpack_oz_report_status.o umfpack_oz_report_symbolic.o \
	    umfpack_oz_report_triplet.o umfpack_oz_report_vector.o \
	    umfpack_oz_solve.o umfpack_oz_symbolic.o umfpack_oz_transpose.o \
	    umfpack_oz_triplet_to_col.o umfpack_oz_scale.o \
	    umfpack_oz_load_numeric.o umfpack_oz_save_numeric.o \
	    umfpack_oz_load_symbolic.o umfpack_oz_save_symbolic.o \
	    umfpack_o_timer.o umfpack_o_tictoc.o \
	    amd_o_aat.o amd_o_1.o amd_o_2.o amd_o_dump.o \
	    amd_o_postorder.o amd_o_post_tree.o amd_o_defaults.o amd_o_order.o \
	    amd_o_control.o amd_o_info.o amd_o_valid.o

luflop: luflop.cc
	$(MKOCT) luflop.cc -I$(OCT_SPARSE_INC) -o luflop.oct

#----------------------------------------
# umfpack library to link with octave
#----------------------------------------

octave: umfpack
	ld -r -o ../../../$(OCTUMFPACK_LIB) \
	    umf_o_analyze.o umf_o_apply_order.o umf_o_colamd.o umf_o_free.o \
	    umf_o_fsize.o umf_o_is_permutation.o umf_o_malloc.o \
	    umf_o_realloc.o umf_o_report_perm.o umf_o_singletons.o \
	    umf_od_lhsolve.o umf_od_uhsolve.o umf_od_triplet_map_nox.o \
	    umf_od_triplet_nomap_x.o umf_od_triplet_nomap_nox.o \
	    umf_od_triplet_map_x.o umf_od_assemble_fixq.o \
	    umf_od_store_lu_drop.o umf_od_assemble.o umf_od_blas3_update.o \
	    umf_od_build_tuples.o umf_od_create_element.o umf_od_dump.o \
	    umf_od_extend_front.o umf_od_garbage_collection.o \
	    umf_od_get_memory.o umf_od_init_front.o umf_od_kernel.o \
	    umf_od_kernel_init.o umf_od_kernel_wrapup.o umf_od_local_search.o \
	    umf_od_lsolve.o umf_od_ltsolve.o umf_od_mem_alloc_element.o \
	    umf_od_mem_alloc_head_block.o umf_od_mem_alloc_tail_block.o \
	    umf_od_mem_free_tail_block.o umf_od_mem_init_memoryspace.o \
	    umf_od_report_vector.o umf_od_row_search.o umf_od_scale_column.o \
	    umf_od_set_stats.o umf_od_solve.o umf_od_symbolic_usage.o \
	    umf_od_transpose.o umf_od_tuple_lengths.o umf_od_usolve.o \
	    umf_od_utsolve.o umf_od_valid_numeric.o umf_od_valid_symbolic.o \
	    umf_od_grow_front.o umf_od_start_front.o umf_od_2by2.o \
	    umf_od_store_lu.o umf_od_scale.o umfpack_od_wsolve.o \
	    umfpack_od_col_to_triplet.o umfpack_od_defaults.o \
	    umfpack_od_free_numeric.o umfpack_od_free_symbolic.o \
	    umfpack_od_get_numeric.o umfpack_od_get_lunz.o \
	    umfpack_od_get_symbolic.o umfpack_od_numeric.o \
	    umfpack_od_qsymbolic.o umfpack_od_report_control.o \
	    umfpack_od_report_info.o umfpack_od_report_matrix.o \
	    umfpack_od_report_numeric.o umfpack_od_report_perm.o \
	    umfpack_od_report_status.o umfpack_od_report_symbolic.o \
	    umfpack_od_report_triplet.o umfpack_od_report_vector.o \
	    umfpack_od_solve.o umfpack_od_symbolic.o umfpack_od_transpose.o \
	    umfpack_od_triplet_to_col.o umfpack_od_scale.o \
	    umfpack_od_load_numeric.o umfpack_od_save_numeric.o \
	    umfpack_od_load_symbolic.o umfpack_od_save_symbolic.o \
	    umf_oz_lhsolve.o umf_oz_uhsolve.o umf_oz_triplet_map_nox.o \
	    umf_oz_triplet_nomap_x.o umf_oz_triplet_nomap_nox.o \
	    umf_oz_triplet_map_x.o umf_oz_assemble_fixq.o \
	    umf_oz_store_lu_drop.o umf_oz_assemble.o umf_oz_blas3_update.o \
	    umf_oz_build_tuples.o umf_oz_create_element.o umf_oz_dump.o \
	    umf_oz_extend_front.o umf_oz_garbage_collection.o \
	    umf_oz_get_memory.o umf_oz_init_front.o umf_oz_kernel.o \
	    umf_oz_kernel_init.o umf_oz_kernel_wrapup.o umf_oz_local_search.o \
	    umf_oz_lsolve.o umf_oz_ltsolve.o umf_oz_mem_alloc_element.o \
	    umf_oz_mem_alloc_head_block.o umf_oz_mem_alloc_tail_block.o \
	    umf_oz_mem_free_tail_block.o umf_oz_mem_init_memoryspace.o \
	    umf_oz_report_vector.o umf_oz_row_search.o umf_oz_scale_column.o \
	    umf_oz_set_stats.o umf_oz_solve.o umf_oz_symbolic_usage.o \
	    umf_oz_transpose.o umf_oz_tuple_lengths.o umf_oz_usolve.o \
	    umf_oz_utsolve.o umf_oz_valid_numeric.o umf_oz_valid_symbolic.o \
	    umf_oz_grow_front.o umf_oz_start_front.o umf_oz_2by2.o \
	    umf_oz_store_lu.o umf_oz_scale.o umfpack_oz_wsolve.o \
	    umfpack_oz_col_to_triplet.o umfpack_oz_defaults.o \
	    umfpack_oz_free_numeric.o umfpack_oz_free_symbolic.o \
	    umfpack_oz_get_numeric.o umfpack_oz_get_lunz.o \
	    umfpack_oz_get_symbolic.o umfpack_oz_numeric.o \
	    umfpack_oz_qsymbolic.o umfpack_oz_report_control.o \
	    umfpack_oz_report_info.o umfpack_oz_report_matrix.o \
	    umfpack_oz_report_numeric.o umfpack_oz_report_perm.o \
	    umfpack_oz_report_status.o umfpack_oz_report_symbolic.o \
	    umfpack_oz_report_triplet.o umfpack_oz_report_vector.o \
	    umfpack_oz_solve.o umfpack_oz_symbolic.o umfpack_oz_transpose.o \
	    umfpack_oz_triplet_to_col.o umfpack_oz_scale.o \
	    umfpack_oz_load_numeric.o umfpack_oz_save_numeric.o \
	    umfpack_oz_load_symbolic.o umfpack_oz_save_symbolic.o \
	    umfpack_o_timer.o umfpack_o_tictoc.o \
	    amd_o_aat.o amd_o_1.o amd_o_2.o amd_o_dump.o \
	    amd_o_postorder.o amd_o_post_tree.o amd_o_defaults.o amd_o_order.o \
	    amd_o_control.o amd_o_info.o amd_o_valid.o

#-------------------------------------------------------------------------------
# Remove all but the files in the original distribution
#-------------------------------------------------------------------------------

purge: clean
	- $(RM) *.oct* *.dll
