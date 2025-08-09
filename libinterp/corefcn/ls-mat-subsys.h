////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2025 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if ! defined (octave_ls_mat_subsys_h)
#define octave_ls_mat_subsys_h 1

#include "octave-config.h"

#include <iosfwd>
#include <string>

OCTAVE_BEGIN_NAMESPACE(octave)

OCTINTERP_API octave_value
load_mcos_object (const octave_value& objmetadata);

class subsystem_handler
{
  public:

    // load methods
    bool read_mat_subsystem (const octave_value& subsys, bool swap);

    bool read_filewrapper (const Cell& fwrap_data, bool swap);

    std::string get_class_name (const uint32_t class_id);

    std::tuple<bool, uint32_t, uint32_t>
    get_object_dependencies (const uint32_t obj_id);

    octave_map
    get_object_properties (const uint32_t obj_type_id, const uint32_t class_id,
                           bool saveobj_type);

    bool check_dyn_props (const uint32_t obj_dep_id);

    // save methods
    uint32NDArray set_mcos_object_metadata (const octave_value& obj);

    uint8NDArray create_filewrapper_metadata ();

    void serialize_to_cell_array ();

    octave_value get_serialized_data () const { return m_serialized_data; }

    bool has_serialized_data () const
    { return m_serialized_data.is_defined (); }

  private:

    uint32_t get_name_index (const std::string& name);

    // helper functions for set_mcos_object_metadata
    std::pair<std::string, std::string>
    parse_class_name (const std::string& full_class_name);

    uint32_t
    get_or_create_class_id (const std::string& namespace_name,
                            const std::string& class_name);

    uint32NDArray
    create_metadata_array (const dim_vector& obj_dims,
                           const std::vector<uint32_t>& object_ids,
                           uint32_t class_id);

    void
    process_object_properties (const std::vector<std::tuple<octave_map, uint32_t, bool>>& saveobj_data,
                               const std::vector<bool>& is_new,
                               uint32_t class_id);

  private:

    const uint32_t m_filewrapper_version = 4;

    // load variables
    octave_value m_type_java;
    octave_value m_type_handle;

    // save variables
    uint32_t m_object_id_counter = 0;  // To manage dependency graphs; FIXME: unused?
    uint32_t m_class_id_counter = 0;
    uint32_t m_saveobj_object_counter = 0;
    uint32_t m_normal_object_counter = 0;

    octave_value m_serialized_data;  // required for writing nbytes when saving

    // load-save variables
    uint32_t m_num_names;  // number of unique class and property names
    string_vector m_prop_class_names;

    std::array<size_t, 8> m_region_offsets;

    std::vector<uint32_t> m_class_name_refs;
    std::vector<uint32_t> m_object_id_refs;
    std::vector<uint32_t> m_object_prop_fields;
    std::vector<uint32_t> m_saveobj_prop_fields;
    std::vector<uint32_t> m_dynamic_prop_refs;

    std::vector<octave_value> m_fwrap_prop_vals;
    Cell m_fwrap_default_vals;

};

OCTAVE_END_NAMESPACE(octave)

#endif
