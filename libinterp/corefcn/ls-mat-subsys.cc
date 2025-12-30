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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "byte-swap.h"

#include "cdef-class.h"
#include "cdef-utils.h"
#include "error.h"
#include "interpreter-private.h"
#include "load-save.h"
#include "ls-mat-subsys.h"
#include "ov-classdef.h"

static inline bool
is_valid_mcos_object (const octave_value& metadata)
{
  // Metadata must be N X 1 uint32 array, N>=3
  if (! metadata.is_uint32_type ())
    {
      if (metadata.isstruct () && metadata.is_scalar_type ())
        {
          // Metadata for enumeration class objects are stored as structs
          // with a field "EnumerationInstanceTag" set to 0xDD000000.

          const octave_scalar_map& md = metadata.scalar_map_value ();
          if (md.contains ("EnumerationInstanceTag"))
            {
              const octave_value& enum_tag = md.contents ("EnumerationInstanceTag");
              if (enum_tag.is_uint32_type () && enum_tag.is_scalar_type ()
                  && static_cast<uint32_t> (enum_tag.uint32_scalar_value ())
                       == 0xDD000000)
                warning_with_id ("Octave:load:classdef-not-supported",
                    "load: MATLAB enumeration class object cannot be loaded. "
                    "Returning as %s",
                    metadata.class_name ().c_str ());
            }
        }

      return false;
    }

  const uint32NDArray& md = metadata.uint32_array_value ();
  if (md.ndims () != 2)
    return false;

  dim_vector dv = md.dims ();
  if (dv(1) != 1)
    return false;

  if (md.numel () < 3)
    return false;

  // first value must be 0xDD000000
  if (static_cast<uint32_t> (md(0, 0)) != 0xDD000000)
    return false;

  return true;
}

OCTAVE_BEGIN_NAMESPACE(octave)

static octave_value
search_mcos_classdef_objects (const octave_value& val, bool save_mode)
{
  if (save_mode && val.is_classdef_object ())
    {
      subsystem_handler *sh = octave::__get_load_save_system__ ().get_subsystem_handler ();
      return octave_value (sh->set_mcos_object_metadata (val));
    }
  else if (! save_mode && is_valid_mcos_object (val))
    {
      return load_mcos_object (val);
    }

  // iterate through cell arrays
  if (val.iscell ())
    {
      Cell cv = val.cell_value ();
      for (octave_idx_type i = 0; i < cv.numel (); i++)
        cv(i) = search_mcos_classdef_objects (cv(i), save_mode);

      return cv;
    }

  // iterate through struct arrays
  if (val.isstruct ())
    {
      if (val.is_scalar_type ())
        {
          octave_scalar_map mv = val.scalar_map_value ();
          for (auto it = mv.begin (); it != mv.end (); ++it)
            {
              std::string field_name = mv.key (it);
              octave_value field_val = mv.contents (it);
              mv.assign (field_name,
                         search_mcos_classdef_objects (field_val, save_mode));
            }
          return octave_value (mv);
        }
      else
        {
          octave_map mv = val.map_value ();
          string_vector field_names = mv.fieldnames ();

          for (octave_idx_type field_idx = 0; field_idx < field_names.numel ();
               field_idx++)
            {
              std::string field_name = field_names[field_idx];
              Cell field_values = mv.contents (field_name);

              for (octave_idx_type i = 0; i < field_values.numel (); i++)
                field_values(i) = search_mcos_classdef_objects (field_values(i),
                                                                save_mode);

              mv.assign (field_name, field_values);
            }

          return octave_value (mv);
        }
    }

  return val;
}

octave_value
load_mcos_object (const octave_value& objmetadata)
{
  // Object metadata is a Nx1 uint32 array with the following structure:
  // [0xDD000000, objdims, dim1, dim2, ..., dimN,
  // object_id1, object_id2, ..., object_idN,
  // class_id]

  octave::load_save_system& lss = octave::__get_load_save_system__ ();
  octave::subsystem_handler *sh = lss.get_subsystem_handler ();

  const octave_uint32 *dt = objmetadata.uint32_array_value ().data ();
  if (static_cast<uint32_t>(dt[0]) != 0xDD000000)
    {
      warning_with_id ("Octave:load:invalid-mcos-object",
                       "load: invalid MCOS object metadata. Returning as %s",
                       objmetadata.class_name ().c_str ());
      return objmetadata;
    }
  uint32_t objdims = dt[1];

  dim_vector dv;
  dv.resize (objdims);
  for (octave_idx_type i = 0; i < objdims; i++)
    dv(i) = dt[2 + i];

  int nobjects = dv.numel ();
  std::vector<std::tuple<octave_map, uint32_t, bool>> m (nobjects);

  for (octave_idx_type i = 0; i < nobjects; i++)
    std::get<uint32_t> (m[i]) = dt[2 + objdims + i];

  uint32_t class_id = dt[2 + objdims + nobjects];
  std::string classname = sh->get_class_name (class_id);
  octave::cdef_class cls = octave::lookup_class (classname, false, true);
  if (! cls.ok ())
    {
      warning_with_id ("Octave:load:classdef-not-found",
                        "load: classdef not found. Element loaded as %s",
                        objmetadata.class_name ().c_str ());
      return objmetadata;
    }

  bool skip_constructor = ! cls.get ("ConstructOnLoad").bool_value ();

  for (octave_idx_type i = 0; i < nobjects; i++)
    {
      uint32_t obj_id = std::get<uint32_t> (m[i]);
      if (! lss.is_mcos_object_cache_entry (obj_id))
        {
          // put (scalar) object in cache for this object ID
          octave_value ovc
            = cls.construct (octave_value_list (), skip_constructor);
          lss.set_mcos_object_cache_entry (obj_id, ovc);

          auto [saveobj_type, obj_type_id, obj_dep_id] =
            sh->get_object_dependencies (obj_id);

          std::get<octave_map> (m[i])
            = sh->get_object_properties (obj_type_id, class_id, saveobj_type);
          std::get<bool> (m[i]) = saveobj_type;

          if (sh->check_dyn_props (obj_dep_id))
            warning_with_id ("Octave:load:dynamic-properties-not-supported",
                             "load: dynamic properties cannot be loaded and "
                             "will be skipped ");
        }
    }

  // create object with loaded data
  octave_value obj = cls.construct (octave_value_list (), true);
  octave_classdef *objdef = obj.classdef_object_value ();
  objdef->loadobj (m, dv);

  return obj;
}

bool
subsystem_handler::read_mat_subsystem (const octave_value& subsys, bool swap)
{
  if (! subsys.isstruct ())
    return false;

  const octave_scalar_map& subsys_map = subsys.scalar_map_value ();

  if (subsys_map.contains ("MCOS"))
    {
      const octave_value& mcos_data = subsys_map.contents ("MCOS");
      if (mcos_data.is_defined () && ! mcos_data.isempty ()
          && ! read_filewrapper (mcos_data.cell_value (), swap))
        return false;
    }

  if (subsys_map.contains ("java"))
    {
      const octave_value& java_data = subsys_map.contents ("java");
      if (java_data.is_defined () && ! java_data.isempty ())
        m_type_java = java_data;
    }

  if (subsys_map.contains ("handle"))
    {
      const octave_value& handle_data = subsys_map.contents ("handle");
      if (handle_data.is_defined () && ! handle_data.isempty ())
        m_type_handle = handle_data;
    }

  return true;
}

bool
subsystem_handler::read_filewrapper (const Cell& fwrap_data, bool swap)
{
  const octave_value& fwrap_metadata = fwrap_data(0, 0);
  if (! fwrap_metadata.is_uint8_type ())
    return false;

  const uint8NDArray& fwrap_metadata_array = fwrap_metadata.uint8_array_value ();

  // check version compatibility
  uint32_t version = reinterpret_cast<const uint32_t&> (fwrap_metadata_array(0));
  if (swap)
    swap_bytes<4> (&version, 1);
  if (version > m_filewrapper_version)
    error ("load: filewrapper version %u is not supported. "
           "This file was created with a newer version of MATLAB.",
           version);

  // get number of unique property names and class names
  m_num_names = reinterpret_cast<const uint32_t&> (fwrap_metadata_array(4));
  if (swap)
    swap_bytes<4> (&m_num_names, 1);

  // read region offsets
  for (int i = 0; i < 8; i++)
    {
      m_region_offsets[i] = reinterpret_cast<const uint32_t&>
                              (fwrap_metadata_array(8 + i * 4));
      if (swap)
        swap_bytes<4> (&m_region_offsets[i], 1);
    }

  // read property names and class names
  // stored as list of null terminated integers
  m_prop_class_names.resize (m_num_names);
  size_t name_offset = 40;
  for (uint32_t i = 0; i < m_num_names; i++)
    {
      size_t start_offset = name_offset;
      while (static_cast<uint8_t> (fwrap_metadata_array(name_offset)) != 0)
        name_offset++;

      std::string name;
      for (size_t j = start_offset; j < name_offset; j++)
        name += static_cast<char> (fwrap_metadata_array(j));

      m_prop_class_names[i] = name;
      name_offset++;  // skip the null terminator
    }

  // read property values
  // Property values are stored in fwrap_data(2:end-3).
  octave_idx_type prop_vals_size = fwrap_data.numel () - 5;
  if (prop_vals_size > 0)
    {
      m_fwrap_prop_vals.resize (prop_vals_size);
      for (octave_idx_type i = 0; i < prop_vals_size; i++)
        m_fwrap_prop_vals[i] = fwrap_data(2 + i);
    }
  else
    m_fwrap_prop_vals.clear ();  // mark undefined

  // Default values are stored in fwrap_data(end).
  m_fwrap_default_vals = fwrap_data(fwrap_data.numel () - 1).cell_value ();

  // read m_class_name_refs
  // contains list of indices to classname and namespace qualifier (if any) for
  // each class
  size_t class_name_size = (m_region_offsets[1] - m_region_offsets[0]) / 4;
  m_class_name_refs.resize (class_name_size);
  for (size_t i = 0; i < class_name_size; i++)
    {
      uint32_t val = reinterpret_cast<const uint32_t&>
                       (fwrap_metadata_array(m_region_offsets[0] + i * 4));
      if (swap)
        swap_bytes<4> (&val, 1);
      m_class_name_refs[i] = val;
    }

  // read m_object_id_refs
  // contains class ID, object_type_ID, dependency_ID for each object
  size_t object_id_size = (m_region_offsets[3] - m_region_offsets[2]) / 4;
  m_object_id_refs.resize (object_id_size);
  for (size_t i = 0; i < object_id_size; i++)
    {
      uint32_t val = reinterpret_cast<const uint32_t&>
                       (fwrap_metadata_array(m_region_offsets[2] + i * 4));
      if (swap)
        swap_bytes<4> (&val, 1);
      m_object_id_refs[i] = val;
    }

  // read m_object_prop_fields
  // contains blocks of property name indexes and values for each object
  size_t object_prop_size = (m_region_offsets[4] - m_region_offsets[3]) / 4;
  m_object_prop_fields.resize (object_prop_size);
  for (size_t i = 0; i < object_prop_size; i++)
    {
      uint32_t val = reinterpret_cast<const uint32_t&>
                       (fwrap_metadata_array(m_region_offsets[3] + i * 4));
      if (swap)
        swap_bytes<4> (&val, 1);
      m_object_prop_fields[i] = val;
    }

  // read m_saveobj_prop_fields
  // contains value index for each object with custom saveobj return types
  size_t saveobj_prop_size = (m_region_offsets[2] - m_region_offsets[1]) / 4;
  m_saveobj_prop_fields.resize (saveobj_prop_size);
  for (size_t i = 0; i < saveobj_prop_size; i++)
    {
      uint32_t val = reinterpret_cast<const uint32_t&>
                       (fwrap_metadata_array(m_region_offsets[1] + i * 4));
      if (swap)
        swap_bytes<4> (&val, 1);
      m_saveobj_prop_fields[i] = val;
    }

  // read m_dynamic_prop_refs
  //lists dynamic properties (if any) for each object dependency ID
  size_t dynamic_prop_size = (m_region_offsets[5] - m_region_offsets[4]) / 4;
  m_dynamic_prop_refs.resize (dynamic_prop_size);
  for (size_t i = 0; i < dynamic_prop_size; i++)
    {
      uint32_t val = reinterpret_cast<const uint32_t&>
                       (fwrap_metadata_array(m_region_offsets[4] + i * 4));
      if (swap)
        swap_bytes<4> (&val, 1);
      m_dynamic_prop_refs[i] = val;
    }

  return true;
}

std::string
subsystem_handler::get_class_name (const uint32_t class_id)
{
  // (namespace_idx, classname_idx, X, X)
  // ordered by class ID

  constexpr int block_size = 4;
  uint32_t namespace_idx = m_class_name_refs[class_id * block_size];
  uint32_t class_idx = m_class_name_refs[class_id * block_size + 1];

  std::string classname;
  if (namespace_idx)
    classname = m_prop_class_names[namespace_idx - 1] + ".";
  classname += m_prop_class_names[class_idx - 1];
  return classname;
}

std::tuple<bool, uint32_t, uint32_t>
subsystem_handler::get_object_dependencies (const uint32_t obj_id)
{
  // (class_ID, X, X, saveobj_ID, normal_obj_ID, dependency_ID)
  // ordered by object ID

  constexpr int block_size = 6;
  bool saveobj_type = true;

  uint32_t obj_dep_id = m_object_id_refs[obj_id * block_size + 5];
  uint32_t obj_con_id = m_object_id_refs[obj_id * block_size + 3];
  if (! obj_con_id)
    {
      saveobj_type = false;
      obj_con_id = m_object_id_refs[obj_id * block_size + 4];
    }

  return {saveobj_type, obj_con_id, obj_dep_id};
}

octave_map
subsystem_handler::get_object_properties (const uint32_t obj_type_id,
                                          const uint32_t class_id,
                                          const bool saveobj_ret_type)
{
  // (nprops, prop1_name_idx, prop1_val_type, prop1_val, prop2_name_idx ...)
  // ordered by object type ID

  const std::vector<uint32_t>& offset_fields
    = saveobj_ret_type ? m_saveobj_prop_fields : m_object_prop_fields;
  const uint32_t* ptr = offset_fields.data();
  const uint32_t* end_ptr = ptr + offset_fields.size();

  // skip to the correct obj_type_id block
  constexpr int block_size = 3;
  for (octave_idx_type current_id = 0; current_id < obj_type_id; current_id++)
    {
      uint32_t nprops = *ptr++;
      ptr += nprops * block_size;
      ptr += (1 + (nprops * block_size)) % 2;  // padding
      if (ptr >= end_ptr)
        error ("Could not load file");
    }

  uint32_t nprops = *ptr++;
  octave_value& default_vals = m_fwrap_default_vals(class_id);
  default_vals = search_mcos_classdef_objects (default_vals, false);
  octave_map prop_vals = default_vals.map_value ();

  for (octave_idx_type i = 0; i < nprops && ptr + 2 < end_ptr; i++)
    {
      uint32_t prop_name_idx = *ptr++;
      uint32_t prop_val_type = *ptr++;
      uint32_t prop_val_idx = *ptr++;

      const std::string& prop_name = m_prop_class_names[prop_name_idx - 1];

      switch (prop_val_type)
        {
        case 0:
          // Property value is enum/string reference
          {
            warning_with_id ("Octave:load:enum-string-ref",
                   "load: property '%s' is an enum reference. "
                   "This may not be supported and is read as a string.",
                   prop_name.c_str ());
            const std::string& prop_val = m_prop_class_names[prop_val_idx - 1];
            prop_vals.assign (prop_name, octave_value (prop_val));
          }
          break;

        case 1:
          // Property value points to a cell in m_fwrap_prop_vals
          {
            octave_value& prop_val = m_fwrap_prop_vals[prop_val_idx];
            prop_val = search_mcos_classdef_objects (prop_val, false);
            prop_vals.assign (prop_name, prop_val);
          }
          break;

        case 2:
          // prop_val_idx is the property value itself? (Not sure)
          {
            prop_vals.assign (prop_name, octave_value (prop_val_idx));
          }
          break;

        default:
          warning ("load: unknown property value type %u for property '%s'",
                   prop_val_type, prop_name.c_str ());
          break;
        }
    }

  return prop_vals;
}

bool
subsystem_handler::check_dyn_props (const uint32_t obj_dep_id)
{
  // (ndynprops, dynprop1_obj_id, dynprop2_obj_id ...)
  // ordered by dependency ID

  // FIXME: Actually implement support when Octave supports dynamicprops.
  //        For now, only check if exists.

  const uint32_t *ptr = m_dynamic_prop_refs.data ();
  const uint32_t *end_ptr = ptr + m_dynamic_prop_refs.size ();

  bool has_dyn_props = false;
  for (uint32_t current_id = 0; current_id < obj_dep_id; current_id++)
    {
      uint32_t nprops = *ptr++;
      ptr += nprops;
      ptr += (1 + nprops) % 2;  // padding
      if (ptr >= end_ptr)
          return false;
    }

  uint32_t nprops = *ptr++;
  if (nprops)
    has_dyn_props = true;
  return has_dyn_props;
}

std::pair<std::string, std::string>
subsystem_handler::parse_class_name (const std::string& full_class_name)
{
  std::string namespace_name;
  std::string class_name;

  // separate namespace from class name
  size_t last_dot_pos = full_class_name.find_last_of ('.');
  if (last_dot_pos != std::string::npos)
    {
      namespace_name = full_class_name.substr (0, last_dot_pos);
      class_name = full_class_name.substr (last_dot_pos + 1);
    }
  else
    {
      namespace_name = "";
      class_name = full_class_name;
    }

  return std::make_pair (namespace_name, class_name);
}

uint32_t
subsystem_handler::get_name_index (const std::string& name)
{
  for (uint32_t i = 0; i < m_prop_class_names.numel (); i++)
    {
      if (m_prop_class_names[i] == name)
        return i + 1;  // names are 1-indexed
    }

  m_prop_class_names.append (name);
  m_num_names = m_prop_class_names.numel ();
  return m_num_names;
}

uint32_t
subsystem_handler::get_or_create_class_id (const std::string& namespace_name,
                                           const std::string& class_name)
{
  uint32_t current_class_id = 1;
  bool class_exists = false;

  // search through existing m_class_name_refs to find this class
  for (size_t i = 0; i < m_class_name_refs.size(); i += 4)
    {
      uint32_t existing_namespace_idx = m_class_name_refs[i];
      uint32_t existing_class_idx = m_class_name_refs[i + 1];

      std::string existing_namespace
        = ((existing_namespace_idx > 0)
           ? m_prop_class_names[existing_namespace_idx - 1] : "");
      std::string existing_class = m_prop_class_names[existing_class_idx - 1];

      if (existing_namespace == namespace_name && existing_class == class_name)
        {
          class_exists = true;
          break;
        }
      current_class_id++;
    }

  if (! class_exists)
    {
      m_class_id_counter++;

      // individual names can be shared
      uint32_t namespace_idx = (namespace_name.empty()
                                ? 0 : get_name_index (namespace_name));
      uint32_t class_name_idx = get_name_index (class_name);

      m_class_name_refs.push_back (namespace_idx);
      m_class_name_refs.push_back (class_name_idx);
      m_class_name_refs.push_back (0);
      m_class_name_refs.push_back (0);
    }

  return current_class_id;
}

uint32NDArray
subsystem_handler::create_metadata_array (const dim_vector& obj_dims,
                                          const std::vector<uint32_t>& object_ids,
                                          uint32_t class_id)
{
  uint32_t obj_ndims = obj_dims.ndims ();
  uint32_t nobjects = obj_dims.numel ();

  // metadata format: [0xDD000000, objdims, dim1, dim2, ..., dimN,
  //                   object_id1, object_id2, ..., object_idN, class_id]
  uint32NDArray metadata (dim_vector (3 + obj_ndims + nobjects, 1));

  metadata(0) = 0xDD000000;  // magic number for MCOS objects
  metadata(1) = obj_ndims;

  for (uint32_t i = 0; i < obj_ndims; i++)
    metadata(2 + i) = obj_dims(i);

  for (uint32_t i = 0; i < nobjects; i++)
    metadata(2 + obj_ndims + i) = object_ids[i];

  metadata(2 + obj_ndims + nobjects) = class_id;

  return metadata;
}

void
subsystem_handler::process_object_properties (const std::vector<std::tuple<octave_map, uint32_t, bool>>& saveobj_data,
                                              const std::vector<bool>& is_new,
                                              uint32_t class_id)
{
  // Save positions for insertion at the end
  size_t object_id_refs_pos = m_object_id_refs.size ();
  size_t object_prop_fields_pos = m_object_prop_fields.size ();
  size_t saveobj_prop_fields_pos = m_saveobj_prop_fields.size ();

  // local storage for this batch of objects
  std::vector<uint32_t> local_object_id_refs;
  std::vector<uint32_t> local_object_prop_fields;
  std::vector<uint32_t> local_saveobj_prop_fields;
  std::vector<uint32_t> local_dynamic_prop_refs;

  for (size_t obj_idx = 0; obj_idx < saveobj_data.size (); obj_idx++)
    {
      const auto& obj_tuple = saveobj_data[obj_idx];
      const octave_map& prop_map = std::get<octave_map> (obj_tuple);
      bool saveobj_ret_type = std::get<bool> (obj_tuple);

      if (is_new[obj_idx])
        {
          uint32_t saveobj_object_id = 0;
          uint32_t normal_object_id = 0;
          std::vector<uint32_t>& target_prop_fields
            = (saveobj_ret_type ? local_saveobj_prop_fields
                                : local_object_prop_fields);

          if (saveobj_ret_type)
            saveobj_object_id = ++m_saveobj_object_counter;
          else
            normal_object_id = ++m_normal_object_counter;

          // TODO: Add dependency IDs
          local_object_id_refs.push_back (class_id);
          local_object_id_refs.push_back (0);
          local_object_id_refs.push_back (0);
          local_object_id_refs.push_back (saveobj_object_id);
          local_object_id_refs.push_back (normal_object_id);
          local_object_id_refs.push_back (0);

          string_vector prop_names = prop_map.fieldnames ();
          uint32_t num_props = prop_names.numel ();
          target_prop_fields.push_back (num_props);

          for (octave_idx_type prop_idx = 0; prop_idx < num_props; prop_idx++)
            {
              std::string prop_name = prop_names[prop_idx];
              octave_value prop_value = (prop_map.contents (prop_name))(0);

              prop_value = search_mcos_classdef_objects (prop_value, true);

              uint32_t field_name_idx = get_name_index (prop_name);
              uint32_t cell_number_idx = m_fwrap_prop_vals.size ();
              m_fwrap_prop_vals.push_back (prop_value);

              // (field_name_idx, field_val_type, cell_idx)
              target_prop_fields.push_back (field_name_idx);
              target_prop_fields.push_back (1);
              target_prop_fields.push_back (cell_number_idx);
            }

          // TODO: Parse for different property types

          if ((num_props * 3 + 1) % 2 != 0)
            target_prop_fields.push_back (0);  // padding

          // dummy values for compatibility
          local_dynamic_prop_refs.push_back (0);
          local_dynamic_prop_refs.push_back (0);
        }
    }

  // Insert at saved positions instead of appending to end
  m_object_id_refs.insert (m_object_id_refs.begin () + object_id_refs_pos,
                           local_object_id_refs.begin (),
                           local_object_id_refs.end ());
  m_object_prop_fields.insert (m_object_prop_fields.begin () + object_prop_fields_pos,
                               local_object_prop_fields.begin (),
                               local_object_prop_fields.end ());
  m_saveobj_prop_fields.insert (m_saveobj_prop_fields.begin () + saveobj_prop_fields_pos,
                                local_saveobj_prop_fields.begin (),
                                local_saveobj_prop_fields.end ());
  m_dynamic_prop_refs.insert (m_dynamic_prop_refs.end (),
                              local_dynamic_prop_refs.begin (),
                              local_dynamic_prop_refs.end ());
}

uint32NDArray
subsystem_handler::set_mcos_object_metadata (const octave_value& obj)
{
  std::string full_class_name = obj.class_name ();
  auto [namespace_name, class_name] = parse_class_name (full_class_name);

  uint32_t current_class_id = get_or_create_class_id (namespace_name, class_name);

  std::vector<bool> is_new (obj.numel ());
  std::vector<std::tuple<octave_map, uint32_t, bool>> saveobj_data
    = obj.classdef_object_value ()->saveobj (is_new);

  dim_vector obj_dims = obj.dims ();
  std::vector<uint32_t> actual_object_ids;
  actual_object_ids.reserve (obj_dims.numel ());

  for (const auto& obj_tuple : saveobj_data)
    {
      uint32_t obj_id = std::get<uint32_t> (obj_tuple);
      actual_object_ids.push_back (obj_id);
    }


  process_object_properties (saveobj_data, is_new, current_class_id);

  return create_metadata_array (obj_dims, actual_object_ids, current_class_id);
}

uint8NDArray
subsystem_handler::create_filewrapper_metadata ()
{
  // helper to write data for object ID = 0
  // Note: MATLAB has values for some kind of placeholder object with ID = 0
  auto write_object_id_0 = [](uint8_t*& ptr, int count) -> void
    {
      uint32_t zero = 0;
      for (int i = 0; i < count; i++)
        {
          std::memcpy (ptr, &zero, 4);
          ptr += 4;
        }
    };

  // calculate region offsets
  size_t current_offset = 40;  // start after header (version + m_num_names + 8 offsets)

  for (octave_idx_type i = 0; i < m_prop_class_names.numel (); i++)
    current_offset += m_prop_class_names[i].length () + 1;  // +1 for null terminator

  // padding
  size_t padding_length = 0;
  if (current_offset % 8 != 0)
    {
      padding_length = 8 - (current_offset % 8);
      current_offset += padding_length;
    }

  m_region_offsets[0] = current_offset;  // class_name metadata

  current_offset += (m_class_name_refs.size () + 4) * 4;
  m_region_offsets[1] = current_offset;  // saveobj prop metadata

  current_offset += m_saveobj_prop_fields.size () * 4;
  if (m_saveobj_prop_fields.size () > 0)
    current_offset += 8;
  m_region_offsets[2] = current_offset;  // object id metadata

  current_offset += (m_object_id_refs.size () + 6) * 4;
  m_region_offsets[3] = current_offset;  // object prop metadata

  current_offset += m_object_prop_fields.size () * 4;
  if (m_object_prop_fields.size () > 0)
    current_offset += 8;
  m_region_offsets[4] = current_offset;  // dynamicprop metadata

  // Offsets 5-7: Unknown purposes
  current_offset += m_dynamic_prop_refs.size () * 4;
  current_offset += 8;
  m_region_offsets[5] = current_offset;
  m_region_offsets[6] = current_offset;
  current_offset += 8;
  m_region_offsets[7] = current_offset;  // end of metadata

  size_t& metadata_size = m_region_offsets[7];

  // metadata format:
  // version, m_num_names, region offsets, names
  // class name metadata, saveobj prop metadata
  // object id metadata, obj prop metadata
  // dynamicprop metadata + unknowns
  uint8NDArray metadata (dim_vector (metadata_size, 1));
  uint8_t* metadata_ptr = reinterpret_cast<uint8_t*> (metadata.rwdata ());

  uint32_t version = m_filewrapper_version;
  std::memcpy (metadata_ptr, &version, 4);
  metadata_ptr += 4;

  std::memcpy (metadata_ptr, &m_num_names, 4);
  metadata_ptr += 4;

  for (int i = 0; i < 8; i++)
    {
      uint32_t offset_value = static_cast<uint32_t> (m_region_offsets[i]);
      std::memcpy (metadata_ptr, &offset_value, 4);
      metadata_ptr += 4;
    }

  for (octave_idx_type i = 0; i < m_prop_class_names.numel (); i++)
    {
      const std::string& name = m_prop_class_names[i];
      std::memcpy (metadata_ptr, name.c_str (), name.length ());
      metadata_ptr += name.length ();
      *metadata_ptr++ = 0;  // null terminator
    }
  for (size_t i = 0; i < padding_length; i++)
    *metadata_ptr++ = 0;  // padding

  write_object_id_0 (metadata_ptr, 4);
  for (size_t i = 0; i < m_class_name_refs.size (); i++)
    {
      uint32_t value = m_class_name_refs[i];
      std::memcpy (metadata_ptr, &value, 4);
      metadata_ptr += 4;
    }

  if (m_saveobj_prop_fields.size () > 0)
    write_object_id_0 (metadata_ptr, 2);  // slot 0 padding
  for (size_t i = 0; i < m_saveobj_prop_fields.size (); i++)
    {
      uint32_t value = m_saveobj_prop_fields[i];
      std::memcpy (metadata_ptr, &value, 4);
      metadata_ptr += 4;
    }

  write_object_id_0 (metadata_ptr, 6);  // slot 0 padding
  for (size_t i = 0; i < m_object_id_refs.size (); i++)
    {
      uint32_t value = m_object_id_refs[i];
      std::memcpy (metadata_ptr, &value, 4);
      metadata_ptr += 4;
    }

  if (m_object_prop_fields.size () > 0)
    write_object_id_0 (metadata_ptr, 2);  // slot 0 padding
  for (size_t i = 0; i < m_object_prop_fields.size (); i++)
    {
      uint32_t value = m_object_prop_fields[i];
      std::memcpy (metadata_ptr, &value, 4);
      metadata_ptr += 4;
    }

  write_object_id_0 (metadata_ptr, 2);  // slot 0 padding
  for (size_t i = 0; i < m_dynamic_prop_refs.size (); i++)
    {
      uint32_t value = m_dynamic_prop_refs[i];
      std::memcpy (metadata_ptr, &value, 4);
      metadata_ptr += 4;
    }

  // write 8 bytes of zeros for region_offset[6]
  for (int i = 0; i < 8; i++)
    *metadata_ptr++ = 0;

  return metadata;
}

void
subsystem_handler::serialize_to_cell_array ()
{
  if (! m_class_id_counter)
    return;

  // create cell array with size = m_fwrap_prop_vals.size () + 2 + 3
  // (for the additional cells)
  octave_idx_type cell_size = m_fwrap_prop_vals.size () + 5;
  Cell result (dim_vector (cell_size, 1));

  result(0) = create_filewrapper_metadata ();
  result(1) = octave_value (Matrix ());  // empty matrix for some reason

  // set remaining cells to property values
  for (size_t i = 0; i < m_fwrap_prop_vals.size (); i++)
    result(i + 2) = m_fwrap_prop_vals[i];

  // unknown cells
  // These contain some class-level metadata
  // typically a cell array of empty 1x0 structs for each class ID
  octave_idx_type base_idx = m_fwrap_prop_vals.size () + 2;
  uint32_t num_class_ids = m_class_id_counter + 1;

  Cell u3_structs (dim_vector (num_class_ids, 1));
  for (uint32_t i = 0; i < num_class_ids; i++)
    u3_structs(i) = octave_value (octave_map (dim_vector (1, 0)));
  result(base_idx) = octave_value (u3_structs);

  int32NDArray u2_zeros (dim_vector (num_class_ids, 1));
  for (uint32_t i = 0; i < num_class_ids; i++)
    u2_zeros(i) = 0;
  result(base_idx + 1) = octave_value (u2_zeros);

  // contains default values - not separately tagged in Octave
  m_fwrap_default_vals.resize (dim_vector (num_class_ids, 1));
  for (uint32_t i = 0; i < num_class_ids; i++)
    m_fwrap_default_vals(i) = octave_value (octave_map (dim_vector (1, 0)));
  result(base_idx + 2) = octave_value (m_fwrap_default_vals);

  // storing only because save -> save_mat5_element_length which needs the
  // actual contents to write nbytes
  m_serialized_data = result;
}

OCTAVE_END_NAMESPACE(octave)
