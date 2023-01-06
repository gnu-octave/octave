////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2018-2023 The Octave Project Developers
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

#include <iomanip>
#include <list>
#include <ostream>
#include <sstream>

#include "Cell.h"
#include "error.h"
#include "octave-preserve-stream-state.h"
#include "ov.h"
#include "oct-map.h"
#include "pager.h"
#include "syminfo.h"

OCTAVE_BEGIN_NAMESPACE(octave)

void
symbol_info::display_line (std::ostream& os,
                           const std::list<whos_parameter>& params) const
{
  std::string dims_str = m_value.get_dims_str ();

  auto i = params.begin ();

  preserve_stream_state stream_state (os);

  while (i != params.end ())
    {
      whos_parameter param = *i;

      if (param.command != '\0')
        {
          // Do the actual printing.

          switch (param.modifier)
            {
            case 'l':
              os << std::setiosflags (std::ios::left)
                 << std::setw (param.parameter_length);
              break;

            case 'r':
              os << std::setiosflags (std::ios::right)
                 << std::setw (param.parameter_length);
              break;

            case 'c':
              if (param.command == 's')
                {
                  int front = param.first_parameter_length
                              - dims_str.find ('x');
                  int back = param.parameter_length
                             - dims_str.length ()
                             - front;
                  front = (front > 0) ? front : 0;
                  back = (back > 0) ? back : 0;

                  os << std::setiosflags (std::ios::left)
                     << std::setw (front)
                     << ""
                     << std::resetiosflags (std::ios::left)
                     << dims_str
                     << std::setiosflags (std::ios::left)
                     << std::setw (back)
                     << ""
                     << std::resetiosflags (std::ios::left);
                }
              else
                {
                  os << std::setiosflags (std::ios::left)
                     << std::setw (param.parameter_length);
                }
              break;

            default:
              error ("whos_line_format: modifier '%c' unknown",
                     param.modifier);
            }

          switch (param.command)
            {
            case 'a':
              {
                char tmp[6];

                tmp[0] = (m_is_complex ? 'c' : ' ');
                tmp[1] = (m_is_sparse ? 's' : ' ');
                tmp[2] = (m_is_formal ? 'f' : ' ');
                tmp[3] = (m_is_global ? 'g' : ' ');
                tmp[4] = (m_is_persistent ? 'p' : ' ');
                tmp[5] = 0;

                os << tmp;
              }
              break;

            case 'b':
              os << m_value.byte_size ();
              break;

            case 'c':
              os << m_value.class_name ();
              break;

            case 'e':
              os << m_value.numel ();
              break;

            case 'n':
              os << m_name;
              break;

            case 's':
              if (param.modifier != 'c')
                os << dims_str;
              break;

            case 't':
              os << m_value.type_name ();
              break;

            default:
              error ("whos_line_format: command '%c' unknown",
                     param.command);
            }

          os << std::resetiosflags (std::ios::left)
             << std::resetiosflags (std::ios::right);
          i++;
        }
      else
        {
          os << param.text;
          i++;
        }
    }
}

// FIXME: should we be using std::map<symbol_info> instead of a list?

octave_value symbol_info_list::varval (const std::string& name) const
{
  for (const auto& syminfo : m_lst)
    {
      if (name == syminfo.name ())
        return syminfo.value ();
    }

  return octave_value ();
}

std::list<std::string> symbol_info_list::names (void) const
{
  std::list<std::string> retval;

  for (const auto& syminfo : m_lst)
    retval.push_back (syminfo.name ());

  return retval;
}

octave_map
symbol_info_list::map_value (const std::string& caller_function_name,
                             int nesting_level) const
{
  std::size_t len = m_lst.size ();

  Cell name_info (len, 1);
  Cell size_info (len, 1);
  Cell bytes_info (len, 1);
  Cell class_info (len, 1);
  Cell global_info (len, 1);
  Cell sparse_info (len, 1);
  Cell complex_info (len, 1);
  Cell nesting_info (len, 1);
  Cell persistent_info (len, 1);

  std::size_t j = 0;

  for (const auto& syminfo : m_lst)
    {
      octave_scalar_map ni;

      ni.assign ("function", caller_function_name);
      ni.assign ("level", nesting_level);

      name_info(j) = syminfo.name ();
      global_info(j) = syminfo.is_global ();
      persistent_info(j) = syminfo.is_persistent ();

      octave_value val = syminfo.value ();

      size_info(j) = val.size ();
      bytes_info(j) = val.byte_size ();
      class_info(j) = val.class_name ();
      sparse_info(j) = val.issparse ();
      complex_info(j) = val.iscomplex ();
      nesting_info(j) = ni;

      j++;
    }

  octave_map info;

  info.assign ("name", name_info);
  info.assign ("size", size_info);
  info.assign ("bytes", bytes_info);
  info.assign ("class", class_info);
  info.assign ("global", global_info);
  info.assign ("sparse", sparse_info);
  info.assign ("complex", complex_info);
  info.assign ("nesting", nesting_info);
  info.assign ("persistent", persistent_info);

  return info;
}

void
symbol_info_list::print_descriptor (std::ostream& os,
                                    const std::list<whos_parameter> params) const
{
  std::ostringstream param_buf;

  preserve_stream_state stream_state (os);

  for (const auto& param : params)
    {
      if (param.command != '\0')
        {
          // Do the actual printing
          switch (param.modifier)
            {
            case 'l':
              os << std::setiosflags (std::ios::left)
                 << std::setw (param.parameter_length);
              param_buf << std::setiosflags (std::ios::left)
                        << std::setw (param.parameter_length);
              break;

            case 'r':
              os << std::setiosflags (std::ios::right)
                 << std::setw (param.parameter_length);
              param_buf << std::setiosflags (std::ios::right)
                        << std::setw (param.parameter_length);
              break;

            case 'c':
              if (param.command != 's')
                {
                  os << std::setiosflags (std::ios::left)
                     << std::setw (param.parameter_length);
                  param_buf << std::setiosflags (std::ios::left)
                            << std::setw (param.parameter_length);
                }
              break;

            default:
              os << std::setiosflags (std::ios::left)
                 << std::setw (param.parameter_length);
              param_buf << std::setiosflags (std::ios::left)
                        << std::setw (param.parameter_length);
            }

          if (param.command == 's' && param.modifier == 'c')
            {
              int a = param.first_parameter_length - param.balance;
              a = (a < 0 ? 0 : a);
              int b = param.parameter_length - a - param.text.length ();
              b = (b < 0 ? 0 : b);
              os << std::setiosflags (std::ios::left) << std::setw (a)
                 << "" << std::resetiosflags (std::ios::left) << param.text
                 << std::setiosflags (std::ios::left)
                 << std::setw (b) << ""
                 << std::resetiosflags (std::ios::left);
              param_buf << std::setiosflags (std::ios::left)
                        << std::setw (a)
                        << "" << std::resetiosflags (std::ios::left)
                        << param.line
                        << std::setiosflags (std::ios::left)
                        << std::setw (b) << ""
                        << std::resetiosflags (std::ios::left);
            }
          else
            {
              os << param.text;
              param_buf << param.line;
            }
          os << std::resetiosflags (std::ios::left)
             << std::resetiosflags (std::ios::right);
          param_buf << std::resetiosflags (std::ios::left)
                    << std::resetiosflags (std::ios::right);
        }
      else
        {
          os << param.text;
          param_buf << param.line;
        }
    }

  os << param_buf.str ();
}

void symbol_info_list::display (std::ostream& os,
                                const std::string& format) const
{
  if (! m_lst.empty ())
    {
      std::size_t bytes = 0;
      std::size_t elements = 0;

      std::list<whos_parameter> params = parse_whos_line_format (format);

      print_descriptor (os, params);

      octave_stdout << "\n";

      for (const auto& syminfo : m_lst)
        {
          syminfo.display_line (os, params);

          octave_value val = syminfo.value ();

          elements += val.numel ();
          bytes += val.byte_size ();
        }

      os << "\nTotal is " << elements
         << (elements == 1 ? " element" : " elements")
         << " using " << bytes << (bytes == 1 ? " byte" : " bytes")
         << "\n";
    }
}

std::list<whos_parameter>
symbol_info_list::parse_whos_line_format (const std::string& format) const
{
  int idx;
  std::size_t format_len = format.length ();
  char garbage;
  std::list<whos_parameter> params;

  std::size_t bytes1;
  int elements1;

  std::string param_string = "abcenst";
  Array<int> param_length (dim_vector (param_string.length (), 1));
  Array<std::string> param_names (dim_vector (param_string.length (), 1));
  std::size_t pos_a, pos_b, pos_c, pos_e, pos_n, pos_s, pos_t;

  pos_a = param_string.find ('a'); // Attributes
  pos_b = param_string.find ('b'); // Bytes
  pos_c = param_string.find ('c'); // Class
  pos_e = param_string.find ('e'); // Elements
  pos_n = param_string.find ('n'); // Name
  pos_s = param_string.find ('s'); // Size
  pos_t = param_string.find ('t'); // Type

  param_names(pos_a) = "Attr";
  param_names(pos_b) = "Bytes";
  param_names(pos_c) = "Class";
  param_names(pos_e) = "Elements";
  param_names(pos_n) = "Name";
  param_names(pos_s) = "Size";
  param_names(pos_t) = "Type";

  for (std::size_t i = 0; i < param_string.length (); i++)
    param_length(i) = param_names(i).length ();

  // The attribute column needs size 6.
  param_length(pos_a) = 6;

  // Calculating necessary spacing for name column,
  // bytes column, elements column and class column

  for (const auto& syminfo : m_lst)
    {
      std::stringstream ss1, ss2;
      std::string str;

      str = syminfo.name ();
      param_length(pos_n) = ((str.length ()
                              > static_cast<std::size_t> (param_length(pos_n)))
                             ? str.length () : param_length(pos_n));

      octave_value val = syminfo.value ();

      str = val.type_name ();
      param_length(pos_t) = ((str.length ()
                              > static_cast<std::size_t> (param_length(pos_t)))
                             ? str.length () : param_length(pos_t));

      elements1 = val.numel ();
      ss1 << elements1;
      str = ss1.str ();
      param_length(pos_e) = ((str.length ()
                              > static_cast<std::size_t> (param_length(pos_e)))
                             ? str.length () : param_length(pos_e));

      bytes1 = val.byte_size ();
      ss2 << bytes1;
      str = ss2.str ();
      param_length(pos_b) = ((str.length ()
                              > static_cast<std::size_t> (param_length(pos_b)))
                             ? str.length () : param_length (pos_b));
    }

  idx = 0;
  while (static_cast<std::size_t> (idx) < format_len)
    {
      whos_parameter param;
      param.command = '\0';

      if (format[idx] == '%')
        {
          param.modifier = 'r';
          param.parameter_length = 0;

          int a = 0;
          int b = -1;
          int balance = 1;
          unsigned int items;
          std::size_t pos;
          std::string cmd;

          // Parse one command from format
          cmd = format.substr (idx, format.length ());
          pos = cmd.find (';');
          if (pos == std::string::npos)
            error ("parameter without ; in format");

          cmd = cmd.substr (0, pos+1);

          idx += cmd.length ();

          // FIXME: use iostream functions instead of sscanf!

          if (cmd.find_first_of ("crl") != 1)
            items = sscanf (cmd.c_str (), "%c%c:%d:%d:%d;",
                            &garbage, &param.command, &a, &b, &balance);
          else
            items = sscanf (cmd.c_str (), "%c%c%c:%d:%d:%d;",
                            &garbage, &param.modifier, &param.command,
                            &a, &b, &balance) - 1;

          if (items < 2)
            error ("whos_line_format: found parameter structure without command");

          // Exception case of bare class command 'c' without modifier 'l/r'
          if (param.modifier == 'c'
              && param_string.find (param.command) == std::string::npos)
            {
              param.modifier = 'r';
              param.command = 'c';
            }

          // Insert data into parameter
          param.first_parameter_length = 0;
          pos = param_string.find (param.command);
          if (pos == std::string::npos)
            error ("whos_line_format: '%c' is not a command", param.command);

          param.parameter_length = param_length(pos);
          param.text = param_names(pos);
          param.line.assign (param_names(pos).length (), '=');

          param.parameter_length = (a > param.parameter_length
                                    ? a : param.parameter_length);
          if (param.command == 's' && param.modifier == 'c' && b > 0)
            param.first_parameter_length = b;

          if (param.command == 's')
            {
              // Have to calculate space needed for printing
              // matrix dimensions Space needed for Size column is
              // hard to determine in prior, because it depends on
              // dimensions to be shown.  That is why it is
              // recalculated for each Size-command int first,
              // rest = 0, total;
              int rest = 0;
              int first = param.first_parameter_length;
              int total = param.parameter_length;

              for (const auto& syminfo : m_lst)
                {
                  octave_value val = syminfo.value ();
                  std::string dims_str = val.get_dims_str ();
                  int first1 = dims_str.find ('x');
                  int total1 = dims_str.length ();
                  int rest1 = total1 - first1;
                  rest = (rest1 > rest ? rest1 : rest);
                  first = (first1 > first ? first1 : first);
                  total = (total1 > total ? total1 : total);
                }

              if (param.modifier == 'c')
                {
                  if (first < balance)
                    first += balance - first;
                  if (rest + balance < param.parameter_length)
                    rest += param.parameter_length - rest - balance;

                  param.parameter_length = first + rest;
                  param.first_parameter_length = first;
                  param.balance = balance;
                }
              else
                {
                  param.parameter_length = total;
                  param.first_parameter_length = 0;
                }
            }
          else if (param.modifier == 'c')
            error ("whos_line_format: modifier 'c' not available for command '%c'",
                   param.command);

          // What happens if format contains negative numbers
          // at param_length positions?
          param.balance = (b < 0 ? 0 : param.balance);
          param.first_parameter_length = (b < 0
                                          ? 0
                                          : param.first_parameter_length);
          param.parameter_length = (a < 0
                                    ? 0
                                    : (param.parameter_length
                                       < param_length(pos_s)
                                       ? param_length(pos_s)
                                       : param.parameter_length));

          params.push_back (param);
        }
      else
        {
          // Text string, to be printed as it is ...
          std::string text;
          std::size_t pos;
          text = format.substr (idx, format.length ());
          pos = text.find ('%');
          if (pos != std::string::npos)
            text = text.substr (0, pos);

          // Push parameter into list ...
          idx += text.length ();
          param.text=text;
          param.line.assign (text.length (), ' ');
          params.push_back (param);
        }
    }

  return params;
}

OCTAVE_END_NAMESPACE(octave)
