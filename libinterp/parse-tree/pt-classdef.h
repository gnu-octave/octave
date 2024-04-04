////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2012-2024 The Octave Project Developers
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

#if ! defined (octave_tree_classdef_h)
#define octave_tree_classdef_h 1

#include "octave-config.h"

class octave_value;

#include "pt-cmd.h"
#include "pt-delimiter-list.h"
#include "pt-exp.h"
#include "pt-walk.h"
#include "pt-id.h"
#include "token.h"

#include <list>

OCTAVE_BEGIN_NAMESPACE(octave)

class coment_list;
class interpreter;
class tree_arg_validation;

class tree_superclass_ref : public tree_expression
{
public:

  tree_superclass_ref (const std::string& meth, const std::string& cls, const token& tok)
    : m_method_name (meth), m_class_name (cls), m_token (tok)
  { }

  OCTAVE_DISABLE_CONSTRUCT_COPY_MOVE (tree_superclass_ref)

  ~tree_superclass_ref () = default;

  filepos beg_pos () const { return m_token.beg_pos (); }
  filepos end_pos () const { return m_token.end_pos (); }

  std::string method_name () const
  {
    return m_method_name;
  }

  std::string class_name () const { return m_class_name; }

  tree_superclass_ref * dup (symbol_scope& scope) const;

  octave_value evaluate (tree_evaluator& tw, int nargout = 1)
  {
    octave_value_list retval = evaluate_n (tw, nargout);

    return retval.length () > 0 ? retval(0) : octave_value ();
  }

  octave_value_list evaluate_n (tree_evaluator& tw, int nargout = 1);

  void accept (tree_walker& tw)
  {
    tw.visit_superclass_ref (*this);
  }

private:

  // The name of the method to call.  This is the text before the
  // "@" and may be of the form "object.method".
  std::string m_method_name;

  // The name of the superclass.  This is the text after the "@"
  // and may be of the form "object.method".
  std::string m_class_name;

  token m_token;
};

class tree_metaclass_query : public tree_expression
{
public:

  tree_metaclass_query (const std::string& cls, const token& tok)
    : m_class_name (cls), m_token (tok)
  { }

  OCTAVE_DISABLE_CONSTRUCT_COPY_MOVE (tree_metaclass_query)

  ~tree_metaclass_query () = default;

  filepos beg_pos () const { return m_token.beg_pos (); }
  filepos end_pos () const { return m_token.end_pos (); }

  std::string class_name () const { return m_class_name; }

  tree_metaclass_query * dup (symbol_scope& scope) const;

  octave_value evaluate (tree_evaluator&, int nargout = 1);

  octave_value_list evaluate_n (tree_evaluator& tw, int nargout = 1)
  {
    return ovl (evaluate (tw, nargout));
  }

  void accept (tree_walker& tw)
  {
    tw.visit_metaclass_query (*this);
  }

private:

  std::string m_class_name;

  token m_token;
};

class tree_classdef_attribute
{
public:

  tree_classdef_attribute (tree_identifier *i)
    : m_id (i)
  { }

  tree_classdef_attribute (tree_identifier *i, const token eq_tok, tree_expression *e)
    : m_id (i), m_eq_tok (eq_tok), m_expr (e)
  { }

  tree_classdef_attribute (const token& not_tok, tree_identifier *i, bool b)
    : m_not_tok (not_tok), m_id (i), m_neg (b)
  { }

  OCTAVE_DISABLE_COPY_MOVE (tree_classdef_attribute)

  ~tree_classdef_attribute ()
  {
    delete m_id;
    delete m_expr;
  }

  filepos beg_pos () const { return m_not_tok ? m_not_tok.beg_pos () : m_id->beg_pos (); }
  filepos end_pos () const { return m_expr ? m_expr->end_pos () : m_id->end_pos (); }

  tree_identifier * ident () { return m_id; }

  tree_expression * expression () { return m_expr; }

  bool negate () { return m_neg; }

  void accept (tree_walker& tw)
  {
    tw.visit_classdef_attribute (*this);
  }

private:

  token m_not_tok;
  tree_identifier *m_id;
  token m_eq_tok;
  tree_expression *m_expr {nullptr};
  bool m_neg {false};
};

class tree_classdef_attribute_list : public std::list<tree_classdef_attribute *>
{
public:

  tree_classdef_attribute_list () { }

  tree_classdef_attribute_list (tree_classdef_attribute *a) { push_back (a); }

  tree_classdef_attribute_list (const std::list<tree_classdef_attribute *>& a)
    : std::list<tree_classdef_attribute *> (a)
  { }

  OCTAVE_DISABLE_COPY_MOVE (tree_classdef_attribute_list)

  ~tree_classdef_attribute_list ();

  tree_classdef_attribute_list * mark_in_delims (const token& open_delim, token& close_delim)
  {
    m_delims.push (open_delim, close_delim);
    return this;
  }

  void accept (tree_walker& tw)
  {
    tw.visit_classdef_attribute_list (*this);
  }

private:

  tree_delimiter_list m_delims;
};

class tree_classdef_superclass
{
public:

  tree_classdef_superclass (const token& fqident)
    : m_fqident (fqident)
  { }

  OCTAVE_DISABLE_CONSTRUCT_COPY_MOVE (tree_classdef_superclass)

  ~tree_classdef_superclass () = default;

  void set_separator (const token& sep_tok) { m_sep_tok = sep_tok; }

  std::string class_name () { return m_fqident.text (); }

  void accept (tree_walker& tw)
  {
    tw.visit_classdef_superclass (*this);
  }

private:

  // The '<' or '&&' token introducing an element of a superclass list
  // element.  Is there a better name for it?

  token m_sep_tok;

  // The fully-qualified identifier token for this superclass element.
  token m_fqident;
};

class tree_classdef_superclass_list
  : public std::list<tree_classdef_superclass *>
{
public:

  tree_classdef_superclass_list () { }

  tree_classdef_superclass_list (tree_classdef_superclass *sc)
  {
    push_back (sc);
  }

  tree_classdef_superclass_list (const std::list<tree_classdef_superclass *>& a)
    : std::list<tree_classdef_superclass *> (a)
  { }

  OCTAVE_DISABLE_COPY_MOVE (tree_classdef_superclass_list)

  ~tree_classdef_superclass_list ();

  void accept (tree_walker& tw)
  {
    tw.visit_classdef_superclass_list (*this);
  }
};

class tree_base_classdef_block : public tree
{
public:

  tree_base_classdef_block (const token& block_tok, tree_classdef_attribute_list *a, const token& end_tok)
    : m_block_tok (block_tok), m_attr_list (a), m_end_tok (end_tok)
  { }

  OCTAVE_DISABLE_CONSTRUCT_COPY_MOVE (tree_base_classdef_block)

  ~tree_base_classdef_block ()
  {
    delete m_attr_list;
  }

  comment_list leading_comments () const { return m_block_tok.leading_comments (); }

  tree_classdef_attribute_list * attribute_list () { return m_attr_list; }

  void accept (tree_walker&) { }

protected:

  token m_block_tok;

  // List of attributes that apply to this class.
  tree_classdef_attribute_list *m_attr_list;

  token m_end_tok;
};

template <typename T>
class tree_classdef_block : public tree_base_classdef_block
{
public:

  tree_classdef_block (const token& block_tok, tree_classdef_attribute_list *a, T *elt_list, const token& end_tok)
    : tree_base_classdef_block (block_tok, a, end_tok), m_elt_list (elt_list)
  { }

  OCTAVE_DISABLE_CONSTRUCT_COPY_MOVE (tree_classdef_block)

  ~tree_classdef_block ()
  {
    delete m_elt_list;
  }

  filepos beg_pos () const { return m_block_tok.beg_pos (); }
  filepos end_pos () const { return m_end_tok.end_pos (); }

  T * element_list () { return m_elt_list; }

private:

  T *m_elt_list;
};

// FIXME: should this class be derived from tree?

class tree_classdef_property
{
public:

  tree_classdef_property (tree_arg_validation *av);

  OCTAVE_DISABLE_CONSTRUCT_COPY_MOVE (tree_classdef_property)

  ~tree_classdef_property ();

  comment_list leading_comments ();

  void doc_string (const std::string& s) { m_doc_string = s; }

  std::string doc_string () const { return m_doc_string; }

  bool have_doc_string () const { return ! m_doc_string.empty (); }

  tree_identifier * ident ();

  tree_expression * expression ();

  void accept (tree_walker& tw)
  {
    tw.visit_classdef_property (*this);
  }

private:

  tree_arg_validation *m_av;

  std::string m_doc_string;
};

class tree_classdef_property_list : public std::list<tree_classdef_property *>
{
public:

  tree_classdef_property_list () { }

  tree_classdef_property_list (tree_classdef_property *p) { push_back (p); }

  tree_classdef_property_list (const std::list<tree_classdef_property *>& a)
    : std::list<tree_classdef_property *> (a) { }

  OCTAVE_DISABLE_COPY_MOVE (tree_classdef_property_list)

  ~tree_classdef_property_list ();

  void accept (tree_walker& tw)
  {
    tw.visit_classdef_property_list (*this);
  }
};

class tree_classdef_properties_block : public tree_classdef_block<tree_classdef_property_list>
{
public:

  tree_classdef_properties_block (const token& block_tok, tree_classdef_attribute_list *a, tree_classdef_property_list *plist, const token& end_tok)
    : tree_classdef_block<tree_classdef_property_list> (block_tok, a, plist, end_tok)
  { }

  OCTAVE_DISABLE_CONSTRUCT_COPY_MOVE (tree_classdef_properties_block)

  ~tree_classdef_properties_block () = default;

  tree_classdef_property_list * property_list () { return element_list (); }

  void accept (tree_walker& tw)
  {
    tw.visit_classdef_properties_block (*this);
  }
};

class tree_classdef_method_list : public std::list<octave_value>
{
public:

  tree_classdef_method_list () { }

  tree_classdef_method_list (const octave_value& f) { push_back (f); }

  tree_classdef_method_list (const std::list<octave_value>& a)
    : std::list<octave_value> (a) { }

  OCTAVE_DISABLE_COPY_MOVE (tree_classdef_method_list)

  ~tree_classdef_method_list () = default;

  void accept (tree_walker& tw)
  {
    tw.visit_classdef_method_list (*this);
  }
};

class tree_classdef_methods_block : public tree_classdef_block<tree_classdef_method_list>
{
public:

  tree_classdef_methods_block (const token& block_tok, tree_classdef_attribute_list *a, tree_classdef_method_list *mlist, const token& end_tok)
    : tree_classdef_block<tree_classdef_method_list> (block_tok, a, mlist, end_tok)
  { }

  OCTAVE_DISABLE_CONSTRUCT_COPY_MOVE (tree_classdef_methods_block)

  ~tree_classdef_methods_block () = default;

  tree_classdef_method_list * method_list () { return element_list (); }

  void accept (tree_walker& tw)
  {
    tw.visit_classdef_methods_block (*this);
  }
};

class tree_classdef_event
{
public:

  tree_classdef_event (tree_identifier *i = nullptr);

  OCTAVE_DISABLE_COPY_MOVE (tree_classdef_event)

  ~tree_classdef_event ()
  {
    delete m_id;
  }

  tree_identifier * ident () { return m_id; }

  void accept (tree_walker& tw)
  {
    tw.visit_classdef_event (*this);
  }

private:

  tree_identifier *m_id;
};

class tree_classdef_event_list : public std::list<tree_classdef_event *>
{
public:

  tree_classdef_event_list () { }

  tree_classdef_event_list (tree_classdef_event *e) { push_back (e); }

  tree_classdef_event_list (const std::list<tree_classdef_event *>& a)
    : std::list<tree_classdef_event *> (a)
  { }

  OCTAVE_DISABLE_COPY_MOVE (tree_classdef_event_list)

  ~tree_classdef_event_list ();

  void accept (tree_walker& tw)
  {
    tw.visit_classdef_event_list (*this);
  }
};

class tree_classdef_events_block : public tree_classdef_block<tree_classdef_event_list>
{
public:

  tree_classdef_events_block (const token& block_tok, tree_classdef_attribute_list *a, tree_classdef_event_list *elist, const token& end_tok)
    : tree_classdef_block<tree_classdef_event_list> (block_tok, a, elist, end_tok)
  { }

  OCTAVE_DISABLE_CONSTRUCT_COPY_MOVE (tree_classdef_events_block)

  ~tree_classdef_events_block () = default;

  tree_classdef_event_list * event_list () { return element_list (); }

  void accept (tree_walker& tw)
  {
    tw.visit_classdef_events_block (*this);
  }
};

class tree_classdef_enum
{
public:

  tree_classdef_enum (tree_identifier *i, const token& open_paren, tree_expression *e, const token& close_paren);

  OCTAVE_DISABLE_CONSTRUCT_COPY_MOVE (tree_classdef_enum)

  ~tree_classdef_enum ()
  {
    delete m_id;
    delete m_expr;
  }

  tree_identifier * ident () { return m_id; }

  token open_paren () const { return m_open_paren; }

  tree_expression * expression () { return m_expr; }

  token close_paren () const { return m_close_paren; }

  void accept (tree_walker& tw)
  {
    tw.visit_classdef_enum (*this);
  }

private:

  tree_identifier *m_id;
  token m_open_paren;
  tree_expression *m_expr;
  token m_close_paren;
};

class tree_classdef_enum_list : public std::list<tree_classdef_enum *>
{
public:

  tree_classdef_enum_list () { }

  tree_classdef_enum_list (tree_classdef_enum *e) { push_back (e); }

  tree_classdef_enum_list (const std::list<tree_classdef_enum *>& a)
    : std::list<tree_classdef_enum *> (a)
  { }

  OCTAVE_DISABLE_COPY_MOVE (tree_classdef_enum_list)

  ~tree_classdef_enum_list ();

  void accept (tree_walker& tw)
  {
    tw.visit_classdef_enum_list (*this);
  }
};

class tree_classdef_enum_block : public tree_classdef_block<tree_classdef_enum_list>
{
public:

  tree_classdef_enum_block (const token& block_tok, tree_classdef_attribute_list *a, tree_classdef_enum_list *elist, const token& end_tok)
    : tree_classdef_block<tree_classdef_enum_list> (block_tok, a, elist, end_tok)
  { }

  OCTAVE_DISABLE_CONSTRUCT_COPY_MOVE (tree_classdef_enum_block)

  ~tree_classdef_enum_block () = default;

  tree_classdef_enum_list * enum_list () { return element_list (); }

  void accept (tree_walker& tw)
  {
    tw.visit_classdef_enum_block (*this);
  }
};

// FIXME: should this class be derived from tree?

class tree_classdef_body
{
public:

  typedef std::list<tree_classdef_properties_block *>::iterator property_list_iterator;
  typedef std::list<tree_classdef_properties_block *>::const_iterator property_list_const_iterator;

  typedef std::list<tree_classdef_methods_block *>::iterator method_list_iterator;
  typedef std::list<tree_classdef_methods_block *>::const_iterator method_list_const_iterator;

  typedef std::list<tree_classdef_events_block *>::iterator event_list_iterator;
  typedef std::list<tree_classdef_events_block *>::const_iterator event_list_const_iterator;

  typedef std::list<tree_classdef_enum_block *>::iterator enum_list_iterator;
  typedef std::list<tree_classdef_enum_block *>::const_iterator enum_list_const_iterator;

  tree_classdef_body ();

  tree_classdef_body (tree_classdef_properties_block *pb);

  tree_classdef_body (tree_classdef_methods_block *mb);

  tree_classdef_body (tree_classdef_events_block *evb);

  tree_classdef_body (tree_classdef_enum_block *enb);

  OCTAVE_DISABLE_COPY_MOVE (tree_classdef_body)

  ~tree_classdef_body ();

  comment_list leading_comments () const;

  tree_classdef_body * append (tree_classdef_properties_block *pb)
  {
    m_property_lst.push_back (pb);
    m_all_elements.push_back (pb);
    return this;
  }

  tree_classdef_body * append (tree_classdef_methods_block *mb)
  {
    m_method_lst.push_back (mb);
    m_all_elements.push_back (mb);
    return this;
  }

  tree_classdef_body * append (tree_classdef_events_block *evb)
  {
    m_event_lst.push_back (evb);
    m_all_elements.push_back (evb);
    return this;
  }

  tree_classdef_body * append (tree_classdef_enum_block *enb)
  {
    m_enum_lst.push_back (enb);
    m_all_elements.push_back (enb);
    return this;
  }

  std::list<tree_classdef_properties_block *> property_list ()
  {
    return m_property_lst;
  }

  std::list<tree_classdef_methods_block *> method_list ()
  {
    return m_method_lst;
  }

  std::list<tree_classdef_events_block *> event_list ()
  {
    return m_event_lst;
  }

  std::list<tree_classdef_enum_block *> enum_list ()
  {
    return m_enum_lst;
  }

  void accept (tree_walker& tw)
  {
    tw.visit_classdef_body (*this);
  }

private:

  std::list<tree_classdef_properties_block *> m_property_lst;

  std::list<tree_classdef_methods_block *> m_method_lst;

  std::list<tree_classdef_events_block *> m_event_lst;

  std::list<tree_classdef_enum_block *> m_enum_lst;

  std::list<tree_base_classdef_block *> m_all_elements;
};

// Classdef definition.

class tree_classdef : public tree_command
{
public:

  tree_classdef (const symbol_scope& scope, const token& cdef_tok, tree_classdef_attribute_list *a, tree_identifier *i, tree_classdef_superclass_list *sc, tree_classdef_body *b, const token& end_tok, const std::string& pn = "", const std::string& fn = "")
    : m_scope (scope), m_cdef_tok (cdef_tok), m_attr_list (a), m_id (i), m_supclass_list (sc), m_body (b), m_end_tok (end_tok), m_pack_name (pn), m_file_name (fn)
  {
    cache_doc_string ();
  }

  OCTAVE_DISABLE_CONSTRUCT_COPY_MOVE (tree_classdef)

  ~tree_classdef ()
  {
    delete m_attr_list;
    delete m_id;
    delete m_supclass_list;
    delete m_body;
  }

  filepos beg_pos () const { return m_cdef_tok.beg_pos (); }
  filepos end_pos () const { return m_end_tok.end_pos (); }

  symbol_scope scope () { return m_scope; }

  tree_classdef_attribute_list *
  attribute_list () { return m_attr_list; }

  tree_identifier * ident () { return m_id; }

  tree_classdef_superclass_list *
  superclass_list () { return m_supclass_list; }

  tree_classdef_body * body () { return m_body; }

  comment_list leading_comments () const { return m_cdef_tok.leading_comments (); }

  std::string package_name () const { return m_pack_name; }

  std::string file_name () const { return m_file_name; }

  octave_value make_meta_class (interpreter& interp,
                                bool is_at_folder = false);

  std::string doc_string () const { return m_doc_string; }

  void accept (tree_walker& tw)
  {
    tw.visit_classdef (*this);
  }

private:

  void cache_doc_string ()
  {
    // First non-copyright comments found above and below classdef
    // keyword are candidates for the documentation string.  Use the
    // first one that is not empty.

    comment_list comments = m_cdef_tok.leading_comments ();

    m_doc_string = comments.find_doc_string ();

    if (m_doc_string.empty ())
      {
        comments = m_body->leading_comments ();

        m_doc_string = comments.find_doc_string ();
      }
  }

  // The scope that was used when parsing the classdef object and that
  // corresponds to any identifiers that were found in attribute lists
  // (for example).  Used again when computing the meta class object.

  symbol_scope m_scope;

  token m_cdef_tok;

  tree_classdef_attribute_list *m_attr_list;

  tree_identifier *m_id;

  tree_classdef_superclass_list *m_supclass_list;

  tree_classdef_body *m_body;

  token m_end_tok;

  std::string m_doc_string;

  std::string m_pack_name;
  std::string m_file_name;
};

OCTAVE_END_NAMESPACE(octave)

#endif

