/* Copyright (C) 2007 Michael Goffioul
**
** This program is free software; you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation; either version 2 of the License, or
** (at your option) any later version.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
**
** You should have received a copy of the GNU General Public License
** along with this program; If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef __JAVA__H__
#define __JAVA__H__

#include "oct.h"
#include "config.h"
#ifndef OCTAVE_EXPORT
#include "oct-dlldefs.h"
#endif // OCTAVE_EXPORT
#include <jni.h>

#ifdef JAVAPKG_BUILD
# define JAVAPKG_API OCTAVE_EXPORT
#else
# define JAVAPKG_API OCTAVE_IMPORT
#endif

template <class T>
class java_local_ref
{
public:
  java_local_ref (JNIEnv *_env)
    : jobj (0), detached (false), env (_env)
    { }

  java_local_ref (JNIEnv *_env, T obj)
    : jobj (obj), detached (false), env (_env)
    { }

  ~java_local_ref (void)
    {
      release ();
    }

  T& operator= (T obj)
    {
      release ();
      jobj = obj;
      detached = false;
      return jobj;
    }
  operator bool () const { return (jobj != 0); }
  operator T () { return jobj; }

  void detach () { detached = true; }

private:
  void release (void)
    {
      if (env && jobj && ! detached)
        env->DeleteLocalRef (jobj);
      jobj = 0;
    }

  java_local_ref (void)
    : jobj (0), detached (false), env (0)
    { }
	

protected:
  T jobj;
  bool detached;
  JNIEnv *env;
};

typedef java_local_ref<jobject> jobject_ref;
typedef java_local_ref<jclass> jclass_ref;
typedef java_local_ref<jstring> jstring_ref;
typedef java_local_ref<jobjectArray> jobjectArray_ref;
typedef java_local_ref<jintArray> jintArray_ref;
typedef java_local_ref<jbyteArray> jbyteArray_ref;
typedef java_local_ref<jdoubleArray> jdoubleArray_ref;
typedef java_local_ref<jthrowable> jthrowable_ref;

extern JAVAPKG_API std::string jstring_to_string (JNIEnv* jni_env, jstring s);
extern JAVAPKG_API std::string jstring_to_string (JNIEnv* jni_env, jobject obj);
extern JAVAPKG_API octave_value box (JNIEnv* jni_env, jobject jobj, jclass jcls = 0);
extern JAVAPKG_API octave_value box_more (JNIEnv* jni_env, jobject jobj, jclass jcls = 0);
extern JAVAPKG_API int unbox (JNIEnv* jni_env, const octave_value& val, jobject_ref& jobj, jclass_ref& jcls);
extern JAVAPKG_API int unbox (JNIEnv* jni_env, const octave_value_list& args, jobjectArray_ref& jobjs, jobjectArray_ref& jclss);

extern JAVAPKG_API bool Vjava_convert_matrix;
extern JAVAPKG_API bool Vjava_unsigned_conversion;
extern JAVAPKG_API bool Vjava_debug;

class JAVAPKG_API octave_java : public octave_base_value
{
public:
  octave_java (void)
    : java_object (0), java_class (0)
    { }

  octave_java (const octave_java& jobj)
    : java_object (0), java_class (0)
    {
      init (jobj.java_object, jobj.java_class);
    }

  octave_java (jobject obj, jclass cls = 0)
    : java_object (0)
    {
      init (obj, cls);
    }

  ~octave_java (void)
    {
      release ();
    }

  jobject to_java () const { return java_object; }
  jclass to_class () const { return java_class; }
  std::string java_class_name () const { return java_type; }
	
  octave_base_value* clone(void) const { return new octave_java(*this); }
  octave_base_value* empty_clone(void) const { return new octave_java(); }

  bool is_defined(void) const { return true; }

  bool is_map (void) const { return true; }

  string_vector map_keys(void) const;

  dim_vector dims(void) const;

  void print(std::ostream& os, bool pr_as_read_syntax = false) const
    {
      os << "<Java object: " << java_type << ">";
      newline(os);
    }

  void print_raw(std::ostream& os, bool pr_as_read_syntax = false) const
    {
      print(os, pr_as_read_syntax);
    }

  octave_value_list subsref (const std::string& type, const std::list<octave_value_list>& idx, int nargout);
	
  octave_value subsref (const std::string& type,
			const std::list<octave_value_list>& idx)
    {
      octave_value_list retval = subsref (type, idx, 1);
      return (retval.length () > 0 ? retval(0) : octave_value ());
    }

  octave_value subsasgn (const std::string& type, const std::list<octave_value_list>& idx, const octave_value& rhs);

  octave_value convert_to_str_internal (bool pad, bool force, char type) const;

  bool is_string (void) const
    {
      JNIEnv *current_env = thread_jni_env ();

      if (current_env && java_object)
        {
          jclass_ref cls (current_env, current_env->FindClass ("java/lang/String"));
          return current_env->IsInstanceOf (java_object, cls);
        }
      return false;
    }

  static JNIEnv* thread_jni_env (void);

  octave_value do_java_invoke (JNIEnv* jni_env, const std::string& name,
      const octave_value_list& args);
  
  octave_value do_java_invoke (const std::string& name, const octave_value_list& args)
    { return do_java_invoke(thread_jni_env (), name, args); }

  static octave_value do_java_invoke (JNIEnv* jni_env, const std::string& class_name,
    const std::string& name, const octave_value_list& args);
  
  static octave_value do_java_invoke (const std::string& class_name, 
      const std::string& name, const octave_value_list& args)
    { return do_java_invoke(thread_jni_env (), class_name, name, args); }

  static octave_value do_java_create (JNIEnv* jni_env, const std::string& name,
      const octave_value_list& args);
  
  static octave_value do_java_create (const std::string& name, const octave_value_list& args)
    { return do_java_create (thread_jni_env (), name, args); }

  octave_value do_java_get (JNIEnv* jni_env, const std::string& name);
  
  octave_value do_java_get (const std::string& name)
    { return do_java_get (thread_jni_env (), name); }

  static octave_value do_java_get (JNIEnv* jni_env, const std::string& class_name,
      const std::string& name);
  
  static octave_value do_java_get (const std::string& class_name, const std::string& name)
    { return do_java_get (thread_jni_env (), class_name, name); }

  octave_value do_java_set (JNIEnv* jni_env, const std::string& name, const octave_value& val);
  
  octave_value do_java_set (const std::string& name, const octave_value& val)
    { return do_java_set (thread_jni_env (), name, val); }

  static octave_value do_java_set (JNIEnv* jni_env, const std::string& class_name,
      const std::string& name, const octave_value& val);
  
  static octave_value do_java_set (const std::string& class_name, const std::string& name,
      const octave_value& val)
    { return do_java_set (thread_jni_env (), class_name, name, val); }

private:
  void init (jobject jobj, jclass jcls)
    {
      JNIEnv *current_env = thread_jni_env ();

      if (current_env)
        {
          if (jobj)
            java_object = current_env->NewGlobalRef (jobj);
          if (jcls)
            java_class = reinterpret_cast<jclass> (current_env->NewGlobalRef (jcls));
          else if (java_object)
            {
              jclass_ref ocls (current_env, current_env->GetObjectClass (java_object));
              java_class = reinterpret_cast<jclass> (current_env->NewGlobalRef (jclass (ocls)));
            }

          if (java_class)
            {
              jclass_ref clsCls (current_env, current_env->GetObjectClass (java_class));
              jmethodID mID = current_env->GetMethodID (clsCls, "getCanonicalName", "()Ljava/lang/String;");
              jobject_ref resObj (current_env, current_env->CallObjectMethod (java_class, mID));
              java_type = jstring_to_string (current_env, resObj);
            }
        }
    }

  void release ()
    {
      JNIEnv *current_env = thread_jni_env ();

      if (current_env)
        {
          if (java_object)
            current_env->DeleteGlobalRef (java_object);
          if (java_class)
            current_env->DeleteGlobalRef (java_class);
          java_object = 0;
          java_class = 0;
        }
    }

private:
  DECLARE_OCTAVE_ALLOCATOR
	
  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA

  jobject java_object;
  jclass java_class;
  std::string java_type;
};

#endif /* __JAVA__H__ */
