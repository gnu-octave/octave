////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2007-2023 The Octave Project Developers
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


//! @file ov-java.cc
//!
//! Provides Octave's Java interface.

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#if defined (HAVE_WINDOWS_H)
#  include <windows.h>
#endif

#include <algorithm>
#include <array>
#include <fstream>
#include <map>
#include <string>
#include <vector>

#include <clocale>

#include "Cell.h"
#include "builtin-defun-decls.h"
#include "cmd-edit.h"
#include "defaults.h"
#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "file-ops.h"
#include "file-stat.h"
#include "fpucw-wrappers.h"
#include "interpreter.h"
#include "interpreter-private.h"
#include "load-path.h"
#include "lo-sysdep.h"
#include "oct-env.h"
#include "oct-process.h"
#include "oct-shlib.h"
#include "ov-java.h"
#include "parse.h"
#include "variables.h"

#if defined (HAVE_JAVA)
#include <jni.h>
#endif

#if defined (HAVE_JAVA)

// FIXME: Should these values be configurable at run time?

#if defined (OCTAVE_USE_WINDOWS_API)
#  define LIBJVM_FILE_NAME "jvm.dll"
#elif defined (__APPLE__)
#  define LIBJVM_FILE_NAME "libjvm.dylib"
#  define JAVA_HOME_CMD "/usr/libexec/java_home"
#else
#  define LIBJVM_FILE_NAME "libjvm.so"
#endif

#define TO_JOBJECT(obj) reinterpret_cast<jobject> (obj)
#define TO_JCLASS(obj) reinterpret_cast<jclass> (obj)

#define TO_JNIENV(env) reinterpret_cast<JNIEnv *> (env)

typedef jint (JNICALL *JNI_CreateJavaVM_t) (JavaVM **pvm, JNIEnv **penv,
    void *args);

typedef jint (JNICALL *JNI_GetCreatedJavaVMs_t) (JavaVM **pvm, jsize bufLen,
    jsize *nVMs);

template <typename T>
class java_local_ref
{
public:

  java_local_ref (JNIEnv *env)
    : m_jobj (nullptr), m_detached (false), m_env (env) { }

  java_local_ref (JNIEnv *env, T obj)
    : m_jobj (obj), m_detached (false), m_env (env) { }

  ~java_local_ref (void) { release (); }

  T& operator = (T obj)
  {
    release ();

    m_jobj = obj;
    m_detached = false;

    return m_jobj;
  }

  operator bool () const { return (m_jobj != 0); }
  operator T () { return m_jobj; }

  void detach (void) { m_detached = true; }

protected:

  T m_jobj;
  bool m_detached;
  JNIEnv *m_env;

private:

  java_local_ref (void)
    : m_jobj (0), m_detached (false), m_env (0)
  { }

  void release (void)
  {
    if (m_env && m_jobj && ! m_detached)
      m_env->DeleteLocalRef (m_jobj);

    m_jobj = nullptr;
  }

};

typedef java_local_ref<jobject> jobject_ref;
typedef java_local_ref<jclass> jclass_ref;
typedef java_local_ref<jstring> jstring_ref;
typedef java_local_ref<jobjectArray> jobjectArray_ref;
typedef java_local_ref<jintArray> jintArray_ref;
typedef java_local_ref<jbyteArray> jbyteArray_ref;
typedef java_local_ref<jdoubleArray> jdoubleArray_ref;
typedef java_local_ref<jthrowable> jthrowable_ref;

static std::string
jstring_to_string (JNIEnv *jni_env, jstring s);

static std::string
jstring_to_string (JNIEnv *jni_env, jobject obj);

static octave_value
box (JNIEnv *jni_env, void *jobj, void *jcls_arg = nullptr);

static octave_value
box_more (JNIEnv *jni_env, void *jobj_arg, void *jcls_arg = nullptr);

static bool
unbox (JNIEnv *jni_env, const octave_value& val, jobject_ref& jobj,
       jclass_ref& jcls);

static bool
unbox (JNIEnv *jni_env, const octave_value_list& args,
       jobjectArray_ref& jobjs, jobjectArray_ref& jclss);

extern "C"
{
  JNIEXPORT jboolean JNICALL
  Java_org_octave_Octave_call (JNIEnv *, jclass, jstring, jobjectArray,
                               jobjectArray);

  JNIEXPORT void JNICALL
  Java_org_octave_OctaveReference_doFinalize (JNIEnv *, jclass, jint);

  JNIEXPORT void JNICALL
  Java_org_octave_Octave_doInvoke (JNIEnv *, jclass, jint, jobjectArray);

  JNIEXPORT void JNICALL
  Java_org_octave_Octave_doEvalString (JNIEnv *, jclass, jstring);

  JNIEXPORT jboolean JNICALL
  Java_org_octave_Octave_needThreadedInvokation (JNIEnv *, jclass);
}

//! The pointer to a java virtual machine either created in the current thread
//! or attached this thread to it.

static JavaVM *jvm = nullptr;

//! Whether the current thread is attached to the jvm given by #jvm.
//! This is @c false also if no jvm exists, i.e. if #jvm is @c nullptr.
//! @see #initialize_jvm()
//! @see #terminate_jvm()

static bool jvm_attached = false;

//! Need to keep hold of the shared library handle until exit.
//! @see #initialize_jvm()
//! @see #terminate_jvm()

static octave::dynamic_library jvm_lib;

static std::map<int, octave_value> listener_map;
static std::map<int, octave_value> octave_ref_map;
static int octave_java_refcount = 0;

//! The thread id of the currently executing thread or @c -1 if this is
//! unknown.
//! @see #initialize_java()

static long octave_thread_ID = -1;

bool Vjava_matrix_autoconversion = false;
bool Vjava_unsigned_autoconversion = true;
bool Vdebug_java = false;

OCTAVE_BEGIN_NAMESPACE(octave)

class JVMArgs
{
public:

  JVMArgs (void)
  {
    m_vm_args.version = JNI_VERSION_1_6;
    m_vm_args.nOptions = 0;
    m_vm_args.options = nullptr;
    m_vm_args.ignoreUnrecognized = false;
  }

  ~JVMArgs (void)
  {
    clean ();
  }

  JavaVMInitArgs * to_args ()
  {
    update ();
    return &m_vm_args;
  }

  void add (const std::string& opt)
  {
    m_java_opts.push_back (opt);
  }

  void read_java_opts (const std::string& filename)
  {
    std::ifstream js = sys::ifstream (filename.c_str ());

    if (! js.bad () && ! js.fail ())
      {
        std::string line;

        while (! js.eof () && ! js.fail ())
          {
            std::getline (js, line);

            if (line.find ('-') == 0)
              m_java_opts.push_back (line);
            else if (line.length () > 0 && Vdebug_java)
              warning ("invalid JVM option, skipping: %s", line.c_str ());
          }
      }
  }

private:

  void clean (void)
  {
    if (m_vm_args.options != nullptr)
      {
        for (int i = 0; i < m_vm_args.nOptions; i++)
          delete [] m_vm_args.options[i].optionString;

        delete [] m_vm_args.options;

        m_vm_args.options = nullptr;
        m_vm_args.nOptions = 0;
      }
  }

  void update (void)
  {
    clean ();

    if (m_java_opts.size () > 0)
      {
        int index = 0;

        m_vm_args.nOptions = m_java_opts.size ();
        m_vm_args.options = new JavaVMOption [m_vm_args.nOptions];

        for (const auto& opt : m_java_opts)
          {
            if (Vdebug_java)
              octave_stdout << opt << std::endl;
            m_vm_args.options[index++].optionString = strsave (opt.c_str ());
          }

        m_java_opts.clear ();
      }
  }

private:

  JavaVMInitArgs m_vm_args;

  std::list<std::string> m_java_opts;
};

OCTAVE_END_NAMESPACE(octave)

//! The java initialization directory is given by the environment variable
//! @c OCTAVE_JAVA_DIR if defined; otherwise it is the directory of Octave's
//! m-files defining Java functions.
//!
//! The Java initialization directory is the directory where resides:
//!
//! - @c octave.jar, defining the java classes implementing octave's java
//!   interface,
//! - @c javaclasspath.txt, defining the installation defined portion of the
//!   (static) classpath,
//! - @c java.opts, defining the configurable options of the java virtual
//!   machine.
//!
//! Note that the (static) java classpath of the java virtual machine starts
//! with @c octave.jar, and that the static java classpath ends with what
//! is read from @c javaclasspath.txt located in the initial java directory.
//! Moreover, the java virtual machine is created essentially with
//! the options given by @c java.opts.

static std::string
initial_java_dir (void)
{
  static std::string java_dir;

  if (java_dir.empty ())
    {
      java_dir = octave::sys::env::getenv ("OCTAVE_JAVA_DIR");

      if (java_dir.empty ())
        java_dir = (octave::config::fcn_file_dir ()
                    + octave::sys::file_ops::dir_sep_str () + "java");
    }

  return java_dir;
}

//! Return the classpath in the given file @c filepath as a string.
//!
//! In the classpath file, each line which is neither empty nor a comment, is
//! interpreted as a segment of a path.  Comment lines are those starting with
//! a @c # or with a @c % in the very first column.
//!
//! @param filepath The path to the file (usually @c classpath.txt) containing
//!   a portion of the classpath.
//!
//! @returns A string consisting of the lines of @c filepath which are neither
//!   comments nor empty without trailing whitespace separated by
//!   'octave::directory_path::path_sep_str()'.  The returned string also
//!   starts with that path separator.

static std::string
read_classpath_txt (const std::string& filepath)
{
  std::string classpath;

  std::ifstream fs = octave::sys::ifstream (filepath.c_str ());

  if (! fs.bad () && ! fs.fail ())
    {
      std::string line;

      while (! fs.eof () && ! fs.fail ())
        {
          std::getline (fs, line);
          if (line.length () > 0 && line[0] != '#' && line[0] != '%')
            {
              // prepend separator character
              classpath.append (octave::directory_path::path_sep_str ());

              // append content of line without whitespace
              int last = line.find_last_not_of (" \t\f\v\r\n");

              classpath.append (octave::sys::file_ops::tilde_expand (line.substr (0, last+1)));
            }
        }
    }

  return (classpath);
}

//! Return the initial classpath.
//!
//! The initial classpath starts with a pointer to @c octave.jar which is
//! located in the initial java directory given by #java_init_dir().
//!
//! @attention This is nowhere documented and also the script
//! @c javaclasspath.m drops this.  On the other hand, this is vital because
//! @c octave.jar contains the java core classes of octave's java interface.
//!
//! The rest of the classpath is read sequentially from files
//! @c javaclasspath.txt located in either:
//!
//! - the current directory,
//! - the user's home directory,
//! - the initial java directory returned by #initial_java_dir()
//!
//! @returns The initial classpath.

static std::string
initial_class_path (void)
{
  std::string java_dir = initial_java_dir ();

  std::string retval = java_dir;

  // Find octave.jar file.
  if (! retval.empty ())
    {
      std::string sep = octave::sys::file_ops::dir_sep_str ();

      std::string jar_file = java_dir + sep + "octave.jar";

      octave::sys::file_stat jar_exists (jar_file);

      if (jar_exists)
        {
          // Initialize static classpath to octave.jar.
          retval = jar_file;

          // The base classpath has been set.
          // Try to find an optional file specifying classpaths in 3 places.
          // 1) Current directory
          // 2) User's home directory
          // 3) Octave installation directory where octave.jar resides

          std::string cwd      = octave::sys::env::get_current_directory ();
          std::string home_dir = octave::sys::env::get_home_directory ();

          // The filename is "javaclasspath.txt", but historically has been
          // "classpath.txt" so both are supported.
          std::vector<std::string> cp_list = {"javaclasspath.txt",
                                              "classpath.txt"
                                             };

          for (std::string filename : cp_list)
            {
              std::string cp_file = filename;
              octave::sys::file_stat cp_exists;

              // Try to find classpath file in the current directory.

              cp_exists = octave::sys::file_stat (cp_file);
              if (cp_exists)
                {
                  // File found.  Add its contents to the static classpath.
                  std::string classpath = read_classpath_txt (cp_file);
                  retval.append (classpath);
                }

              // Try to find classpath file in the user's home directory.

              if (cwd != home_dir)
                {
                  cp_file = '~' + sep + filename;
                  cp_file = octave::sys::file_ops::tilde_expand (cp_file);
                  cp_exists = octave::sys::file_stat (cp_file);
                  if (cp_exists)
                    {
                      // File found.  Add its contents to the static classpath.
                      std::string classpath = read_classpath_txt (cp_file);
                      retval.append (classpath);
                    }
                }

              // Try to find classpath file in the Octave install directory.

              if (cwd != java_dir)
                {
                  cp_file = java_dir + sep + filename;
                  cp_exists = octave::sys::file_stat (cp_file);
                  if (cp_exists)
                    {
                      // File found.  Add its contents to the static classpath.
                      std::string classpath = read_classpath_txt (cp_file);
                      retval.append (classpath);
                    }
                }
            }
        }
      else
        error ("octave.jar does not exist: %s", jar_file.c_str ());
    }
  else
    error ("initial java dir is empty");

  return retval;
}

static std::string
get_jvm_lib_path_in_subdir (std::string java_home_path)
{
  // This assumes that whatever architectures are installed are appropriate for
  // this machine
#if defined (OCTAVE_USE_WINDOWS_API)
  const std::array<const std::string, 2> subdirs = {"bin/client", "bin/server"};
#else
  const std::array<const std::string, 8> subdirs =
  {
    "jre/lib/server", "jre/lib", "lib/client", "lib/server",
    "jre/lib/amd64/client", "jre/lib/amd64/server",
    "jre/lib/i386/client", "jre/lib/i386/server"
  };
#endif

  for (std::size_t i = 0; i < subdirs.size (); i++)
    {
      std::string candidate = java_home_path + "/" + subdirs[i]
                              + "/" LIBJVM_FILE_NAME;
      if (octave::sys::file_stat (candidate))
        return candidate;
    }
  return "";
}

#if defined (OCTAVE_USE_WINDOWS_API)

OCTAVE_BEGIN_NAMESPACE(octave)
// Declare function defined in sysdep.cc
extern LONG
get_regkey_value (HKEY h_rootkey, const std::string subkey,
                  const std::string name, octave_value& value);
OCTAVE_END_NAMESPACE(octave)

static std::string
get_jvm_lib_path_from_registry ()
{
  // In Windows, find the location of the JRE from the registry
  std::string key, jversion, value;

  // First search for JRE >= 15
  key = R"(software\javasoft\jdk)";

  jversion = octave::sys::env::getenv ("JAVA_VERSION");
  bool maybe_version_15_or_newer = true;
  octave_value regval;
  LONG retval;
  if (jversion.empty ())
    {
      value = "CurrentVersion";
      retval = octave::get_regkey_value (HKEY_LOCAL_MACHINE, key, value,
                                         regval);

      if (retval != ERROR_SUCCESS)
        {
          // Search for JRE < 15
          maybe_version_15_or_newer = false;
          key = R"(software\javasoft\jre)";
          retval = octave::get_regkey_value (HKEY_LOCAL_MACHINE, key, value,
                                             regval);

          if (retval != ERROR_SUCCESS)
            {
              // Search for JRE < 9
              key = R"(software\javasoft\java runtime environment)";
              retval = octave::get_regkey_value (HKEY_LOCAL_MACHINE, key,
                                                 value, regval);
            }
        }

      if (retval != ERROR_SUCCESS)
        error ("unable to find Java Runtime Environment: %s::%s",
               key.c_str (), value.c_str ());

      jversion = regval.xstring_value ("initialize_jvm: registry value "
                                       R"("%s" at "%s" must be a string)",
                                       value.c_str (), key.c_str ());
    }

  std::string jvm_lib_path;
  if (maybe_version_15_or_newer)
    {
      // Look for value used by JRE >= 15
      key = key + '\\' + jversion;
      value = "JavaHome";
      retval = octave::get_regkey_value (HKEY_LOCAL_MACHINE, key, value,
                                         regval);

      if (retval != ERROR_SUCCESS)
        error ("unable to find Java Runtime Environment: %s::%s",
               key.c_str (), value.c_str ());

      jvm_lib_path
        = regval.xstring_value (R"(initialize_jvm: registry value "%s" at )"
                                R"("%s" must be a string)",
                                value.c_str (), key.c_str ())
          + R"(\bin\server\jvm.dll)";

      if (! jvm_lib_path.empty ())
        return jvm_lib_path;

    }

  // Search for JRE < 15
  key = R"(software\javasoft\jre\)" + jversion;
  value = "RuntimeLib";
  retval = octave::get_regkey_value (HKEY_LOCAL_MACHINE, key, value,
                                     regval);

  if (retval != ERROR_SUCCESS)
    {
      // Search for JRE < 9
      key = R"(software\javasoft\java runtime environment\)" + jversion;
      retval = octave::get_regkey_value (HKEY_LOCAL_MACHINE, key, value,
                                         regval);
    }

  if (retval != ERROR_SUCCESS)
    error ("unable to find Java Runtime Environment: %s::%s",
           key.c_str (), value.c_str ());

  jvm_lib_path
    = regval.xstring_value (R"(initialize_jvm: registry value "%s" at )"
                            R"("%s" must be a string)",
                            value.c_str (), key.c_str ());

  if (jvm_lib_path.empty ())
    error ("unable to find Java Runtime Environment: %s::%s",
           key.c_str (), value.c_str ());

  return jvm_lib_path;
}
#endif

//! Initialize the java virtual machine (jvm) and field #jvm if necessary.
//!
//! If the jvm exists and is initialized, #jvm points to it, i.e. is not 0
//! and there is nothing to do.
//!
//! If #jvm is 0 and if at least one jvm exists, attach the current thread to
//! it by setting #jvm_attached.  Otherwise, create a #jvm with some hard-
//! coded options:
//!
//! - '-Djava.class.path=classpath', where @c classpath is given by
//!   #initial_class_path().
//! - '-Xrs'
//!
//! Further options are read from the file @c java.opts in the directory given
//! by #java_init_dir().
//!
//! Note that #initial_class_path() determines the initial classpath.  This
//! is the static classpath which cannot be changed.  Elements of the dynamic
//! classpath can be added and removed using the m-file scripts
//! @c javaaddpath.m and @c javarmpath.m.
//!
//! @see #terminate_jvm()

static void
initialize_jvm (void)
{
  // Most of the time JVM already exists and has been initialized.
  // Also it seems, as if jvm is set, the jvm is already attached.
  // This does not fit terminate_jvm.
  if (jvm)
    return;

  JNIEnv *current_env;
  const char *static_locale = setlocale (LC_ALL, nullptr);
  std::string locale;
  if (static_locale)
    locale = std::string (static_locale);

  octave::dynamic_library lib ("");
  std::string jvm_lib_path;

  // Check whether the Java VM library is already loaded or linked in.
  JNI_CreateJavaVM_t create_vm = reinterpret_cast<JNI_CreateJavaVM_t>
                                 (lib.search ("JNI_CreateJavaVM"));
  JNI_GetCreatedJavaVMs_t get_vm = reinterpret_cast<JNI_GetCreatedJavaVMs_t>
                                   (lib.search ("JNI_GetCreatedJavaVMs"));

  if (create_vm && get_vm)
    jvm_lib_path = "linked in or loaded libraries";
  else
    {
      // JAVA_HOME environment variable takes precedence
      std::string java_home_env = octave::sys::env::getenv ("JAVA_HOME");
      if (! java_home_env.empty ())
        {
          jvm_lib_path = get_jvm_lib_path_in_subdir (java_home_env);

          // If JAVA_HOME does not look like a Java directory, use it anyway
          // to fail with a useful error message indicating the directory
          if (jvm_lib_path.empty ())
            jvm_lib_path = java_home_env + "/" LIBJVM_FILE_NAME;
        }

#  if defined (__APPLE__)
      // Use standard /usr/libexec/java_home if available.
      if (jvm_lib_path.empty ())
        {
          octave::sys::file_stat libexec_java_home_exists (JAVA_HOME_CMD);
          if (libexec_java_home_exists)
            {
              // FIXME: Should this command be fully configurable at run
              // time?  Or is it OK for the options to be fixed here?

              std::string java_home_cmd = std::string (JAVA_HOME_CMD)
                                          + " --failfast --version 1.6+ 2>/dev/null";

              octave::process_execution_result rslt
                = octave::run_command_and_return_output (java_home_cmd);

              if (rslt.exit_status () == 0)
                {
                  std::string output = rslt.stdout_output ();
                  std::string found_path = output.substr (0, output.length() - 1);
                  std::string jvm_lib_found = get_jvm_lib_path_in_subdir (found_path);
                  if (!jvm_lib_found.empty ())
                    jvm_lib_path = jvm_lib_found;
                }
            }
        }
#  endif

      if (jvm_lib_path.empty ())
        {
#if defined (OCTAVE_USE_WINDOWS_API)
          jvm_lib_path = get_jvm_lib_path_from_registry ();
#else
          // Fall back to JAVA_LDPATH, determined by the build system
          jvm_lib_path = std::string (JAVA_LDPATH) + "/" LIBJVM_FILE_NAME;
#endif
        }

      lib = octave::dynamic_library (jvm_lib_path);

      if (! lib)
        error ("unable to load Java Runtime Environment from %s",
               jvm_lib_path.c_str ());

      create_vm = reinterpret_cast<JNI_CreateJavaVM_t>
                  (lib.search ("JNI_CreateJavaVM"));
      get_vm = reinterpret_cast<JNI_GetCreatedJavaVMs_t>
               (lib.search ("JNI_GetCreatedJavaVMs"));

      if (! create_vm)
        error ("unable to find JNI_CreateJavaVM in %s", jvm_lib_path.c_str ());

      if (! get_vm)
        error ("unable to find JNI_GetCreatedJavaVMs in %s",
               jvm_lib_path.c_str ());
    }

  //! The number of created jvm's.
  jsize nVMs = 0;

  if (get_vm (&jvm, 1, &nVMs) == 0 && nVMs > 0)
    {
      // At least one JVM exists, try to attach the current thread to it.

      switch (jvm->GetEnv (reinterpret_cast<void **> (&current_env),
                           JNI_VERSION_1_6))
        {
        case JNI_EDETACHED:
          // Attach the current thread
          JavaVMAttachArgs m_vm_args;
          m_vm_args.version = JNI_VERSION_1_6;
          m_vm_args.name = const_cast<char *> ("octave");
          m_vm_args.group = nullptr;
          if (jvm->AttachCurrentThread (reinterpret_cast<void **> (&current_env),
                                        &m_vm_args) < 0)
            error ("JVM internal error, unable to attach octave to existing JVM");
          break;

        case JNI_EVERSION:
          error ("JVM internal error, the required JNI version is not supported");

        case JNI_OK:
          // Don't do anything, the current thread is already attached to JVM
          break;
        }

      jvm_attached = true;
    }
  else
    {
      // No JVM exists, create one

      octave::JVMArgs m_vm_args;

      // Hard-coded options for the jvm.
      m_vm_args.add ("-Djava.class.path=" + initial_class_path ());
#if defined (HAVE_BROKEN_PTHREAD_STACKSIZE)
      m_vm_args.add ("-Djdk.lang.processReaperUseDefaultStackSize=true");
#endif
      m_vm_args.add ("-Xrs");

      // Additional options given by file java.opts.
      m_vm_args.read_java_opts (initial_java_dir () +
                                octave::sys::file_ops::dir_sep_str () +
                                "java.opts");

      if (create_vm (&jvm, &current_env, m_vm_args.to_args ()) != JNI_OK)
        error ("unable to start Java VM in %s", jvm_lib_path.c_str ());
    }

  jvm_lib = lib;

  setlocale (LC_ALL, locale.c_str ());
}

//! Terminate the current jvm, if there is any.
//!
//! Otherwise, detach the jvm if this thread is attached to it and unload it
//! if this thread created it itself.
//!
//! @see #initialize_jvm()

static void
terminate_jvm (void)
{
  // There is nothing to do if jvm is not set (= nullptr).
  if (jvm)
    {
      // FIXME: Seems that if jvm_attached is always true if jvm is not null.
      if (jvm_attached)
        jvm->DetachCurrentThread ();
      else
        jvm->DestroyJavaVM ();

      jvm = nullptr;
      jvm_attached = false;

      if (jvm_lib)
        jvm_lib.close ();

      octave_set_default_fpucw ();
    }
}

//! Converts a Java string object to std::string.
//!{
static std::string
jstring_to_string (JNIEnv *jni_env, jstring s)
{
  std::string retval;

  if (jni_env)
    {
      const char *cstr = jni_env->GetStringUTFChars (s, nullptr);
      retval = cstr;
      jni_env->ReleaseStringUTFChars (s, cstr);
    }

  return retval;
}

static std::string
jstring_to_string (JNIEnv *jni_env, jobject obj)
{
  std::string retval;

  if (jni_env && obj)
    {
      jclass_ref cls (jni_env, jni_env->FindClass ("java/lang/String"));
      if (cls)
        {
          if (jni_env->IsInstanceOf (obj, cls))
            retval = jstring_to_string (jni_env,
                                        reinterpret_cast<jstring> (obj));
        }
    }

  return retval;
}
//!}

//! Returns a reference to the jni (java native interface) environment of the
//! Java virtual machine #jvm.
//!
//! @returns A reference to jni, if #jvm is present, otherwise @c nullptr.

static inline JNIEnv *
thread_jni_env (void)
{
  JNIEnv *env = nullptr;

  if (jvm)
    jvm->GetEnv (reinterpret_cast<void **> (&env), JNI_VERSION_1_6);

  return env;
}

#endif

bool
octave_java::is_java_string (void) const
{
#if defined (HAVE_JAVA)

  JNIEnv *current_env = thread_jni_env ();

  if (current_env && m_java_object)
    {
      jclass_ref cls (current_env, current_env->FindClass ("java/lang/String"));
      return current_env->IsInstanceOf (TO_JOBJECT (m_java_object), cls);
    }

  return false;

#else

  // This shouldn't happen because construction of octave_java objects is
  // supposed to be impossible if Java is not available.

  panic_impossible ();

#endif
}

bool
octave_java::is_instance_of (const std::string& cls_name) const
{
#if defined (HAVE_JAVA)

  JNIEnv *current_env = thread_jni_env ();

  std::string cls_cpp = cls_name;
  std::replace (cls_cpp.begin (), cls_cpp.end (), '.', '/');

  if (current_env && m_java_object)
    {
      jclass_ref cls (current_env, current_env->FindClass (cls_cpp.c_str ()));
      if (current_env->ExceptionCheck ())
        current_env->ExceptionClear ();
      else
        return current_env->IsInstanceOf (TO_JOBJECT (m_java_object), cls);
    }
  return false;

#else

  octave_unused_parameter (cls_name);

  // This shouldn't happen because construction of octave_java objects is
  // supposed to be impossible if Java is not available.

  panic_impossible ();

#endif
}

#if defined (HAVE_JAVA)

static octave_value
check_exception (JNIEnv *jni_env)
{
  octave_value retval;

  jthrowable_ref ex (jni_env, jni_env->ExceptionOccurred ());

  if (ex)
    {
      if (Vdebug_java)
        jni_env->ExceptionDescribe ();

      jni_env->ExceptionClear ();

      jclass_ref jcls (jni_env, jni_env->GetObjectClass (ex));
      jmethodID mID = jni_env->GetMethodID (jcls, "toString",
                                            "()Ljava/lang/String;");
      jstring_ref js (jni_env,
                      reinterpret_cast<jstring> (jni_env->CallObjectMethod (ex,
                          mID)));
      std::string msg = jstring_to_string (jni_env, js);

      error ("[java] %s", msg.c_str ());
    }
  else
    retval = Matrix ();

  return retval;
}

static jclass
find_octave_class (JNIEnv *jni_env, const char *name)
{
  static std::string class_loader;
  static jclass uiClass = nullptr;

  jclass jcls = jni_env->FindClass (name);

  if (jcls == nullptr)
    {
      jni_env->ExceptionClear ();

      if (! uiClass)
        {
          if (class_loader.empty ())
            {
              jclass_ref syscls (jni_env,
                                 jni_env->FindClass ("java/lang/System"));
              jmethodID mID = jni_env->GetStaticMethodID
                              (syscls,
                               "getProperty",
                               "(Ljava/lang/String;)Ljava/lang/String;");
              jstring_ref js (jni_env,
                              jni_env->NewStringUTF ("octave.class.loader"));
              js = reinterpret_cast<jstring> (jni_env->CallStaticObjectMethod
                                              (syscls, mID, jstring (js)));
              class_loader = jstring_to_string (jni_env, jstring (js));
              std::replace (class_loader.begin (), class_loader.end (),
                            '.', '/');
            }

          jclass_ref uicls (jni_env,
                            jni_env->FindClass (class_loader.c_str ()));

          if (! uicls)
            {
              jni_env->ExceptionClear ();

              // Try the netbeans way
              std::replace (class_loader.begin (), class_loader.end (),
                            '/', '.');
              jclass_ref jcls2 (jni_env,
                                jni_env->FindClass ("org/openide/util/Lookup"));
              jmethodID mID = jni_env->GetStaticMethodID
                              (jcls2, "getDefault", "()Lorg/openide/util/Lookup;");
              jobject_ref lObj (jni_env,
                                jni_env->CallStaticObjectMethod (jcls2, mID));
              mID = jni_env->GetMethodID (jcls2, "lookup",
                                          "(Ljava/lang/Class;)Ljava/lang/Object;");
              jclass_ref cLoaderCls (jni_env,
                                     jni_env->FindClass ("java/lang/ClassLoader"));
              jobject_ref cLoader (jni_env,
                                   jni_env->CallObjectMethod
                                   (lObj, mID, jclass (cLoaderCls)));
              mID = jni_env->GetMethodID (cLoaderCls, "loadClass",
                                          "(Ljava/lang/String;)Ljava/lang/Class;");
              jstring_ref js (jni_env,
                              jni_env->NewStringUTF (class_loader.c_str ()));
              uicls = reinterpret_cast<jclass>
                      (jni_env->CallObjectMethod (cLoader, mID, jstring (js)));
            }

          if (uicls)
            uiClass = reinterpret_cast<jclass>
                      (jni_env->NewGlobalRef (jclass (uicls)));
        }

      if (uiClass)
        {
          jmethodID mID = jni_env->GetStaticMethodID
                          (uiClass, "findClass", "(Ljava/lang/String;)Ljava/lang/Class;");
          jstring_ref js (jni_env, jni_env->NewStringUTF (name));
          jcls = reinterpret_cast<jclass>
                 (jni_env->CallStaticObjectMethod (uiClass, mID, jstring (js)));
        }
    }

  return jcls;
}

static dim_vector
compute_array_dimensions (JNIEnv *jni_env, jobject obj)
{
  jobjectArray_ref jobj (jni_env, reinterpret_cast<jobjectArray> (obj));
  jclass_ref jcls (jni_env, jni_env->GetObjectClass (obj));
  jclass_ref ccls (jni_env, jni_env->GetObjectClass (jcls));
  jmethodID isArray_ID = jni_env->GetMethodID (ccls, "isArray", "()Z");
  jmethodID getComponentType_ID = jni_env->GetMethodID (ccls,
                                  "getComponentType",
                                  "()Ljava/lang/Class;");

  dim_vector dv (1, 1);
  int idx = 0;

  jobj.detach ();
  while (jcls && jni_env->CallBooleanMethod (jcls, isArray_ID))
    {
      int len = (jobj ? jni_env->GetArrayLength (jobj) : 0);
      if (idx >= dv.ndims ())
        dv.resize (idx+1);
      dv(idx) = len;
      jcls = reinterpret_cast<jclass>
             (jni_env->CallObjectMethod (jcls, getComponentType_ID));
      jobj = len > 0
             ? reinterpret_cast<jobjectArray> (jni_env->GetObjectArrayElement (jobj,
                 0))
             : nullptr;
      idx++;
    }

  octave_set_default_fpucw ();

  return dv;
}

static jobject
make_java_index (JNIEnv *jni_env, const octave_value_list& idx)
{
  jclass_ref ocls (jni_env, jni_env->FindClass ("[I"));
  jobjectArray retval = jni_env->NewObjectArray (idx.length (), ocls, nullptr);
  // Here retval has the same length as idx

  // Fill in entries of idx into retval
  for (int i = 0; i < idx.length (); i++)
    try
      {
        octave::idx_vector v = idx(i).index_vector ();

        jintArray_ref i_array (jni_env, jni_env->NewIntArray (v.length ()));
        jint *buf = jni_env->GetIntArrayElements (i_array, nullptr);
        // Here, buf points to the beginning of i_array

        // Copy v to buf.
        for (int k = 0; k < v.length (); k++)
          buf[k] = v(k);

        // Set retval[i] = i_array
        jni_env->ReleaseIntArrayElements (i_array, buf, 0);
        jni_env->SetObjectArrayElement (retval, i, i_array);

        check_exception (jni_env);
      }
    catch (octave::index_exception& ie)
      {
        // Rethrow to allow more info to be reported later.
        ie.set_pos_if_unset (idx.length (), i + 1);
        throw;
      }

  return retval;
}

static octave_value
get_array_elements (JNIEnv *jni_env, jobject jobj,
                    const octave_value_list& idx)
{
  octave_value retval;
  jobject_ref resObj (jni_env);
  jobject_ref java_idx (jni_env, make_java_index (jni_env, idx));

  jclass_ref helperClass (jni_env,
                          find_octave_class (jni_env,
                              "org/octave/ClassHelper"));
  jmethodID mID = jni_env
                  ->GetStaticMethodID (helperClass, "arraySubsref",
                                       "(Ljava/lang/Object;[[I)Ljava/lang/Object;");
  resObj = jni_env->CallStaticObjectMethod
           (helperClass, mID, jobj, jobject (java_idx));

  if (resObj)
    retval = box (jni_env, resObj);
  else
    retval = check_exception (jni_env);

  octave_set_default_fpucw ();

  return retval;
}

static octave_value
set_array_elements (JNIEnv *jni_env, jobject jobj,
                    const octave_value_list& idx, const octave_value& rhs)
{
  octave_value retval;

  jclass_ref rhsCls (jni_env);
  jobject_ref resObj (jni_env);
  jobject_ref rhsObj (jni_env);
  jobject_ref java_idx (jni_env, make_java_index (jni_env, idx));

  if (unbox (jni_env, rhs, rhsObj, rhsCls))
    {
      jclass_ref helperClass (jni_env,
                              find_octave_class (jni_env,
                                  "org/octave/ClassHelper"));
      jmethodID mID = jni_env->GetStaticMethodID (helperClass, "arraySubsasgn",
                      "(Ljava/lang/Object;[[ILjava/lang/Object;)" "Ljava/lang/Object;");
      resObj = jni_env->CallStaticObjectMethod
               (helperClass, mID, jobj, jobject (java_idx), jobject (rhsObj));
    }

  if (resObj)
    retval = box (jni_env, resObj);
  else
    retval = check_exception (jni_env);

  octave_set_default_fpucw ();

  return retval;
}

static string_vector
get_invoke_list (JNIEnv *jni_env, void *jobj_arg)
{
  jobject jobj = TO_JOBJECT (jobj_arg);

  std::list<std::string> name_list;

  if (jni_env)
    {
      jclass_ref cls (jni_env, jni_env->GetObjectClass (jobj));
      jclass_ref ccls (jni_env, jni_env->GetObjectClass (cls));
      jmethodID getMethods_ID = jni_env->GetMethodID
                                (ccls, "getMethods", "()[Ljava/lang/reflect/Method;");
      jmethodID getFields_ID = jni_env->GetMethodID
                               (ccls, "getFields", "()[Ljava/lang/reflect/Field;");
      jobjectArray_ref mList (jni_env,
                              reinterpret_cast<jobjectArray>
                              (jni_env->CallObjectMethod (cls, getMethods_ID)));
      jobjectArray_ref fList (jni_env,
                              reinterpret_cast<jobjectArray>
                              (jni_env->CallObjectMethod (cls, getFields_ID)));
      int mLen = jni_env->GetArrayLength (mList);
      int fLen = jni_env->GetArrayLength (fList);
      jclass_ref mCls (jni_env,
                       jni_env->FindClass ("java/lang/reflect/Method"));
      jclass_ref fCls (jni_env,
                       jni_env->FindClass ("java/lang/reflect/Field"));
      jmethodID m_getName_ID = jni_env->GetMethodID (mCls, "getName",
                               "()Ljava/lang/String;");
      jmethodID f_getName_ID = jni_env->GetMethodID (fCls, "getName",
                               "()Ljava/lang/String;");

      for (int i = 0; i < mLen; i++)
        {
          jobject_ref meth (jni_env, jni_env->GetObjectArrayElement (mList, i));
          jstring_ref methName (jni_env, reinterpret_cast<jstring>
                                (jni_env->CallObjectMethod (meth,
                                    m_getName_ID)));
          name_list.push_back (jstring_to_string (jni_env, methName));
        }

      for (int i = 0; i < fLen; i++)
        {
          jobject_ref field (jni_env,
                             jni_env->GetObjectArrayElement (fList, i));
          jstring_ref fieldName (jni_env,
                                 reinterpret_cast<jstring>
                                 (jni_env->CallObjectMethod
                                  (field, f_getName_ID)));
          name_list.push_back (jstring_to_string (jni_env, fieldName));
        }

      octave_set_default_fpucw ();
    }

  string_vector v (name_list);

  return v.sort (true);
}

static octave_value
convert_to_string (JNIEnv *jni_env, jobject m_java_object, bool force,
                   char type)
{
  octave_value retval;

  if (jni_env && m_java_object)
    {
      jclass_ref cls (jni_env, jni_env->FindClass ("java/lang/String"));

      if (jni_env->IsInstanceOf (m_java_object, cls))
        retval = octave_value (jstring_to_string (jni_env, m_java_object),
                               type);
      else if (force)
        {
          cls = jni_env->FindClass ("[Ljava/lang/String;");

          if (jni_env->IsInstanceOf (m_java_object, cls))
            {
              jobjectArray array = reinterpret_cast<jobjectArray> (m_java_object);
              int len = jni_env->GetArrayLength (array);
              Cell c (len, 1);

              for (int i = 0; i < len; i++)
                {
                  jstring_ref js (jni_env,
                                  reinterpret_cast<jstring>
                                  (jni_env->GetObjectArrayElement (array, i)));

                  if (js)
                    c(i) = octave_value (jstring_to_string (jni_env, js), type);
                  else
                    c(i) = check_exception (jni_env);
                }

              retval = octave_value (c);
            }
          else
            {
              cls = jni_env->FindClass ("java/lang/Object");
              jmethodID mID = jni_env->GetMethodID (cls, "toString",
                                                    "()Ljava/lang/String;");
              jstring_ref js (jni_env,
                              reinterpret_cast<jstring>
                              (jni_env->CallObjectMethod (m_java_object,
                                  mID)));

              if (js)
                retval = octave_value (jstring_to_string (jni_env, js), type);
              else
                retval = check_exception (jni_env);
            }
        }
      else
        error ("unable to convert Java object to string");

      octave_set_default_fpucw ();
    }

  return retval;
}

#define TO_JAVA(obj) dynamic_cast<octave_java *> ((obj).internal_rep ())

//! Return whether @c jobj shall be automatically converted to an Octave
//! numeric value.
//!
//! If @c jobj is an instance of any of the numeric wrapper classes @c Byte,
//! @c Integer, @c Long, @c Short, @c Float, or @c Double, then it will be
//! converted using the @c java.lang.Number.doubleValue() method.
//!
//! @param jobj Java object being returned to Octave
//! @return @c true if @c jobj shall be converted into a numeric value
//!         automatically, @c false otherwise
static bool
is_auto_convertible_number (JNIEnv *jni_env, jobject jobj)
{
  jclass_ref cls (jni_env);
  cls = jni_env->FindClass ("java/lang/Double");
  if (jni_env->IsInstanceOf (jobj, cls))
    return true;
  cls = jni_env->FindClass ("java/lang/Float");
  if (jni_env->IsInstanceOf (jobj, cls))
    return true;
  cls = jni_env->FindClass ("java/lang/Byte");
  if (jni_env->IsInstanceOf (jobj, cls))
    return true;
  cls = jni_env->FindClass ("java/lang/Short");
  if (jni_env->IsInstanceOf (jobj, cls))
    return true;
  cls = jni_env->FindClass ("java/lang/Integer");
  if (jni_env->IsInstanceOf (jobj, cls))
    return true;
  cls = jni_env->FindClass ("java/lang/Long");
  if (jni_env->IsInstanceOf (jobj, cls))
    return true;

  return false;
}

//! Convert the Java object pointed to by @c jobj_arg with class @c jcls_arg
//! to an Octave value.
//!
//! @param jni_env JNI environment pointer.
//! @param jobj_arg Pointer to a Java object.
//! @param jcls_arg Optional pointer to the Java class of @c jobj_arg.
//!
//! @return
//!   @arg numeric value as a @c double if @c jobj_arg is of type @c Byte,
//!     @c Short, @c Integer, @c Long, @c Float or @c Double
//!   @arg logical value if @c jobj_arg is of type @c Boolean
//!   @arg string value if @c jobj_arg is of type @c Character or @c String
//!   @arg Octave array of numeric, logical, or char type if @c jobj_arg is
//!     a Java array of primitive types
//!   @arg Octave matrix if @c jobj_arg is of type @c org.octave.Matrix and
//!     #Vjava_matrix_autoconversion is enabled
//!   @arg Octave object if @c jobj_arg is of type
//!     @c org.octave.OctaveReference
//!   @arg @c octave_java object wrapping the Java object otherwise.

static octave_value
box (JNIEnv *jni_env, void *jobj_arg, void *jcls_arg)
{
  octave_value retval;

  jobject jobj = TO_JOBJECT (jobj_arg);
  jclass jcls = TO_JCLASS (jcls_arg);

  jclass_ref cls (jni_env);

  if (! jobj)
    retval = Matrix ();

  while (retval.is_undefined ())
    {
      // Convert a scalar of any numeric class wrapping a primitive class
      // (byte, short, integer, long, float, double) to a double value.
      // Test whether java.lang.Number before testing for each type.
      cls = jni_env->FindClass ("java/lang/Number");
      if (jni_env->IsInstanceOf (jobj, cls)
          && is_auto_convertible_number (jni_env, jobj))
        {
          jmethodID m = jni_env->GetMethodID (cls, "doubleValue", "()D");
          retval = jni_env->CallDoubleMethod (jobj, m);
          break;
        }

      cls = jni_env->FindClass ("java/lang/Boolean");
      if (jni_env->IsInstanceOf (jobj, cls))
        {
          jmethodID m = jni_env->GetMethodID (cls, "booleanValue", "()Z");
          retval = (jni_env->CallBooleanMethod (jobj, m) ? true : false);
          break;
        }

      cls = jni_env->FindClass ("java/lang/String");
      if (jni_env->IsInstanceOf (jobj, cls))
        {
          retval = jstring_to_string (jni_env, jobj);
          break;
        }

      cls = jni_env->FindClass ("java/lang/Character");
      if (jni_env->IsInstanceOf (jobj, cls))
        {
          jmethodID m = jni_env->GetMethodID (cls, "charValue", "()C");
          retval = jni_env->CallCharMethod (jobj, m);
          retval = retval.convert_to_str (false, true);
          break;
        }

#define BOX_PRIMITIVE_ARRAY(JAVA_TYPE, JAVA_ID, JAVA_TYPE_CAP, OCTAVE_ID) \
      cls = jni_env->FindClass (JAVA_ID); \
      if (jni_env->IsInstanceOf (jobj, cls)) \
        { \
          const JAVA_TYPE ## Array jarr = reinterpret_cast<JAVA_TYPE ## Array> (jobj); \
          const jsize len = jni_env->GetArrayLength (jarr); \
          OCTAVE_ID ## NDArray d (dim_vector (len, 1)); \
          JAVA_TYPE *buffer = reinterpret_cast<JAVA_TYPE *> (d.fortran_vec ()); \
          jni_env->Get ## JAVA_TYPE_CAP ## ArrayRegion (jarr, 0, len, buffer); \
          retval = d; \
          break; \
        }

      BOX_PRIMITIVE_ARRAY (jdouble,  "[D", Double,  )
      BOX_PRIMITIVE_ARRAY (jboolean, "[Z", Boolean, bool)
      BOX_PRIMITIVE_ARRAY (jfloat,   "[F", Float,   Float)
      BOX_PRIMITIVE_ARRAY (jchar,    "[C", Char,    char)
      BOX_PRIMITIVE_ARRAY (jbyte,    "[B", Byte,    int8)
      BOX_PRIMITIVE_ARRAY (jshort,   "[S", Short,   int16)
      BOX_PRIMITIVE_ARRAY (jint,     "[I", Int,     int32)
      BOX_PRIMITIVE_ARRAY (jlong,    "[J", Long,    int64)

#undef BOX_PRIMITIVE_ARRAY

      if (Vjava_matrix_autoconversion)
        {
          cls = find_octave_class (jni_env, "org/octave/Matrix");

          if (jni_env->IsInstanceOf (jobj, cls))
            {
              jmethodID mID = jni_env->GetMethodID (cls, "getDims", "()[I");
              jintArray_ref iv (jni_env,
                                reinterpret_cast<jintArray>
                                (jni_env->CallObjectMethod (jobj, mID)));
              jint *iv_data = jni_env->GetIntArrayElements (jintArray (iv),
                              nullptr);
              dim_vector dims;
              dims.resize (jni_env->GetArrayLength (jintArray (iv)));

              for (int i = 0; i < dims.ndims (); i++)
                dims(i) = iv_data[i];

              jni_env->ReleaseIntArrayElements (jintArray (iv), iv_data, 0);
              mID = jni_env->GetMethodID (cls, "getClassName",
                                          "()Ljava/lang/String;");
              jstring_ref js (jni_env,
                              reinterpret_cast<jstring>
                              (jni_env->CallObjectMethod (jobj, mID)));

              std::string s = jstring_to_string (jni_env, js);

              if (s == "double")
                {
                  NDArray m (dims);
                  mID = jni_env->GetMethodID (cls, "toDouble", "()[D");
                  jdoubleArray_ref dv (jni_env,
                                       reinterpret_cast<jdoubleArray>
                                       (jni_env->CallObjectMethod (jobj,
                                           mID)));
                  jni_env->GetDoubleArrayRegion (dv, 0, m.numel (),
                                                 m.fortran_vec ());
                  retval = m;
                  break;
                }
              else if (s == "byte")
                {
                  if (Vjava_unsigned_autoconversion)
                    {
                      uint8NDArray m (dims);
                      mID = jni_env->GetMethodID (cls, "toByte", "()[B");
                      jbyteArray_ref dv (jni_env,
                                         reinterpret_cast<jbyteArray>
                                         (jni_env->CallObjectMethod (jobj,
                                             mID)));
                      jni_env->GetByteArrayRegion (dv, 0, m.numel (),
                                                   reinterpret_cast<jbyte *>
                                                   (m.fortran_vec ()));
                      retval = m;
                      break;
                    }
                  else
                    {
                      int8NDArray m (dims);
                      mID = jni_env->GetMethodID (cls, "toByte", "()[B");
                      jbyteArray_ref dv (jni_env,
                                         reinterpret_cast<jbyteArray>
                                         (jni_env->CallObjectMethod (jobj,
                                             mID)));
                      jni_env->GetByteArrayRegion (dv, 0, m.numel (),
                                                   reinterpret_cast<jbyte *>
                                                   (m.fortran_vec ()));
                      retval = m;
                      break;
                    }
                }
              else if (s == "integer")
                {
                  if (Vjava_unsigned_autoconversion)
                    {
                      uint32NDArray m (dims);
                      mID = jni_env->GetMethodID (cls, "toInt", "()[I");
                      jintArray_ref dv (jni_env,
                                        reinterpret_cast<jintArray>
                                        (jni_env->CallObjectMethod (jobj,
                                            mID)));
                      jni_env->GetIntArrayRegion (dv, 0, m.numel (),
                                                  reinterpret_cast<jint *>
                                                  (m.fortran_vec ()));
                      retval = m;
                      break;
                    }
                  else
                    {
                      int32NDArray m (dims);
                      mID = jni_env->GetMethodID (cls, "toInt", "()[I");
                      jintArray_ref dv (jni_env,
                                        reinterpret_cast<jintArray>
                                        (jni_env->CallObjectMethod (jobj,
                                            mID)));
                      jni_env->GetIntArrayRegion (dv, 0, m.numel (),
                                                  reinterpret_cast<jint *>
                                                  (m.fortran_vec ()));
                      retval = m;
                      break;
                    }
                }
            }
        }

      cls = find_octave_class (jni_env, "org/octave/OctaveReference");
      if (jni_env->IsInstanceOf (jobj, cls))
        {
          jmethodID mID = jni_env->GetMethodID (cls, "getID", "()I");
          int ID = jni_env->CallIntMethod (jobj, mID);
          auto it = octave_ref_map.find (ID);

          if (it != octave_ref_map.end ())
            retval = it->second;
          break;
        }

      // No suitable class found.  Return a generic octave_java object.
      retval = octave_value (new octave_java (jobj, jcls));
      break;
    }

  return retval;
}

static octave_value
box_more (JNIEnv *jni_env, void *jobj_arg, void *jcls_arg)
{
  jobject jobj = TO_JOBJECT (jobj_arg);
  jclass jcls = TO_JCLASS (jcls_arg);

  octave_value retval = box (jni_env, jobj, jcls);

  if (retval.isjava ())
    {
      retval = octave_value ();

      jclass_ref cls (jni_env);

      if (retval.is_undefined ())
        {
          cls = jni_env->FindClass ("[D");

          if (jni_env->IsInstanceOf (jobj, cls))
            {
              jdoubleArray jarr = reinterpret_cast<jdoubleArray> (jobj);
              int len = jni_env->GetArrayLength (jarr);

              if (len > 0)
                {
                  Matrix m (1, len);
                  jni_env->GetDoubleArrayRegion (jarr, 0, len,
                                                 m.fortran_vec ());
                  retval = m;
                }
              else
                retval = Matrix ();
            }
        }

      if (retval.is_undefined ())
        {
          cls = jni_env->FindClass ("[[D");

          if (jni_env->IsInstanceOf (jobj, cls))
            {
              jobjectArray jarr = reinterpret_cast<jobjectArray> (jobj);
              int rows = jni_env->GetArrayLength (jarr);
              int cols = 0;

              if (rows > 0)
                {
                  Matrix m;

                  for (int r = 0; r < rows; r++)
                    {
                      jdoubleArray_ref row (jni_env,
                                            reinterpret_cast<jdoubleArray>
                                            (jni_env->GetObjectArrayElement
                                             (jarr, r)));

                      if (m.isempty ())
                        {
                          cols = jni_env->GetArrayLength (row);
                          m.resize (cols, rows);
                        }
                      jni_env->GetDoubleArrayRegion
                      (row, 0, cols, m.fortran_vec () + r * cols);
                    }
                  retval = m.transpose ();
                }
              else
                retval = Matrix ();
            }
        }

      if (retval.is_undefined ())
        {
          cls = jni_env->FindClass ("[Ljava/lang/String;");

          if (jni_env->IsInstanceOf (jobj, cls))
            {
              jobjectArray jarr = reinterpret_cast<jobjectArray> (jobj);
              int len = jni_env->GetArrayLength (jarr);
              Cell m (len, 1);

              for (int i = 0; i < len; i++)
                {
                  jstring_ref js (jni_env,
                                  reinterpret_cast<jstring>
                                  (jni_env->GetObjectArrayElement (jarr, i)));
                  m(i) = jstring_to_string (jni_env, js);
                }

              retval = m;
            }
        }
    }

  if (retval.is_undefined ())
    retval = octave_value (new octave_java (jobj, jcls));

  octave_set_default_fpucw ();

  return retval;
}

static bool
unbox (JNIEnv *jni_env, const octave_value& val, jobject_ref& jobj,
       jclass_ref& jcls)
{
  bool found = true;

  if (val.isjava ())
    {
      octave_java *ovj = TO_JAVA (val);
      jobj = TO_JOBJECT (ovj->to_java ());
      jobj.detach ();
      jcls = jni_env->GetObjectClass (jobj);
    }
  else if (val.is_string ())
    {
      std::string s = val.string_value ();

      jobj = jni_env->NewStringUTF (s.c_str ());
      jcls = jni_env->GetObjectClass (jobj);
    }
  else if (val.iscellstr ())
    {
      const Array<std::string> str_arr = val.cellstr_value ();
      const octave_idx_type n = str_arr.numel ();

      jclass_ref scls (jni_env, jni_env->FindClass ("java/lang/String"));
      jobjectArray array = jni_env->NewObjectArray (n, scls, nullptr);

      for (octave_idx_type i = 0; i < n; i++)
        {
          jstring_ref jstr (jni_env, jni_env->NewStringUTF
                            (str_arr(i).c_str ()));
          jni_env->SetObjectArrayElement (array, i, jstr);
        }

      jobj = array;
      jcls = jni_env->GetObjectClass (jobj);
    }
  else if (val.numel () > 1 && val.dims ().isvector ())
    {
      // FIXME: Is there any way to avoid code duplication here without
      // using a macro?

#define UNBOX_PRIMITIVE_ARRAY(METHOD_T, OCTAVE_T, JAVA_T, JAVA_T_CAP) \
  do \
    { \
      const OCTAVE_T ## NDArray v = val.METHOD_T ## array_value (); \
      JAVA_T ## Array jarr = jni_env->New ## JAVA_T_CAP ## Array (v.numel ()); \
      const JAVA_T *jv = reinterpret_cast<const JAVA_T *> (v.data ()); \
      jni_env->Set ## JAVA_T_CAP ## ArrayRegion (jarr, 0, v.numel (), jv); \
      jobj = reinterpret_cast<jobject> (jarr); \
      jcls = jni_env->GetObjectClass (jobj); \
    } \
  while (0)

      // Note that we do NOT handle char here because they are unboxed
      // into a String[], not into a char array

      if (val.is_double_type ())
        UNBOX_PRIMITIVE_ARRAY (,, jdouble, Double);
      else if (val.islogical ())
        UNBOX_PRIMITIVE_ARRAY (bool_, bool, jboolean, Boolean);
      else if (val.isfloat ())
        UNBOX_PRIMITIVE_ARRAY (float_, Float, jfloat, Float);
      else if (val.is_int8_type ())
        UNBOX_PRIMITIVE_ARRAY (int8_, int8, jbyte, Byte);
      else if (val.is_uint8_type ())
        UNBOX_PRIMITIVE_ARRAY (uint8_, uint8, jbyte, Byte);
      else if (val.is_int16_type ())
        UNBOX_PRIMITIVE_ARRAY (int16_, int16, jshort, Short);
      else if (val.is_uint16_type ())
        UNBOX_PRIMITIVE_ARRAY (uint16_, uint16, jshort, Short);
      else if (val.is_int32_type ())
        UNBOX_PRIMITIVE_ARRAY (int32_, int32, jint, Int);
      else if (val.is_uint32_type ())
        UNBOX_PRIMITIVE_ARRAY (uint32_, uint32, jint, Int);
      else if (val.is_int64_type ())
        UNBOX_PRIMITIVE_ARRAY (int64_, int64, jlong, Long);
      else if (val.is_uint64_type ())
        UNBOX_PRIMITIVE_ARRAY (uint64_, uint64, jlong, Long);

#undef UNBOX_PRIMITIVE_ARRAY
    }
  else if (val.is_real_scalar () || val.is_bool_scalar ())
    {
      // FIXME: Is there any way to avoid code duplication here without
      // using a macro?

#define UNBOX_PRIMITIVE_SCALAR(OCTAVE_T, METHOD_T, JAVA_T, JAVA_CON)    \
  do \
    { \
      const OCTAVE_T ov = val.METHOD_T ## _value (); \
      jclass_ref dcls (jni_env, jni_env->FindClass (JAVA_T)); \
      const jfieldID fid = jni_env->GetStaticFieldID (dcls, "TYPE", "Ljava/lang/Class;"); \
      const jmethodID mid = jni_env->GetMethodID (dcls, "<init>", JAVA_CON); \
      jcls = reinterpret_cast<jclass> (jni_env->GetStaticObjectField (dcls, fid)); \
      jobj = jni_env->NewObject (dcls, mid, ov); \
     } \
   while (0)

      if (val.is_double_type ())
        UNBOX_PRIMITIVE_SCALAR (double, double, "java/lang/Double", "(D)V");
      else if (val.islogical ())
        UNBOX_PRIMITIVE_SCALAR (bool, bool, "java/lang/Boolean", "(Z)V");
      else if (val.isfloat ())
        UNBOX_PRIMITIVE_SCALAR (float, float, "java/lang/Float", "(F)V");
      else if (val.is_int8_type ())
        UNBOX_PRIMITIVE_SCALAR (int8_t, int8_scalar, "java/lang/Byte", "(B)V");
      else if (val.is_uint8_type ())
        UNBOX_PRIMITIVE_SCALAR (uint8_t, uint8_scalar, "java/lang/Byte", "(B)V");
      else if (val.is_int16_type ())
        UNBOX_PRIMITIVE_SCALAR (int16_t, int16_scalar, "java/lang/Short", "(S)V");
      else if (val.is_uint16_type ())
        UNBOX_PRIMITIVE_SCALAR (uint16_t, uint16_scalar, "java/lang/Short", "(S)V");
      else if (val.is_int32_type ())
        UNBOX_PRIMITIVE_SCALAR (int32_t, int32_scalar, "java/lang/Integer", "(I)V");
      else if (val.is_uint32_type ())
        UNBOX_PRIMITIVE_SCALAR (uint32_t, uint32_scalar, "java/lang/Integer", "(I)V");
      else if (val.is_int64_type ())
        UNBOX_PRIMITIVE_SCALAR (int64_t, int64_scalar, "java/lang/Long", "(J)V");
      else if (val.is_uint64_type ())
        UNBOX_PRIMITIVE_SCALAR (uint64_t, uint64_scalar, "java/lang/Long", "(J)V");

#undef UNBOX_PRIMITIVE_SCALAR
    }
  else if (val.isempty ())
    {
      jobj = nullptr;
      jcls = nullptr;
      //jcls = jni_env->FindClass ("java/lang/Object");
    }
  else if (! Vjava_matrix_autoconversion
           && ((val.is_real_matrix ()
                && (val.rows () == 1 || val.columns () == 1))
               || val.is_range ()))
    {
      Matrix m = val.matrix_value ();
      jdoubleArray dv = jni_env->NewDoubleArray (m.numel ());
      jni_env->SetDoubleArrayRegion (dv, 0, m.numel (), m.fortran_vec ());
      jobj = dv;
      jcls = jni_env->GetObjectClass (jobj);
    }
  else if (Vjava_matrix_autoconversion
           && (val.is_matrix_type () || val.is_range ())
           && val.isreal ())
    {
      jclass_ref mcls (jni_env, find_octave_class (jni_env,
                       "org/octave/Matrix"));
      dim_vector dims = val.dims ();
      jintArray_ref iv (jni_env, jni_env->NewIntArray (dims.ndims ()));
      jint *iv_data = jni_env->GetIntArrayElements (jintArray (iv), nullptr);

      for (int i = 0; i < dims.ndims (); i++)
        iv_data[i] = dims(i);

      jni_env->ReleaseIntArrayElements (jintArray (iv), iv_data, 0);

      if (val.is_double_type ())
        {
          NDArray m = val.array_value ();
          jdoubleArray_ref dv (jni_env, jni_env->NewDoubleArray (m.numel ()));
          jni_env->SetDoubleArrayRegion (jdoubleArray (dv), 0, m.numel (),
                                         m.fortran_vec ());
          jmethodID mID = jni_env->GetMethodID (mcls, "<init>", "([D[I)V");
          jobj = jni_env->NewObject (jclass (mcls), mID, jdoubleArray (dv),
                                     jintArray (iv));
          jcls = jni_env->GetObjectClass (jobj);
        }
      else if (val.is_int8_type ())
        {
          int8NDArray m = val.int8_array_value ();
          jbyteArray_ref bv (jni_env, jni_env->NewByteArray (m.numel ()));
          jni_env->SetByteArrayRegion (jbyteArray (bv), 0, m.numel (),
                                       reinterpret_cast<jbyte *>
                                       (m.fortran_vec ()));
          jmethodID mID = jni_env->GetMethodID (mcls, "<init>", "([B[I)V");
          jobj = jni_env->NewObject
                 (jclass (mcls), mID, jbyteArray (bv), jintArray (iv));
          jcls = jni_env->GetObjectClass (jobj);
        }
      else if (val.is_uint8_type ())
        {
          uint8NDArray m = val.uint8_array_value ();
          jbyteArray_ref bv (jni_env, jni_env->NewByteArray (m.numel ()));
          jni_env->SetByteArrayRegion (jbyteArray (bv), 0, m.numel (),
                                       reinterpret_cast<jbyte *>
                                       (m.fortran_vec ()));
          jmethodID mID = jni_env->GetMethodID (mcls, "<init>", "([B[I)V");
          jobj = jni_env->NewObject
                 (jclass (mcls), mID, jbyteArray (bv), jintArray (iv));
          jcls = jni_env->GetObjectClass (jobj);
        }
      else if (val.is_int32_type ())
        {
          int32NDArray m = val.int32_array_value ();
          jintArray_ref v (jni_env, jni_env->NewIntArray (m.numel ()));
          jni_env->SetIntArrayRegion (jintArray (v), 0, m.numel (),
                                      reinterpret_cast<jint *>
                                      (m.fortran_vec ()));
          jmethodID mID = jni_env->GetMethodID (mcls, "<init>", "([I[I)V");
          jobj = jni_env->NewObject
                 (jclass (mcls), mID, jintArray (v), jintArray (iv));
          jcls = jni_env->GetObjectClass (jobj);
        }
      else
        {
          error ("cannot convert matrix of type '%s'",
                 val.class_name ().c_str ());
        }
    }
  else
    {
      jclass rcls = find_octave_class (jni_env, "org/octave/OctaveReference");
      jmethodID mID = jni_env->GetMethodID (rcls, "<init>", "(I)V");
      int ID = octave_java_refcount++;

      jobj = jni_env->NewObject (rcls, mID, ID);
      jcls = rcls;
      octave_ref_map[ID] = val;
    }

  return found;
}

static bool
unbox (JNIEnv *jni_env, const octave_value_list& args,
       jobjectArray_ref& jobjs, jobjectArray_ref& jclss)
{
  bool found = true;

  jclass_ref ocls (jni_env, jni_env->FindClass ("java/lang/Object"));
  jclass_ref ccls (jni_env, jni_env->FindClass ("java/lang/Class"));

  if (! jobjs)
    jobjs = jni_env->NewObjectArray (args.length (), ocls, nullptr);

  if (! jclss)
    jclss = jni_env->NewObjectArray (args.length (), ccls, nullptr);

  for (int i = 0; i < args.length (); i++)
    {
      jobject_ref jobj (jni_env);
      jclass_ref jcls (jni_env);

      found = unbox (jni_env, args(i), jobj, jcls);
      if (! found)
        break;

      jni_env->SetObjectArrayElement (jobjs, i, jobj);
      jni_env->SetObjectArrayElement (jclss, i, jcls);
    }

  return found;
}

//! Returns the id of the current thread.
//!
//! @param jni_env The current environment or @c nullptr.
//!
//! @returns The id of the current thread or -1 otherwise.  The latter happens
//!   if @c jni_env is @c nullptr, for example.

static long
get_current_thread_ID (JNIEnv *jni_env)
{
  if (jni_env)
    {
      // Call Java method static Thread java.lang.Thread.currentThread().
      jclass_ref cls (jni_env, jni_env->FindClass ("java/lang/Thread"));
      jmethodID mID = jni_env->GetStaticMethodID (cls, "currentThread",
                      "()Ljava/lang/Thread;");
      jobject_ref jthread (jni_env, jni_env->CallStaticObjectMethod (cls, mID));

      if (jthread)
        {
          // Call Java method long java.lang.Thread.getId().
          jclass_ref jth_cls (jni_env, jni_env->GetObjectClass (jthread));
          mID = jni_env->GetMethodID (jth_cls, "getId", "()J");
          long result = jni_env->CallLongMethod (jthread, mID);
          return result;
        }
    }

  return -1;
}

//! Run the java method @c org.octave.Octave.checkPendingAction().
//!
//! @returns 0 in any case for good reason.

static int
java_event_hook (void)
{
  JNIEnv *current_env = thread_jni_env ();

  if (current_env)
    {
      // Invoke static void org.octave.Octave.checkPendingAction().
      jclass_ref cls (current_env, find_octave_class (current_env,
                      "org/octave/Octave"));
      jmethodID mID = current_env->GetStaticMethodID
                      (cls, "checkPendingAction", "()V");
      current_env->CallStaticVoidMethod (cls, mID);

      octave_set_default_fpucw ();
    }

  return 0;
}

//! Initialize java including the virtual machine (jvm) if necessary.
//!
//! Initializes the fields #jvm, #jvm_attached, #jvm_lib, and
//! #octave_thread_ID.  To ensure that java is initialized, this method is
//! used as part of octave functions @c javaObject, @c javaMethod,
//! @c __java_get__, @c __java_set__, and @c __java2mat__.

static void
initialize_java (void)
{
  if (! jvm)
    {
      try
        {
          initialize_jvm ();

          JNIEnv *current_env = thread_jni_env ();

          octave::command_editor::add_event_hook (java_event_hook);

          octave_thread_ID = get_current_thread_ID (current_env);
        }
      catch (const std::string msg)
        {
          error ("%s", msg.c_str ());
        }

      octave_set_default_fpucw ();
    }
}

JNIEXPORT jboolean JNICALL
Java_org_octave_Octave_call (JNIEnv *env, jclass, jstring fcnName,
                             jobjectArray argin, jobjectArray argout)
{
  std::string fname = jstring_to_string (env, fcnName);

  int nargout = env->GetArrayLength (argout);
  int nargin = env->GetArrayLength (argin);

  octave_value_list varargin, varargout;

  for (int i = 0; i < nargin; i++)
    varargin(i) = box (env, env->GetObjectArrayElement (argin, i), nullptr);

  varargout = octave::feval (fname, varargin, nargout);

  jobjectArray_ref out_objs (env, argout), out_clss (env);
  out_objs.detach ();
  return unbox (env, varargout, out_objs, out_clss);
}

JNIEXPORT void JNICALL
Java_org_octave_OctaveReference_doFinalize (JNIEnv *, jclass, jint ID)
{
  octave_ref_map.erase (ID);
}

JNIEXPORT void JNICALL
Java_org_octave_Octave_doInvoke (JNIEnv *env, jclass, jint ID,
                                 jobjectArray args)
{
  auto it = octave_ref_map.find (ID);

  if (it != octave_ref_map.end ())
    {
      octave_value val = it->second;
      int len = env->GetArrayLength (args);
      octave_value_list oct_args;

      for (int i = 0; i < len; i++)
        {
          jobject_ref jobj (env, env->GetObjectArrayElement (args, i));
          oct_args(i) = box (env, jobj, nullptr);
        }

      if (val.is_function_handle ())
        {
          octave_function *fcn = val.function_value ();
          octave::feval (fcn, oct_args);
        }
      else if (val.iscell () && val.length () > 0
               && (val.rows () == 1 || val.columns () == 1)
               && val.cell_value()(0).is_function_handle ())
        {
          Cell c = val.cell_value ();
          octave_function *fcn = c(0).function_value ();

          for (int i=1; i<c.numel (); i++)
            oct_args(len+i-1) = c(i);

          octave::feval (fcn, oct_args);
        }
      else
        error ("trying to invoke non-invocable object");
    }
}

JNIEXPORT void JNICALL
Java_org_octave_Octave_doEvalString (JNIEnv *env, jclass, jstring cmd)
{
  octave::interpreter& interp = octave::__get_interpreter__ ();

  std::string s = jstring_to_string (env, cmd);
  int pstatus;
  interp.eval_string (s, false, pstatus, 0);
}

JNIEXPORT jboolean JNICALL
Java_org_octave_Octave_needThreadedInvokation (JNIEnv *env, jclass)
{
  return (get_current_thread_ID (env) != octave_thread_ID);
}

#endif

//! Ctor.

octave_java::octave_java (void)
  : octave_base_value (), m_java_object (nullptr), m_java_class (nullptr)
{
#if ! defined (HAVE_JAVA)

  err_disabled_feature ("Java Objects", "Java");

#endif
}

octave_java::octave_java (const voidptr& jobj, void *jcls)
  : octave_base_value (), m_java_object (nullptr), m_java_class (nullptr)
{
#if defined (HAVE_JAVA)

  init (jobj, jcls);

#else

  octave_unused_parameter (jobj);
  octave_unused_parameter (jcls);

  err_disabled_feature ("Java Objects", "Java");

#endif
}

int octave_java::t_id (-1);

const std::string octave_java::t_name ("octave_java");

void
octave_java::register_type (octave::type_info& ti)
{
#if defined (HAVE_JAVA)

  t_id = ti.register_type (octave_java::t_name, "<unknown>",
                           octave_value (new octave_java ()));

#else

  octave_unused_parameter (ti);

#endif
}

dim_vector
octave_java::dims (void) const
{
#if defined (HAVE_JAVA)

  JNIEnv *current_env = thread_jni_env ();

  if (current_env && m_java_object)
    return compute_array_dimensions (current_env, TO_JOBJECT (m_java_object));
  else
    return dim_vector (1, 1);

#else

  // This shouldn't happen because construction of octave_java objects is
  // supposed to be impossible if Java is not available.

  panic_impossible ();

#endif
}

octave_value_list
octave_java::subsref (const std::string& type,
                      const std::list<octave_value_list>& idx, int nargout)
{
#if defined (HAVE_JAVA)

  octave_value_list retval;
  int skip = 1;

  JNIEnv *current_env = thread_jni_env ();

  switch (type[0])
    {
    case '.':
      if (type.length () > 1 && type[1] == '(')
        {
          octave_value_list ovl;
          count++;
          ovl(1) = octave_value (this);
          ovl(0) = (idx.front ())(0);
          auto it = idx.begin ();
          ovl.append (*++it);
          retval = octave::FjavaMethod (ovl, 1);
          skip++;
        }
      else
        {
          octave_value_list ovl;
          count++;
          ovl(0) = octave_value (this);
          ovl(1) = (idx.front ())(0);
          retval = octave::F__java_get__ (ovl, 1);
        }
      break;

    case '(':
      if (current_env)
        retval = get_array_elements
                 (current_env, TO_JOBJECT (to_java ()), idx.front ());
      break;

    default:
      error ("subsref: Java object cannot be indexed with %c", type[0]);
      break;
    }

  if (idx.size () > 1 && type.length () > 1)
    retval = retval(0).next_subsref (nargout, type, idx, skip);

  return retval;

#else

  octave_unused_parameter (type);
  octave_unused_parameter (idx);
  octave_unused_parameter (nargout);

  // This shouldn't happen because construction of octave_java objects is
  // supposed to be impossible if Java is not available.

  panic_impossible ();

#endif
}

octave_value
octave_java::subsasgn (const std::string& type,
                       const std::list<octave_value_list>& idx,
                       const octave_value& rhs)
{
#if defined (HAVE_JAVA)

  octave_value retval;

  JNIEnv *current_env = thread_jni_env ();

  switch (type[0])
    {
    case '.':
      if (type.length () == 1)
        {
          // field assignment
          octave_value_list ovl;
          count++;
          ovl(0) = octave_value (this);
          ovl(1) = (idx.front ())(0);
          ovl(2) = rhs;
          octave::F__java_set__ (ovl);

          count++;
          retval = octave_value (this);
        }
      else if (type.length () > 2 && type[1] == '(')
        {
          std::list<octave_value_list> new_idx;
          auto it = idx.begin ();
          new_idx.push_back (*it++);
          new_idx.push_back (*it++);
          octave_value_list u = subsref (type.substr (0, 2), new_idx, 1);

          std::list<octave_value_list> next_idx (idx);
          next_idx.erase (next_idx.begin ());
          next_idx.erase (next_idx.begin ());
          u(0).subsasgn (type.substr (2), next_idx, rhs);

          count++;
          retval = octave_value (this);
        }
      else if (type[1] == '.')
        {
          octave_value_list u = subsref (type.substr (0, 1), idx, 1);

          std::list<octave_value_list> next_idx (idx);
          next_idx.erase (next_idx.begin ());
          u(0).subsasgn (type.substr (1), next_idx, rhs);

          count++;
          retval = octave_value (this);
        }
      else
        error ("invalid indexing/assignment on Java object");
      break;

    case '(':
      if (current_env)
        {
          set_array_elements (current_env, TO_JOBJECT (to_java ()),
                              idx.front (), rhs);

          count++;
          retval = octave_value (this);
        }
      break;

    default:
      error ("Java object cannot be indexed with %c", type[0]);
      break;
    }

  return retval;

#else

  octave_unused_parameter (type);
  octave_unused_parameter (idx);
  octave_unused_parameter (rhs);

  // This shouldn't happen because construction of octave_java objects is
  // supposed to be impossible if Java is not available.

  panic_impossible ();

#endif
}

string_vector
octave_java::map_keys (void) const
{
#if defined (HAVE_JAVA)

  JNIEnv *current_env = thread_jni_env ();

  if (current_env)
    return get_invoke_list (current_env, to_java ());
  else
    return string_vector ();

#else

  // This shouldn't happen because construction of octave_java objects is
  // supposed to be impossible if Java is not available.

  panic_impossible ();

#endif
}

octave_value
octave_java::convert_to_str_internal (bool, bool force, char type) const
{
#if defined (HAVE_JAVA)

  JNIEnv *current_env = thread_jni_env ();

  if (current_env)
    return convert_to_string (current_env, TO_JOBJECT (to_java ()), force,
                              type);
  else
    return octave_value ("");

#else

  octave_unused_parameter (force);
  octave_unused_parameter (type);

  // This shouldn't happen because construction of octave_java objects is
  // supposed to be impossible if Java is not available.

  panic_impossible ();

#endif
}

void
octave_java::print (std::ostream& os, bool)
{
  print_raw (os);
  newline (os);
}

void
octave_java::print_raw (std::ostream& os, bool) const
{
  os << "<Java object: " << m_java_classname << '>';
}

// FIXME: Need routines to actually save/load java objects through Serialize.
//        See bug #42112.

bool
octave_java::save_ascii (std::ostream& /* os */)
{
  warning ("save: unable to save java objects, skipping");

  return true;
}

bool
octave_java::load_ascii (std::istream& /* is */)
{
  // Silently skip over java object that was not saved
  return true;
}

bool
octave_java::save_binary (std::ostream& /* os */, bool /* save_as_floats */)
{
  warning ("save: unable to save java objects, skipping");

  return true;
}

bool
octave_java::load_binary (std::istream& /* is */, bool /* swap*/,
                          octave::mach_info::float_format /* fmt */)
{
  // Silently skip over java object that was not saved
  return true;
}

bool
octave_java::save_hdf5 (octave_hdf5_id /* loc_id */, const char * /* name */,
                        bool /* save_as_floats */)
{
  warning ("save: unable to save java objects, skipping");

  return true;
}

bool
octave_java::load_hdf5 (octave_hdf5_id /* loc_id */, const char * /* name */)
{
  // Silently skip object that was not saved
  return true;
}

octave_value
octave_java::do_javaMethod (void *jni_env_arg, const std::string& name,
                            const octave_value_list& args)
{
#if defined (HAVE_JAVA)

  octave_value retval;

  JNIEnv *jni_env = TO_JNIENV (jni_env_arg);

  if (jni_env)
    {
      jobjectArray_ref arg_objs (jni_env), arg_types (jni_env);
      if (unbox (jni_env, args, arg_objs, arg_types))
        {
          jclass_ref helperClass (jni_env, find_octave_class (jni_env, "org/octave/ClassHelper"));
          jmethodID mID = jni_env->GetStaticMethodID (helperClass, "invokeMethod",
                          "(Ljava/lang/Object;Ljava/lang/String;[Ljava/lang/Object;[Ljava/lang/Class;)Ljava/lang/Object;");
          jstring_ref methName (jni_env, jni_env->NewStringUTF (name.c_str ()));
          jobjectArray_ref resObj (jni_env,
                                   reinterpret_cast<jobjectArray> (jni_env->CallStaticObjectMethod (helperClass, mID,
                                       to_java (), jstring (methName), jobjectArray (arg_objs), jobjectArray (arg_types))));
          if (resObj)
            retval = box (jni_env, resObj);
          else
            retval = check_exception (jni_env);
        }

      octave_set_default_fpucw ();
    }

  return retval;

#else

  octave_unused_parameter (jni_env_arg);
  octave_unused_parameter (name);
  octave_unused_parameter (args);

  // This shouldn't happen because construction of octave_java objects is
  // supposed to be impossible if Java is not available.

  panic_impossible ();

#endif
}

octave_value
octave_java::do_javaMethod (const std::string& name,
                            const octave_value_list& args)
{
#if defined (HAVE_JAVA)

  return do_javaMethod (thread_jni_env (), name, args);

#else

  octave_unused_parameter (name);
  octave_unused_parameter (args);

  // This shouldn't happen because construction of octave_java
  // objects is supposed to be impossible if Java is not available.

  panic_impossible ();

#endif
}

octave_value
octave_java::do_javaMethod (void *jni_env_arg,
                            const std::string& class_name,
                            const std::string& name,
                            const octave_value_list& args)
{
#if defined (HAVE_JAVA)

  octave_value retval;

  JNIEnv *jni_env = TO_JNIENV (jni_env_arg);

  if (jni_env)
    {
      jobjectArray_ref arg_objs (jni_env), arg_types (jni_env);
      if (unbox (jni_env, args, arg_objs, arg_types))
        {
          jclass_ref helperClass (jni_env,
                                  find_octave_class (jni_env,
                                      "org/octave/ClassHelper"));
          jmethodID mID = jni_env->GetStaticMethodID (helperClass,
                          "invokeStaticMethod",
                          "(Ljava/lang/String;Ljava/lang/String;[Ljava/lang/Object;[Ljava/lang/Class;)Ljava/lang/Object;");
          jstring_ref methName (jni_env,
                                jni_env->NewStringUTF (name.c_str ()));
          jstring_ref clsName (jni_env,
                               jni_env->NewStringUTF (class_name.c_str ()));
          jobject_ref resObj (jni_env,
                              jni_env->CallStaticObjectMethod (helperClass,
                                  mID,
                                  jstring (clsName),
                                  jstring (methName),
                                  jobjectArray (arg_objs),
                                  jobjectArray (arg_types)));
          if (resObj)
            retval = box (jni_env, resObj);
          else
            retval = check_exception (jni_env);
        }

      octave_set_default_fpucw ();
    }

  return retval;

#else

  octave_unused_parameter (jni_env_arg);
  octave_unused_parameter (class_name);
  octave_unused_parameter (name);
  octave_unused_parameter (args);

  // This shouldn't happen because construction of octave_java
  // objects is supposed to be impossible if Java is not available.

  panic_impossible ();

#endif
}

octave_value
octave_java::do_javaMethod (const std::string& class_name,
                            const std::string& name,
                            const octave_value_list& args)
{
#if defined (HAVE_JAVA)

  return do_javaMethod (thread_jni_env (), class_name, name, args);

#else

  octave_unused_parameter (class_name);
  octave_unused_parameter (name);
  octave_unused_parameter (args);

  // This shouldn't happen because construction of octave_java
  // objects is supposed to be impossible if Java is not available.

  panic_impossible ();

#endif
}

octave_value
octave_java::do_javaObject (void *jni_env_arg, const std::string& name,
                            const octave_value_list& args)
{
#if defined (HAVE_JAVA)

  octave_value retval;

  JNIEnv *jni_env = TO_JNIENV (jni_env_arg);

  if (jni_env)
    {
      jobjectArray_ref arg_objs (jni_env), arg_types (jni_env);

      if (unbox (jni_env, args, arg_objs, arg_types))
        {
          jclass_ref helperClass (jni_env,
                                  find_octave_class (jni_env,
                                      "org/octave/ClassHelper"));
          jmethodID mID = jni_env->GetStaticMethodID (helperClass,
                          "invokeConstructor",
                          "(Ljava/lang/String;[Ljava/lang/Object;[Ljava/lang/Class;)Ljava/lang/Object;");
          jstring_ref clsName (jni_env,
                               jni_env->NewStringUTF (name.c_str ()));
          jobject_ref resObj (jni_env,
                              jni_env->CallStaticObjectMethod (helperClass,
                                  mID,
                                  jstring (clsName),
                                  jobjectArray (arg_objs),
                                  jobjectArray (arg_types)));

          if (resObj)
            retval = octave_value (new octave_java (resObj, nullptr));
          else
            check_exception (jni_env);
        }

      octave_set_default_fpucw ();
    }

  return retval;

#else

  octave_unused_parameter (jni_env_arg);
  octave_unused_parameter (name);
  octave_unused_parameter (args);

  // This shouldn't happen because construction of octave_java
  // objects is supposed to be impossible if Java is not available.

  panic_impossible ();

#endif
}

octave_value
octave_java::do_javaObject (const std::string& name,
                            const octave_value_list& args)
{
#if defined (HAVE_JAVA)

  return do_javaObject (thread_jni_env (), name, args);

#else

  octave_unused_parameter (name);
  octave_unused_parameter (args);

  // This shouldn't happen because construction of octave_java
  // objects is supposed to be impossible if Java is not available.

  panic_impossible ();

#endif
}

octave_value
octave_java::do_java_get (void *jni_env_arg, const std::string& name)
{
#if defined (HAVE_JAVA)

  octave_value retval;

  JNIEnv *jni_env = TO_JNIENV (jni_env_arg);

  if (jni_env)
    {
      jclass_ref helperClass (jni_env,
                              find_octave_class (jni_env,
                                  "org/octave/ClassHelper"));
      jmethodID mID = jni_env->GetStaticMethodID (helperClass, "getField",
                      "(Ljava/lang/Object;Ljava/lang/String;)Ljava/lang/Object;");
      jstring_ref fName (jni_env, jni_env->NewStringUTF (name.c_str ()));
      jobject_ref resObj (jni_env,
                          jni_env->CallStaticObjectMethod (helperClass,
                              mID,
                              to_java (),
                              jstring (fName)));

      if (resObj)
        retval = box (jni_env, resObj);
      else
        retval = check_exception (jni_env);

      octave_set_default_fpucw ();
    }

  return retval;

#else

  octave_unused_parameter (jni_env_arg);
  octave_unused_parameter (name);

  // This shouldn't happen because construction of octave_java
  // objects is supposed to be impossible if Java is not available.

  panic_impossible ();

#endif
}

octave_value
octave_java::do_java_get (const std::string& name)
{
#if defined (HAVE_JAVA)

  return do_java_get (thread_jni_env (), name);

#else

  octave_unused_parameter (name);

  // This shouldn't happen because construction of octave_java
  // objects is supposed to be impossible if Java is not available.

  panic_impossible ();

#endif
}

octave_value
octave_java::do_java_get (void *jni_env_arg, const std::string& class_name,
                          const std::string& name)
{
#if defined (HAVE_JAVA)

  octave_value retval;

  JNIEnv *jni_env = TO_JNIENV (jni_env_arg);

  if (jni_env)
    {
      jclass_ref helperClass (jni_env,
                              find_octave_class (jni_env,
                                  "org/octave/ClassHelper"));
      jmethodID mID = jni_env->GetStaticMethodID (helperClass,
                      "getStaticField",
                      "(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/Object;");
      jstring_ref cName (jni_env, jni_env->NewStringUTF (class_name.c_str ()));
      jstring_ref fName (jni_env, jni_env->NewStringUTF (name.c_str ()));
      jobject_ref resObj (jni_env,
                          jni_env->CallStaticObjectMethod (helperClass, mID,
                              jstring (cName),
                              jstring (fName)));
      if (resObj)
        retval = box (jni_env, resObj);
      else
        retval = check_exception (jni_env);

      octave_set_default_fpucw ();
    }

  return retval;

#else

  octave_unused_parameter (jni_env_arg);
  octave_unused_parameter (class_name);
  octave_unused_parameter (name);

  // This shouldn't happen because construction of octave_java
  // objects is supposed to be impossible if Java is not available.

  panic_impossible ();

#endif
}

octave_value
octave_java::do_java_get (const std::string& class_name,
                          const std::string& name)
{
#if defined (HAVE_JAVA)

  return do_java_get (thread_jni_env (), class_name, name);

#else

  octave_unused_parameter (class_name);
  octave_unused_parameter (name);

  // This shouldn't happen because construction of octave_java
  // objects is supposed to be impossible if Java is not available.

  panic_impossible ();

#endif
}

octave_value
octave_java::do_java_set (void *jni_env_arg, const std::string& name,
                          const octave_value& val)
{
#if defined (HAVE_JAVA)

  octave_value retval;

  JNIEnv *jni_env = TO_JNIENV (jni_env_arg);

  if (jni_env)
    {
      jobject_ref jobj (jni_env);
      jclass_ref jcls (jni_env);

      if (unbox (jni_env, val, jobj, jcls))
        {
          jclass_ref helperClass (jni_env,
                                  find_octave_class (jni_env,
                                      "org/octave/ClassHelper"));
          jmethodID mID = jni_env->GetStaticMethodID (helperClass, "setField",
                          "(Ljava/lang/Object;Ljava/lang/String;Ljava/lang/Object;)V");
          jstring_ref fName (jni_env, jni_env->NewStringUTF (name.c_str ()));
          jni_env->CallStaticObjectMethod (helperClass, mID, to_java (),
                                           jstring (fName), jobject (jobj));
          check_exception (jni_env);
        }

      octave_set_default_fpucw ();
    }

  return retval;

#else

  octave_unused_parameter (jni_env_arg);
  octave_unused_parameter (name);
  octave_unused_parameter (val);

  // This shouldn't happen because construction of octave_java
  // objects is supposed to be impossible if Java is not available.

  panic_impossible ();

#endif
}

octave_value
octave_java::do_java_set (const std::string& name, const octave_value& val)
{
#if defined (HAVE_JAVA)

  return do_java_set (thread_jni_env (), name, val);

#else

  octave_unused_parameter (name);
  octave_unused_parameter (val);

  // This shouldn't happen because construction of octave_java
  // objects is supposed to be impossible if Java is not available.

  panic_impossible ();

#endif
}

octave_value
octave_java::do_java_set (void *jni_env_arg, const std::string& class_name,
                          const std::string& name, const octave_value& val)
{
#if defined (HAVE_JAVA)

  octave_value retval;

  JNIEnv *jni_env = TO_JNIENV (jni_env_arg);

  if (jni_env)
    {
      jobject_ref jobj (jni_env);
      jclass_ref jcls (jni_env);

      if (unbox (jni_env, val, jobj, jcls))
        {
          jclass_ref helperClass (jni_env,
                                  find_octave_class (jni_env,
                                      "org/octave/ClassHelper"));
          jmethodID mID = jni_env->GetStaticMethodID (helperClass,
                          "setStaticField",
                          "(Ljava/lang/String;Ljava/lang/String;Ljava/lang/Object;)V");
          jstring_ref cName (jni_env,
                             jni_env->NewStringUTF (class_name.c_str ()));
          jstring_ref fName (jni_env, jni_env->NewStringUTF (name.c_str ()));
          jni_env->CallStaticObjectMethod (helperClass, mID, jstring (cName),
                                           jstring (fName), jobject (jobj));
          check_exception (jni_env);
        }

      octave_set_default_fpucw ();
    }

  return retval;

#else

  octave_unused_parameter (jni_env_arg);
  octave_unused_parameter (class_name);
  octave_unused_parameter (name);
  octave_unused_parameter (val);

  // This shouldn't happen because construction of octave_java
  // objects is supposed to be impossible if Java is not available.

  panic_impossible ();

#endif
}

octave_value
octave_java::do_java_set (const std::string& class_name,
                          const std::string& name,
                          const octave_value& val)
{
#if defined (HAVE_JAVA)

  return do_java_set (thread_jni_env (), class_name, name, val);

#else

  octave_unused_parameter (class_name);
  octave_unused_parameter (name);
  octave_unused_parameter (val);

  // This shouldn't happen because construction of octave_java
  // objects is supposed to be impossible if Java is not available.

  panic_impossible ();

#endif
}

void
octave_java::init (void *jobj_arg, void *jcls_arg)
{
#if defined (HAVE_JAVA)

  jobject jobj = TO_JOBJECT (jobj_arg);
  jclass jcls = TO_JCLASS (jcls_arg);

  JNIEnv *current_env = thread_jni_env ();

  if (current_env)
    {
      if (jobj)
        m_java_object = current_env->NewGlobalRef (jobj);

      if (jcls)
        m_java_class = current_env->NewGlobalRef (jcls);
      else if (m_java_object)
        {
          jclass_ref ocls (current_env,
                           current_env->GetObjectClass(TO_JOBJECT (m_java_object)));
          m_java_class = current_env->NewGlobalRef (jclass (ocls));
        }

      if (m_java_class)
        {
          jclass_ref clsCls (current_env,
                             current_env->GetObjectClass (TO_JCLASS (m_java_class)));
          jmethodID mID = current_env->GetMethodID (clsCls,
                          "getCanonicalName",
                          "()Ljava/lang/String;");
          jobject_ref resObj (current_env,
                              current_env->CallObjectMethod (TO_JCLASS (m_java_class), mID));
          m_java_classname = jstring_to_string (current_env, resObj);
        }
    }

#else

  octave_unused_parameter (jobj_arg);
  octave_unused_parameter (jcls_arg);

  // This shouldn't happen because construction of octave_java
  // objects is supposed to be impossible if Java is not available.

  panic_impossible ();

#endif
}

void
octave_java::release (void)
{
#if defined (HAVE_JAVA)

  JNIEnv *current_env = thread_jni_env ();

  if (current_env)
    {
      if (m_java_object)
        current_env->DeleteGlobalRef (TO_JOBJECT (m_java_object));

      if (m_java_class)
        current_env->DeleteGlobalRef (TO_JCLASS (m_java_class));

      m_java_object = nullptr;
      m_java_class = nullptr;
    }

#else

  // This shouldn't happen because construction of octave_java objects is
  // supposed to be impossible if Java is not available.

  panic_impossible ();

#endif
}

OCTAVE_BEGIN_NAMESPACE(octave)

// DEFUN blocks below must be outside of HAVE_JAVA block so that documentation
// strings are always available, even when functions are not.

DEFUN (__java_init__, , ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{status} =} __java_init__ ()
Internal function used @strong{only} when debugging the Java interface.

This function will directly call @code{initialize_java} to create an instance
of a JVM.

The return variable @var{status} is 1 if the Java interface has been created,
and 0 otherwise.
@end deftypefn */)
{
#if defined (HAVE_JAVA)

  octave_value retval = 0;

  initialize_java ();

  retval = 1;

  return retval;

#else

  err_disabled_feature ("__java_init__", "Java");

#endif
}

DEFUN (__java_exit__, , ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} __java_exit__ ()
Internal function used @strong{only} when debugging Java interface.

Function will directly call terminate_jvm to destroy the current JVM instance.
@end deftypefn */)
{
#if defined (HAVE_JAVA)

  terminate_jvm ();

  return ovl ();

#else

  err_disabled_feature ("__java_exit__", "Java");

#endif
}

DEFUN (javaObject, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{jobj} =} javaObject (@var{classname})
@deftypefnx {} {@var{jobj} =} javaObject (@var{classname}, @var{arg1}, @dots{})
Create a Java object of class @var{classsname}, by calling the class
constructor with the arguments @var{arg1}, @dots{}

The first example below creates an uninitialized object, while the second
example supplies an initial argument to the constructor.

@example
@group
x = javaObject ("java.lang.StringBuffer")
x = javaObject ("java.lang.StringBuffer", "Initial string")
@end group
@end example

@seealso{javaMethod, javaArray}
@end deftypefn */)
{
#if defined (HAVE_JAVA)

  if (args.length () == 0)
    print_usage ();

  std::string classname = args(0).xstring_value ("javaObject: CLASSNAME must be a string");

  initialize_java ();

  JNIEnv *current_env = thread_jni_env ();

  octave_value_list tmp;
  for (int i=1; i<args.length (); i++)
    tmp(i-1) = args(i);

  return ovl (octave_java::do_javaObject (current_env, classname, tmp));

#else

  octave_unused_parameter (args);

  err_disabled_feature ("javaObject", "Java");

#endif
}

/*
## The tests below merely check if javaObject works at all.  Whether
## it works properly, i.e., creates the right values, is a matter of
## Java itself.  Create a Short and check if it really is a short, i.e.,
## whether it overflows.
%!testif HAVE_JAVA; usejava ("jvm")
%! assert (javaObject ("java.lang.Short", 40000).doubleValue < 0);
*/

DEFUN (javaMethod, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{ret} =} javaMethod (@var{methodname}, @var{obj})
@deftypefnx {} {@var{ret} =} javaMethod (@var{methodname}, @var{obj}, @var{arg1}, @dots{})
Invoke the method @var{methodname} on the Java object @var{obj} with the
arguments @var{arg1}, @dots{}.

For static methods, @var{obj} can be a string representing the fully
qualified name of the corresponding class.

When @var{obj} is a regular Java object, structure-like indexing can be
used as a shortcut syntax.  For instance, the two following statements are
equivalent

@example
@group
  ret = javaMethod ("method1", x, 1.0, "a string")
  ret = x.method1 (1.0, "a string")
@end group
@end example

@code{javaMethod} returns the result of the method invocation.

@seealso{methods, javaObject}
@end deftypefn */)
{
#if defined (HAVE_JAVA)

  if (args.length () < 2)
    print_usage ();

  std::string methodname = args(0).xstring_value ("javaMethod: METHODNAME must be a string");

  initialize_java ();

  JNIEnv *current_env = thread_jni_env ();

  octave_value retval;

  octave_value_list tmp;
  for (int i=2; i<args.length (); i++)
    tmp(i-2) = args(i);

  if (args(1).isjava ())
    {
      octave_java *jobj = TO_JAVA (args(1));
      retval = jobj->do_javaMethod (current_env, methodname, tmp);
    }
  else if (args(1).is_string ())
    {
      std::string cls = args(1).string_value ();
      retval = octave_java::do_javaMethod (current_env, cls, methodname, tmp);
    }
  else
    error ("javaMethod: OBJ must be a Java object or a string");

  return retval;

#else

  octave_unused_parameter (args);

  err_disabled_feature ("javaMethod", "Java");

#endif
}

/*
%!testif HAVE_JAVA; usejava ("jvm")
%! jver = javaMethod ("getProperty", "java.lang.System", "java.version");
%! jver = strsplit (jver, ".");
%! if (numel (jver) > 1)
%!   assert (isfinite (str2double (jver{1})));
%!   assert (isfinite (str2double (jver{2})));
%! else
%!   assert (isfinite (str2double (jver{1})));
%! endif
*/

DEFUN (__java_get__, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{val} =} __java_get__ (@var{obj}, @var{name})
Get the value of the field @var{name} of the Java object @var{obj}.

For static fields, @var{obj} can be a string representing the fully
qualified name of the corresponding class.

When @var{obj} is a regular Java object, structure-like indexing can be used
as a shortcut syntax.  For instance, the two following statements are
equivalent

@example
@group
  __java_get__ (x, "field1")
  x.field1
@end group
@end example

@seealso{__java_set__, javaMethod, javaObject}
@end deftypefn */)
{
#if defined (HAVE_JAVA)

  if (args.length () != 2)
    print_usage ();

  std::string name = args(1).xstring_value ("__java_get__: NAME must be a string");

  initialize_java ();

  JNIEnv *current_env = thread_jni_env ();

  octave_value retval;

  if (args(0).isjava ())
    {
      octave_java *jobj = TO_JAVA (args(0));
      retval = jobj->do_java_get (current_env, name);
    }
  else if (args(0).is_string ())
    {
      std::string cls = args(0).string_value ();
      retval = octave_java::do_java_get (current_env, cls, name);
    }
  else
    error ("__java_get__: OBJ must be a Java object or a string");

  return retval;

#else

  octave_unused_parameter (args);

  err_disabled_feature ("__java_get__", "Java");

#endif
}

DEFUN (__java_set__, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{obj} =} __java_set__ (@var{obj}, @var{name}, @var{val})
Set the value of the field @var{name} of the Java object @var{obj} to
@var{val}.

For static fields, @var{obj} can be a string representing the fully
qualified named of the corresponding Java class.

When @var{obj} is a regular Java object, structure-like indexing can be
used as a shortcut syntax.  For instance, the two following statements are
equivalent

@example
@group
  __java_set__ (x, "field1", val)
  x.field1 = val
@end group
@end example

@seealso{__java_get__, javaMethod, javaObject}
@end deftypefn */)
{
#if defined (HAVE_JAVA)

  if (args.length () != 3)
    print_usage ();

  std::string name = args(1).xstring_value ("__java_set__: NAME must be a string");

  initialize_java ();

  JNIEnv *current_env = thread_jni_env ();

  octave_value retval;

  if (args(0).isjava ())
    {
      octave_java *jobj = TO_JAVA (args(0));
      retval = jobj->do_java_set (current_env, name, args(2));
    }
  else if (args(0).is_string ())
    {
      std::string cls = args(0).string_value ();
      retval = octave_java::do_java_set (current_env, cls, name, args(2));
    }
  else
    error ("__java_set__: OBJ must be a Java object or a string");

  return retval;

#else

  octave_unused_parameter (args);

  err_disabled_feature ("__java_set__", "Java");

#endif
}

DEFUN (__java2mat__, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{mat} =} __java2mat__ (@var{javaobj})
Undocumented internal function.
@end deftypefn */)
{
#if defined (HAVE_JAVA)

  if (args.length () != 1)
    print_usage ();

  initialize_java ();

  JNIEnv *current_env = thread_jni_env ();

  octave_value_list retval;

  if (args(0).isjava ())
    {
      octave_java *jobj = TO_JAVA (args(0));
      retval = ovl (box_more (current_env, jobj->to_java (), nullptr));
    }
  else
    retval = ovl (args(0));

  return retval;

#else

  octave_unused_parameter (args);

  err_disabled_feature ("__java2mat__", "Java");

#endif
}

DEFUN (java_matrix_autoconversion, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} java_matrix_autoconversion ()
@deftypefnx {} {@var{old_val} =} java_matrix_autoconversion (@var{new_val})
@deftypefnx {} {@var{old_val} =} java_matrix_autoconversion (@var{new_val}, "local")
Query or set the internal variable that controls whether Java arrays are
automatically converted to Octave matrices.

The default value is false.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@seealso{java_unsigned_autoconversion, debug_java}
@end deftypefn */)
{
#if defined (HAVE_JAVA)

  return set_internal_variable (Vjava_matrix_autoconversion, args, nargout,
                                "java_matrix_autoconversion");

#else

  octave_unused_parameter (args);
  octave_unused_parameter (nargout);

  err_disabled_feature ("java_matrix_autoconversion", "Java");

#endif
}

DEFUN (java_unsigned_autoconversion, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} java_unsigned_autoconversion ()
@deftypefnx {} {@var{old_val} =} java_unsigned_autoconversion (@var{new_val})
@deftypefnx {} {@var{old_val} =} java_unsigned_autoconversion (@var{new_val}, "local")
Query or set the internal variable that controls how integer classes are
converted when @code{java_matrix_autoconversion} is enabled.

When enabled, Java arrays of class Byte or Integer are converted to matrices
of class uint8 or uint32 respectively.  The default value is true.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@seealso{java_matrix_autoconversion, debug_java}
@end deftypefn */)
{
#if defined (HAVE_JAVA)

  return set_internal_variable (Vjava_unsigned_autoconversion, args, nargout,
                                "java_unsigned_autoconversion");

#else

  octave_unused_parameter (args);
  octave_unused_parameter (nargout);

  err_disabled_feature ("java_unsigned_autoconversion", "Java");

#endif
}

DEFUN (debug_java, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} debug_java ()
@deftypefnx {} {@var{old_val} =} debug_java (@var{new_val})
@deftypefnx {} {@var{old_val} =} debug_java (@var{new_val}, "local")
Query or set the internal variable that determines whether extra debugging
information regarding the initialization of the JVM and any Java exceptions
is printed.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@seealso{java_matrix_autoconversion, java_unsigned_autoconversion}
@end deftypefn */)
{
#if defined (HAVE_JAVA)

  return set_internal_variable (Vdebug_java, args, nargout, "debug_java");

#else

  octave_unused_parameter (args);
  octave_unused_parameter (nargout);

  err_disabled_feature ("debug_java", "Java");

#endif
}

// Outside of #if defined (HAVE_JAVA) because it is desirable to be able
// to test for the presence of a Java object without having Java
// installed.

DEFUN (isjava, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} isjava (@var{x})
Return true if @var{x} is a Java object.
@seealso{class, typeinfo, isa, javaObject}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).isjava ());
}

/*
## Check automatic conversion of java primitive arrays into octave types.
%!testif HAVE_JAVA; usejava ("jvm")
%! assert (javaObject ("java.lang.String", "hello").getBytes (),
%!         int8 ([104 101 108 108 111]'));

## Check automatic conversion of octave types into java primitive arrays.
## Note that uint8 is casted to int8.
%!testif HAVE_JAVA; usejava ("jvm")
%! assert (javaMethod ("binarySearch", "java.util.Arrays", [90 100 255], 255), 2);
%! assert (javaMethod ("binarySearch", "java.util.Arrays", uint8 ([90 100 255]), uint8 (255)) < 0);
%! assert (javaMethod ("binarySearch", "java.util.Arrays", uint8 ([90 100 128]), uint8 (128)) < 0);
%! assert (javaMethod ("binarySearch", "java.util.Arrays", uint8 ([90 100 127]), uint8 (127)), 2);
%! assert (javaMethod ("binarySearch", "java.util.Arrays", uint16 ([90 100 128]), uint16 (128)), 2);

## Check we can create objects that wrap java literals
%!testif HAVE_JAVA; usejava ("jvm") <*38821>
%! assert (class (javaObject ("java.lang.Byte",     uint8 (1))),
%!         "java.lang.Byte");
%! assert (class (javaObject ("java.lang.Byte",      int8 (1))),
%!         "java.lang.Byte");
%! assert (class (javaObject ("java.lang.Short",   uint16 (1))),
%!         "java.lang.Short");
%! assert (class (javaObject ("java.lang.Short",    int16 (1))),
%!         "java.lang.Short");
%! assert (class (javaObject ("java.lang.Integer", uint32 (1))),
%!         "java.lang.Integer");
%! assert (class (javaObject ("java.lang.Integer",  int32 (1))),
%!         "java.lang.Integer");
%! assert (class (javaObject ("java.lang.Long",    uint64 (1))),
%!         "java.lang.Long");
%! assert (class (javaObject ("java.lang.Long",     int64 (1))),
%!         "java.lang.Long");

## More checks of java numeric and boolean class instances
%!testif HAVE_JAVA; usejava ("jvm")
%! n = javaObject ("java.lang.Double", 1.35);
%! assert (n.compareTo (1.0), 1);
%! assert (n.compareTo (1.35), 0);
%! assert (n.compareTo (10), -1);
%! assert (n.isInfinite (), false);

%!testif HAVE_JAVA; usejava ("jvm") <51804>
%! n = javaObject ("java.lang.Float", 1.35);
%! assert (n.compareTo (1.0), 1);
%! assert (n.compareTo (1.35), 0);
%! assert (n.compareTo (10), -1);
%! assert (n.doubleValue (), 1.35, 1e7);

%!testif HAVE_JAVA; usejava ("jvm")
%! n = javaObject ("java.lang.Long", (int64 (1)));
%! assert (n.compareTo (int64 (0)), 1);
%! assert (n.compareTo (int64 (1)), 0);
%! assert (n.compareTo (int64 (2)), -1);
%! assert (n.toString (), "1");

%!testif HAVE_JAVA; usejava ("jvm") <51804>
%! n = javaObject ("java.lang.Integer", 1.35);
%! assert (n.compareTo (0), 1);
%! assert (n.compareTo (1), 0);
%! assert (n.compareTo (2), -1);

%!testif HAVE_JAVA; usejava ("jvm") <51804>
%! n = javaObject ("java.lang.Short", 1.35);
%! assert (n.compareTo (0), 1);
%! assert (n.compareTo (1), 0);
%! assert (n.compareTo (2), -1);

%!testif HAVE_JAVA; usejava ("jvm")
%! n = javaObject ("java.lang.Byte", int8 (17));
%! assert (n.compareTo (int8 (20)), -3);
%! assert (n.compareTo (int8 (10)), 7);
%! assert (n.compareTo (int8 (17)), 0);

%!testif HAVE_JAVA; usejava ("jvm")
%! b = javaObject ("java.lang.Boolean", true);
%! assert (b.compareTo (true), 0);
%! assert (b.compareTo (false), 1);
%! b = javaObject ("java.lang.Boolean", false);
%! assert (b.compareTo (true), -1);
%! assert (b.compareTo (false), 0);

## Test for automatic conversion of specific numeric classes
%!testif HAVE_JAVA; usejava ("jvm") <*48013>
%! assert (javaMethod ("valueOf", "java.lang.Byte",     int8 (1)), 1)
%! assert (javaMethod ("valueOf", "java.lang.Short",   int16 (1)), 1)
%! assert (javaMethod ("valueOf", "java.lang.Integer", int32 (1)), 1)
%! assert (javaMethod ("valueOf", "java.lang.Long",    int64 (1)), 1)
%! assert (javaMethod ("valueOf", "java.lang.Float",  single (1)), 1)
%! assert (javaMethod ("valueOf", "java.lang.Double", double (1)), 1)
%! assert (class (javaMethod ("valueOf", "java.math.BigDecimal", double (1))),
%!         "java.math.BigDecimal")
%! assert (class (javaMethod ("valueOf", "java.math.BigInteger",  int64 (1))),
%!         "java.math.BigInteger")

## Automatic conversion from string cell array into String[]
%!testif HAVE_JAVA; usejava ("jvm") <*45290>
%! assert (javaMethod ("binarySearch", "java.util.Arrays", {"aaa", "bbb", "ccc", "zzz"}, "aaa"), 0);
%! assert (javaMethod ("binarySearch", "java.util.Arrays", {"aaa", "bbb", "ccc", "zzz"}, "zzz"), 3);
%! assert (javaMethod ("binarySearch", "java.util.Arrays", {"aaa", "bbb", "ccc", "zzz"}, "hhh") < 0);

## Test that Octave index syntax allows Java object method calls with args
%!testif HAVE_JAVA; usejava ("jvm") <*51152>
%! s = javaObject ("java.lang.String", "Octave");
%! assert (s.length (), 6)
%! assert (s.charAt (0), "O")
%! assert (s.charAt (5), "e")
%! assert (s.matches ("^Octave$"))
%! assert (s.startsWith ("Oct"))
%! ## same tests with Java object as part of another indexing expression
%! a(1).s = s;
%! assert (! a(1).s.isEmpty ())
%! assert (a(1).s.length (), 6)
%! assert (a(1).s.charAt (0), "O")
%! assert (a(1).s.charAt (5), "e")
%! assert (a(1).s.matches ("^Octave$"))
%! assert (a(1).s.startsWith ("Oct"))

## Check for basic usability of the java awt library
## Skip the test on OS X where we currently have Java 9 and attempting
## to use awt causes Octave to exit with a message about Java not being
## installed (it is) instead of returning false.
%!testif HAVE_JAVA; ! ismac () && usejava ("jvm") && usejava ("awt") && have_window_system ()
%! frame = javaObject ("java.awt.Frame");
%! frame.setResizable (true);
%! assert (frame.isResizable ());
*/

OCTAVE_END_NAMESPACE(octave)
