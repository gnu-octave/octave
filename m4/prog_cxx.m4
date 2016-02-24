# Copyright (C) 2001-2016 Free Software Foundation, Inc.
#
#
# This is pretty much copy and paste of the unreleased autoconf macros
# (where it is named AC_PROG_CC_C11).  Once we are dependent on
# autoconf version with that macro, we can simply change to use that
# one instead.  Copied from autoconf commit 04be2b7a (2016/02/06)
#
#
# This file is part of Autoconf.  This program is free
# software; you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the
# Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, see <http://www.gnu.org/licenses/>.

# OCTAVE_PROG_CXX_CXX11 ([ACTION-IF-AVAILABLE], [ACTION-IF-UNAVAILABLE])
# -------------------------------------------------------------------
# If the C++ compiler is not in ISO CXX11 mode by default, try to add
# an option to output variable CXX to make it so.  This macro tries
# various options that select ISO C++11 on some system or another.  It
# considers the compiler to be in ISO C++11 mode if it handles all the
# tests from the C++98 checks, plus the following: Language features
# (auto, constexpr, decltype, default/deleted constructors, delegate
# constructors, final, initialiser lists, lambda functions, nullptr,
# override, range-based for loops, template brackets without spaces,
# unicode literals) and library features (array, memory (shared_ptr,
# weak_ptr), regex and tuple types).
AC_DEFUN([OCTAVE_PROG_CXX_CXX11],
[OCTAVE_CXX_STD_TRY([cxx11],
[OCTAVE_CXX_CXX11_TEST_HEADER
OCTAVE_CXX_CXX98_TEST_HEADER],
[OCTAVE_CXX_CXX11_TEST_BODY
OCTAVE_CXX_CXX98_TEST_BODY],
dnl Try
dnl GCC		-std=gnu++11 (unused restrictive mode: -std=c++11) [and 0x variants]
dnl IBM XL C	-qlanglvl=extended0x
dnl		(pre-V12.1; unused restrictive mode: -qlanglvl=stdcxx11)
dnl HP aC++	-AA
dnl Intel ICC	-std=c++11 -std=c++0x
dnl Solaris	N/A (no support)
dnl Tru64	N/A (no support)
dnl with extended modes being tried first.
[[-std=gnu++11 -std=c++11 -std=gnu++0x -std=c++0x -qlanglvl=extended0x -AA]], [$1], [$2])[]dnl
])# OCTAVE_PROG_CXX_CXX11


# OCTAVE_CXX_STD_TRY(STANDARD, TEST-PROLOGUE, TEST-BODY, OPTION-LIST,
#		  ACTION-IF-AVAILABLE, ACTION-IF-UNAVAILABLE)
# ----------------------------------------------------------------
# Check whether the C++ compiler accepts features of STANDARD (e.g
# `cxx98', `cxx11') by trying to compile a program of TEST-PROLOGUE
# and TEST-BODY.  If this fails, try again with each compiler option
# in the space-separated OPTION-LIST; if one helps, append it to CXX.
# If eventually successful, run ACTION-IF-AVAILABLE, else
# ACTION-IF-UNAVAILABLE.
AC_DEFUN([OCTAVE_CXX_STD_TRY],
[AC_MSG_CHECKING([for $CXX option to enable ]m4_translit(m4_translit($1, [x], [+]), [a-z], [A-Z])[ features])
AC_LANG_PUSH(C++)dnl
AC_CACHE_VAL(ac_cv_prog_cxx_$1,
[ac_cv_prog_cxx_$1=no
ac_save_CXX=$CXX
AC_LANG_CONFTEST([AC_LANG_PROGRAM([$2], [$3])])
for ac_arg in '' $4
do
  CXX="$ac_save_CXX $ac_arg"
  _AC_COMPILE_IFELSE([], [ac_cv_prog_cxx_$1=$ac_arg])
  test "x$ac_cv_prog_cxx_$1" != "xno" && break
done
rm -f conftest.$ac_ext
CXX=$ac_save_CXX
])# AC_CACHE_VAL
ac_prog_cxx_stdcxx_options=
case "x$ac_cv_prog_cxx_$1" in
  x)
    AC_MSG_RESULT([none needed]) ;;
  xno)
    AC_MSG_RESULT([unsupported]) ;;
  *)
    ac_prog_cxx_stdcxx_options=" $ac_cv_prog_cxx_$1"
    CXX=$CXX$ac_prog_cxx_stdcxx_options
    AC_MSG_RESULT([$ac_cv_prog_cxx_$1]) ;;
esac
AC_LANG_POP(C++)dnl
AS_IF([test "x$ac_cv_prog_cxx_$1" != xno], [$5], [$6])
])# OCTAVE_CXX_STD_TRY

# OCTAVE_CXX_CXX98_TEST_HEADER
# -------------------------
# A C++ header suitable for testing for CXX98.
AC_DEFUN([OCTAVE_CXX_CXX98_TEST_HEADER],
[[
#include <algorithm>
#include <cstdlib>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <list>
#include <map>
#include <set>
#include <sstream>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

namespace test {
  typedef std::vector<std::string> string_vec;
  typedef std::pair<int,bool> map_value;
  typedef std::map<std::string,map_value> map_type;
  typedef std::set<int> set_type;

  template<typename T>
  class printer {
  public:
    printer(std::ostringstream& os): os(os) {}
    void operator() (T elem) { os << elem << std::endl; }
  private:
    std::ostringstream& os;
  };
}
]])# OCTAVE_CXX_CXX98_TEST_HEADER

# OCTAVE_CXX_CXX98_TEST_BODY
# -----------------------
# A C++ body suitable for testing for CXX98, assuming the corresponding header.
AC_DEFUN([OCTAVE_CXX_CXX98_TEST_BODY],
[[

try {
  // Basic string.
  std::string teststr("ASCII text");
  teststr += " string";

  // Simple vector.
  test::string_vec testvec;
  testvec.push_back(teststr);
  testvec.push_back("foo");
  testvec.push_back("bar");
  if (testvec.size() != 3) {
    throw std::runtime_error("vector size is not 1");
  }

  // Dump vector into stringstream and obtain string.
  std::ostringstream os;
  for (test::string_vec::const_iterator i = testvec.begin();
       i != testvec.end(); ++i) {
    if (i + 1 != testvec.end()) {
      os << teststr << '\n';
    }
  }
  // Check algorithms work.
  std::for_each(testvec.begin(), testvec.end(), test::printer<std::string>(os));
  std::string os_out = os.str();

  // Test pair and map.
  test::map_type testmap;
  testmap.insert(std::make_pair(std::string("key"),
                                std::make_pair(53,false)));

  // Test set.
  int values[] = {9, 7, 13, 15, 4, 18, 12, 10, 5, 3, 14, 19, 17, 8, 6, 20, 16, 2, 11, 1};
  test::set_type testset(values, values + sizeof(values)/sizeof(values[0]));
  std::list<int> testlist(testset.begin(), testset.end());
  std::copy(testset.begin(), testset.end(), std::back_inserter(testlist));
} catch (const std::exception& e) {
  std::cerr << "Caught exception: " << e.what() << std::endl;

  // Test fstream
  std::ofstream of("test.txt");
  of << "Test ASCII text\n" << std::flush;
  of << "N= " << std::hex << std::setw(8) << std::left << 534 << std::endl;
  of.close();
}
std::exit(0);
]])


# OCTAVE_CXX_CXX11_TEST_HEADER
# -------------------------
# A C++ header suitable for testing for CXX11.
AC_DEFUN([OCTAVE_CXX_CXX11_TEST_HEADER],
[[
#include <deque>
#include <functional>
#include <memory>
#include <tuple>
#include <array>
#include <regex>
#include <iostream>

namespace cxx11test
{
  typedef std::shared_ptr<std::string> sptr;
  typedef std::weak_ptr<std::string> wptr;

  typedef std::tuple<std::string,int,double> tp;
  typedef std::array<int, 20> int_array;

  constexpr int get_val() { return 20; }

  struct testinit
  {
    int i;
    double d;
  };

  class delegate  {
  public:
    delegate(int n) : n(n) {}
    delegate(): delegate(2354) {}

    virtual int getval() { return this->n; };
  protected:
    int n;
  };

  class overridden : public delegate {
  public:
    overridden(int n): delegate(n) {}
    virtual int getval() override final { return this->n * 2; }
  };

  class nocopy {
  public:
    nocopy(int i): i(i) {}
    nocopy() = default;
    nocopy(const nocopy&) = delete;
    nocopy & operator=(const nocopy&) = delete;
  private:
    int i;
  };
}
]])# OCTAVE_CXX_CXX11_TEST_HEADER

# OCTAVE_CXX_CXX11_TEST_BODY
# -----------------------
# A C++ body suitable for testing for CXX11, assuming the corresponding header.
AC_DEFUN([OCTAVE_CXX_CXX11_TEST_BODY],
[[
{
  // Test auto and decltype
  std::deque<int> d;
  d.push_front(43);
  d.push_front(484);
  d.push_front(3);
  d.push_front(844);
  int total = 0;
  for (auto i = d.begin(); i != d.end(); ++i) { total += *i; }

  auto a1 = 6538;
  auto a2 = 48573953.4;
  auto a3 = "String literal";

  decltype(a2) a4 = 34895.034;
}
{
  // Test constexpr
  short sa[cxx11test::get_val()] = { 0 };
}
{
  // Test initialiser lists
  cxx11test::testinit il = { 4323, 435234.23544 };
}
{
  // Test range-based for and lambda
  cxx11test::int_array array = {9, 7, 13, 15, 4, 18, 12, 10, 5, 3, 14, 19, 17, 8, 6, 20, 16, 2, 11, 1};
  for (int &x : array) { x += 23; }
  std::for_each(array.begin(), array.end(), [](int v1){ std::cout << v1; });
}
{
  using cxx11test::sptr;
  using cxx11test::wptr;

  sptr sp(new std::string("ASCII string"));
  wptr wp(sp);
  sptr sp2(wp);
}
{
  cxx11test::tp tuple("test", 54, 45.53434);
  double d = std::get<2>(tuple);
  std::string s;
  int i;
  std::tie(s,i,d) = tuple;
}
{
  static std::regex filename_regex("^_?([a-z0-9_.]+-)+[a-z0-9]+$");
  std::string testmatch("Test if this string matches");
  bool match = std::regex_search(testmatch, filename_regex);
}
{
  cxx11test::int_array array = {9, 7, 13, 15, 4, 18, 12, 10, 5, 3, 14, 19, 17, 8, 6, 20, 16, 2, 11, 1};
  cxx11test::int_array::size_type size = array.size();
}
{
  // Test constructor delegation
  cxx11test::delegate d1;
  cxx11test::delegate d2();
  cxx11test::delegate d3(45);
}
{
  // Test override and final
  cxx11test::overridden o1(55464);
}
{
  // Test nullptr
  char *c = nullptr;
}
{
  // Test template brackets
  std::vector<std::pair<int,char*>> v1;
}
{
dnl FIXME: Octave-specific change, this feature test is modified from the
dnl original Autoconf source. The "const" type qualifier is strictly required,
dnl needed for this test to pass with clang for example.
  // Unicode literals
  const char *utf8 = u8"UTF-8 string \u2500";
  const char16_t *utf16 = u"UTF-8 string \u2500";
  const char32_t *utf32 = U"UTF-32 string \u2500";
}
]])
