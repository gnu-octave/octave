#include <iostream.h>
#include <assert.h>
#include <builtin.h>

#include "Map.h"

int SIZE;

char **keys;
double *values;

void
add (char *x[], double y[], Map<double>& a)
{
  for (int i = 0; i < SIZE; ++i)
    a[x[i]] = y[i];
}

void
makekeys (void)
{
  for (int i = 0; i < SIZE; ++i)
    {
      char *tmp = new char [10];
      sprintf (tmp, "index_%d", i);
      keys[i] = tmp;
    }
}

void
makevalues (void)
{
  for (int i = 0; i < SIZE; ++i)
    values[i] = i + 1;
}

void
printMap (Map<double>& a)
{
  int maxprint = 1000;
  cout << "[";
  int k = 0;
  for (Pix i = a.first (); i != 0 && k < maxprint; a.next (i), ++k) 
    cout << "(" << a.key (i) << ", " <<  a.contents (i) << ") ";

  if (i != 0)
    cout << "...]\n";
  else
    cout << "]\n";
}

void
CHtest (void)
{
  CHMap<double> a (-1.0, SIZE);
  add (keys, values, a);

  cout << "a: ";
  printMap (a);

  assert (a.length () == SIZE);

  for (int j = 0; j < SIZE; ++j)
    assert (a.contains (keys[j]));

//  assert (a[SIZE+1] = -1);

  for (j = 0; j < SIZE; ++j)
    a.del (keys[j]);

  assert (a.empty ());

  assert (a.OK ());
}

int
main (int argv, char** argc)
{
  SIZE = 100;

  keys = new char *[SIZE];
  makekeys ();

  values = new double [SIZE];
  makevalues ();

  cout << "CHtest\n";
  CHtest();

  return 0;
}
