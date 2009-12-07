#include <iostream>
#include <octave/oct.h>
#include <octave/octave.h>
#include <octave/parse.h>

int
main (void)
{
  string_vector argv (2);
  argv(0) = "embedded";
  argv(1) = "-q";

  octave_main (2, argv.c_str_vec(), 1);

  octave_idx_type n = 2;
  Matrix a_matrix = Matrix (1, 2);

  std::cout << "GCD of [";
  for (octave_idx_type i = 0; i < n; i++)
    {
      a_matrix (i) = 5 * (i + 1); 
      if (i != 0)
        std::cout << ", " << 5 * (i + 2);
      else
        std::cout << 5 * (i + 2);
    }
  std::cout << "] is ";

  octave_value_list in = octave_value (a_matrix);
  octave_value_list out = feval ("gcd", in, 1);

  if (!error_state && out.length () > 0)
    {
      a_matrix = out(0).matrix_value ();
      if (a_matrix.numel () == 1)
        std::cout << a_matrix(0) << "\n";
      else
        std::cout << "invalid\n";
    }
  else
    std::cout << "invalid\n";

  return 0;
}
