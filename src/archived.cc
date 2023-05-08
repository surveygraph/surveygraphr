#include <R.h>
#include <Rdefines.h>

/*

In this file we archive code snippets, such as functions, that we found helpful
when learning the C interface for R.

*/

SEXP archived_inputoutput(SEXP m, SEXP n) 
{
  SEXP result = PROTECT(NEW_INTEGER(1));
  INTEGER(result)[0] = 2 * INTEGER(m)[0] + 3 * INTEGER(n)[0];
  Rprintf("processing %d and %d\n", INTEGER(m)[0], INTEGER(n)[0]);
  Rprintf("output will be %d\n", INTEGER(result)[0]);
  UNPROTECT(1);
  return result;
}
