#include <R.h>
#include <Rdefines.h>

#include <vector>

#include "surveygraph.h"

SEXP surveygraphr_dummy(SEXP m, SEXP n) 
{
  surveygraph S;
  S.dummy(INTEGER(m)[0], INTEGER(n)[0]);

  SEXP result = PROTECT(NEW_INTEGER(1));
  INTEGER(result)[0] = 2 * INTEGER(m)[0] + INTEGER(n)[0];
  //Rprintf("processing %d and %d\n", INTEGER(m)[0], INTEGER(n)[0]);
  UNPROTECT(1);
  return result;
}

SEXP surveygraphr_diving(SEXP m) 
{
  surveygraph S;
  S.diving(1);

  SEXP result = PROTECT(NEW_INTEGER(1));
  //Rprintf("processing %d and %d\n", INTEGER(m)[0], INTEGER(n)[0]);
  INTEGER(result)[0] = 1;
  UNPROTECT(1);
  return result;
}
