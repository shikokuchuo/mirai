/* mirai - C level code ----------------------------------------------------- */

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Visibility.h>

SEXP nano_AioSymbol;
SEXP nano_StateSymbol;

static void RegisterSymbols(void) {
  nano_AioSymbol = Rf_install("aio");
  nano_StateSymbol = Rf_install("state");
}

SEXP mirai_scm(void) {
  return R_MissingArg;
}

SEXP mirai_create(SEXP aio, SEXP value) {

  SEXP key = Rf_getAttrib(Rf_findVarInFrame(aio, nano_AioSymbol), nano_StateSymbol);
  R_MakeWeakRef(key, value, R_NilValue, TRUE);
  SEXP klass = PROTECT(Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(klass, 0, Rf_mkChar("mirai"));
  SET_STRING_ELT(klass, 1, Rf_mkChar("recvAio"));
  Rf_classgets(aio, klass);
  UNPROTECT(1);
  return aio;

}

static const R_CallMethodDef CallEntries[] = {
  {"mirai_create", (DL_FUNC) &mirai_create, 2},
  {"mirai_scm", (DL_FUNC) &mirai_scm, 0},
  {NULL, NULL, 0}
};

void attribute_visible R_init_mirai(DllInfo* dll) {
  RegisterSymbols();
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}

