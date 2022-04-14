
// #define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

extern SEXP rsvg_bitmap_to_native_raster_();

static const R_CallMethodDef CEntries[] = {

  {"rsvg_bitmap_to_native_raster_", (DL_FUNC) &rsvg_bitmap_to_native_raster_, 2},
  {NULL , NULL, 0}
};


void R_init_ggsvg(DllInfo *info) {
  R_registerRoutines(
    info,      // DllInfo
    NULL,      // .C
    CEntries,  // .Call
    NULL,      // Fortran
    NULL       // External
  );
  R_useDynamicSymbols(info, FALSE);
}



