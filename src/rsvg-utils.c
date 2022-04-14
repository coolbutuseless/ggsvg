


#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>




SEXP rsvg_bitmap_to_native_raster_(SEXP bitmap_, SEXP dim_) {

  if (length(dim_) != 3) {
    error("Expecting a 3D matrix back from rsvg");
  }
  if (INTEGER(dim_)[0] != 4) {
    error("Expecting RGBA data back from rsvg");
  }

  int width  = INTEGER(dim_)[1];
  int height = INTEGER(dim_)[2];

  // Allocate the nativeRaster as an integer matrix
  SEXP nr_     = PROTECT(allocVector(INTSXP, width * height));
  SEXP nr_dim_ = PROTECT(allocVector(INTSXP, 2));

  unsigned char *bp  = RAW(bitmap_);
  int *nrp = INTEGER(nr_);

  // Twiddle Cairo's bitmap in RGBA format into
  // native raster ARGB format
  for (int i=0; i < height * width; i++) {
    nrp[i] =
      (bp[(i << 2) + 0]) << 16 |   // R
      (bp[(i << 2) + 1]) <<  8 |   // G
      (bp[(i << 2) + 2]) <<  0 |   // B
      (bp[(i << 2) + 3]) << 24 ;   // A
  }

  // Make the integer matrix into a proper nativeRaster object
  SET_CLASS(nr_, mkString("nativeRaster"));
  INTEGER(nr_dim_)[0] = height;
  INTEGER(nr_dim_)[1] = width;
  setAttrib(nr_, R_DimSymbol, nr_dim_);

  UNPROTECT(2);
  return nr_;
}
