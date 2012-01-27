#include "tommath.h"
#ifdef BN_MP_ADD_C
/* LibTomMath, multiple-precision integer library -- Tom St Denis
 *
 * LibTomMath is a library that provides multiple-precision
 * integer arithmetic as well as number theoretic functionality.
 *
 * The library was designed directly after the MPI library by
 * Michael Fromberger but has been written from scratch with
 * additional optimizations in place.
 *
 * The library is free for all purposes without any express
 * guarantee it works.
 *
 * Tom St Denis, tomstdenis@gmail.com, http://math.libtomcrypt.com
 */

/* high level addition (handles signs) */
int mp_add (mp_int * a, mp_int * b, mp_int * c)
{
  int     sa, sb, res;

  /* get sign of both inputs */
  sa = SIGN(a);
  sb = SIGN(b);

  /* handle two cases, not four */
  if (sa == sb) {
    /* both positive or both negative */
    /* add their magnitudes, copy the sign */
    SET_SIGN(c,sa);
    res = s_mp_add (a, b, c);
  } else {
    /* one positive, the other negative */
    /* subtract the one with the greater magnitude from */
    /* the one of the lesser magnitude.  The result gets */
    /* the sign of the one with the greater magnitude. */
    if (mp_cmp_mag (a, b) == MP_LT) {
      SET_SIGN(c,sb);
      res = s_mp_sub (b, a, c);
    } else {
      SET_SIGN(c,sa);
      res = s_mp_sub (a, b, c);
    }
  }
  return res;
}

#else

MP_DUMMY_LINKER_DEF

#endif

/* $Source: /cvs/libtom/libtommath/bn_mp_add.c,v $ */
/* $Revision: 1.3 $ */
/* $Date: 2006/03/31 14:18:44 $ */
