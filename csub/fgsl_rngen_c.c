#include "config.h"
#include <gsl/gsl_rng.h>

const gsl_rng_type *fgsl_aux_rng_assign(int i) {
    const gsl_rng_type *res;
    switch(i) {
	case -1:
	    res = gsl_rng_default;
	    break;
	case 1:
	    res = gsl_rng_borosh13;
	    break;
	case 2:
	    res = gsl_rng_coveyou;
	    break;
	case 3:
	    res = gsl_rng_cmrg;
	    break;
	case 4:
	    res = gsl_rng_fishman18;
	    break;
	case 5:
	    res = gsl_rng_fishman20;
	    break;
	case 6:
	    res = gsl_rng_fishman2x;
	    break;
	case 7:
	    res = gsl_rng_gfsr4;
	    break;
	case 8:
	    res = gsl_rng_knuthran;
	    break;
	case 9:
	    res = gsl_rng_knuthran2;
	    break;
	case 10:
	    res = gsl_rng_lecuyer21;
	    break;
	case 11:
	    res = gsl_rng_minstd;
	    break;
	case 12:
	    res = gsl_rng_mrg;
	    break;
	case 13:
	    res = gsl_rng_mt19937;
	    break;
	case 14:
	    res = gsl_rng_mt19937_1999;
	    break;
	case 15:
	    res = gsl_rng_mt19937_1998;
	    break;
	case 16:
	    res = gsl_rng_r250;
	    break;
	case 17:
	    res = gsl_rng_ran0;
	    break;
	case 18:
	    res = gsl_rng_ran1;
	    break;
	case 19:
	    res = gsl_rng_ran2;
	    break;
	case 20:
	    res = gsl_rng_ran3;
	    break;
	case 21:
	    res = gsl_rng_rand;
	    break;
	case 22:
	    res = gsl_rng_rand48;
	    break;
	case 23:
	    res = gsl_rng_random128_bsd;
	    break;
	case 24:
	    res = gsl_rng_random128_glibc2;
	    break;
	case 25:
	    res = gsl_rng_random128_libc5;
	    break;
	case 26:
	    res = gsl_rng_random256_bsd;
	    break;
	case 27:
	    res = gsl_rng_random256_glibc2;
	    break;
	case 28:
	    res = gsl_rng_random256_libc5;
	    break;
	case 29:
	    res = gsl_rng_random32_bsd;
	    break;
	case 30:
	    res = gsl_rng_random32_glibc2;
	    break;
	case 31:
	    res = gsl_rng_random32_libc5;
	    break;
	case 32:
	    res = gsl_rng_random64_bsd;
	    break;
	case 33:
	    res = gsl_rng_random64_glibc2;
	    break;
	case 34:
	    res = gsl_rng_random64_libc5;
	    break;
	case 35:
	    res = gsl_rng_random8_bsd;
	    break;
	case 36:
	    res = gsl_rng_random8_glibc2;
	    break;
	case 37:
	    res = gsl_rng_random8_libc5;
	    break;
	case 38:
	    res = gsl_rng_random_bsd;
	    break;
	case 39:
	    res = gsl_rng_random_glibc2;
	    break;
	case 40:
	    res = gsl_rng_random_libc5;
	    break;
	case 41:
	    res = gsl_rng_randu;
	    break;
	case 42:
	    res = gsl_rng_ranf;
	    break;
	case 43:
	    res = gsl_rng_ranlux;
	    break;
	case 44:
	    res = gsl_rng_ranlux389;
	    break;
	case 45:
	    res = gsl_rng_ranlxd1;
	    break;
	case 46:
	    res = gsl_rng_ranlxd2;
	    break;
	case 47:
	    res = gsl_rng_ranlxs0;
	    break;
	case 48:
	    res = gsl_rng_ranlxs1;
	    break;
	case 49:
	    res = gsl_rng_ranlxs2;
	    break;
	case 50:
	    res = gsl_rng_ranmar;
	    break;
	case 51:
	    res = gsl_rng_slatec;
	    break;
	case 52:
	    res = gsl_rng_taus;
	    break;
	case 53:
	    res = gsl_rng_taus2;
	    break;
	case 54:
	    res = gsl_rng_taus113;
	    break;
	case 55:
	    res = gsl_rng_transputer;
	    break;
	case 56:
	    res = gsl_rng_tt800;
	    break;
	case 57:
	    res = gsl_rng_uni;
	    break;
	case 58:
	    res = gsl_rng_uni32;
	    break;
	case 59:
	    res = gsl_rng_vax;
	    break;
	case 60:
	    res = gsl_rng_waterman14;
	    break;
	case 61:
	    res = gsl_rng_zuf;
	    break;
	case 62:
	    res = gsl_rng_knuthran2002;
	    break;
	default :
	    res = NULL;
	    break;
    }
    return res;
}
