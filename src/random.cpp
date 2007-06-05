// Thomas Hoffmann
// Created:  6/30/2006

#include <R.h>
#include "random.h"

void rndAttach()
{
  GetRNGstate();
}
void rndDetach()
{
  PutRNGstate();
}


// Uniform random number U[0,1)
double RUnif() { return unif_rand(); };
//double RUnif() { return( (double)rand() / ( (double)(RAND_MAX) + 1 ) ); }
double RUnifExt( double min, double max ){
  return( unif_rand() * (max-min) + min );
}

// N(0,1)
double RNormal() { return norm_rand(); }

// N(mean,sigma^2)
double RNormalExt( double mean, double sigma ){
  return( mean + RNormal()*sigma );
}

// Gets a random integer b/n min & max, including both endpoints
int RandInt( int min, int max ){
  int range = (max - min) + 1;
  return(  (int)(RUnif() * range)  + min );
}
