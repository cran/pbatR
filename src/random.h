// random.h
// Thomas Hoffmann
// ReCreated: 10/30/2006
//

#ifndef _random_h_
#define _random_h_

// you are expected to set the seed now in R
void rndAttach();
void rndDetach();

// Generate basic distributions
double RUnif();  // random uniform[0,1)
double RUnifExt( double min, double max ); // random uniform[min,max)
double RNormal();  // random N(0,1)
double RNormalExt( double mean, double sigma ); // random N(mean,sigma)
int RandInt( int min, int max );  // random integer in [min, max]

#endif
