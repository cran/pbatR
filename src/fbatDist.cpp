/* Thomas Hoffmann
 * fbatDist.cpp
 * Created: 5/19/2007
 * Last modified: 7/05/2007
 */

/* This has been _completely_ verified via routines in 'powerSim.cpp'
 *  that compared this directly with the FBAT routines,
 *  at least for bi-allelic markers, which is the current target!
 * Hence this option is no longer necessary, and is removed,
 *  but could be put in if you suspect any abberant behaviour.
 */
//#define DEBUG_FBATDIST

#include "fbatDist.h"

bool first( int a, int b, int c ) {
  return( a>0 && b==0 && c==0 );
}
bool second( int b, int a, int c ) {
  return( a>0 && b==0 && c==0 );
}
bool third( int c, int b, int a ) {
  return( a>0 && b==0 && c==0 );
}

bool first_second( int a, int b, int c ) {
  return( a>0 && b>0 && c==0 );
}
bool first_third( int a, int c, int b ) {
  return( a>0 && b>0 && c==0 );
}
bool second_third( int c, int b, int a ) {
  return( a>0 && b>0 && c==0 );
}
bool all( int a, int b, int c ) {
  return( a>0 && b>0 && c>0 );
}

void printFamily( int *p1, int *p2,
                  int *ca, int *cb,
                  int numSibs ) {
  cout << "P: " << p1[0] << " "  << p1[1] << ", " << p2[0] << " " << p2[1] << endl << "C: ";
  for( int i=0; i<numSibs; i++ )
    cout << ca[i] << " " << cb[i] << ", ";
  cout << endl;
}

bool pG( int gP1, int gP2, // parental mating type
         int nAA, int nAB, int nBB, // number of offspring with said genotype
         double pg[3] ) // output parameter, prob of each genotype
{
  // swap gP1 and gP2 if gP1 is missing
  if( gP1==gMiss ) {
    gP1 = gP2;
    gP2 = gMiss;
  }

  double nSum = nAA + nAB + nBB;

  // neither parent is missing
  if( gP1!=gMiss ) {
    switch( gP1 ){
    case gAA:
      switch( gP2 ) {
      case gAA:
	pg[gAA] = 1.00;
	pg[gAB] = 0.00;
	pg[gBB] = 0.00;
        return(true);
      case gAB:
	pg[gAA] = 0.50;
	pg[gAB] = 0.50;
	pg[gBB] = 0.00;
        return(true);
      case gBB:
	pg[gAA] = 0.00;
	pg[gAB] = 1.00;
	pg[gBB] = 0.00;
        return(true);
      }

    case gAB:
      switch( gP2 ) {
      case gAA:
	pg[gAA] = 0.50;
	pg[gAB] = 0.50;
	pg[gBB] = 0.00;
        return(true);
      case gAB:
	pg[gAA] = 0.25;
	pg[gAB] = 0.50;
	pg[gBB] = 0.25;
        return(true);
      case gBB:
	pg[gAA] = 0.00;
	pg[gAB] = 0.50;
	pg[gBB] = 0.50;
        return(true);
      }

    case gBB:
      switch( gP2 ) {
      case gAA:
	pg[gAA] = 0.00;
	pg[gAB] = 1.00;
	pg[gBB] = 0.00;
        return(true);
      case gAB:
	pg[gAA] = 0.00;
	pg[gAB] = 0.50;
	pg[gBB] = 0.50;
        return(true);
      case gBB:
	pg[gAA] = 0.00;
	pg[gAB] = 0.00;
	pg[gBB] = 1.00;
        return(true);
      }
    }
  }

  // one parent is missing

  switch( gP1 ) {
  case gAA:
  case gBB:
    // one homozygous parent
    if( first(nAA,nAB,nBB) ) {
      pg[gAA] = 1.00;
      pg[gAB] = 0.00;
      pg[gBB] = 0.00;
      return(true);
    }
    if( second(nAA,nAB,nBB) ) {
      pg[gAA] = 0.00;
      pg[gAB] = 1.00;
      pg[gBB] = 0.00;
      return(true);
    }
    if( third(nAA,nAB,nBB) ) {
      pg[gAA] = 0.00;
      pg[gAB] = 0.00;
      pg[gBB] = 1.00;
      return(true);
    }

    if( first_second(nAA,nAB,nBB) ) {
      pg[gAA] = 0.50;
      pg[gAB] = 0.50;
      pg[gBB] = 0.00;
      return(true);
    }
    if( first_third(nAA,nAB,nBB) ) {
      pg[gAA] = 0.50;
      pg[gAB] = 0.00;
      pg[gBB] = 0.50;
      return(true);
    }
    if( second_third(nAA,nAB,nBB) ) {
      pg[gAA] = 0.00;
      pg[gAB] = 0.50;
      pg[gBB] = 0.50;
      return(true);
    }
  }

  // otherwise both parents are missing,
  //  or the parent is heterozygous
  // either are the same
  if( gP1==gAB || gP1==gMiss ) {
    // one heterozygous parent
    if( first(nAA,nAB,nBB) ) {
      pg[gAA] = 1.00;
      pg[gAB] = 0.00;
      pg[gBB] = 0.00;
      return(true);
    }
    if( second(nAA,nAB,nBB) ) {
      pg[gAA] = 0.00;
      pg[gAB] = 1.00;
      pg[gBB] = 0.00;
      return(true);
    }
    if( third(nAA,nAB,nBB) ) {
      pg[gAA] = 0.00;
      pg[gAB] = 0.00;
      pg[gBB] = 1.00;
      return(true);
    }

    if( !first_third(nAA,nAB,nBB) && !all(nAA,nAB,nBB) ){
      pg[gAA] = (double)nAA / nSum;
      pg[gAB] = (double)nAB / nSum;
      pg[gBB] = (double)nBB / nSum;
      return(true);
    }

    pg[gAA] = ( pow(4,nSum-1) - pow(3,nSum-1) ) / ( pow(4,nSum) - 2*pow(3,nSum) + pow(2,nSum) );
    pg[gBB] = pg[gAA];
    pg[gAB] = 1.0 - pg[gAA] - pg[gBB];
    return(true);
  }

  if( (gP1==gAA||gP1==gBB) && gP2==gMiss
      && all(nAA,nAB,nBB) ) {
#ifndef LOOKUP_COMPARE
    cout << "WARNING: impossible genotype in file." << endl;
    //printFamily();
#endif
    return(true);
  }

  cout << "failed all cases!" << endl;
  cout << gP1 << " " << gP2 << endl;
  return(false);
}

// allele code
int gCode( int a, int b )
{
  if( a==0 || b==0 ) return( gMiss );
  if( a==ALLELE_A && b==ALLELE_A ) return( gAA ); // alteration used to be 1 and two
  if( a==ALLELE_B && b==ALLELE_B ) return( gBB );
  return( gAB );
}

// NOTE: assumes ALLELE_A is the disease allele
int xCode( int a, int b, int MODEL ) {
  switch( MODEL ){
  case( MODEL_ADDITIVE ):
    return( (int)(a==ALLELE_A) + (int)(b==ALLELE_A) );
  case( MODEL_DOMINANT ):
    return( (int)(a==ALLELE_A || b==ALLELE_A) );
  case( MODEL_RECESSIVE ):
    return( (int)(a==ALLELE_A && b==ALLELE_A) );
  }
  cout << "xCode (1) out of bounds! " << a << " " << b << endl;
  return( -1 ); // should never get here
}
int xCode( int g, int MODEL ) {
  switch( g ) {
  case gAA:
    return( xCode( ALLELE_A, ALLELE_A, MODEL ) );
  case gAB:
    return( xCode( ALLELE_A, ALLELE_B, MODEL ) );
  case gBB:
    return( xCode( ALLELE_B, ALLELE_B, MODEL ) );
  }
  cout << "xCode (2) out of bounds! " << g << endl;
  return( -1 ); // should never get here
}

int ggConvert( int g1, int g2 ) {
  return( g1*3 + g2 );
}

bool pG( int n,
         int *p1, int *p2, // parental alleles
         int *ca, int *cb, // childrens alleles
         double pg[3] ) // output parameter, prob of each genotype
{
  int nG[3] = {0,0,0};
  for( int i=0; i<n; i++ )
    nG[ gCode( ca[i], cb[i] ) ]++;

  return( pG( gCode(p1[0],p1[1]), gCode(p2[0],p2[1] ),
          nG[0], nG[1], nG[2],
          pg ) );
}

bool pGG( int gP1, int gP2, // parental mating type
          int nAA, int nAB, int nBB, // number of offspring with said genotype
          double pgg[9] ) // output parameter, prob of each genotype
{
  // swap gP1 and gP2 if gP1 is missing
  if( gP1==gMiss ) {
    gP1 = gP2;
    gP2 = gMiss;
  }

  double n = nAA + nAB + nBB;

  // NOTE: unspecified genotypes are 0!
  for( int i=0; i<9; i++ )
    pgg[i] = 0.0;

  if( gP2!=gMiss ) {
    // neither parent is missing
    double pg[3];
    pG( gP1, gP2,  nAA, nAB, nBB,  pg );
    pgg[gAAgAA] = pg[gAA]*pg[gAA];
    pgg[gAAgAB] = pg[gAA]*pg[gAB];
    pgg[gAAgBB] = pg[gAA]*pg[gBB];
    pgg[gABgAA] = pg[gAB]*pg[gAA];
    pgg[gABgAB] = pg[gAB]*pg[gAB];
    pgg[gABgBB] = pg[gAB]*pg[gBB];
    pgg[gBBgAA] = pg[gBB]*pg[gAA];
    pgg[gBBgAB] = pg[gBB]*pg[gAB];
    pgg[gBBgBB] = pg[gBB]*pg[gBB];
    return(true);
  }

  if( gP1!=gMiss ) {
    // just one parent is missing
    switch( gP1 ) {
    case gBB: // they are the same...
    case gAA:
      // one homozygous parent - all good under BB
      if( first(nAA,nAB,nBB) ) {
        // only happens under BB
        pgg[gAAgAA] = 1.00;
        return(true);
      }
      if( second(nAA,nAB,nBB) ) {
        pgg[gABgAB] = 1.00;
        return(true);
      }
      if( third(nAA,nAB,nBB) ) {
        // only happens under AA
        pgg[gBBgBB] = 1.00;
        return(true);
      }

      if( first_second(nAA,nAB,nBB) ) {
        // only happens under AA
        pgg[gAAgAB] = pow(2,n-2)/(pow(2,n)-2);
        pgg[gAAgAA] = (pow(2,n-2)-1)/(pow(2,n)-2);
        pgg[gABgAB] = pgg[gAAgAA];

        pgg[gABgAA] = pgg[gAAgAB];
        return(true);
      }
      if( first_third(nAA,nAB,nBB) ) {
        cout << "Impossible genotypes, 1 missing parent." << endl;
        return(false);
      }
      if( second_third(nAA,nAB,nBB) ) {
        // only happens under BB
        pgg[gBBgAB] = pow(2,n-2)/(pow(2,n)-2);
        pgg[gBBgBB] = (pow(2,n-2)-1)/(pow(2,n)-2);
        pgg[gABgAB] = pgg[gBBgBB];

        pgg[gABgBB] = pgg[gBBgAB];
        return(true);
      }
    }
  }

  // otherwise both parents are missing,
  //  or the parent is heterozygous
  // either are the same
  if( gP1==gAB || gP1==gMiss ) {
    // this is pretty much a copy of the lower part of the above...
    if( first(nAA,nAB,nBB) ) {
      pgg[gAAgAA] = 1.00;
      return(true);
    }
    if( second(nAA,nAB,nBB) ) {
      pgg[gABgAB] = 1.00;
      return(true);
    }
    if( third(nAA,nAB,nBB) ) {
      pgg[gBBgBB] = 1.00;
      return(true);
    }

    if( first_second(nAA,nAB,nBB) ) {
      pgg[gAAgAA] = ( nAA * (nAA-1) ) / (n * (n-1));
      pgg[gABgAB] = ( nAB * (nAB-1) ) / (n * (n-1));
      pgg[gAAgAB] = ( nAA * nAB ) / (n * (n-1));

      pgg[gABgAA] = pgg[gAAgAB];
      return(true);
    }
    if( first_third(nAA,nAB,nBB) || all(nAA,nAB,nBB) ) {
      pgg[gAAgAA] = (pow(4,n-2)-pow(3,n-2)) / (pow(4,n)-2*pow(3,n)+pow(2,n));
      pgg[gBBgBB] = pgg[gAAgAA];
      pgg[gAAgAB] = 2.0 * pgg[gAAgAA];
      pgg[gBBgAB] = 2.0 * pgg[gAAgAA];

      pgg[gAAgBB] = pow(4,n-2) / (pow(4,n)-2*pow(3,n)+pow(2,n));
      pgg[gABgAB] = ( pow(4,n-1) - 8*pow(3,n-2) + pow(2,n) ) / (pow(4,n)-2*pow(3,n)+pow(2,n));

      pgg[gABgAA] = pgg[gAAgAB];
      pgg[gABgBB] = pgg[gBBgAB];
      pgg[gBBgAA] = pgg[gAAgBB];
      return(true);
    }
    if( second_third(nAA,nAB,nBB) ) {
      pgg[gBBgBB] = ( nBB * (nBB-1) ) / (n * (n-1));
      pgg[gABgAB] = ( nAB * (nAB-1) ) / (n * (n-1));
      pgg[gBBgAB] = ( nBB * nAB ) / (n * (n-1));;

      pgg[gABgBB] = pgg[gBBgAB];
      return(true);
    }
  }

  return(false);
}

bool pGG( int n,
          int *p1, int *p2, // parental alleles
          int *ca, int *cb, // childrens alleles
          double pgg[9] ) { // output parameter, prob of each genotype
  int nG[3] = {0,0,0};
  for( int i=0; i<n; i++ )
    nG[ gCode( ca[i], cb[i] ) ]++;

  return( pGG( gCode(p1[0],p1[1]), gCode(p2[0],p2[1]),
          nG[0], nG[1], nG[2],
          pgg ) );
}

// computes sum_j X-E(X|S)
// - i.e. family-wise contribution to fbat statistic
double fbat_Si( int n,
                int *p1, int *p2, // parental alleles
                int *ca, int *cb, // childrens alleles
                double *y,           // childrens trait
                int model,        // genetic model (a/d/r)
                double &fbat_Vi, // variance of what calculating
                double offset,
                int nPhenotyped ) { // hack for power
  double pg[3];
  pG( n,  p1, p2,  ca, cb,  pg );

  double exj = 0;
  int g;
  for( g=0; g<3; g++ )
    exj += pg[g] * xCode( g, model );

  double Si = 0.0;
  int j;
  for( j=0; j<n && j<nPhenotyped; j++ ) {
    // (X-E(X|S))*Y
    Si += ( xCode( ca[j], cb[j], model ) - exj ) * (y[j]-offset);
    //cout << "Si=" << Si << endl;
  }

  // now the variance

  // we've got a special case if there is only one child...
  if( n==1 || nPhenotyped==1 ) {
    fbat_Vi = 0.0;
    // E(X^2)
    for( int g=0; g<3; g++ ) {
      double x = xCode(g,model);
      fbat_Vi += x*x*pg[g];
    }
    // - E(X)^2
    fbat_Vi -= exj*exj;
    fbat_Vi *= (y[0]-offset)*(y[0]-offset); // and don't loose the trait!

    if( n==1 || nPhenotyped==1 ) {
      return( Si );
    }
  }

  // otherwise we've got multiple offspring
  double pgg[9];
  pGG( n,  p1, p2,  ca, cb,  pgg );

#ifdef DEBUG_FBATDIST
  double pgg_sum=0.0;
  int gg;
  for( gg=0; gg<9; gg++ ) pgg_sum += pgg[gg];
  if( pgg_sum<0.99 || pgg_sum>1.01 ) {
    printFamily( p1, p2,  ca, cb,  n );
    cout << "pgg_sum = " << pgg_sum << endl;
    for( gg=0; gg<9; gg++ )
      cout << " P[" << gg << "]=" << pgg[gg] << endl;
    exit(1);
  }
#endif

  // The coding from the paper works fine for the variance
  //  so long as there is _more_ than one offspring!
  double sumTj=0;
  for( j=0; j<n && j<nPhenotyped; j++ ) sumTj += (y[j]-offset);

  // the first term
  fbat_Vi = 0.0;
  int g1, g2;
  for( g1=0; g1<3; g1++ )
    for( g2=0; g2<3; g2++ )
      fbat_Vi += xCode(g1,model)*xCode(g2,model)
        *( pgg[ggConvert(g1,g2)] - pg[g1]*pg[g2] );
  fbat_Vi *= sumTj*sumTj;

  // the second term
  for( j=0; j<n && j<nPhenotyped; j++ ) {
    double sum = 0.0; // jth term sum over g, g~
    for( g1=0; g1<3; g1++ ) {
      sum += pow((double)xCode(g1,model),2)*pg[g1];
      for( g2=0; g2<3; g2++ ) {
        sum -= xCode(g1,model)*xCode(g2,model) * pgg[ggConvert(g1,g2)];
      }
    }
    sum *= (y[j]-offset) * (y[j]-offset);

    fbat_Vi += sum; // ah... there was the rub, this coding works just fine...
  }

  // and that computes the variance!
  //fbat_Vi = Si * Si; // just for a test...

  return( Si );
}


#ifdef LOOKUP_COMPARE
bool fuzzyEqual( double a, double b ) {
  return( fabs(a-b) <= 0.0001 );
}

// Make sure this works with our lookup table created from fbat
void recursiveFillLookupCompare( int curSib, int numSibs,
                          int *p1, int *p2,
                          int *ca, int *cb )
{
  if( curSib >=1 ){
    // need to do a recursive call

    for( int cai=1; cai<3; cai++ ){
      for( int cbi=1; cbi<3; cbi++ ){
        ca[curSib-1] = cai;
        cb[curSib-1] = cbi;

        recursiveFillLookupCompare( curSib-1, numSibs, p1, p2, ca, cb );
      }
    }
  }else{
    // needs to be computed

    // first by the new code
    double pg[3];
    if( !pG( numSibs,  p1, p2,  ca, cb,  pg ) )
      printFamily( p1, p2, ca, cb, numSibs );

    // now by the lookup
    int index = indexLookup( p1, p2,  ca, cb,  numSibs );
    if( chart_g1(index) != -1 ) {
      // then it's an informative family
      if( !fuzzyEqual( chart_g1(index), pg[0] ) ||
          !fuzzyEqual( chart_g2(index), pg[1] ) ||
          !fuzzyEqual( chart_g3(index), pg[2] ) ) {
        cout << "Lookup failure! You: " << pg[0] << " " << pg[1] << " " << pg[2] << "Fbat: " << chart_g1(index) << " " << chart_g2(index) << " " << chart_g3(index) << endl;
        printFamily( p1, p2,  ca, cb,  numSibs );
      }

    }else{
      // need to think about this, get above first...
    }
  }

  // fell out of all of the normal cases...
  if( (gP1==gAA||gP1==gBB) && gP2==gMiss
      && all(nAA,nAB,nBB) ) {
#ifndef LOOKUP_COMPARE
    cout << "WARNING: impossible genotype in file." << endl;
    printFamily();
#endif
    return(true);
  }

  cout << "failed all cases!" << endl;
  cout << gP1 << " " << gP2 << endl;
  return(false);
}

void fillLookupCompare()
{
  int i;

  int N=4; // temporary...
  cout << "Table size = " << N << endl;

  // now begin the recursive fill
  // - setup
  int p1[2], p2[2], ca[N], cb[N];
  for( int numSibs=1; numSibs<=N; numSibs++ ){
    for( int p1ai=0; p1ai<3; p1ai++ ){
      for( int p1bi=0; p1bi<3; p1bi++ ){
        if( p1ai==0 && p1bi!=0 ) continue;
        if( p1ai!=0 && p1bi==0 ) continue;

        p1[0] = p1ai;
        p1[1] = p1bi;
        for( int p2ai=0; p2ai<3; p2ai++ ){
          for( int p2bi=0; p2bi<3; p2bi++ ){
            if( p2ai==0 && p2bi!=0 ) continue;
            if( p2ai!=0 && p2bi==0 ) continue;

            p2[0] = p2ai;
            p2[1] = p2bi;

            recursiveFillLookupCompare( numSibs, numSibs, p1, p2, ca, cb );
          }
        }
      }
    }
  }
}


int main()
{
  MODEL model = ADDITIVE;
  setupLookup( model );
  fillLookupCompare();
}
#endif
