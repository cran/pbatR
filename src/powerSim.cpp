/* Thomas Hoffmann
 * simMainGen.cpp
 * Created 05/28/2007
 */

// FOR THE STUPID MAC VERSION?
#undef error
// ?NOISREV CAM DIPUTS EHT ROF

#include "thmalloc.h"

/* The idea is to completely replace the power calculations in pbatR by
 *  coding them directly, and only using via simulation for simplicity
 */

/** The disease allele is A **/

// don't reset the random number generator, so we will come up with
//  the same results every time...
#define DONT_RESET

// test with FBAT, but only on the first iteration...
// - fully debugged, no longer needed...
//#define TEST_FBAT

#include "fbatDist.h"
#include <fstream>
#include <string>
using namespace std;

const int ASCERTAINMENT_AFFECTED = 0;
const int ASCERTAINMENT_UNAFFECTED = 1;
const int ASCERTAINMENT_NA = 2;

//int ITERATION_KILLER = 200;

bool test_fbat=false; // runs when true

// draws up a random binary trait
int binaryTrait( int c_a, int c_b,
                 double penAA, double penAB, double penBB )
{
  int g = gCode( c_a, c_b );
  if( g == gAA )
    return( (int)(RUnif() < penAA) );
  if( g == gAB )
    return( (int)(RUnif() < penAB) );
  if( g == gBB )
    return( (int)(RUnif() < penBB) );
  //cout << "binaryTrait(...) out of bounds!" << endl;
  Rprintf("binaryTrait(...{ out of bounds!\n");
  return( -1 );
}

// draws up a random quantitative trait from Falconer & McKay's model
double qtl( int c_a, int c_b,
            double sdXi, double a,
            int model )
{
  int X = xCode( c_a, c_b, model );
  //return( RNormalExt( a*X, a*sdXi ) );
  return( RNormalExt( a*X, 1 ) ); // should be this, right?
}

void randomAllelesFill( int *pM, int *pD,
                        double cumProb[4]  )
{
  for( int i=0; i<2; i++ ) {
    double r = RUnif();

    if( r<=cumProb[DA_MA] ) {
      pD[i] = ALLELE_A;
      pM[i] = ALLELE_A;
    }else if( r<=cumProb[DA_MB] ) {
      pD[i] = ALLELE_A;
      pM[i] = ALLELE_B;
    }else if( r<=cumProb[DB_MA] ) {
      pD[i] = ALLELE_B;
      pM[i] = ALLELE_A;
    }else if( r<=cumProb[DB_MB] ) {
      pD[i] = ALLELE_B;
      pM[i] = ALLELE_B;
    }else{
      //cout << "randomAllelesFill(...) cumProb is out of bounds!" << endl;
      Rprintf("randomAllelesFill(...) cumProb is out of bounds!\n");
    }
  }
}

bool onlyAffected( int ascertainment[], int numOffspring ) {
  for( int i=0; i<numOffspring; i++ )
    if( ascertainment[i] != ASCERTAINMENT_AFFECTED )
      return( false );
  return( true );
}

double power( int numOffspring, int numParents, int numFamilies,
              bool additionalOffspringPhenos,
              int* ascertainment,
              int modelGen, int modelTest,
              double afreqMarker,
              double penAA, double penAB, double penBB, // binary traits, used when heritability=0.0
              double heritability, double* contsAscertainmentLower, double* contsAscertainmentUpper, // continuous traits
              double pDiseaseAlleleGivenMarkerAllele, double afreqDSL,
              double abs_z_alpha_2, // critical value
              double offset,
              int numSim,
              int ITERATION_KILLER )
{
  // debug -- are we passing in the parameters correctly? CHECK.
  /*
  cout << "numOffspring " << numOffspring << " numParents " << numParents << " numFamilies" << numFamilies << endl
       << "additionalOffspringPhenos " << additionalOffspringPhenos << endl
       << "model " << model << endl
       << "afreqMarker " << afreqMarker << endl
       << "penAA " << penAA << " penAB " << penAB << " penBB " << penBB << endl\
       << "heritability " << heritability << " contsAscertainmentLower[0] " << contsAscertainmentLower[0] << " contsAscertainmentUpper[0] " << contsAscertainmentUpper[0] << endl
       << "pDiseaseAlleleGivenMarkerAllele " << pDiseaseAlleleGivenMarkerAllele << " afreqDSL " << afreqDSL << endl
       << "abs_z_alpha_2 " << abs_z_alpha_2 << endl
       << "offset " << offset << endl
       << "numSim " << numSim << endl;
  */

  // 04/17/2008 update -- efficiency? -- still throws away some, but not nearly so much for a rare disease?
  if( onlyAffected( ascertainment, numOffspring ) ) {
    double maxPen = penAA;
    if( penAB > penAA ) maxPen = penAB;
    if( penBB > penAA ) maxPen = penBB;

    penAA /= maxPen;
    penAB /= maxPen;
    penBB /= maxPen;
  }
  // 04/17/2008 update END

  // allocate memory for the simulations
  // - the marker
  THMALLOC2(int, _p1M, p1M, numFamilies, 2); //int p1M[numFamilies][2]; // first parents genotype
  THMALLOC2(int, _p2M, p2M, numFamilies, 2); //int p2M[numFamilies][2]; // second parents genotype
  THMALLOC2(int, _cM_a, cM_a, numFamilies, numOffspring); THMALLOC2(int, _cM_b, cM_b, numFamilies, numOffspring); //int cM_a[numFamilies][numOffspring], cM_b[numFamilies][numOffspring]; // children genotypes
  // - the DSL
  THMALLOC2(int, _p1D, p1D, numFamilies, 2); //int p1D[numFamilies][2]; // first parents genotype
  THMALLOC2(int, _p2D, p2D, numFamilies, 2); //int p2D[numFamilies][2]; // second parents genotype
  THMALLOC2(int, _cD_a, cD_a, numFamilies, numOffspring); THMALLOC2(int, _cD_b, cD_b, numFamilies, numOffspring); //int cD_a[numFamilies][numOffspring], cD_b[numFamilies][numOffspring]; // children genotypes
  THMALLOC2(double, _trait, trait, numFamilies, numOffspring); //double trait[numFamilies][numOffspring];

  THMALLOC(double, _Si, Si, numFamilies); //double Si[numFamilies];
  THMALLOC(double, _Vi, Vi, numFamilies); //double Vi[numFamilies];

  // calculate the probablities for when not the marker
  double p[4];
  p[DA_MA] = pDiseaseAlleleGivenMarkerAllele * afreqMarker;
  p[DA_MB] = afreqDSL - p[DA_MA];
  p[DB_MA] = (1 - pDiseaseAlleleGivenMarkerAllele) * afreqMarker;
  p[DB_MB] = (1-afreqDSL) - p[DB_MA];
  //cout << "probs " << p[0] << " " << p[1] << " " << p[2] << " " << p[3] << endl; // debug only

  // now, for randomly generating from those probabilities
  double cumProb[4];
  cumProb[0] = p[0];
  for( int i=1; i<4; i++ ) {
    cumProb[i] = p[i] + cumProb[i-1];
    if( p[i] < 0.0 ) {
      // impossible choices of parameters then!
      return -1;
    }
  }

  //cout << "cumProbs " << cumProb[0] << " " << cumProb[1] << " " << cumProb[2] << " " << cumProb[3] << endl; // debug only
  //return( 0 );

  // calculations for generating continuous traits
  double a=0.0, varXi=0.0, sdXi=0.0;
  const double SIGMA = 1.0;
  if( heritability!=0.0 ) {
    // toggles on continuous traits then
    // this code is mirrored in power.R (needed for offset calculation in fact)
    double p = afreqDSL;
    double h = sqrt(heritability);
    switch( modelGen ) {
    case MODEL_ADDITIVE:
      varXi = 2*p*(1-p);
      break;
    case MODEL_DOMINANT:
      varXi = 2*p - 5*p*p + 4*p*p*p - p*p*p*p;
      break;
    case MODEL_RECESSIVE:
      varXi = p*p - p*p*p*p;
      break;
    }
    sdXi = sqrt(varXi);
    a = SIGMA * h / sqrt( varXi * ( 1 - h*h ) );

    // new, code X=0,2 for additive/dominant I guess (well, it really does make them comparable then to the recessive)
    // - then our calculations should all match
    //if( model != MODEL_ADDITIVE )
    //  a /= 2.0;

    //cout << "c++ a = " << a << endl;
  }

  // loop across the simulations
  int numRejected=0;
  for( int sim=0; sim<numSim; sim++ ) {
    //cout << "sim: " << sim << endl;
    // generate the data
    int fam = 0;
    int numIters = 0;
    while( fam<numFamilies ) {
      bool goodFamily=true; // a precaution, shouldn't be necessary

      // generate the parents
      randomAllelesFill( p1M[fam], p1D[fam], cumProb );
      randomAllelesFill( p2M[fam], p2D[fam], cumProb );

      // generate the offspring's genotypes from the parents
      // generate the offspring's trait
      int child;
      for( child=0; child<numOffspring && goodFamily; child++ ) {
        // need to transfer in haplotypes
        int c_a_R = RUnif()<0.5;
        int c_b_R = RUnif()<0.5;

        // marker locus
        cM_a[fam][child] = p1M[fam][c_a_R];
        cM_b[fam][child] = p2M[fam][c_b_R];

        // disease locus
        cD_a[fam][child] = p1D[fam][c_a_R];
        cD_b[fam][child] = p2D[fam][c_b_R];

        // generate the trait according to the DSL
        if( heritability == 0.0 ) {
          // then we should do a binary trait
          trait[fam][child] = binaryTrait( cD_a[fam][child], cD_b[fam][child],
                                           penAA, penAB, penBB );
        }else{
          // we should do a continuous trait!
          trait[fam][child] = qtl( cD_a[fam][child], cD_b[fam][child],
                                   sdXi, a, modelGen );
        }

        // make sure it satisfied ascertainment contditions
        if( heritability==0 ) {
          // binary trait conditions
          if(  ( ascertainment[child]==ASCERTAINMENT_AFFECTED && trait[fam][child]!=1 )
              ||  ( ascertainment[child]==ASCERTAINMENT_UNAFFECTED && trait[fam][child]!=0 )  ) {
            // need to draw a new individual, this one's no good
            //sim--;
            goodFamily = false;
            continue;
          }
        }else{
          // continuous trait ascertainment conditions
          // unaffected: in the tails
          // affected: in the middle portion
          //if( ( ascertainment[child]==ASCERTAINMENT_UNAFFECTED &&
          //      (trait[fam][child]>contsAscertainmentLower[child] && trait[fam][child]<contsAscertainmentUpper[child]) )
          //    || ( ascertainment[child]==ASCERTAINMENT_AFFECTED &&
          //        (trait[fam][child]<contsAscertainmentLower[child] || trait[fam][child]>contsAscertainmentUpper[child]) ) ) {

          // altered to just always use the ascertainment criterion
          if( trait[fam][child]<contsAscertainmentLower[child] || trait[fam][child]>contsAscertainmentUpper[child] ) {
            goodFamily = false;
            continue;
          }
        }

      }

      // erase parents if necessary
      if( numParents < 2 ) {
        p2M[fam][0] = 0;
        p2M[fam][1] = 0;
      }
      if( numParents < 1 ) {
        p1M[fam][0] = 0;
        p1M[fam][1] = 0;
      }

      // was a successful family
      if( goodFamily ) {
        fam++;
        numIters = 0;
      }
      //else
      //  cout << "what??? it was needed?????" << endl; // son of a mother fucking bitch...

      if( numIters >= ITERATION_KILLER && ITERATION_KILLER!=0 ) {
        //cout << "ITERATION_KILLER" << endl;
        Rprintf("ITERATION_KILLER\n");
        return( -2 );
      }
      numIters++;
    }

    int numPhenotyped = numOffspring;
    if( !additionalOffspringPhenos ) numPhenotyped = 1;

    // NOTE: WARNING: DEBUG ONLY!!!! PLEASE KILL!!!!
    //model = MODEL_ADDITIVE;

    // now test the data
    for( fam=0; fam<numFamilies; fam++ ) {
      Si[fam] = fbat_Si( numOffspring,
                         p1M[fam], p2M[fam],
                         cM_a[fam], cM_b[fam],
                         trait[fam],
                         modelTest, // DEBUG ONLY DEBUG ONLY DEBUG ONLY
                         Vi[fam],
                         offset,
                         numPhenotyped );

      //cout << "Si[" << fam << "]=" << Si[fam] << " " << "Vi=" << Vi[fam] << endl;
    }

    /*
    // debug, trios only
    for( fam=0; fam<numFamilies; fam++ ) {
      cout << p1M[fam][0] << "/" << p1M[fam][1] << " ("
           << p1D[fam][0] << "/" << p1D[fam][1] << ") "
           << p2M[fam][0] << "/" << p2M[fam][1] << " ("
           << p2D[fam][0] << "/" << p2D[fam][1] << ") "
           << cM_a[fam][0] << "/" << cM_b[fam][0] << " ("
           << cD_a[fam][0] << "/" << cD_b[fam][0] << ") "
           << trait[fam][0] << endl;
    }
    return(0);
    */

    double num=0.0, den=0.0;
    // this isn't the most numerically stable, put should
    //  be okay just for power
    for( fam=0; fam<numFamilies; fam++ ) {
      num += Si[fam];
      den += Vi[fam];
    }
    double stat = num / sqrt(den);

    // debug fbatDist.h -- with fbat! -- this has been completely debugged.

#ifdef TEST_FBAT
    if( test_fbat ) {
      // output the pedigree
      ofstream ped("killme.ped", ios::out);
      ofstream phe("killme.phe", ios::out);

      ped << "m0" << endl;
      phe << "qtl" << endl;
      for( fam=0; fam<numFamilies; fam++ ) {
        // pid id idfath idmoth sex affection

        // the parents
        ped << fam << " 1 0 0 0 0 " << p1M[fam][0] << " " << p1M[fam][1] << endl
            << fam << " 2 0 0 0 0 " << p2M[fam][0] << " " << p2M[fam][1] << endl;

        // and all the offspring
        for( int child=0; child<numOffspring; child++ ) {
          int affection = (int)(trait[fam][child])+1;

          if( heritability!=0 ) {
            affection = 0; // missing for qtl's
            phe << fam << " " << (3+child) << " " << trait[fam][child] << endl;
          }

          ped << fam << " " << (3+child) << " 1 2 0 " << affection  << " " << cM_a[fam][child] << " " << cM_b[fam][child] << endl;
        }
      }

      ped.close();
      phe.close();

      // now create a file to run with fbat, and run it
      ofstream fc("killme.sh", ios::out);
      fc << "fbat << %%" << endl;
      fc << "minsize 0" << endl;
      fc << "load killme.ped" << endl;
      if( heritability!=0 ) fc << "load killme.phe" << endl;
      fc << "model " << MODEL_FBAT_CHARS[modelGen] << endl;
      fc << "offset " << offset << endl;
      if( heritability!=0 ) fc << "trait qtl" << endl;
      fc << "fbat" << endl;
      fc << "quit" << endl;
      fc << "%%" << endl;
      fc.close();
      system( "sh killme.sh" );

      // and compare our computed statistic...
      //cout << "FBAT statistic:" << stat << endl;
      //cout << "num " << num << endl;
      //cout << "var " << den << endl;
      Rprintf("FBAT statistic: %f\nnum %f\nvar %f\n", stat, num, den);
      return(0);

      // and set it so it doesn't run the next iteration
      test_fbat = false;
    }
#endif

    //cout << "num = " << num << " den = " << den << endl;
    //cout << "stat = " << stat << endl;
    numRejected += (int)(fabs(stat) >= abs_z_alpha_2);

  }

  // compute the empirical power
  return( (double)numRejected / (double)numSim );
}

extern "C" {
  void powerR( int *numOffspring, int *numParents, int *numFamilies,
	       int *additionalOffspringPhenos,
	       int *ascertainment,
	       int *modelGen, int *modelTest,
	       double *afreqMarker,
	       double *penAA, double *penAB, double *penBB,
               double *heritability, double *contsAscertainmentLower, double *contsAscertainmentUpper,
               double *pDiseaseAlleleGivenMarkerAllele, double *afreqDSL,
	       double *abs_z_alpha_2,
	       double *offset,
	       int *numSim,
               int *ITERATION_KILLER,
	       double *result
	       ) {
    rndAttach();

#ifdef TEST_FBAT
    test_fbat=true;
#endif

    *result = power( *numOffspring, *numParents, *numFamilies,
		     (bool)(*additionalOffspringPhenos),
		     ascertainment,
		     *modelGen, *modelTest,
		     *afreqMarker,
		     *penAA, *penAB, *penBB,
                     *heritability, contsAscertainmentLower, contsAscertainmentUpper,
                     *pDiseaseAlleleGivenMarkerAllele, *afreqDSL,
		     *abs_z_alpha_2,
		     *offset,
		     *numSim,
                     *ITERATION_KILLER  );

#ifndef DONT_RESET
    rndDetach();
#endif
  }
}
