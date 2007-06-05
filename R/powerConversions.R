## All the functions here convert things to
##  penAA, penAB, penBB, returned as a vector.

MOI_ADDITIVE <- 1;
MOI_MULTIPLICATIVE <- 2;
MOI_RECESSIVE <- 3;
MOI_DOMINANT <- 4;

## helper functions
h_K <- function( paa, pab, pbb, p, K )
  return( paa*p^2 + 2*pab*p*(1-p) + pbb*(1-p)^2 - K )

h_model <- function( paa, pab, pbb, model ) {
  if( model==MOI_ADDITIVE )
    return( paa-pab - (pab-pbb) )
  if( model==MOI_DOMINANT )
    return( paa-pab )
  if( model==MOI_RECESSIVE )
    return( pab-pbb )
  if( model==MOI_MULTIPLICATIVE )
    return( paa/pab - pab/pbb )

  stop( "h_model MOI not understood." )
}

h_orAllelic <- function( paa, pab, pbb, p,  orAllelic ) {
  a <- paa*p^2 + pab*p*(1-p)
  b <- pbb*(1-p)^2 + pab*p*(1-p)
  c <- (1-paa)*p^2 + (1-pab)*p*(1-p)
  d <- (1-pbb)*(1-p)^2 + (1-pab)*p*(1-p)

  return( orAllelic - a*d/b/c )
}

solve_allelicOR <- function( pen, afreq, popPrev, orAllelic, moi ) {
  paa <- pen[1]
  pab <- pen[2]
  pbb <- pen[3]
  return( h_K( paa, pab, pbb, afreq, popPrev )^2
          + h_model( paa, pab, pbb, moi )^2
          + h_orAllelic( paa, pab, pbb, afreq, orAllelic )^2 )
}

## export - solving for pen** given allelic OR + info
pen_allelicOR <- function( moi, afreq, popPrev, orAllelic ) {
  res <- optim( rep(0.3,3), fn=solve_allelicOR,
                moi=moi, afreq=afreq, popPrev=popPrev, orAllelic=orAllelic )
  #print( str( res ) )
  return( res$par )  ## value is value at
}
## export - reverse solving for allelic OR given pen** and afreq
allelic_OR <- function( paa, pab, pbb, p ) {
  return( -h_orAllelic( paa, pab, pbb, p, 0 ) )
}

##################
## OR functions ##

## export - calculating OR1, OR2
calc_or <- function( paa, pab, pbb, p ) {
  case0 <- pbb*(1-p)^2
  case1 <- 2*pab*p*(1-p)
  case2 <- paa*p^2
  cont0 <- (1-pbb)*(1-p)^2
  cont1 <- 2*(1-pab)*p*(1-p)
  cont2 <- (1-paa)*p^2

  or1 <- 1/( case0 * cont1 / case1 / cont0 )  ## OR from options, OR_heterozygous, OR1 output
  or2 <- 1/( case0 * cont2 / case2 / cont0 )  ## OR_homozygous, OR2 output

  return( c(or1=or1, or2=or2) )
}

h_or <- function( paa, pab, pbb, afreq, or1 ) {
  return( or1 - calc_or(paa,pab,pbb,afreq)[1] )
}

solve_or1 <- function( pen, afreq, popPrev, or1, moi ) {
  paa <- pen[1]
  pab <- pen[2]
  pbb <- pen[3]
  return( h_K( paa, pab, pbb, afreq, popPrev )^2
          + h_model( paa, pab, pbb, moi )^2
          + h_or( paa, pab, pbb, afreq, or1 )^2 )
}

## export - solving for pen** given OR
pen_or1 <- function( moi, afreq, popPrev, or1 ) {
  res <- optim( rep(0.3,3), fn=solve_or1,
                moi=moi, afreq=afreq, popPrev=popPrev, or1=or1 )
  return( res$par )
}

##################
## AR functions ##

## export - calculting AR from pens
calc_ar <- function( paa, pab, pbb, p ) {
  case0 <- pbb*(1-p)^2
  case1 <- 2*pab*p*(1-p)
  case2 <- paa*p^2
  cont0 <- (1-pbb)*(1-p)^2
  cont1 <- 2*(1-pab)*p*(1-p)
  cont2 <- (1-paa)*p^2

  caseA <- case2 + 0.5*case1
  caseB <- case0 + 0.5*case1
  contA <- cont2 + 0.5*cont1
  contB <- cont0 + 0.5*cont1

  K <- caseA+caseB

  ar <- ( 1 - caseB/K/(contB+caseB) )  ## attributable risk
  return( ar )
}

h_ar <- function( paa, pab, pbb, afreq, ar ) {
  return( ar - calc_ar(paa,pab,pbb,afreq) )
}

solve_ar <- function( pen, afreq, popPrev, ar, moi ) {
  paa <- pen[1]
  pab <- pen[2]
  pbb <- pen[3]
  return( h_K( paa, pab, pbb, afreq, popPrev )^2
          + h_model( paa, pab, pbb, moi )^2
          + h_ar( paa, pab, pbb, afreq, ar )^2 )
}

## export - solving for pen** given AR
pen_ar <- function( moi, afreq, popPrev, ar ) {
  res <- optim( rep(0.3,3), fn=solve_ar,
                moi=moi, afreq=afreq, popPrev=popPrev, ar=ar )
  return( res$par )
}

## export -- population prevalence!
calc_popPrev <- function( paa, pab, pbb, p ) {
  return( paa*p^2 + 2*pab*p*(1-p) + pbb*(1-p)^2 )
}

###########
## DEBUG ##
###########
debug_powerConversions <- function() {
  pen_allelicOR( moi=MOI_ADDITIVE, afreq=0.2, popPrev=0.3, orAllelic=2.471 ) ## should return 0.6271, 0.4227, 0.2182
  allelic_OR( 0.6271, 0.4227, 0.2182, 0.2 )

  calc_or( 0.6271, 0.4227, 0.2182, 0.2 )
  pen_or1( moi=MOI_ADDITIVE, afreq=0.2, popPrev=0.3, or1=2.622 )

  calc_ar( 0.6271, 0.4227, 0.2182, 0.2 )
  pen_ar( moi=MOI_ADDITIVE, afreq=0.2, popPrev=0.3, ar=0.2726 )
}
