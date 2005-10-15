####################################################################
# Thomas Hoffmann                                                  #
# CREATED:  some time ago                                          #
# MODIFIED: 06/29/2005                                             #
####################################################################

##################################################
# I can't find the right string functions in R;  #
#  so I just created a few real quick to aid us. #
##################################################

strlen <- function( s ) { return(nchar(s)) };

strfindf <- function( s, p, start=1 ) {
  ls <- strlen(s);  lp <- strlen(p);
  if( ls < lp ) return(-1);
  for( i in start:(ls-lp+1) ){
    if( p==substring(s,i,i+lp-1) ) #@$%!!!
      return(i);
  }
  return(-1); # not found
}

# 10/06/2005
# find - from behind!
strfindb <- function( s, p, start=strlen(s)-strlen(p) ) {
  lp <- strlen(p);
  for( i in seq(start,1,by=-1) ) {
    if( p==substring(s,i,i+lp-1) ) #@$%!!!
      return(i);
  }
  return(-1); # not found    
}
# get's the path of a file / executable
str.getpath <- function( s ) {
  # two delimiters
  loc1 <- strfindb(s,"/");
  loc2 <- strfindb(s,"\\");
  if( loc2 > loc1 )
    loc1=loc2;

  # make sure it's a path (and not just cwd)
  if( loc1 < 2 )
    return("");

  # return the substring
  return( substring( s, 1, loc1-1 ) );
}
  

str.extract.after <- function( s, p='~' ) {
  loc <- strfindf( s,p );
  if( loc==-1 ) return("");
  return( substring( s, loc+strlen(p) ) );
}

str.file.extension <- function( s, extension='txt' ){
  if( substring(extension,1,1)!="." ) #@$%!!!
    extension = paste(".",extension,sep="");
  
  if( strlen(s) > strlen(extension) ){
    if( substring(s,strlen(s)-strlen(extension)+1) == extension )
      return( s );
  }
  return( paste(s,extension,sep="") );
}

#substring( string, start, stop=... )
#substr( string, start, stop )
# WARNING: substr() does not exist in s-plus, but it does the same
#  thing as substring() with the third parameter.. strange R;
#  probably for backwards compatibility??

