getVersion <- function() {
  return(  installed.packages()["pbatR","Version"]  );
}

pbat.current <- function(){
  cat( "Checking version of pbatR... " );
  try( {
    filename <- "http://www.people.fas.harvard.edu/~tjhoffm/pbatRversion.txt";
    file <- file( filename );
    lines <- readLines( file, n=3 );
    curVersion <- lines[1];
    fixes <- lines[2];
    notes <- lines[3];
    close( file );
    if( curVersion == getVersion() ) {
      cat( "version is current.\n" );
    }else{
      cat( "version is NOT CURRENT. Consider updating (see http://www.people.fas.harvard.edu/~tjhoffm/pbatR.html for details).\n" );
      if( nchar( fixes ) > 0 ) {
        cat( "The new version fixes:\n ", fixes, "\n", sep="" );
      }else{
        cat( "No version fixes specified.\n" );
      }
    }

    if( nchar( notes ) > 0 )
      cat( "Notes:\n ", notes, "\n", sep="" );
    
    return( invisible() );
  }, silent=TRUE );
  cat( "version check FAILED.\n" );
  return( invisible() );
}
