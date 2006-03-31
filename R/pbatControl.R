####################################################################
# Thomas Hoffmann                                                  #
# CREATED:  06/23/2006                                             #
#                                                                  #
# DESCRIPTION:                                                     #
#  Controlling pbat (currently only works in unix).                #
#  Future expansion mostly - this would allow us to include the    #
#   power calculations of pbat in pbatR                            #
####################################################################

##############################################################
## WARNING: *nix ONLY                                        #
## Control over pbat!!!                                      #
##############################################################
pbatControl <- function( commands, filename='pbatProgamControl.sh', intern=TRUE ){
  ## Run the command and get the results
  res <- programControl( pbat.get(), commands, filename=filename, intern=intern );

  ## Kill the talk-to file
  ##if( file.exists(filename) )
  ##  file.remove(filename)

  ## Return the results
  return( res );
}

##############################################################
## WARNING: *nix ONLY                                        #
## Send a prespecified listing of commands to any program    #
##############################################################
programControl <- function( program, commands, filename='systemControl.sh', intern=TRUE ){
  ## Creates a shell
  file <- file( filename, 'w' );
  
  cat( '\n', program, ' <<EOF\n', sep='', file=file );
  
  for( i in 1:length(commands) )
    cat( commands[i], '\n', sep='', file=file );
  
  cat( '\EOF', file=file );
  close( file );

  sh <- 'sh';
  if( isWindows() )
    sh <- paste( "\"", .path.package(package="pbatR"), "/exec/sh.exe", "\"", sep="" );
    ##sh <- paste( .path.package(package="pbatR"), "\\exec\\sh.exe", sep="" );

  ##cat( "SH SH SH SH\n" );
  ##cat( sh, "\n" );
  ##cat( paste( sh, filename ) );

  if( isWindows() ){
    ##return( system( paste( sh, filename ), intern=FALSE ) ); ## confuses our output pipes otherwise
    return( .C( "launchPbatPower", file=filename  ) );
  }
    
  return( system( paste( sh, filename ), intern=intern ) );
}

## I'd really like to have a function to generate the random data from pbat for me?
## 9, killme.txt, onlysnps=(1=yes,0=no), 
pbat.generateData <- function( filenamePrefix, onlysnps=TRUE, trios='1000', snps='1000', p='0.1', h='0.01' ){
  if( isWindows() )
    warning( "Feature probably only available in linux; relies on 'sh' script which  could probably be found for windows." );

  findPbatdata(); # I despise it's inability to search your PATH for that wretched file

  outfname <- 'killme.txt';
  if( onlysnps==TRUE ) {
    onlysnps <- 1;
  }else{
    onlysnps <- 0;
  }

  ## And control PBAT for the finale
  pbatControl( c('','',9, outfname, onlysnps, trios, snps, p, h, "") );

  ## Kill that log file
  if( file.exists(outfname) ) file.remove( outfname );

  ## lastly rename the nonsense names
  file.rename( from='100k.phe', to=paste(filenamePrefix,'.phe',sep='') );
  file.rename( from='100k.ped', to=paste(filenamePrefix,'.ped',sep='') );

  ## all done
  return();
}



## Taken from createCommandfile(...)
findPbatdata <- function() {
  pbatdatafile <- paste( str.getpath(pbat.get()), "/pbatdata.txt", sep="" );
  if( file.exists( pbatdatafile ) )
    file.copy( from=pbatdatafile, to=paste(getwd(),"/pbatdata.txt",sep="") );

  ## If we can't find it, then try other things
  if( !file.exists( paste(getwd(),"/pbatdata.txt",sep="") ) ) {

    ## See if it's anywhere in the path
    newLoc <- pathFindFile("pbatdata.txt");
    if( newLoc != "" ){
      ## found it! copy it over!
      file.copy( from=newLoc, to=paste(getwd(),"/pbatdata.txt",sep="") ); ## seems to have to be in cwd - more than just the path somewhere
    }else{
      ## Last thing to try is to download from the internet
      getPbatdata(); ## but puts it in pbat dir first
      if( file.exists( pbatdatafile ) ) {
        file.copy( from=pbatdatafile, to=paste(getwd(),"/pbatdata.txt",sep="") );
      }
    }

    ## So make sure that it finally got copied in
    if( !file.exists( paste(getwd(),"/pbatdata.txt",sep="") ) ) {
      stop( paste("'pbatdata.txt was not found in the current",
                  " working directory '", getwd(),
                  "', or in the pbat directory '", pbatdatafile, "'",
                  ", or anywhere in your current path,",
                  " and it could not be downloaded online. ",
                  " Please see",
                  " http://www.biostat.harvard.edu/~clange/Downloading%20PBAT.htm",
                  " for more details.", sep="" ) );
    }

    ## Old coding below (changed order to look)
    
    ## Now, see if we can find it in the pbat directory location, and go from there...
    ##getPbatdata();
    ##if( file.exists( pbatdatafile ) )
    ##  file.copy( from=pbatdatafile, to=paste(getwd(),"/pbatdata.txt",sep="") );
    ##if( !file.exists( paste(getwd(),"/pbatdata.txt",sep="") ) ) {
    ##  if( pathFindFile( "pbatdata.txt" ) == "" ){
    ##    stop( paste("'pbatdata.txt was not found in the current",
    ##                " working directory '", getwd(),
    ##                "', or in the pbat directory '", pbatdatafile, "'",
    ##                ", or anywhere in your current path,",
    ##                " and it could not be downloaded online. ",
    ##                " Please see",
    ##                " http://www.biostat.harvard.edu/~clange/Downloading%20PBAT.htm",
    ##                " for more details.", sep="" ) );
    ##  }
    ##}
  }
}



## debug
#pbatControl( c('','',-1) )  ## starts and exits the program
#pbatControl( c('','',-1), intern=FALSE )  ## starts and exits the program
#pbat.generateData( 'barmasterfoo', trios='10', snps='2' );
