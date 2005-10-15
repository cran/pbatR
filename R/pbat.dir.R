####################################################################
# Thomas Hoffmann                                                  #
# CREATED:     06/??/2005                                          #
# MODIFIED:    06/??/2005                                          #
# DESCRIPTION: Loading back in the output from pbat.               #
####################################################################

# a <intersect> b
vectorIntersection <- function( a, b ) {
  remList <- c();
  for( i in 1:length(a) ) {
    if( sum(a[i]==b) < 1 )
      remList <- c(remList, i);
  }
  if( length(remList) > 0 )
    a <- a[-remList];
  return(a);
} # DEBUGGED

# a-b
vectorSubtraction <- function( a, b ) {
  # coding only altered a touch from 'vectorIntersection'
  remList <- c();
  for( i in 1:length(a) ) {
    if( sum(a[i]==b) > 0 )
      remList <- c(remList, i);
  }
  if( length(remList) > 0 )
    a <- a[-remList];
  return(a);
}  

# gets the current pbat log file
getPbatlogs <- function() {
  strs <- dir(pattern="pbatlog.*"); # regular expressions
  datStrs <- dir(pattern="pbatlog.*dat");
  headerStrs <- dir(pattern="pbatlog.*header");

  return( vectorSubtraction( vectorSubtraction( strs, datStrs ), headerStrs ) );
} # DEBUGGED

# Idea: call getPbatlogs before running, and then after running
#        to get the new name.
getPbatlog <- function( beforeLogs, afterLogs ) {
  log <- vectorSubtraction( afterLogs, beforeLogs );
  if( length(log)!=1 ) {
    if( length(log)<1 )
      stop( "Pbat terminated before a log-file could be written." );
    stop( "Two possible logs were found - if you are running pbat twice simulataneously in the same directory, bad things happen." );
  }

  return(log);
} # DEBUGGED

loadPbatlog <- function( log ) {
  pbatCall <- NULL; pbatData <- NULL;

  if( !file.exists(log) )
    stop( paste("Cannot load the pbat logfile '",log,"'; file does not exist",sep="") );

  # If .header & .dat file exist
  if( file.exists(paste(log,".dat",sep="")) && file.exists(paste(log,".header",sep="")) ) {
    # First load in the data and the header
    header <- read.table( paste(log,".header",sep=""),
                          sep="&", comment.char="", header=TRUE );
    pbatData <- read.table( log, sep="&", header=FALSE );
    print(length(pbatData)) # They don't match - What???
    print(length(header))
    #names(pbatData) <- names(header);
    warning( "header and data don't match!!!" );

    # Now load in the call
    logfile <- file( paste(log,".dat",sep=""), open="r", blocking=FALSE );
    pbatCall <- readLines(logfile);
    NUMLINES <- length(pbatCall);
    close(logfile);
  }else {
    # .header & .dat don't exist

    # First get the number of lines to prevent an infinite loop.
    #  Yes, this is unnecessarily slow, but not enough to warrant concern,
    #  and R is being difficult this morning.
    
    logfile <- file(log, open="r", blocking=FALSE);
    tmp <- readLines(logfile);
    NUMLINES <- length(tmp);
    close(logfile);

    if( NUMLINES>0 ) {
    
      ;# Now, start reading in the input
      
      logfile <- file(log, open="r", blocking=FALSE);
      on.exit(close(logfile));
      
      MARKERSTR <- "Group&";
      
      ;# read the lines in from the log file, checking for the header...
      line <- readLines( logfile, n=1 );
      namesVector <- NULL;
      lastLine=-1;
      for( i in 1:NUMLINES ){
        if( substring(line,1,strlen(MARKERSTR))==MARKERSTR ) {
          namesVector <- make.names( unlist(strsplit(line,"&")) );
          break;
        }else{
          pbatCall <- c(pbatCall, line);
          line <- readLines( logfile, n=1 );
          ##print( line );
        }
        lastLine=i;
      }
      if( !is.null(namesVector) && lastLine<NUMLINES ) {
        pbatData <- read.table( logfile, header=FALSE, sep="&" );
        if( length(namesVector) != length(pbatData) ) {
          warning( "Names vector is of improper length! I don't know what to do!" );
          print( "Names:" );
          print( namesVector );
        }else{
          names(pbatData) <- namesVector;
        }
        ##print( namesVector ); # DEBUG ONLY
        ;#names( pbatData ) <- namesVector;
      } else if( lastLine>=NUMLINES ) {
        ## PBAT error - no headers! Try again. NEW 11/15/2005
        pbatData <- read.table( log, header=FALSE, sep="&" );
      }
    } else{
      warning( "No logfile exists." );
      pbatCall="";
      pbatData="";
    }
  }
  
  return( list( call=pbatCall, data=pbatData ) );
}

loadCurrentPbatLog <- function( beforeLogs ) {
  # Get the current logs
  afterLogs <- getPbatlogs();
  # Do the difference and find the string of the most recently
  #  run log!
  strLog <- getPbatlog( beforeLogs, afterLogs );
  # Load and return that log.
  return( loadPbatlog( strLog ) );
}
