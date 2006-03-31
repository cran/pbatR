####################################################################
# Thomas Hoffmann                                                  #
# CREATED:     06/21/2005                                          #
# MODIFIED:    07/08/2005                                          #
#                                                                  #
# DESCRIPTION:                                                     #
#  The major pbat-interface commands.                              #
#   ( pbat.files, pbat.obj, all of the S4 methods )                #
####################################################################

####################################################################
#                                                                  #
# Making pbat be a class....                                       #
#                                                                  #
####################################################################
pbat <- function( x, ... )
  UseMethod( "pbat" );

summary.pbat <- function( object, ... ) {
  x <- object; ## need for R CMD check
  
  # print out the pretty call
  if( !is.null(x$call) ) {
    print( "Call:" );
    print( x$call );
  }

  # now, print out the results?
  catn( "Results:" );
  print( x$results );
}

plot.pbat <- function( x, ... ) {
  if( x$fbat=="logrank" && !is.null(x$rcode) && x$rcode!="" ) {
    pbat.logrank.replot(load=x$rcode);
  } else {
    stop( "No plot available (only for logrank)." );
  }
}

print.pbat <- function( x, ... ) {
  catn( "Class members:" );
  catn( " $call             The formula used." );
  catn( " $pbat.call        The batch-file commands sent to pbat." );
  catn( " $results          ** The results of pbat execution in a data.frame object. **" );
  catn( " $results.logfile  Filename of the raw results of pbat (may also have extension .hdr & .dat); esp. useful if pbat outputs an unknown format that fails to be read in (in case this happens in later releases of PBAT). Note this is _in the current working directory_." );
  catn( " $rcode            Name of the file containing plots for logrank." );
  catn( " $fbat             'pc','gee', or 'logrank'" );
  catn( " $commandfile      Name of the file cantaining pbat batchfile." );
  catn( "Type '?pbat' for more details." );
}

write.pbat <- function(x, filename) {
  if( str.file.extension(filename,extension='csv')==filename ) {
    write.pbat.csv(x, filename);
    return(invisible());
  }

  
  ## updates 01/20/2006
  f <- file( filename, "w" );
  #cat( paste("PBAT ",as.character(x$fbat),"\n",sep=""), file=f );
  if( !is.null(x$call) ) {
    cat( "** FORMULA **\n", file=f );
    write( x$call, file=f );
    cat( "\n\n", file=f );
  }
  if( !is.null(x$pbat.call) ) {
    cat( "** PBAT BATCH FILE: **\n", file=f );
    write( x$pbat.call, file=f );
    cat( "\n\n", file=f );
  }
  if( !is.null(x$results) ) {
    cat( "** PBAT RESULTS **:\n", file=f );
    write.table( x$results, file=f, sep=" & ", row.names=FALSE, quote=FALSE );  ## sep consistent with Christoph
    cat( "\n\n", file=f );
  }

  if( !is.null(x$rcode) && x$rcode!="") {
    cat( "** R SOURCE CODE (all that follows): **\n", file=f );
    close(f);
    ##write( x$rcode, file=f );
    ##cat( "\n\n", file=f );
    file.append( filename, x$rcode );
  }else{
    close(f);
  }
  return(invisible());
}

write.pbat.csv <- function(x, filename) {
  filename <- str.file.extension(filename,extension='csv');
  
  quotify <- function( strList ) {
    for( i in 1:length(strList) )
      strList[i] <- paste("\"",strList[i],"\"");
    return( strList );
  }
  
  ## updates 01/20/2006
  f <- file( filename, "w" );
  #cat( paste("PBAT ",as.character(x$fbat),"\n",sep=""), file=f );
  if( !is.null(x$call) ) {
    cat( "** FORMULA **\n", file=f );
    write( x$call, file=f ); 
    cat( "\n\n", file=f ); ## all fine
  }
  if( !is.null(x$pbat.call) ) {
    cat( "** PBAT BATCH FILE: **\n", file=f );
    write( quotify(x$pbat.call), file=f );
    cat( "\n\n", file=f );
  }
  if( !is.null(x$results) ) {
    cat( "** PBAT RESULTS **:\n", file=f );
    write.table( x$results, file=f, sep=",", row.names=FALSE, quote=FALSE );
    cat( "\n\n", file=f );
  }

  if( !is.null(x$rcode) && x$rcode!="") {## major changes here
    cat( "** R SOURCE CODE (all that follows; in quotes for csv format): **\n", file=f );
    close(f);
    ##printFile2FileQuotesAppend( filename, x$rcode );
    printFile2FileQuotesAppend( x$rcode, filename );  ## syntax backwards!
  }else{
    close(f);
  }
  
  return(invisible());
}


####################################################################
#                                                                  #
# Functions that work on the files.                                #
#                                                                  #
####################################################################

## 05/23/06 - MASSIVE alterations in this coding!
############################################################################
# \name{pbat.files}                                                        #
# \description{                                                            #
#   Typically this function will not be run by the user, and instead,      #
#   you will use the related \code{\link{pbat.pc.files}},                  #
#   \code{\link{pbat.gee.files}}, \code{\link{pbat.logrank.files}}         #
#   functions.                                                             #
#                                                                          #
#   This is mostly included for future expansion.                          #
# }                                                                        #
# \arguments{                                                              #
#   \item{pedfile}{Name of the pedigree file, as a string.  Extension is   #
#     not needed, and the phenotype file is assumed to have similar        #
#     filename unless otherwise specified (by setting                      #
#     'phefile="othername.phe"'). }                                        #
#   \item{tempPbatCommand.txt}{Name of the command file for pbat to read   #
#     in.}                                                                 #
#   \item{...}{Pbat options, as described in the function                  #
#     \code{\link{pbat.create.commandfile}}. }                             #
############################################################################
pbat.files <- function( pedfile, phefile,
                        fbat="gee",
                        commandfile="",
                        logrank.outfile="",
                        LOAD.OUTPUT=TRUE,
                        ... )
{
  curTimeStamp = getTimeStamp();
  if( isTimeStamped(pedfile) ) {
    curTimeStamp = extractTimeStamp( pedfile );
  }else if( isTimeStamped(phefile) ) {
    curTimeStamp = extractTimeStamp( phefile );
  }
  ##print( "curTimeStamp" );
  ##print( curTimeStamp );
  ## It time-stamps correctly, even with symbolic

  if( commandfile=="" )
    commandfile <- paste("pbat",curTimeStamp,"cmd.txt",sep="");
  
  # Create the command file
  logfile <- pbat.create.commandfile( pedfile=pedfile, phefile=phefile, fbat=fbat, ## 05/31/06 fix
    commandfile=commandfile, ... );
  ##print( logfile ); ## DEBUG ONLY

  if( logrank.outfile=="" & fbat=="logrank" ) { # so we get the same timestamp :)
    logrank.outfile <- paste( "pbat",curTimeStamp,".R", sep="" );
  }

  # Kill the 'spluscode.txt' file
  if( file.exists( "spluscode.txt" ) )
    file.remove( "spluscode.txt" );
  
  # call the system 'pbat' command
  #TMPOUT <- paste( "pbat", curTimeStamp, "output.txt", sep="" );
  ## 01/09/2006 rewrite for multiple processes
  ## 01/18/2006 fix to allow spaces in windows
  ## 01/24/2006 Windows version of system spin-locks!! removing completely
  ## 05/23/2006 Altering for potential new clustering method...
  mode <- pbat.getmode()
  numProcesses <- mode$jobs;
  CLUSTER.TIME <- mode$refresh;

  if( mode$mode == "single" ){
    ## The original
    clearCommands()
    if( isWindows() ) {
      addCommand( paste( "\"", pbat.get(), "\" \"", commandfile, sep="" ) );
    }else{
      ##print( "SINGLE Command" );
      ##print( paste( pbat.get(), commandfile ) );
      addCommand( paste( pbat.get(), commandfile ) );
    }
    runCommands();
  }else if( mode$mode != "cluster" ){
    ## The original multiple spawning method
    clearCommands();
    for( i in 1:numProcesses ) {
      if( isWindows() ) {
        addCommand( paste( "\"", pbat.get(), "\" \"", commandfile, "\"",
                          " ", i, " ", numProcesses, sep="" ) );
      }else{
        ##print( "MULTIPLE Command" );
        ##print( paste( pbat.get(), commandfile, i, numProcesses ) );
        addCommand( paste( pbat.get(), commandfile, i, numProcesses ) );
      }
    }
    runCommands();
  }else{
    ## The bsub method for clusters... rather inefficient, but it's for clusters that don't support the above.
    
    clearCommands(); ## remember otherwise it spin-locks
    
    filenameSH <- rep( "", numProcesses );
    filenameTouch <- rep( "", numProcesses );
    finished <- 0;
    
    for( i in 1:numProcesses ) {
      ## set the filenames
      filenameSH[i] <- paste( 'pbatCluster', curTimeStamp, '.', i, '.sh', sep="" );
      filenameTouch[i] <- paste( 'pbatCluster', curTimeStamp, '.', i, '.touch', sep="" );

      ## create the shell file
      file <- file( filenameSH[i], 'w' );
      ##print( "CLUSTER Command" );
      ##print( paste( pbat.get(), commandfile, i, numProcesses ) );
      catn( pbat.get(), commandfile, i, numProcesses, file=file );
      catn( 'touch', filenameTouch[i], file=file );  ## add an is.finished() command for some people?
      close(file);

      ## run the shell file (well, add it to the queue)
      addCommand( paste( mode$cluster, filenameSH[i] ) );
    }

    ## now run all of the shell files
    runCommands();

    ## wait for all the files to have been 'touched'
    if( CLUSTER.TIME>0 ){
      while( finished != numProcesses ) {
        if( file.exists(filenameTouch[finished+1]) ){
          ## The next file in line finished
          finished <- finished+1;
        }else{
          ## It isn't finished, so sleep so it doesn't eat up CPU time
          Sys.sleep( CLUSTER.TIME );
        }
      }
    }else{
      print( "Commands have been batched. When you quit, save your workspace [q(save='yes')], and restore it later. Check whether it has finished with is.finished(res) to be sure." );
      LOAD.OUTPUT <- FALSE;
    }

    ## Delete all the .sh and .touch files
    for( i in 1:numProcesses ) {
      file.remove( filenameSH[i] );
      file.remove( filenameTouch[i] );
    }
  }

  ##if( !file.exists( TMPOUT ) )
  ##  stop( "Either pbat execution was terminated, or pbat executable couldn't be found.  In the latter case, you need the 'pbat' software - see set.pbat() for more details and a web-link to download." ); # this might not work... seems to so far though :)
  ##printFile( TMPOUT ); # So we can see the pbat output... anyway to tell if erred?

  # if logrank, plot the picture
  if( fbat=="logrank" )
    pbat.logrank.replot(save=logrank.outfile);

  # future expansion, maybe load in some of the results (if gee/pc),
  #  and sort them by conditional power...

  # Get the getPbatlog()
  ##print( "loading logfile" );
  ##print( logfile );
  ##res <- loadPbatlog( logfile );
  if( LOAD.OUTPUT ) {
    res <- loadPbatlogExtended( logfile ); ## 01/09/2006
  }else{
    res <- NULL;
  }

  pbatObj <- list();
  pbatObj$call <- NULL; # set by upper function
  pbatObj$pbat.call <- res$call;
  pbatObj$results <- res$data;
  pbatObj$rcode <- logrank.outfile;
  pbatObj$results.logfile <- logfile;
  if( fbat!="logrank" ) pbatObj$rcode <- "";
  pbatObj$fbat <- fbat;
  pbatObj$commandfile <- commandfile; ## Addition so it can be cleaned
  ##print( names(pbatObj) );
  class(pbatObj) <- c("pbat", "list");
  ##print( names(pbatObj) );

  if( CLUSTER.TIME==0 && mode$mode=='cluster' ){
    ## new additions for is.finished
    pbatObj$filenameTouch <- filenameTouch;
    pbatObj$filenameSH <- filenameSH;
  }
  
  return( pbatObj );
}

## CLUSTER mode alteration 1
## Check to see if the cluster mode has finished
is.finished <- function( pbatObj=NULL, clean=TRUE ){
  if( is.null(pbatObj) ) pbatObj <- pbat.last();

  if( is.null(pbatObj$filenameTouch) || length(pbatObj)==0 )
    return( NA );

  ## see if all the filenames have been touched
  for( i in 1:length(pbatObj$filenameTouch) ){
    if( !file.exists(pbatObj$filenameTouch[i]) )
      return(FALSE);
  }

  ## and clean up if they've finished
  if( clean ){
    for( i in 1:length(pbatObj$filenameTouch) ){
      file.remove( pbatObj$filenameSH[i] );
      file.remove( pbatObj$filenameTouch[i] );
    }
  }
  return( TRUE );
}

## CLUSTER mode alteration 2
## Reloading in the output file
pbat.load <- function( pbatObj=NULL ){
  if( is.null(pbatObj) ) pbatObj <- pbat.last();

  pbatObj$results <- loadPbatlogExtended( pbatObj$results.logfile );
  return( pbatObj );
}

## CLUSTER mode alteration 3
pbat.concatenate <- function( pbatObj=NULL, filename="myResults.txt", clean=FALSE ){
  if( is.null(pbatObj) ) pbatObj <- pbat.last();

  loadPbatlogConcatenate( pbatObj$results.logfile, filename, clean );
}

####################################################################
#                                                                  #
# Functions on the 'phe' and 'ped' objects.                        #
#                                                                  #
####################################################################

## Ordering on phe and ped here is opposite to that of
##  pbat.files! Not so good! But we potentially break
##  others coding if we change it now.
####################################################################
# pbat.obj(..)           <EXTERNAL>                                #
# DESCRIPTION: takes a phe object and a ped "object" {not          #
#   explicitly defined classes, but as described in write.phe()    #
#   and write.ped() }                                              #
# PARAM phe         phe "object" as described in write.phe()       #
#       ped         ped "object" as described in write.ped()       #
#       fileprefix  prefix of the output datafile (phe & ped must  #
#                                                  match)          #
# (PARAM) ...  pbat options, as referenced to in the function      #
#                pbat.files().                                     #
####################################################################
pbat.obj <- function( phe, ped, file.prefix, LOAD.OUTPUT=TRUE, ... ) {
  #write.phe( paste( file.prefix, ".phe", sep="" ), phe );
  #write.ped( paste( file.prefix, ".ped", sep="" ), ped );
  #return( pbat.files( file.prefix, ... ) );

  ## Write out files to disk if necessary
  if( !is.sym(ped) ) {
    write.ped( paste( file.prefix, ".ped", sep="" ), ped );
    pedname <- file.prefix;
  }else{
    pedname <- get.sym( ped );
  }

  if( !is.sym(phe) ) {
    write.phe( paste( file.prefix, ".phe", sep="" ), phe );
    phename <- file.prefix;
  }else{
    phename <- get.sym( phe );
  }

  ## run the command
  res <- pbat.files( pedname, phename, LOAD.OUTPUT=LOAD.OUTPUT, ... );

  ## take note of what was symbollic (for the clean routine)
  ##  Nevermind - the clean routine doesn't need this!
  #if( is.sym(ped) )
  #  res$pedSym <- TRUE;
  #if( is.sym(phe) )
  #  res$pheSym <- TRUE;

  #if( CLEAN & LOAD.OUTPUT )
  #  pbat.clean( res ); ## doesn't delete the logrank stuff

  ## and return the result
  return( res );
}

####################################################################
# pbat.logrank.replot()                                            #
# DESCRIPTION: (Re)plots the survival graphs from running either   #
#   pbat.logrank(...) or pbat.logrank.files(...)                   #
# PARAM  save  filename to copy the current plotting commands to   #
# PARAM  load  filename to load previously 'save'd plots           #
#               [ or just call source(load) ]                      #
####################################################################
pbat.logrank.replot <- function( save="", load="" ) {
  if( save!="" && load!="" )
    stop( "You can only save or load at a time!" );
  
  if( save!="" ) {
    file.copy( "spluscode.txt", str.file.extension(save,extension=".R") );
  }else if(load!="" ) {
    source( str.file.extension(load,extension="R") );
  }else{
    source( "spluscode.txt" ); # call on load or otherwise
  }
}

####################################################################
# printFile(...)                                                   #
# DESCRIPTION: Like running cat <filename> from unix prompt,       #
#   but from within R.                                             #
# PARAM filename  Name of the file to print out.                   #
####################################################################
printFile <- function( filename ) {
  file = file(filename, "rt", blocking=FALSE );
  ##on.exit(close(file));
  ##print( readLines(file) ); ## Alteration 11/15/2005
  lines <- readLines(file);
  if( length(lines)>=1 ) {
    for( i in 1:length(lines) )
      cat( lines[i], "\n" );
  }
  close(file);
}

####################################################################
# printFile(...)                                                   #
# DESCRIPTION: Like running cat <filename> from unix prompt,       #
#   but from within R.                                             #
# PARAM filename  Name of the file to print out.                   #
####################################################################
printFile2FileQuotesAppend <- function( filename, filenameAppend ) {
  file = file(filename, "rt", blocking=FALSE );
  fileA = file(filenameAppend, "at", blocking=FALSE );
  ##on.exit(close(file));
  ##on.exit(close(fileA));
  ##print( readLines(file) ); ## Alteration 11/15/2005
  lines <- readLines(file);
  if( length(lines)>=1 ) {
    for( i in 1:length(lines) )
      cat( "\"", lines[i], "\"", "\n", sep="", file=fileA );
  }
  close(file); ## should fix
  close(fileA);
}

####################################################################
#                                                                  #
#                                                                  #
#                                                                  #
#                                                                  #
#                                                                  #
#                                                                  #
#                                                                  #
#                                                                  #
#                                                                  #
#                                                                  #
####################################################################
