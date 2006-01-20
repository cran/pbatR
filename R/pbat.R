####################################################################
# Thomas Hoffmann                                                  #
# CREATED:     06/21/2005                                          #
# MODIFIED:    07/08/2005                                          #
# DESCRIPTION: The major pbat-interface commands.                  #
####################################################################

####################################################################
#                                                                  #
# Making pbat be a class....                                       #
#                                                                  #
####################################################################
pbat <- function( x, ... )
  UseMethod( "pbat" );

summary.pbat <- function( object, ... ) {
  # print out the pretty call
  if( !is.null(x$call) ) {
    print( "Call:" );
    print( x$call );
  }

  # now, print out the results?
  print( "Results:" );
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
  print( "Class members:" );
  print( " $call             The formula used." );
  print( " $pbat.call        The batch-file commands sent to pbat." );
  print( " $results          ** The results of pbat execution in a data.frame object. **" );
  print( " $results.logfile  Filename of the raw results of pbat (may also have extension .hdr & .dat); esp. useful if pbat outputs an unknown format that fails to be read in. Note this is _in the current working directory_." );
  print( " $rcode            Name of the file containing plots for logrank." );
  print( " $fbat             'pc','gee', or 'logrank'" );
  print( "Type '?pbat' for more details." );
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
# Hmm... lots of redundant code here... thought it would make the  #
#  options clearer, but I'm not so sure anymore...                 #
#                                                                  #
####################################################################

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
pbat.files <- function( pedfile, fbat="gee",
                        commandfile="",
                        logrank.outfile="",
                        ... )
{
  curTimeStamp = getTimeStamp();
  if( isTimeStamped(pedfile) )
    curTimeStamp = extractTimeStamp( pedfile );
  
  if( commandfile=="" )
    commandfile <- paste("pbat",curTimeStamp,"cmd.txt",sep="");
  
  # Create the command file
  logfile = pbat.create.commandfile( pedfile, fbat=fbat,
    commandfile=commandfile, ... );

  if( logrank.outfile=="" & fbat=="logrank" ) { # so we get the same timestamp :)
    #logrank.outfile <- paste( substring(logfile,1,strlen(pedfile)-3), "phe", sep="" );
    logrank.outfile <- paste( "pbat",curTimeStamp,".R", sep="" );
  }

  # NEW NEW
  # Kill the 'spluscode.txt' file
  if( file.exists( "spluscode.txt" ) )
    file.remove( "spluscode.txt" );
  # NEW NEW
  
  
  # call the system 'pbat' command
  #system( paste( pbat.get(), commandfile ) );
  TMPOUT <- paste( "pbat", curTimeStamp, "output.txt", sep="" );
  #if( file.exists( TMPOUT ) )
  #  file.remove( TMPOUT ); # see if pbat was killed by user?
  ##print( "COMMAND TO BE RUN:" ); # DEBUG ONLY!!!
  ##print( paste( pbat.get(), commandfile, "> ", TMPOUT ) );
  ##print( "PATH:" );
  ##print( getwd() );
  ##system( paste( pbat.get(), commandfile, "> ", TMPOUT ), intern=TRUE );
  # 10/07/2005 -- output redirection failing!!!
  ##  Instead it should fail when trying to load some of the input in...

  ## 01/09/2006 rewrite for multiple processes
  ## 01/18/2006 fix to allow spaces in windows
  numProcesses <- pbat.getNumProcesses();
  if( numProcesses == 1 ) {
    if( isWindows() ){
      system( paste( "\"", pbat.get(), "\" \"", commandfile, "\"", sep="" ),
              intern=TRUE );
    }else{
      system( paste( pbat.get(), commandfile ), intern=TRUE );
    }
  }else{
    clearCommands();
    for( i in 1:numProcesses ) {
      if( isWindows() ) {
        addCommand( paste( "\"", pbat.get(), "\" \"", commandfile, "\"",
                           " ", i, " ", numProcesses, sep="" ) );
      }else{
        addCommand( paste( pbat.get(), commandfile, i, numProcesses ) );
      }
    }
    runCommands();
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
  res <- loadPbatlogExtended( logfile ); ## 01/09/2006

  pbatObj <- list();
  pbatObj$call <- NULL; # set by upper function
  pbatObj$pbat.call <- res$call;
  pbatObj$results <- res$data;
  pbatObj$rcode <- logrank.outfile;
  pbatObj$results.logfile <- logfile;
  if( fbat!="logrank" ) pbatObj$rcode <- "";
  pbatObj$fbat <- fbat;
  ##print( names(pbatObj) );
  class(pbatObj) <- c("pbat", "list");
  ##print( names(pbatObj) );
  return( pbatObj );
}


####################################################################
#                                                                  #
# Functions on the 'phe' and 'ped' objects.                        #
#                                                                  #
####################################################################

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
pbat.obj <- function( phe, ped, file.prefix, ... ) {
  write.phe( paste( file.prefix, ".phe", sep="" ), phe );
  write.ped( paste( file.prefix, ".ped", sep="" ), ped );
  return( pbat.files( file.prefix, ... ) );
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
