####################################################################
# Thomas Hoffmann                                                  #
# CREATED:     06/07/2005                                          #
# RE-CREATED:  06/20/2005                                          #
# MODIFIED:    06/29/2005                                          #
#                                                                  #
# DESCRIPTION: Methods for setting and getting the pbat binary     #
#    executable filename, and methods for reading/writing          #
#    both .phe/.ped files.                                         #
####################################################################

isWindows <- function()
  return( Sys.info()["sysname"]=="Windows" );

####################################################################
#                                                                  #
# PBAT PATH GET AND SET FOR THE EXECUTABLE.                        #
#                                                                  #
####################################################################



####################################################################
# pbat.get.fname(...)                                              #
# DESCRIPTION: gets $HOME/.pbat.R                                  #
# RETURN: string of what was just described.                       #
####################################################################
pbat.get.fname <- function() {
  if( Sys.getenv("R_USER")=="" )
    return( "~/.pbat.R" );  # A *nix oddity...
  return( paste( Sys.getenv("R_USER"), "/.pbat.R", sep="" ) ); #win
}


####################################################################
# pbat.get()                                                       #
# DESCRIPTION: Returns the current stored name of the pbat         #
#   executable.  NOTE: This is saved between sessions for a given  #
#   user.                                                          #
# RETURN  current stored name of the pbat executable.              #
####################################################################
pbat.get <- function() {
  filename <- pbat.get.fname();
  if( file.exists(filename)==FALSE )
    return("pbat"); # default program name

  file <- file( filename, "r" );
  on.exit( close(file) );

  tmp <- readLines(file, 1, TRUE);
  if( tmp=="" ) tmp="pbat";
  return( tmp );
}


####################################################################
# pbat.set(...)                                                    #
# DESCRIPTION: Sets the name of the pbat executable.  NOTE: This is#
#   retained between sessions for a given user.                    #
# PARAM  executableStr  String of the pbat executable name, e.g.   #
#                         "c:/pbat/pbat25.exe"                     #
####################################################################
pbat.set <- function( executableStr="", CLEAR=FALSE ) {
  if( executableStr=="" && CLEAR==FALSE ) {
    ;# now do the work!
    form <- tktoplevel();
    if( isWindows() ) {
      executableStr <- tclvalue(tkgetOpenFile(filetypes="{{Pbat Executable} {.exe}}"));
    } else{
      executableStr <- tclvalue(tkgetOpenFile()); # Unix exe's have no extension!  You've been in windows too long!
    }
    if( !nchar(executableStr) ) {
      tkdestroy(form);
      warning( "Pbat not set." );
      return(invisible());
    }
    tkdestroy(form);
  }
  if( executableStr!="" & file.exists(executableStr)==FALSE )
    warning( paste("File may not exist.  If '",
                   executableStr,
                   "' is in your path, this is safe to ignore.  Make sure you are using '\\\\' or '/'.",
                   sep="") );

  if( CLEAR==TRUE )
    executableStr = "";

  file <- file( pbat.get.fname(), "w" );
  on.exit( close(file) );

  cat( executableStr, file=file );
  cat( "\n", file=file );
}  


####################################################################
#                                                                  #
# .phe & .ped FILE READING AND WRITING                             #
#                                                                  #
####################################################################


####################################################################
# read.badheader(...)                                              #
# DESCRIPTION: Use when the number of headers does not match the   #
#              data.                                               #
# WARNING: DO NOT do read.badheader( ...., header=T )              #
# PARAM: file  file or filename                                    #
#        sep   seperator                                           #
#        ...   other parameters to 'read.table'; header=T not sup. #
# RETURN $header  headers read from the file                       #
#        $table   table read from the file (read.table(header=F))  #
####################################################################
read.badheader <- function( file, sep="", lowercase=TRUE, ... ) {
  # The idea behind this is we read in the header, and then piggy-back
  #  onto the read.table function!
  # Some of this coding was taken from read.table...

  # Open the file (unless it's already a file)
  if (is.character(file)) {
    file <- file(file, "r")
    on.exit(close(file))
  }
  
  # Read in the header line.
  header <- scan(file, what="", sep=sep,
                 nlines=1, quiet=TRUE, skip=0, strip.white=TRUE,
                 blank.lines.skip=TRUE, comment.char="#" );
  if( lowercase )
    header <- tolower(header); # convenience

  # Read in the rest of the table, prevent that error described above
  #  in the "WARNING" section.
  table <- read.table( file, sep=sep, header=FALSE, ... );

  return( list( header=header, table=table) );
} ### VERIFIED ###

####################################################################
# write.badheader(...)                                             #
# DESCRIPTION: Use when the number of headers does not match the   #
#              data; writes to disk.                               #
# PARAM: file       file (connection)  or filename                 #
#        dataframe  data frame to write to disk                    #
#        header     header to write                                #
# (PARAM):  sep        seperator                                   #
#           col.names  T/F whether to print column names           #
#           row.names  T/F whether to print row names              #
#           ...        other parameters to 'read.table'            #
####################################################################
write.badheader <- function( file, dataframe, header,
                             col.names=FALSE, row.names=FALSE,
                             sep=" ", ... ) {
  # like read.badheader - piggy-back on write.table :)
  
  # Open the file (unless it's already a file)
  if (is.character(file)) {
    file <- file(file, "w")
    on.exit(close(file))
  }
  
  # write the header
  cat( header, file=file, sep=sep );
  cat( "\n", file=file );

  # write the table
  write.table( dataframe, file, col.names=col.names, row.names=row.names, sep=sep, quote=FALSE );
} ## VERIFIED ##


####################################################################
# dfr.r(...)                                                       #
# Removes columns from a dataframe, searching by column names.     #
# PARAM df      data frame                                         #
#       strVec  vector of strings representing column names to     #
#                eliminate                                         #
# RETURNS dataframe with columns removed                           #
####################################################################
dfr.r <- function( df, strVec ) {
  ####################################
  # data frame remove, using strings #
  ####################################
  dfr <- function( df, str ) {
    if( sum(names(df)==str) != 1 ) {
      warning("dfr: no headers match, or more than one header matches");
      return( df );
    }
    return( df[-which(names(df)==str)] );
  }

  for( i in 1:length(strVec) )
    df <- dfr( df, strVec[i] );

  return( df );
}

as.phe <- function( ... ) {
  return( write.phe(NULL,...) );
}
as.ped <- function( ... ) {
  return( write.ped(NULL,...) );
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
