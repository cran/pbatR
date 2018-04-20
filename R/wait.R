####################################################################
# Thomas Hoffmann                                                  #
# CREATED:  some time ago                                          #
#                                                                  #
# DESCRIPTION:                                                     #
#  This file contains the interfacing to the c++ routines to       #
#  control multiple processors (the spawning method).              #
####################################################################

## Build this with the following command:
## R CMD SHLIB wait.cpp

## Windows is horrible, and can be built with something similar to:
##  set path=%path%;c:\perl\bin;c:\apps\Rtools\bin;C:\Mingw\bin;C:\Program Files\R\rw2010\bin
##  set path=c:\perl\bin;c:\apps\Rtools\bin;C:\Mingw\bin;C:\Program Files\R\rw2010\bin
##  Rcmd.exe SHLIB wait.cpp
## well it's not horrible, it's just that the path isn't set up automatically for us

## dyn.load("wait")

addCommandR <- function( str ) { # Can't be addCommand, or it interacts with some new stuff they did...
  .C( "addCommand", as.character(str) );
  return( invisible() );
}

clearCommandsR <- function( str ) {
  .C( "clearCommands" );
  return( invisible() );
}

runCommandsR <- function( str ) {
  .C( "runCommands" );
  return( invisible() );
}
