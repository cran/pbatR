## Build this with the following command:
## R CMD SHLIB wait.cpp

## Windows is horrible, and can be built with something similar to:
##  set path=%path%;c:\perl\bin;c:\apps\Rtools\bin;C:\Mingw\bin;C:\Program Files\R\rw2010\bin
##  set path=c:\perl\bin;c:\apps\Rtools\bin;C:\Mingw\bin;C:\Program Files\R\rw2010\bin
##  Rcmd.exe SHLIB wait.cpp

## dyn.load("wait")

addCommand <- function( str ) {
  .C( "addCommand", as.character(str) );
  return( invisible() );
}

clearCommands <- function( str ) {
  .C( "clearCommands" );
  return( invisible() );
}

runCommands <- function( str ) {
  .C( "runCommands" );
  return( invisible() );
}
