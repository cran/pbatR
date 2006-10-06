pbat.help <- function() {
  cat( "-----------------------------------------------\n" );
  cat( date(), "P2BAT Report\n" );
  cat( "-----------------------------------------------\n" );
  cat( "OS:   ", version$os, "\n", sep="" );
  cat( "ARCH: ", version$arch, "\n", sep="" );
  cat( "R:    ", version$version.string, "\n", sep="" );
  cat( "-----------------------------------------------\n" );
  cat( "** P2BAT v",installed.packages()["pbatR","Version"],"**\n", sep="" );
  cat( "pbatR v", installed.packages()["pbatR","Version"], " (check to ensure this is most recent [http://www.people.fas.harvard.edu/~tjhoffm/pbatR.html])\n", sep="" );
  cat( "pbat v? \n" );
  cat( "[Run the binary/executable file by clicking on it in windows to find, or running it from the command prompt if that doesn't work in linux.]\n" );
  cat( "-----------------------------------------------\n" );
  cat( "pbat binary/executable: '", pbat.get(), "'\n", sep="" );
  if( file.exists( pbat.get() ) ) {
    cat( " [[binary exists]]\n" );
  }else{
    cat( " Ensure this file is in your path -- this warning indicates that it could not be found. If you get errors similar to 'pbat not found', see pbat.set with '?pbat.set' from the R command line, and try setting it to the full path to pbat.  If you are in unix, try from the command prompt (outside of R) the command 'which pbat' or 'which pbat32', e.g., to get the full path.\n" );
  }
  mode <- pbat.getmode();
  cat( "p2bat mode: mode='", mode$mode, "', jobs='", mode$jobs, "', cluster='", mode$cluster, "', refresh='", mode$refresh, "'\n", sep="" );
  cat( "-----------------------------------------------\n" );
  cat( "Assuming the above does not solve your problem, please describe, and include the text above if possible (don't forget to fill in 'pbat version' if you don't mind), and send to the package maintainer. Thanks.\n" );
  cat( "GUI [pbat()] / command line [pbat.m(...)] / other?\n" );
  cat( "function erroring on?\n" );
  cat( "Please describe:\n" );
  cat( "\n" );
  cat( "[For further information on this package, please type '?pbat'.]\n" );
  cat( "[The webpage [http://www.people.fas.harvard.edu/~tjhoffm/pbatR.html] also provides detailed installation instructions.]\n" );
}
