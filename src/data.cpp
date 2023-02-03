/* Converts pbat output to a csv */

//#define _DATA_CPP_DEBUG_ // only defined when debugging, so I can run it from the command line

#include <iostream>
#include <fstream>
#include <stdio.h>
#include <stdlib.h>
#include <string>
#include <cstring>
using namespace std;

#include <R.h>

const int LINE_SIZE = 10000;
const int GROUP_LEN = 5;


// partially gets a line; if it's too big, it truncates the line.
// returns if it was a partial line
bool getPartialLine( ifstream &infile, char *str, int length )
{
  int i;
  for( i=0; i<length-1 && !infile.eof(); i++ ){
    str[i] = infile.get();
    if( str[i] == '\r' ) {
      i--;
      continue;  // windows just in case
    }
    if( str[i] == '\n' ) {
      str[i] = '\0'; // make it null terminate, and overwrite the '\n'
      return(false); // not a partial line
    }
  }

  str[i] = '\0'; // null terminate it
  if( infile.eof() ) {
    str[i-1] = '\0'; // otherwise it turns up a really wierd character at the end!!!
    return(false); // file didn't end in a '\n'
  }
  return(true); // it was a partial line
} // new addition

int numChars( const char *str, char c='&' )
{
  int count=0;

  int strLen = strlen( str );
  for( int i=0; i<strLen; i++ )
    if( str[i] == c ) count++;

  return(count);
}

bool containsChar( const char *str, char c='&' )
{
  int strLen = strlen( str );

  for( int i=0; i<strLen; i++ )
    if( str[i] == c )
      return( true );

  return( false );
}

bool firstIsGroup( const char *str )
{
  int start=0, strLen=strlen(str);
  for( start=0; start<strLen; start++ )
    if( str[start]!=' ' && str[start]!='\t' )
      break; // so start is set to before the first whitespace
  //cout << "start = " << start << endl;

  if( start > strLen-GROUP_LEN )
    return( false );

  //cout << "going to group comparison" << endl;

  if( str[start]=='G'
      && str[++start]=='r'
      && str[++start]=='o'
      && str[++start]=='u'
      && str[++start]=='p' )
    return( true );

  return( false );
}

void replaceChar( char *str, char find='&', char rep=',' )
{
  int strLen = strlen( str );

  for( int i=0; i<strLen; i++ )
    if( str[i]==find )
      str[i] = rep;
}

string headerFix( const char *str, char c=',' )
{
  string header = "\"";
  header += str;
  header += "\"";
  for( int i=0; i<(int)header.size(); i++ ){
    if( header[i] == c ){
      header = header.substr( 0, i ) + "\",\"" + header.substr( i+1, header.size() );
      i+=2; // need to advance past the comma since it moved!
    }
  }
  return( header );
}

void terminateStr( char *str )
{
  // removes white spaces and ',' off the end,
  //  as it's confusing read.csv
  int last;
  while( (last = (strlen(str)-1)) != -1
	 && (str[last] == ','
	     || str[last] == ' '
	     || str[last] == '\t') )
    str[last] = '\0';
}

// returns if touched (sometimes empty if you cut it up too damned much)
bool convertPbatlog( const char* pbatlogfile, const char* callfile,
		     const char* resultfile, int append=0 )
{
  bool touched=false;

  // open up the files
  ifstream infile( pbatlogfile );

  /*
  std::_Ios_Openmode mode = ios::out;
  if( append != 0 )
    mode = ios::out | ios::trunc;
  ofstream outfileCall( callfile, (std::_Ios_Openmode)append );
  ofstream outfileResult( resultfile, (std::_Ios_Openmode)append );
  */

  // really no need to distinguish -- append will create if necessary
  ofstream outfileCall( callfile, ios::app );
  ofstream outfileResult( resultfile, ios::app );

  // now start reading in the input
  char line[LINE_SIZE];
  strcpy( line, "" );
  bool pastAnd = false;
  bool firstTimePast = true;
  while( !infile.eof() ){
    // gets the input even if the line was too long...
    // [results lines should _never_ be to long]

    bool partial = getPartialLine( infile, line, LINE_SIZE-1 );

    ////cout << "line(" << line << ")" << endl;

    // and then continue along with the code...

    ////cout << line << endl;
    if( !pastAnd ){
      ////cout << " - placing into callfile" << endl;
      if( containsChar( line ) )
	pastAnd=true;
      else if( append == 0 ) {
	outfileCall << line;
	if( !partial )
	  outfileCall << endl;
      }
    }

    if( pastAnd ){
      //cout << " - placing into outfile" << endl;

      // need to replace all of the &'s with ,'s
      replaceChar( line );

      // new -- replace all the stupid *'s
      replaceChar( line, '*', ' ' );
      ////cout << line << endl;

      // see if it's a header / need a header
      if( firstTimePast == true ){
	firstTimePast = false;

	// first time past - need to see if there are headers...
	if( firstIsGroup( line ) ) {
	  // first was group!
	  if( append == 0 ) { // then need the header
	    //outfileResult << line << endl;

	    // we need to reformat the header
	    outfileResult << headerFix( line ) << endl;
	  }
          touched = true;
	}else{
          touched = true;

	  //first wasn't a group
	  if( append == 0 ) {
	    // then we need to create and put a header on it!
	    terminateStr( line ); // 1/22/07 update fix remove NA's...?
	    int numCols = numChars( line, ',' ) + 1;
	    ////cout << "numCols = " << numCols << endl;
	    for( int j=0; j<numCols; j++ ) {
	      outfileResult << "C" << j;
	      if( j<numCols-1 )
		outfileResult << ",";
	    }
	    outfileResult << endl;
	  }

	  // and still need to output the line anyway
	  terminateStr( line );
	  outfileResult << line << endl;
	}
      }else{
	// and then output the line
	terminateStr( line );
	outfileResult << line << endl;
      }
    }
  }

  // done, so close all the files
  infile.close();
  outfileCall.close();
  outfileResult.close();

  ////cout << " Touched status " << touched << endl;

  // and return if touched! (if there was actually any output in this file...)
  return( touched );
}

#ifdef _DATA_CPP_DEBUG_

int main( int argc, const char* argv[] )
{
  /*
  char str[100];
  strcpy( str, "testing ... ,,, , ," );
  terminateStr( str );
  cout << str << "#" << endl;

  cout << headerFix( "OH,I,WISH,I,WERE,A,PBAT" ) << endl;
  return(0);
  */

  if( argc != 5 ) {
    cout << "Usage: data <pbatlogfile> <callfile> <resultfile> <append>" << endl;
    return(0);
  }

  const char *pbatlogfile = argv[1];
  const char *callfile = argv[2];
  const char *resultfile = argv[3];
  char *junk;
  int append = (int)strtod( argv[4], &junk );

  cout << "pbatlogfile=" << pbatlogfile << endl;
  cout << "callfile=" << callfile << endl;
  cout << "resultfile=" << resultfile << endl;
  cout << "append=" << append << endl;

  // so far so good

  // start debugging the functions
  cout << "should be 5: " << numChars( "& & & 5 ands & &" ) << endl;
  cout << "should be 1: " << containsChar( "  foo&foo" ) << endl;
  cout << "should be 0: " << containsChar( "should be 0" ) << endl;

  cout << "should be 1: " << firstIsGroup( "Group   " ) << endl;
  cout << "should be 1: " << firstIsGroup( " Group  " ) << endl;
  cout << "should be 1: " << firstIsGroup( "  Group " ) << endl;
  cout << "should be 1: " << firstIsGroup( "   Group" ) << endl;
  cout << "should be 0: " << firstIsGroup( "    Grou" ) << endl;

  char strt[1000];
  strcpy( strt, "Oh & I & wish & I & were & a & PBAT & ..." );
  cout << "before replacement: " << strt << endl;
  replaceChar( strt );
  cout << "after replacement: " << strt << endl;

  // everything in the above works just fine...
  // so then we should be testing the real deal!

  convertPbatlog( pbatlogfile, callfile, resultfile, append );
  // and fully debugged!!! now all we have to do is bring it into p2bat
}

#else

extern "C" {
  // c++ version of loadPbatlog; _must_ be followed up in R
  //  by read.csv( resultfile )
  void launchPbatlog( char **pbatlogfile, char **callfile, char **resultfile, int *append )
  {
    convertPbatlog( *pbatlogfile, *callfile, *resultfile, *append );
  }

  // c++ version of loadPbatlogExtended; _must_ be followed up in R
  //  by read.csv( resultfile )
  void launchPbatlogExtended( char **pbatlogfile, char **callfile, char **resultfile, int *pieces )
  {
    if( *pieces == 0 ){
      //cout << "ERR: pieces == 0" << endl; // should never happen
      Rprintf("ERR: pieces == 0\n");
      return;
    }

    ////cout << "callfile " << *callfile << endl;
    ////cout << "resultfile " << *resultfile << endl;

    char currentlog[LINE_SIZE];

    /*
    // load in the first one without appending
    sprintf( currentlog, "%s_1_%i", *pbatlogfile, *pieces );
    convertPbatlog( currentlog, *callfile, *resultfile, 0 );

    // now load in the rest _with_ appending
    for( int i=2; i<=*pieces; i++ ){
      sprintf( currentlog, "%s_%i_%i", *pbatlogfile, i, *pieces );
      convertPbatlog( currentlog, *callfile, *resultfile, 1 );
    }*/

    // update 7/13/07
    // load in appending based on if it has been touched
    // - some of the output files can be empty...
    bool touched = false;
    for( int i=1; i<=*pieces; i++ ) {
      snprintf( currentlog, LINE_SIZE, "%s_%i_%i", *pbatlogfile, i, *pieces );
      //sprintf( currentlog, "%s_%i_%i", *pbatlogfile, i, *pieces );

      ////cout << "Parsing log '" << currentlog << "'" << endl;

      // make it so once touched, it _stays_ touched...
      bool newTouched = convertPbatlog( currentlog, *callfile, *resultfile, (int)touched );
      touched = touched || newTouched;

      //touched = true; // cheating / garbage
    }

    ////cout << "launchPbatlogExtended finished" << endl;
  }
}

#endif
