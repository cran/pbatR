// for debugging, compile with -D_KLUDGE_DEBUG_
// g++ kludge.cpp -D_KLUDGE_DEBUG_ -o kludge

#include <iostream>
#include <fstream>
#include <string>
#include <cstring>
using namespace std;

const int LINE_SIZE = 10000;
const int NUMREAD = 10;

char kludge_field_str[1000];

// returns the number of fields in line
int kludge_numfields( char *line )
{
  int numfields = 1;
  int index=0;
  while( line[index]!='\0' ){
    if( line[index] == ',' )
      numfields++;

    // go to the next character
    index++;
  }
  return( numfields );
}

void kludge_fill_fields( char *line, int* fieldStart, int* fieldLength )
{
  // start the first field
  int curfield=0;
  fieldStart[curfield] = 0;
  fieldLength[curfield] = 0;

  int index=0;
  while( line[index]!='\0' ){
    //cout << "index " << index << " fieldStart[0] " << fieldStart[0] << " line[index] " << line[index] << " curfield " << curfield << endl;
    if( line[index] == ',' ){
      curfield++;
      fieldLength[curfield] = 0;
      fieldStart[curfield] = index+1;
    }else if( fieldLength[curfield]==0 &&
	      (line[index]==' ' || line[index]=='\t') ) {
      // eliminate leading white spaces
      fieldStart[curfield] = index+1;
    }else{
      fieldLength[curfield]++;
    }
    index++;
  }
}

// extracts preceeding white spaces by previous design
//  and deceeding white spaces in the code
char* kludge_field( char *line, int field, int* fieldStart, int* fieldLength )
{
  //cout << "entered kludge_field";
  memcpy( kludge_field_str, &line[fieldStart[field]], sizeof(char)*fieldLength[field] );
  int curpos = fieldLength[field];
  kludge_field_str[ curpos ] = '\0';
  curpos--;
  //cout << kludge_field_str << endl;
  while( curpos>=0 && (kludge_field_str[curpos]==' ' || kludge_field_str[curpos]=='\t') ) {
    kludge_field_str[curpos] = '\0';
    curpos--;
  }

  return( kludge_field_str );
}

// debug routine
void kludgePrintFields( int numfields, char *line, int* fieldStart, int* fieldLength )
{
  int curfield;
  for( curfield=0; curfield<numfields; curfield++ )
    cout << "(" << kludge_field( line, curfield, fieldStart, fieldLength ) << ")";
  cout << endl;
}

bool field_is_star( char *line, int field, int* fieldStart, int* fieldLength ){
  /*
  char *strfield = kludge_field( line, field, fieldStart, fieldLength );
  if( strfield[0]=='*' )
    return( true );
  return( false );
  */

  // oops. on conversion this becomes _an empty field_
  char *strfield = kludge_field( line, field, fieldStart, fieldLength );
  //cout << "is_star(" << strfield << ") " << (strlen(strfield)==0) << endl;
  return( strlen(strfield)==0 );
}

bool is_number( char c ) {
  if( c=='0' ||
      c=='1' ||
      c=='2' ||
      c=='3' ||
      c=='4' ||
      c=='5' ||
      c=='6' ||
      c=='7' ||
      c=='8' ||
      c=='9' ) return( true );
  return( false );
}

bool field_is_numeric( char *line, int field, int* fieldStart, int* fieldLength ){
  char *strfield = kludge_field( line, field, fieldStart, fieldLength );
  bool prevWasNumber=false;
  bool foundFirstNumber=false;
  //bool foundPeriod=false; // unused apparently!
  int index=0;
  int len = strlen(strfield);
  for( index=0; index<len; index++ ) {
    if( is_number(strfield[index]) || strfield[index]=='.') {
      if( !foundFirstNumber ){
	foundFirstNumber=true;
	prevWasNumber=true;
      }else {
	if( !prevWasNumber )
	  return( false ); // needs to be contiguous
      }
    }
    index++;
  }
  //cout << "strfield (" << strfield << ") " << foundFirstNumber << endl;
  return( foundFirstNumber );
}

bool field_is_P( char *line, int field, int* fieldStart, int* fieldLength ) {
  char *strfield = kludge_field( line, field, fieldStart, fieldLength );
  if( strfield[0] == 'P'
      && strfield[1] == ' '
      && strfield[2] == ':' )
    return( true );
  return( false );
}


void fillFieldInfo( char *line, int* fieldStart, int* fieldLength,
		    int numfields,
		    bool *isStar, bool *isNumeric, bool *isP )
{
  for( int f=0; f<numfields; f++ ) {
    isStar[f] = field_is_star( line, f, fieldStart, fieldLength );
    isNumeric[f] = field_is_numeric( line, f, fieldStart, fieldLength );
    isP[f] = field_is_P( line, f, fieldStart, fieldLength );
  }
}

void printLine( char *line, int* fieldStart, int *fieldLength,
		int numfields,
		bool *printToken,
		ofstream &outfile )
{
  bool firstField = true;
  for( int f=0; f<numfields; f++ ) {
    if( printToken[f]) {
      char *field = kludge_field( line, f, fieldStart, fieldLength );
      if( !firstField ) outfile << ",";
      outfile << field;
      firstField = false;
    }
  }
  //outfile << endl;
}

extern "C" {
  void kludgeConvert( const char** infilename, const char** outfilename,
		      int *status ) {
    /* Assumptions:
     *  - we are reading in the .csv file converted already
     *    - this means that no line as a ',' at the end,
     *      so the number of ',' characters implies the length of the line
     *    - there is a header line, this represents the end-all be-all width
     *  - only two different column widths
     *  - Lines aren't longer than LINE_SIZE
     * first line is good
     */

    // Only called when read.table fails after our conversion routine
    //  because the number of columns isn't properly specified

    /* How it works:
     * - reads in NUMREAD lines of messed lines and NUMREAD lines of good code
     * - decide the number of columns in each, and how many need to remove
     *   - mark fields as character or integer
     *   - mark cols that are duplicates
     *   - mark cols as '***' cols
     *   - priority remove:
     *     - duplicates in all NUMREAD lines
     *     - '***' cols
     */

    /* OKAY.... that's not only way too complicated, it won't work!!
     * New plan -- only three sets of the bloody 'stars' in a row,
     *  followed by killing the wretched duplicates afterward
     */

    char line[LINE_SIZE];

    // get the header line
    char header[LINE_SIZE];

    // open the files
    ifstream infile( infilename[0] );
    ofstream outfile( outfilename[0] );

    // get the header line and size we expect
    infile.getline( header, LINE_SIZE-1 );
    int numfields = kludge_numfields( header );

    // output the header line
    outfile << header << endl;

    // info on the good line
    bool good_isstar[numfields];
    bool good_isnum[numfields];
    bool good_isp[numfields];
    bool found_good = false;
    int firstP=-1; // the first 'P :.*' field location

    while( infile.getline( line, LINE_SIZE-1 ) ) {
      // strip off the carriage return
      int llen;
      llen = strlen(line);
      if( line[llen]=='\r' || line[llen]=='\n' ) line[llen]='\0';
      llen = strlen(line);
      if( line[llen]=='\r' || line[llen]=='\n' ) line[llen]='\0';
      //cout << "line(" << line << ")" << endl;

      // we've gotten the next line into line
      // check the number of fields
      int curnumfields = kludge_numfields( line );
      if( numfields == curnumfields ) {
	// if we haven't found a good line yet, we need to copy some stuff first
	if( !found_good ) {
	  found_good = true;

	  int fieldStart[numfields];
	  int fieldLength[numfields];
	  kludge_fill_fields( line, fieldStart, fieldLength );
	  fillFieldInfo( line, fieldStart, fieldLength,  curnumfields,
			 good_isstar, good_isnum, good_isp );
	  // also find the firstP
	  for( int i=0; i<numfields; i++ ) {
	    if( good_isp[i] && firstP==-1 ) {
	      firstP=i;
	      break;
	    }
	  }
	}

	// just output it and keep going
	outfile << line << endl;
	continue;
      }else if( numfields > curnumfields ) {
	*status = (int)false;  // couldn't be loaded
	return;
      }

      // oh hell, we've got a problem...
      //cout << "problem line " << line << endl;
      //cout << "curnumfields " << curnumfields << endl;

      // filling in the fields
      int fieldStart[curnumfields];
      int fieldLength[curnumfields];

      // fill in the types arrays
      bool isstar[curnumfields];
      bool isnum[curnumfields];
      bool isp[curnumfields];
      bool printToken[curnumfields];

      kludge_fill_fields( line, fieldStart, fieldLength );
      fillFieldInfo( line, fieldStart, fieldLength,  curnumfields,
		     isstar, isnum, isp );
      //kludgePrintFields( curnumfields, line, fieldStart, fieldLength );
      int alterednumfields = curnumfields;
      int numStars = 0;
      char previous[500];
      strcpy( previous, "" );
      bool found_a_damned_p = false;

      //cout << "alterednumfields " << alterednumfields << " numfields " << numfields << endl;
      for( int i=0; i<curnumfields; i++ ) {
	printToken[i] = true;

	// stop if we're done
	if( alterednumfields<=numfields && (!isp[i]||found_a_damned_p))
	  continue;

	// try to remove stars
	if( isstar[i] ) {
	  //cout << "we found a star" << endl;
	  if( numStars==3 ) {
	    //cout << "and we've got three of them!" << endl;
	    // kill it!
	    printToken[i]=false;
	    alterednumfields--;
	  }else {
	    numStars++;
	  }
	}

	if( numStars==3 && isnum[i] ) {
	  // try to remove double digits
	  // it's always past these stars
	  strcpy( previous, kludge_field(line,i-1,fieldStart,fieldLength) );
	  char *curfield = kludge_field(line,i,fieldStart,fieldLength);
	  if( strcmp( previous, curfield ) == 0 ) {
	    // kill it
	    printToken[i] = false;
	    alterednumfields--;
	  }
	  //cout << "previous (" << previous << ") current(" << curfield << ")" << endl;
	}

	// new nasty special case
	if( !found_a_damned_p && isp[i]==true ) {
	  //cout << "found a damned p" << endl;
	  found_a_damned_p = true;

	  // find out how far from the left we really are...
	  int fromLeft=0, j;
	  for( j=0; j<i; j++ )
	    fromLeft += (int)printToken[j];

	  // find out if we need to shift then to the left...
	  if( firstP!=fromLeft ) {
	    // need to shift
	    int numShift = fromLeft - firstP;
	    //cout << "num shift = " << numShift << endl;
	    //cout << "alterednumfields = " << alterednumfields << endl;
	    int curpos = i-1;
	    while( numShift>0 && curpos>0 ) {
	      if( printToken[curpos] ) {
		printToken[curpos] = false;
		numShift--;
		alterednumfields--;
	      }
	      curpos--;
	    }
	  }
	}
      }

      // through with the line, are we okay now?
      if( alterednumfields > numfields ) {
	//cout << "alterednumfields size " << alterednumfields << endl;
	// we failed
	*status = (int)false;
	return;
      }

      // we're good, print the modified line
      printLine( line, fieldStart, fieldLength,
		 curnumfields,
		 printToken,
		 outfile );

      for( int a=alterednumfields; a<numfields; a++ )
	outfile << ",NA";
      outfile << endl;
    }

    outfile << endl;  // R warning 'incomplete final line...'

    *status = (int)true;
  }

  void kludgeConvertAwful( const char **filename, const char **outfilename )
  {
    // can't fail, first loops through to find max size,
    // then pads everything else to work with it!

    char line[LINE_SIZE];

    int maxcols = -1;

    // get the maximum column width
    ifstream infile( filename[0] );
    while( infile.getline( line, LINE_SIZE-1 ) ) {
      int numcols = kludge_numfields( line );
      if( numcols > maxcols ) maxcols=numcols;
    }
    infile.close();

    // reload to fit to maximum width
    infile.open( filename[0] );
    ofstream outfile(outfilename[0]);
    while( infile.getline( line, LINE_SIZE-1 ) ) {
      // strip off the carriage return
      int llen;
      llen = strlen(line);
      if( line[llen]=='\r' || line[llen]=='\n' ) line[llen]='\0';
      llen = strlen(line);
      if( line[llen]=='\r' || line[llen]=='\n' ) line[llen]='\0';

      // get the column width
      int ncols = kludge_numfields( line );
      outfile << line;
      for( int k=0; k<maxcols-ncols; k++ )
	outfile << ",NA";
      outfile << endl;
    }
  }
}



#ifdef _KLUDGE_DEBUG_

int main( int argc, const char* argv[] ) {
  if( argc != 4 ) {
    cout << "syntax: <progname> infile outfile1 outfile2" << endl;
    return(1);
  }

  cout << "infile: " << argv[1] << endl;
  cout << "outfile1: " << argv[2] << endl;
  cout << "outfile2: " << argv[3] << endl;

  int status = false;
  kludgeConvert( argv[1], argv[2], &status );
  cout << "status: " << status << endl;
  kludgeConvertAwful( argv[1], argv[3] );
  cout << "awful conversion completed." << endl;
  return( status );
}

#endif
