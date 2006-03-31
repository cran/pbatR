#ifdef WAIT_WINDOWS_DEFINE

#include <windows.h>
#include <iostream>
#include <string>
#include <fstream>
#include <vector>
using namespace std;

/* PROTOTYPES */

// launching the PBAT
HANDLE launchRedirectedPBAT( HANDLE out, HANDLE in, HANDLE err, string executable );

// handling the input and output threads
void handleOutputThread( HANDLE h );
DWORD WINAPI handleInputThread( LPVOID lpvThreadParam );

// running the program
int run( char *arg1 );

/* GLOBAL VARS */

// some global vars (to talk b/n the threads)
bool threadRun = true;
vector<string> commands;
bool justGotInput = false;

/* THE CODE */

/*
// MAIN
int main( int argc, char *argv[] )
{
  cout << "sh2.cpp v3" << endl; // version check debug

  // the first argument (if executed correctly)
  char arg1[1000];
  
  // check the usage of the program
  if( argc < 2 ){
    cout << "Usage: sh.exe file.sh" << endl;
    return(0);
  }else
    strcpy( arg1, argv[1] );
  
  run( arg1 );
}
*/

// arg1 is the name of the 'sh' file to open
int run( char *arg1 )
{
  // the name of the sh file that we are trying to open (DEBUG)
  cout << arg1 << endl;

  // Read in the input.
  // We _EXPECT_ this to be in a particular format (since we created it!):
  // [[empty line]]
  // executable <<EOF
  // ...
  // EOF
  ifstream infile( arg1 );
  string line;
  getline( infile, line ); // get the empty line
  cout << "LINE: " << line << endl;
  string executable;
  infile >> executable;
  cout << "Executable: " << executable << endl;
  getline( infile, line );
  while( getline( infile, line ) ) {
    if( line != "EOF" ){
      cout << "LINE: " << line << endl;
      commands.push_back( line );
    }else{
      cout << "CLOSING LINE: " << line << endl;
    }
  }

  // So all of the necessary commands have been pushed
  //  on to the vector commands!

  /*****************************************************/

  // 'file' handles for the input and output redirection
  HANDLE outrTemp, outr, outw, inwTemp, inr, inw, errw;
  
  // security attrs
  SECURITY_ATTRIBUTES sa;
  memset( &sa, 0, sizeof(SECURITY_ATTRIBUTES) );
  sa.nLength = sizeof(SECURITY_ATTRIBUTES);
  //sa.lpSecurityDescriptor = NULL;
  sa.bInheritHandle = true;
  
  // creating the ouput pipe for the child
  if( !CreatePipe( &outrTemp, &outw, &sa, 0 ) ) {
    cout << "Couldn't create the child output pipe." << endl;
    return( 1 );
  }

  // duplicate some of the pipes in case that's why the PBAT command line is behaving so erratically

  // handle for the ouput writing
  if( !DuplicateHandle( GetCurrentProcess(), outw, GetCurrentProcess(), &errw, 0, TRUE, DUPLICATE_SAME_ACCESS) ) {
    cout << "Couldn't duplicate the handle for output writing." << endl;
    return( 1 );
  }
  
  // handle for the input
  if( !CreatePipe( &inr, &inwTemp, &sa, 0 ) ) {
    cout << "Could not create the pipe for the input." << endl;
    return( 1 );
  }
  
  // Create handles
  if( !DuplicateHandle( GetCurrentProcess(), outrTemp, GetCurrentProcess(), &outr, 0, FALSE, DUPLICATE_SAME_ACCESS) ) {
    cout << "Couldn't duplicate the handle." << endl;
    return( 1 );
  }
  if( !DuplicateHandle( GetCurrentProcess(), inwTemp, GetCurrentProcess(), &inw, 0, FALSE, DUPLICATE_SAME_ACCESS ) ) {
    cout << "Could not duplicate the handle." << endl;
    return( 1 );
  }
  
  // close some handles
  if( !CloseHandle(outrTemp)
      || !CloseHandle(inwTemp) )
    cout << "Couldn't close the handles that shouldn't be inherited." << endl;
  
  // close the std input file so ReadFile(...) will fail when the input thread should die.
  HANDLE stdInput = GetStdHandle( STD_INPUT_HANDLE );
  if( stdInput == INVALID_HANDLE_VALUE ) {
    cout << "Couldn't get the std input handle." << endl;
    return( 1 );
  }

  // launch the coveted PBAT-child
  HANDLE childP = launchRedirectedPBAT( outw, inr, errw, executable );
  if( childP == NULL ) {
    cout << "Launching redirected PBAT failed." << endl;
    return( 1 );
  }
  
  // close all of the pipe handles
  if( !CloseHandle( outw )
      || !CloseHandle( inr )
      || !CloseHandle( errw ) ) {
    cout << "Couldn't close some of the pipe handles." << endl;
    return( 1 );
  }

  // the thread to handle the input for the PBAT-child
  DWORD idThr;
  HANDLE inpThr = CreateThread( NULL, 0, handleInputThread, (LPVOID)inw, 0, &idThr );
  if( inpThr == NULL ){
    cout << "Couldn't create the input thread for the PBAT-child." << endl;
    return( 1 );
  }
  

  // go with the child!
  handleOutputThread( outr );
  
  // close the standard input
  CloseHandle( stdInput );
  
  // Tell the thread to exit and wait for thread to die.
  threadRun = false;
  
  // wait forever for the thread to die
  if( WaitForSingleObject( inpThr, INFINITE ) == WAIT_FAILED ) {
    cout << "The PBAT-child was terminated abnormally!" << endl;
  }
  
  // close the handles
  CloseHandle( outr );
  CloseHandle( inw );
  
  return( 0 );
}


// for launching the redirected pbat
HANDLE launchRedirectedPBAT( HANDLE out, HANDLE in, HANDLE err, string executable )
{
  // setup the process startup info
  STARTUPINFO si;
  memset( &si, 0, sizeof(STARTUPINFO) );
  si.cb = sizeof( STARTUPINFO );
  si.dwFlags = STARTF_USESTDHANDLES;

  si.hStdOutput = out;
  si.hStdInput  = in;
  si.hStdError  = err;
  
  // now, set it up to execute pbat!
  char executableChar[1000];
  strcpy( executableChar, executable.c_str() );
  PROCESS_INFORMATION pi; // process information filled in if launched
  if( !CreateProcess( NULL, executableChar, NULL, NULL, TRUE, CREATE_NEW_CONSOLE, NULL, NULL, &si, &pi ) ) {
    cout << "Couldn't create the pbat process..." << endl;
    return( NULL );
  }
  
  // close other handles
  if( !CloseHandle(pi.hThread) )
    return( NULL );
  
  // return the child process handle
  return( pi.hProcess );
}


// handles getting input back from PBAT
void handleOutputThread( HANDLE h )
{
  CHAR buff[256];
  DWORD nBytesRead;
  DWORD nBytesWrites;
  
  while( 1 ){
    // try to read from the input PBAT file
    if( !ReadFile( h, buff, sizeof(buff),
		   &nBytesRead, NULL ) 
	|| nBytesRead==0 ) {
      if( GetLastError() == ERROR_BROKEN_PIPE )
	break; // should happen, apparently
      
      // otherwise we've got a problem...
      cout << "Couldn't read from the input file from PBAT!" << endl;
    }
    
    // and show it onscreen
    // (we could alternatively hide this...)
    if( !WriteConsole( GetStdHandle(STD_OUTPUT_HANDLE), buff, nBytesRead, &nBytesWrites, NULL) )
      cout << "Couldn't write it on-screen." << endl;
    
    cout << "Just got input" << endl;
    justGotInput = true;
  }
}


// handles getting 'input' from the 'console' - input that we
//  are using to control it
DWORD WINAPI handleInputThread( LPVOID lpvThreadParam )
{
  DWORD nBytesRead, nBytesWrites;
  HANDLE hPipeWrite = (HANDLE)lpvThreadParam;
  
  int curLine=0;
  
  char strLine[1000];
  while( threadRun && curLine<commands.size() ){
    while( !justGotInput )
      Sleep(1000);
    Sleep(1000); // still wait a little more
    strcpy( strLine, commands[curLine].c_str() );
    strcat( strLine, "\n" );
    nBytesRead = strlen(strLine);
    cout << "ABOUT TO PIPE: '" << strLine << "'" << endl;
    WriteFile( hPipeWrite, strLine, nBytesRead, &nBytesWrites, NULL );
    curLine++;
  }
  
  // pbat killing hack
  while( threadRun ){
    Sleep(1000);
    strcpy( strLine, "-1\n" );
    nBytesRead = strlen(strLine);
    
    cout << "PBAT HACK: '" << strLine << "'" << endl;
    WriteFile( hPipeWrite, strLine, nBytesRead, &nBytesWrites, NULL );
    curLine++;
  }
  
  return( 1 );
}

/* THE INTERFACING CODE */

extern "C" {
  void launchPbatPower( char ** file ){
    run( *file );
  }
}


#endif
