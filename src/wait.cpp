// Leave the following in for windows, but not for *nix
//#define WAIT_WINDOWS_DEFINE

#ifdef WAIT_WINDOWS_DEFINE
 #include <windows.h>
 #include <stdio.h>
#else
 #include <unistd.h>
 #include <sys/wait.h>
#endif

#include <iostream>
#include <vector>
#include <string>

static std::vector<std::string> cmds;

extern "C" {

  void addCommand( char **str ) {
    cmds.push_back(*str);
  }
  
  void clearCommands() {
    cmds.clear();
  }

  /* I was testing if there was a maximum string length. This appears not to be the case!
  void pprint( char **str ) {
    std::cout << *str << std::endl;
  }
  */


#ifdef WAIT_WINDOWS_DEFINE
  int runCommands() {
    std::vector<PROCESS_INFORMATION> piVec;
    
    // start up the processes
    int i=0;
    for( i=0; i<(int)cmds.size(); i++ ) {
      STARTUPINFO si;
      ZeroMemory( &si, sizeof(si) );
      si.cb = sizeof(si);

      PROCESS_INFORMATION pi;
      ZeroMemory( &pi, sizeof(pi) );

      char curCmd[1000];
      strcpy( curCmd, cmds[i].c_str() );
      if( !CreateProcess( NULL, curCmd,
			  NULL, NULL, FALSE, 0, NULL, NULL, &si, &pi ) ) {
	std::cerr << "Couldn't create process '" << cmds[i] << std::endl;
      }else{
	piVec.push_back(pi);
      }
    }

    // wait for them all to finish and flush the thread handles
    for( i=0; i<(int)cmds.size(); i++ ) {
      WaitForSingleObject( piVec[i].hProcess, INFINITE );
      CloseHandle( piVec[i].hProcess );
      CloseHandle( piVec[i].hThread );
    }
    return(0);
  }

#else
  int runCommands() {
    // fork doesn't always seem to return 0 for child/parent
    int parentPid = getpid();
    
    // loop and fork
    int i=0;
    for( i=0; i<(int)cmds.size(); i++ ) {
      int pid = fork();
      
      if( getpid() != parentPid ){
	///sleep(i+1); // DEBUG ONLY
	//std::cout << cmds[i] << std::endl;
	system( cmds[i].c_str() );
	exit(0);
      }
    }
    
    if( getpid() == parentPid ){
      // should always be true!
      //std::cout << "waiting..." << std::endl; // DEBUG
      
      for( i=0; i<(int)cmds.size(); i++ ) {
	int status=0;
	waitpid( -1, &status, 0 );
      }
      
      //std::cout << "Finished" << std::endl; // DEBUG
    }

    return(0);
  }
#endif

}
