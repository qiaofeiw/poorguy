// c++ header
#include <iostream>

#include "playera.hh"

int main(int argc, char* argv[]){
	int port=0;
	int mode=0;
	if(argc==1 || argc>3 ){
		std::cout   << "usage: myPlayer portnumber [mode]" << std::endl
			<< "port - port on which the player will listen" << std::endl
			<< "mode - 0 show standard messages" << std::endl
			<< "       1 show more messages" << std::endl;
		exit(0);		
	}
	if(argc >1){
		port = atoi(argv[1]);
	}
	if(argc >2){
		mode = atoi(argv[2]);
	}	
	std::cout << "Program started" << std::endl;
	// use the derived class!
	PlayerA p(port,mode);
	p.run();
	return 0;
}
