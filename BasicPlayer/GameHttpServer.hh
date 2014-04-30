/*
    Copyright (C) 2009 Stephan Schiffel <stephan.schiffel@gmx.de>
                  2009 Robert Stelzmann <robert.stelzmann@inf.tu-dresden.de>
                  2008 Norbert Manthey <norbert.manthey@inf.tu-dresden.de>

    This file is part of the GGP starter code.

    The GGP starter code is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    The GGP starter code is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with the GGP starter code.  If not, see <http://www.gnu.org/licenses/>.
 */

// inclusion guard
#ifndef GAMEHTTPSERVER_HH
#define GAMEHTTPSERVER_HH

// c++ header
#include <cstdio>
#include <cstdlib>
#include <string>

// c header
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/wait.h>

class GameHttpServer{
	private:

		// socket for new connections
		int socket_fd;

		// socket for current connection
		int connection_fd;

		// clients address information
		struct sockaddr_in clientAddressInfo;

		// waits for a client to connect
		void waitForConnection();
		
		// receive (part of) the message from current connection
		int receive(char *buffer, int length);

	public:
		// constructor and destructor
		GameHttpServer(unsigned int port);
		~GameHttpServer();

		/**
		* /brief Waits (is blocking) for an incoming http-request and set up the connection,
		* should not be called again until sendResponse is called
		* /return Returns the content of the accepted request without the http-header
		*/
		const std::string acceptRequest(void);

		/**
		* /brief Send a response to the via acceptRequest connected client,
		* should be called only after a acceptRequest call
		* /param response The content of the response with http-header
		*/
		void sendResponse(const std::string& message);

};

#endif // GAMEHTTPSERVER_HH
