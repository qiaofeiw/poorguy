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

#include <iostream>
#include <cstdlib>
#include <sstream>
#include <memory.h>
#include "GameHttpServer.hh"

GameHttpServer::GameHttpServer(unsigned int port){
	
	// init socket
	if ((socket_fd = socket(AF_INET, SOCK_STREAM, 0)) == -1) {
		perror("setting up socket");
		exit(1);
	}

	// set socket options
	int byte=1;
	if (setsockopt(socket_fd,SOL_SOCKET,SO_REUSEADDR,&byte,sizeof(int)) == -1) {
		perror("set socket options");
		exit(1);
	}
	
	struct sockaddr_in server_address_info;
	memset(&server_address_info, '\0', sizeof(sockaddr_in)); // zero the struct
	server_address_info.sin_family = AF_INET;         // host byte order
	server_address_info.sin_port = htons(port);     // short, network byte order
	server_address_info.sin_addr.s_addr = INADDR_ANY; // automatically fill with my IP
	
	// bind socket
	if (bind(socket_fd, (struct sockaddr *)&server_address_info, sizeof(struct sockaddr)) == -1) {
		perror("binding socket with addressinfo");
		exit(1);
	}
	
	// listen to socken
	if (listen(socket_fd, 1) == -1) {
		perror("start listening");
		exit(1);
	}
}

GameHttpServer::~GameHttpServer(){
	if(connection_fd != -1) close(connection_fd);
	if(socket_fd != -1) close(socket_fd);
}

const std::string GameHttpServer::acceptRequest(void) {
	// wait for connection, which has to be established before receiving anything
	waitForConnection();
	
	// get HTTP message, extract content
	std::string message="";
	int numbytes;  // length of the received data
	char buffer[8193];
	
	// receive until we have the complete http-header
	std::string::size_type headerLength;
	do{
		numbytes=receive(buffer, 8192);
		buffer[numbytes]='\0';
		message+=buffer;
	} while ((headerLength = message.find("\r\n\r\n")) == std::string::npos);
	headerLength+=4;

	// extract the content length from the header
	std::string::size_type contentLengthStart;
	if ((contentLengthStart=message.find("\r\nContent-length:")) == std::string::npos) {
		contentLengthStart=message.find("\r\nContent-Length:");
	}
	if(contentLengthStart == std::string::npos) {
		std::cout << "message has no content length field:" << std::endl << message << std::endl;
		close(connection_fd); // close child socket
		connection_fd=-1;
		return "invalid message";
	} else {
		contentLengthStart+=18;
		std::string::size_type contentLengthEnd = message.find("\r\n", contentLengthStart + 1);
		std::string contentLengthStr = message.substr(contentLengthStart, contentLengthEnd - contentLengthStart);
		unsigned int messageLength = atoi(contentLengthStr.c_str()) + headerLength;
		
		// receive remaining message
		while(message.length()<messageLength){
			numbytes=receive(buffer, 8192);
			buffer[numbytes]='\0';
			message+=buffer;
		}
		
		// return the content of the message without http header
		message=message.substr(headerLength);
		return message;
	}
}

int GameHttpServer::receive(char *buffer, int length) {
	int numbytes;
	if ((numbytes=recv(connection_fd, (void*)buffer, length, 0)) == -1) {
		perror("recveiving");
		close(connection_fd); // close child socket
		close(socket_fd); // child own socket
		exit(1);
	}
	return numbytes;
}

void GameHttpServer::sendResponse(const std::string& message) {
	std::stringstream response;
	response << "HTTP/1.0 200 OK\r\n";
	response << "Content-type: text/acl\r\n";
	response << "Content-length: " << message.length();
	response << "\r\n\r\n";
	response << message;
	std::string responseStr=response.str();
	if ( send(connection_fd, responseStr.c_str(), responseStr.length(), 0) == -1) {
		perror("send");
	}
	close(connection_fd); // close child socket
	connection_fd=-1;
}

void GameHttpServer::waitForConnection()
{
	int sin_size = sizeof(struct sockaddr_in);
	if ((connection_fd = accept(socket_fd, (struct sockaddr *)&clientAddressInfo, (socklen_t*)&sin_size)) == -1) {
		perror("accepting client");
		exit(1);
	}
}
