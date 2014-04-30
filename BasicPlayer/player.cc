/*
    Copyright (C) 2009 Stephan Schiffel <stephan.schiffel@gmx.de>
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

#include "player.hh"
#include "EclipseTerm.hh"

Player::Player(){
	
}

Player::Player(unsigned int port, unsigned int modus){
	std::cout << "setting up player" << std::endl;
	talkModus = modus;
	if(talkModus & 1) std::cout << "\tinitialising Prolog...";
	prolog = new PrologConnector;
	if(talkModus & 1) std::cout << "done" << std::endl;
	if(talkModus & 1) std::cout << "\tinitialising HttpServer on port " << port << " ...";
	server = new GameHttpServer(port);
	if(talkModus & 1) std::cout << "done" << std::endl;
	std::cout << "setup done" << std::endl;
}

MessageType Player::handleNextMessage(){
	std::cout << "waiting for message from GameController ..." << std::endl;
	// get message
	std::string message=server->acceptRequest();
	if(talkModus & 1) std::cout << "message: " << message << std::endl;
	// work on it with prolog
	return prolog->handleMessage(message.c_str());
}

void Player::sendResponse(const std::string &response){
	server->sendResponse(response);
}

void Player::sendMove(EclipseTerm &move){
	server->sendResponse(prolog->moveToString(move));
}

void Player::initGame(){
	std::cout << "got rules, work on it!!" << std::endl;
}

EclipseTerm Player::calcMove(){
	// simply return one of the possible moves
	return prolog->getLegalMoves(prolog->getOwnRole(),prolog->getCurrentState()).back();
}

void Player::cleanUp(){
}

void Player::handleFirstMessage()
{
	// first message
	MessageType type;
	do {
		type=handleNextMessage();
		if(type != START) {
			sendResponse("invalid message");
		}
	} while (type != START);
	// do your initialisation
	initGame();
	sendResponse("READY");
}

int Player::run(){
	// handle first message containing the rules
	handleFirstMessage();
	// repeat until STOP message from gameserver
	bool gameIsPlaying = true;
	while(gameIsPlaying){
		MessageType type = handleNextMessage();
		
		EclipseTerm legalMove;
		switch(type){
		case START:
			std::cerr << "received START message in the middle of the game" << std::endl;
			std::cerr << "abort player" << std::endl;
			return -1;
			break;
		case PLAY:
			legalMove = calcMove();
			sendMove(legalMove);
			break;
		case STOP:
			cleanUp();
			sendResponse("DONE");
			gameIsPlaying = false;
			break;
		case INVALID:
			std::cerr << "received invalid message" << std::endl;
			sendResponse("invalid message");
		};
	};
	std::cout << "Done" << std::endl;
	return 0;
}
