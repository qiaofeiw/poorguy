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

#ifndef _PLAYER_HH_
#define _PLAYER_HH_

#include <iostream>
#include "GameHttpServer.hh"
#include "PrologConnector.hh"

class Player
{
protected:
	unsigned int talkModus;
	
	PrologConnector *prolog;
	GameHttpServer *server;
	
	Player();
	
	// sending / getting messages
	
	// gets next message and handles it with prolog
	virtual MessageType handleNextMessage();
	
	// sends response to last message
	virtual void sendResponse(const std::string &response);
	
	// sends move as response to last message
	virtual void sendMove(EclipseTerm &move);
	
	// handle game
	virtual void handleFirstMessage();
	virtual void initGame();
	virtual EclipseTerm calcMove();
	virtual void cleanUp();
	
public:
	explicit Player(unsigned int port, unsigned int modus = 0);
	virtual int run();

};

#endif // _PLAYER_HH_
