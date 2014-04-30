#ifndef _PLAYERA_HH_
#define _PLAYERA_HH_

#include "BasicPlayer/player.hh"

class PlayerA : public Player 
{
	// implement these inherited methods in playera.cc
	void initGame();		// do this between START message and READY answer
	EclipseTerm calcMove();	// do this after each PLAY message before send move
        EclipseTerm bestMove(EclipseTerm currentState);
        int maxScore(EclipseTerm state);
        int getOwnScore(EclipseTerm state);
	void cleanUp();	        // do this after STOP message before DONE answer
public:
	explicit PlayerA(unsigned int port=12345, unsigned int mode=0) : Player(port, mode) { }
};

#endif // _PLAYERA_HH_
