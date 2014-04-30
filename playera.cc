#include "playera.hh"
#include "EclipseTerm.hh"

void PlayerA::initGame(){
	std::cout << "Hello, I'm player A" << std::endl;
	std::cout << " my strategy is to play the last possible move!" << std::endl;
	std::cout << "initial state:" << prolog->getCurrentState() << std::endl;
	std::cout << "MatchID:" << prolog->getMatchID() << std::endl;
	std::cout << "startclock:" << prolog->getStartClock() << std::endl;
	std::cout << "playclock:" << prolog->getPlayClock() << std::endl;
	std::cout << "our role:" << prolog->getOwnRole() << std::endl;
	std::cout << "our role index:" << prolog->getIndexOfRole(prolog->getOwnRole()) << std::endl;
}

EclipseTerm PlayerA::calcMove(){
	EclipseTerm currentState=prolog->getCurrentState();
	std::cout << "current state:" << currentState << std::endl;
	std::cout << "calc the move" << std::endl;
	EclipseTerm move=prolog->getLegalMoves(prolog->getOwnRole(),currentState).back();
	return move;
}

void PlayerA::cleanUp(){
	EclipseTerm currentState=prolog->getCurrentState();
	std::cout << "current state:" << currentState << std::endl;
	std::vector<int> goalValues=prolog->getGoalValues(currentState);
	std::vector<int>::const_iterator i=goalValues.begin();
	std::cout << "result: ["<<*i;
	++i;
	for(; i!=goalValues.end(); ++i){
		std::cout << ", " << *i;
	}
	std::cout << "]" << std::endl;
}
