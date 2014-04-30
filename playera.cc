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

EclipseTerm PlayerA::bestMove(EclipseTerm currentState){
	std::list<EclipseTerm> actions = prolog->getLegalMoves(prolog->getOwnRole(), currentState);	
        EclipseTerm action;
	int score = 0;
	std::list<EclipseTerm>::const_iterator iter;
        for (iter = actions.begin(), action = *iter; iter != actions.end(); iter++){
            std::vector<EclipseTerm> moves;
            moves.push_back(*iter);
            int result = maxScore(prolog->getSuccessorState(currentState, moves));
            if (result == 100) {
                return *iter;
            }
            if (result > score) {
                score = result;
                action = *iter;
            }
        }
        return action;
}

int PlayerA::maxScore(EclipseTerm state){
        if (prolog->isTerminal(state)) {
            return prolog->getGoalValues(state)[prolog->getIndexOfRole(prolog->getOwnRole())];
        }
        std::list<EclipseTerm> actions = prolog->getLegalMoves(prolog->getOwnRole(), state);
        int score = 0;
        std::list<EclipseTerm>::const_iterator iter;
        for (iter = actions.begin(); iter !=actions.end(); iter++){
            std::vector<EclipseTerm> moves;
            moves.push_back(*iter);
            int result = getOwnScore(prolog->getSuccessorState(state, moves));
            if (result > score) {
                score = result;
            }
        }
        return score;
}

int PlayerA::getOwnScore(EclipseTerm state){
        std::vector<int> values = prolog->getGoalValues(state);
        return values[prolog->getIndexOfRole(prolog->getOwnRole())];
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
