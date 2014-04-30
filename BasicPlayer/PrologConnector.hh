/*
    Copyright (C) 2008,2009 Stephan Schiffel <stephan.schiffel@gmx.de>

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

#ifndef PROLOGCONNECTOR_HH
#define PROLOGCONNECTOR_HH

class EclipseTerm;

#include <list>
#include <vector>

typedef enum {INVALID=0, START=1, PLAY=2, STOP=3} MessageType;

class PrologConnector {
	public:
	
		PrologConnector();
		~PrologConnector();
		
		// parses the message and does the appropriate thing, i.e.
		// - for a start message: loads the game description and sets the current state to the initial state of the game
		// - for a play message: computes the new current state
		// - for a stop message: computes the new current state and close all logfiles
		MessageType handleMessage(const char *message);
		
		// returns a vector of all the roles of the game in correct order
		std::vector<EclipseTerm> getRoles();
		
		// returns the index of a role
		// The first role has index 0
		int getIndexOfRole(const EclipseTerm &role);
		
		// returns the initial state of the game (as a prolog list of the fluents)
		EclipseTerm getInitialState();
		
		// returns the current state of the game (as a prolog list of the fluents)
		EclipseTerm getCurrentState();
		
		// returns the state that results from executing the given moves in the given state
		// moves contains a move for each role in the order of the roles as given by getRoles() or getIndexOfRole(...)
		EclipseTerm getSuccessorState(const EclipseTerm &state, std::vector<EclipseTerm> moves);
		
		// returns a list of all legal moves of the given role in the given state
		std::list<EclipseTerm> getLegalMoves(const EclipseTerm &role, const EclipseTerm &state);
		
		// returns true if the given fluent is true in the given state
		bool isTrue(const EclipseTerm &fluent, const EclipseTerm &state);
		bool isTrue(const char *gdlFluent, const EclipseTerm &state);
		
		// returns true if the given state is a terminal state
		bool isTerminal(const EclipseTerm &state);
		
		// returns the value of the state for each role of the game
		// if the goal values can't be computed and empty vector is returned
		std::vector<int> getGoalValues(const EclipseTerm &state);
		
		// returns the match id of the current match
		const char *getMatchID();
		
		// returns the role of this player
		EclipseTerm getOwnRole();
		
		// returns the startclock in seconds
		int getStartClock();
		
		// returns the playclock in seconds
		int getPlayClock();
		
		// returns a string representation of the move
		const char *moveToString(const EclipseTerm &move);
		
		// enables or disables the prolog logger
		void setLoggerEnabled(bool enabled);
		
		/**
		 * returns a list of all rules of the game
		 * A rule is composed of the following operators:
		 *   F :- G (corresponds to (<= F G))
		 *   not F  (corresponds to (not F)
		 *   F, G   (corresponds to (and F G))
		 *   F; G   (corresponds to (or F G))
		 */
		std::list<EclipseTerm> getRules();

		void setRules(const std::list<EclipseTerm> &rules);
		
		void printStats();
		
		/**
		 * parses a kif string of the form "(fluent1 ... fluentn)" and returns the state as an EclipseTerm
		 */
		EclipseTerm parseGdlStateString(const char* gdlString);
		
		/**
		 * parses a kif string and returns the term as an EclipseTerm (can be used to convert string representations for roles, fluents, moves, ... to an EclipseTerm)
		 */
		EclipseTerm parseGdlTermString(const char* gdlString);
		
		//EclipseTerm parseGdlDescriptionString(const char* gdlString);
};

#endif
