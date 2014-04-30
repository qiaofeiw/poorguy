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

#include "PrologConnector.hh"
#include "EclipseTerm.hh"

#ifndef ECLIPSE_H
#define ECLIPSE_H
	#include <eclipse.h>
#endif

#include <iostream>
#include <cstdlib>

void panic(char* what_, char* where_)
{
	std::cout 
		<< "what: " << what_ << "\n"
		<< "where: " << where_ << std::endl;
}

PrologConnector::PrologConnector(){
	
	ec_set_option_ptr(EC_OPTION_ECLIPSEDIR, getenv("ECLIPSE_DIR"));
	
	// set local and global store
	// higher values for the local store may be neccessary the game has very complex rules
	ec_set_option_long(EC_OPTION_LOCALSIZE, 128*1024*1024);
	// higher values for the global store may be neccessary if you store a lot of big EclipseTerms (e.g. states with a lot of fluents)
	ec_set_option_long(EC_OPTION_GLOBALSIZE, 128*1024*1024);

	// set the panic callback function
	ec_set_option_ptr(EC_OPTION_PANIC, (void*)panic);

	ec_init();
	
	dident compile_1 = ec_did("compile",1);
	ec_post_goal(ec_term(compile_1,ec_string("prolog/main")));
	if(ec_resume() != PSUCCEED){
		std::cerr << "error starting prolog system!" << std::endl;
	}
}

PrologConnector::~PrologConnector(){
	ec_cleanup();
}

MessageType PrologConnector::handleMessage(const char *message){
	long type=(long)INVALID;
	ec_ref typeRef=ec_ref_create_newvar(), choice=ec_ref_create_newvar();
	ec_post_goal(ec_term(ec_did("handle_message",2),ec_string(message),ec_ref_get(typeRef)));
	if((ec_resume1(choice) != PSUCCEED) || (ec_get_long(ec_ref_get(typeRef),&type) != PSUCCEED)){
		std::cerr << "error in handle_message!" << std::endl;
	}else{
		ec_cut_to_chp(choice);
	}
	ec_ref_destroy(typeRef);
	ec_ref_destroy(choice);
	return (MessageType)type;
}

std::vector<EclipseTerm> PrologConnector::getRoles(){
	std::vector<EclipseTerm> roles;
	ec_ref rolesRef=ec_ref_create_newvar();
	ec_post_goal(ec_term(ec_did("roles",1),ec_ref_get(rolesRef)));
	if(ec_resume() != PSUCCEED){
		std::cerr << "error computing roles!" << std::endl;
	}else{
		pword rolesList=ec_ref_get(rolesRef);
		pword role, tailList;
		while(ec_get_list(rolesList,&role,&tailList) == PSUCCEED){
			roles.push_back(EclipseTerm(role));
			rolesList=tailList;
		}
	}
	ec_ref_destroy(rolesRef);
	return roles;
}

int PrologConnector::getIndexOfRole(const EclipseTerm &role){
	long index=0;
	ec_ref indexRef=ec_ref_create_newvar();
	ec_post_goal(ec_term(ec_did("index_of_role",2),role.getPword(),ec_ref_get(indexRef)));
	if((ec_resume() != PSUCCEED) || (ec_get_long(ec_ref_get(indexRef),&index) != PSUCCEED)){
		std::cerr << "error computing index_of_role!" << std::endl;
	}
	ec_ref_destroy(indexRef);
	return (int)(index-1);
}

EclipseTerm PrologConnector::getInitialState(){
	EclipseTerm t;
	ec_post_goal(ec_term(ec_did("initial_state",1),t.getPword()));
	if((ec_resume() != PSUCCEED)){
		std::cerr << "error getting initial state!" << std::endl;
		t=ec_nil();
	}
	return t;
}

EclipseTerm PrologConnector::getCurrentState(){
	EclipseTerm t;
	ec_post_goal(ec_term(ec_did("get_current_state",1),t.getPword()));
	if((ec_resume() != PSUCCEED)){
		std::cerr << "error getting current state!" << std::endl;
		t=ec_nil();
	}
	return t;
}

EclipseTerm PrologConnector::getSuccessorState(const EclipseTerm &state, std::vector<EclipseTerm> moves){
	EclipseTerm t;
	pword movesList=ec_nil();
	std::vector<EclipseTerm>::reverse_iterator imoves=moves.rbegin();
	for(;imoves!=moves.rend();++imoves){
		movesList=ec_list(imoves->getPword(), movesList);
	}
	ec_post_goal(ec_term(ec_did("state_update",3),state.getPword(),movesList,t.getPword()));
	if((ec_resume() != PSUCCEED)){
		std::cerr << "error getting successor state for state:" << state /*<< " moves:" << moves*/ << std::endl;
		t=ec_nil();
	}
	return t;
}

std::list<EclipseTerm> PrologConnector::getLegalMoves(const EclipseTerm &role, const EclipseTerm &state){
	std::list<EclipseTerm> moves;
	ec_ref movesRef=ec_ref_create_newvar();
	ec_post_goal(ec_term(ec_did("legal_moves",3),role.getPword(),state.getPword(),ec_ref_get(movesRef)));
	if(ec_resume() != PSUCCEED){
		std::cerr << "error computing legal moves for role " << role << " in state " << state << std::endl;
	}else{
		pword movesList=ec_ref_get(movesRef);
		pword move, tailList;
		while(ec_get_list(movesList,&move,&tailList)==PSUCCEED){
			moves.push_back(EclipseTerm(move));
			movesList=tailList;
		}
	}
	ec_ref_destroy(movesRef);
	return moves;
}

bool PrologConnector::isTrue(const EclipseTerm &fluent, const EclipseTerm &state){
	long result=0;
	ec_ref ref=ec_ref_create_newvar();
	ec_post_goal(ec_term(ec_did("is_true",3),fluent.getPword(),state.getPword(),ec_ref_get(ref)));
	if((ec_resume() != PSUCCEED) || (ec_get_long(ec_ref_get(ref),&result) != PSUCCEED)){
		std::cerr << "error checking truth of fluent " << fluent << " in state " << state << std::endl;
	}
	ec_ref_destroy(ref);
	return result!=0;
}

bool PrologConnector::isTrue(const char *gdlFluent, const EclipseTerm &state){
	long result=0;
	ec_ref ref=ec_ref_create_newvar();
	pword fluent=ec_newvar();
	ec_post_goal(ec_term(ec_did("parse_gdl_term_string",2),gdlFluent,fluent));
	ec_post_goal(ec_term(ec_did("is_true",3),fluent,state.getPword(),ec_ref_get(ref)));
	if((ec_resume() != PSUCCEED) || (ec_get_long(ec_ref_get(ref),&result) != PSUCCEED)){
		std::cerr << "error checking truth of fluent!" << std::endl;
	}
	ec_ref_destroy(ref);
	return result!=0;
}

bool PrologConnector::isTerminal(const EclipseTerm &state){
	long result=0;
	ec_ref ref=ec_ref_create_newvar();
	ec_post_goal(ec_term(ec_did("is_terminal",2),state.getPword(),ec_ref_get(ref)));
	if((ec_resume() != PSUCCEED) || (ec_get_long(ec_ref_get(ref),&result) != PSUCCEED)){
		std::cerr << "error checking terminality of state " << state << std::endl;
	}
	ec_ref_destroy(ref);
	return result!=0;
}

std::vector<int> PrologConnector::getGoalValues(const EclipseTerm &state){
	std::vector<int> values;
	ec_ref valuesRef=ec_ref_create_newvar();
	long longValue;
	ec_post_goal(ec_term(ec_did("get_goal_values",2),state.getPword(),ec_ref_get(valuesRef)));
	if(ec_resume() != PSUCCEED){
		std::cerr << "error computing goal values in state " << state << std::endl;
	}else{
		pword valuesList=ec_ref_get(valuesRef);
		pword value, tailList;
		while(ec_get_list(valuesList,&value,&tailList) == PSUCCEED){
			if(ec_get_long(value,&longValue) != PSUCCEED){
				std::cerr << "error computing goal values (value is not a number)!" << std::endl;
				longValue=0;
			}
			values.push_back(longValue);
			valuesList=tailList;
		}
	}
	ec_ref_destroy(valuesRef);
	return values;
}

const char *PrologConnector::getMatchID(){
	ec_ref ref=ec_ref_create_newvar();
	char *matchid;
	ec_post_goal(ec_term(ec_did("get_match_id",1),ec_ref_get(ref)));
	if(((ec_resume() != PSUCCEED)) || (ec_get_string(ec_ref_get(ref),&matchid) != PSUCCEED)){
		std::cerr << "error getting match id!" << std::endl;
		matchid=(char*)"";
	}
	ec_ref_destroy(ref);
	return matchid;
}

EclipseTerm PrologConnector::getOwnRole(){
	EclipseTerm t;
	ec_post_goal(ec_term(ec_did("get_our_role",1),t.getPword()));
	if((ec_resume() != PSUCCEED)){
		std::cerr << "error getting own role!" << std::endl;
		t=ec_nil();
	}
	return t;
}

int PrologConnector::getStartClock(){
	long startclock;
	ec_ref ref=ec_ref_create_newvar();
	ec_post_goal(ec_term(ec_did("get_startclock",1),ec_ref_get(ref)));
	if(((ec_resume() != PSUCCEED)) || (ec_get_long(ec_ref_get(ref), &startclock) != PSUCCEED)){
		std::cerr << "error getting startclock!" << std::endl;
		startclock=0;
	}
	ec_ref_destroy(ref);
	return startclock;
}

int PrologConnector::getPlayClock(){
	long playclock;
	ec_ref ref=ec_ref_create_newvar();
	ec_post_goal(ec_term(ec_did("get_playclock",1),ec_ref_get(ref)));
	if(((ec_resume() != PSUCCEED)) || (ec_get_long(ec_ref_get(ref), &playclock) != PSUCCEED)){
		std::cerr << "error getting playclock!" << std::endl;
		playclock=0;
	}
	ec_ref_destroy(ref);
	return playclock;
}

const char *PrologConnector::moveToString(const EclipseTerm &move){
	return move.toString();
}

void PrologConnector::setLoggerEnabled(bool enabled){
	if(enabled){
		ec_post_string("set_logger_enabled(true)");
	}else{
		ec_post_string("set_logger_enabled(false)");
	}
	ec_resume();
}

std::list<EclipseTerm> PrologConnector::getRules(){
	std::list<EclipseTerm> rules;
	ec_ref rulesRef=ec_ref_create_newvar();
	ec_post_goal(ec_term(ec_did("game_rules",1),ec_ref_get(rulesRef)));
	if(ec_resume() != PSUCCEED){
		std::cerr << "error getting game rules!" << std::endl;
	}else{
		rules=EclipseTerm(ec_ref_get(rulesRef)).getArguments();
	}
	ec_ref_destroy(rulesRef);
	return rules;
}

void PrologConnector::setRules(const std::list<EclipseTerm> &rules){
	EclipseTerm rulesList(rules);
	ec_post_goal(ec_term(ec_did("set_game_rules",1),rulesList.getPword()));
	if(ec_resume() != PSUCCEED){
		std::cerr << "error setting game rules:" << std::endl << rulesList << std::endl;
	}
}

void PrologConnector::printStats(){
	ec_post_goal(ec_term(ec_did("statistics",0)));
	if(ec_resume() != PSUCCEED){
		std::cerr << "error printing prolog statistics!" << std::endl;
	}
}

EclipseTerm PrologConnector::parseGdlStateString(const char* gdlString){
	EclipseTerm t;
	pword gdl=ec_string(gdlString);
	ec_post_goal(ec_term(ec_did("parse_gdl_term_list_string",2),gdl,t.getPword()));
	if(ec_resume() != PSUCCEED){
		std::cerr << "error parsing gdl term list string:" << gdlString << std::endl;
	}
	return t;
}

EclipseTerm PrologConnector::parseGdlTermString(const char* gdlString){
	EclipseTerm t;
	pword gdl=ec_string(gdlString);
	ec_post_goal(ec_term(ec_did("parse_gdl_term_string",2),gdl,t.getPword()));
	if(ec_resume() != PSUCCEED){
		std::cerr << "error parsing gdl term string:" << gdlString << std::endl;
	}
	return t;
}

// EclipseTerm PrologConnector::parseGdlDescriptionString(const char* gdlString){
// 	EclipseTerm t;
// 	pword gdl=ec_string(gdlString);
// 	ec_post_goal(ec_term(ec_did("parse_gdl_description_string",2),gdl,t.getPword()));
// 	if(ec_resume() != PSUCCEED){
// 		std::cerr << "error parsing gdl description:" << gdlString << std::endl;
// 	}
// 	return t;
// }
