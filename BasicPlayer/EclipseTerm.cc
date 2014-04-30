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

#include "EclipseTerm.hh"

#ifndef ECLIPSE_H
#define ECLIPSE_H
	#include <eclipse.h>
#endif

#include <list>

#ifdef DEBUG_ECLIPSE_TERM
	int nb_eclipse_terms=0;
#endif

EclipseTerm::EclipseTerm(const char *name){
	#ifdef DEBUG_ECLIPSE_TERM
		++nb_eclipse_terms;
	#endif
	ref = ec_ref_create_newvar();
	dident d=ec_did(name,0);
	ec_ref_set(ref, ec_atom(d));
}

const char *EclipseTerm::toString() const {
	ec_ref ref=ec_ref_create_newvar();
	char *s;
	ec_post_goal(ec_term(ec_did("convert_to_gdl_string",2),getPword(),ec_ref_get(ref)));
	if(((ec_resume() != PSUCCEED)) || (ec_get_string(ec_ref_get(ref), &s) != PSUCCEED)){
		std::cerr << "error converting term to string!" << std::endl;
		s=(char*)"";
	}
	ec_ref_destroy(ref);
	return s;
}

const char *EclipseTerm::toPrologString() const {
	ec_ref ref=ec_ref_create_newvar();
	char *s;
	ec_post_goal(ec_term(ec_did("term_string",2),getPword(),ec_ref_get(ref)));
	if(((ec_resume() != PSUCCEED)) || (ec_get_string(ec_ref_get(ref), &s) != PSUCCEED)){
		std::cerr << "error converting term to prolog string!" << std::endl;
		s=(char*)"";
	}
	ec_ref_destroy(ref);
	return s;
}

int EclipseTerm::getArity() const {
	int arity=0;
	pword head, tail;
	if(ec_get_list(getPword(),&head,&tail)==PSUCCEED){
		++arity;
		while(ec_get_list(tail,&head,&tail)==PSUCCEED){
			++arity;
		}
	}else{
		arity=ec_arity(getPword());
	}
	return arity;
}

std::list<EclipseTerm> EclipseTerm::getArguments() const {
	std::list<EclipseTerm> args;
	pword head, tail;
	if(ec_get_list(getPword(),&head,&tail)==PSUCCEED){
		args.push_back(EclipseTerm(head));
		while(ec_get_list(tail,&head,&tail)==PSUCCEED){
			args.push_back(EclipseTerm(head));
		}
	}else{
		pword p=getPword(),p2;
		int arity=ec_arity(p);
		for(int i=0;i<arity;++i){
			ec_get_arg(i+1,p,&p2);
			args.push_back(EclipseTerm(p2));
		}
	}
	return args;
}

