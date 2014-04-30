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

#ifndef ECLIPSETERM_HH
#define ECLIPSETERM_HH

#ifndef ECLIPSE_H
#define ECLIPSE_H
	#include <eclipse.h>
#endif

#include <iterator>
#include <list>
#include <vector>
#include <iostream>
#include <iostream>

// #define DEBUG_ECLIPSE_TERM

#ifdef DEBUG_ECLIPSE_TERM
	extern int nb_eclipse_terms;
#endif

class EclipseTerm{
	friend class PrologConnector;
	
	private:
		ec_ref ref;
		
		template<class InputIterator> void init(const char *name, int arity, InputIterator argumentsBegin, const InputIterator &argumentsEnd);
		template<class BidirectionalIterator> void init(BidirectionalIterator elementsBegin, BidirectionalIterator elementsEnd);
	
	public:
		/**
		 * build an EclipseTerm for new variable
		 */
		EclipseTerm(){
			#ifdef DEBUG_ECLIPSE_TERM
				++nb_eclipse_terms;
			#endif
			ref = ec_ref_create_newvar();
		}
		
		EclipseTerm(const pword &init){
			#ifdef DEBUG_ECLIPSE_TERM
				++nb_eclipse_terms;
			#endif
			ref = ec_ref_create_newvar();
			ec_ref_set(ref, init);
		}
		
		/**
		 * copy constructor
		 */
		EclipseTerm(const EclipseTerm &term){
			#ifdef DEBUG_ECLIPSE_TERM
				++nb_eclipse_terms;
			#endif
			ref = ec_ref_create_newvar();
			ec_ref_set(ref, ec_ref_get(term.ref));
		}
		
		/**
		 * build an EclipseTerm for constant with name name
		 */
		EclipseTerm(const char *name);
		
		/**
		 * build an EclipseTerm for number l
		 */
		EclipseTerm(long l){
			#ifdef DEBUG_ECLIPSE_TERM
				++nb_eclipse_terms;
			#endif
			ref = ec_ref_create_newvar();
			ec_ref_set(ref, ec_long(l));
		}
		
		/**
		 * build an EclipseTerm for a function with given name and arguments
		 */
		EclipseTerm(const char *name, const std::list<EclipseTerm> &arguments){
			#ifdef DEBUG_ECLIPSE_TERM
				++nb_eclipse_terms;
			#endif
			init(name, arguments.size(), arguments.begin(), arguments.end());
		}

		/**
		 * build an EclipseTerm for a prolog list with the given elements
		 */
		EclipseTerm(const std::list<EclipseTerm> &listElements){
			#ifdef DEBUG_ECLIPSE_TERM
				++nb_eclipse_terms;
			#endif
			init(listElements.begin(), listElements.end());
		}
		
		/**
		 * build an EclipseTerm for a function with given name, arity and arguments
		 */
		template<class InputIterator> EclipseTerm(const char *name, int arity, InputIterator argumentsBegin, const InputIterator &argumentsEnd){
			#ifdef DEBUG_ECLIPSE_TERM
				++nb_eclipse_terms;
			#endif
			init(name, arity, argumentsBegin, argumentsEnd);
		}
		
		/**
		 * build an EclipseTerm for a prolog list with the given elements
		 */
		template<class BidirectionalIterator>
		EclipseTerm(BidirectionalIterator elementsBegin, BidirectionalIterator elementsEnd){
			#ifdef DEBUG_ECLIPSE_TERM
				++nb_eclipse_terms;
			#endif
			init(elementsBegin, elementsEnd);
		}

		~EclipseTerm(){
			#ifdef DEBUG_ECLIPSE_TERM
				--nb_eclipse_terms;
			#endif
			ec_ref_destroy(ref);
		}
		
		EclipseTerm& operator=(const pword &word)
		{
			ec_ref_set(ref,word);
			return *this;
		}

		EclipseTerm& operator=(const EclipseTerm &term)
		{
			ec_ref_set(ref,ec_ref_get(term.ref));
			return *this;
		}
		
		pword getPword() const{
			return ec_ref_get(ref);
		}
		
		/**
		 * @return a string representation of the term in prefix kif form (gdl notation)
		 */
		const char *toString() const;
		
		/**
		 * @return a string representation of the term in infix form (prolog notation)
		 */
		const char *toPrologString() const;

		/**
		 * @return true if the terms are identical
		 */
		bool operator==(const EclipseTerm &term) const
		{
			return ec_compare(getPword(), term.getPword())==0;
		}
		
		/**
		 * @return true if the unification of the terms succeeds
		 */
		bool unify(EclipseTerm &term)
		{
			return ec_unify(term.getPword(),getPword())==PSUCCEED;
		}

		/**
		 * @return the functor of the term as c-string, or the constant name if the term is a constant
		 * getName() returns NULL for numbers or variables
		 */
		const char* getName() const {
			dident d;
			if(ec_get_functor(getPword(),&d)==PSUCCEED){
				return DidName(d);
			}else if(ec_get_atom(getPword(),&d)==PSUCCEED){
				return DidName(d);
			}else{
				return (const char*)NULL;
			}
		}
		
		/**
		 * @return true if the term is an integer number and sets *l to the number
		 */
		bool isLong(long *l) const {
			return ec_get_long(getPword(),l)==PSUCCEED;
		}
		
		/**
		 * @return true if the term is a variable
		 */
		bool isVariable() const {
			return ec_is_var(getPword())==PSUCCEED;
		}

		/**
		 * @return true if the term is a compound term, i.e. function or list
		 */
		int isCompound() const {
			return ec_arity(getPword())>0;
		}

		/**
		 * @return the number of arguments of the term or the length of the list if the term is a prolog list
		 */
		int getArity() const;

		/**
		 * @return a list of arguments of the term or a list of elements if the term is a prolog list
		 */
		std::list<EclipseTerm> getArguments() const;
		
		friend std::ostream & operator<<(std::ostream &ostream, const EclipseTerm &term){
			return (ostream << term.toString());
		}

};

template<class InputIterator> void EclipseTerm::init(const char *name, int arity, InputIterator argumentsBegin, const InputIterator &argumentsEnd){
	dident d=ec_did(name,arity);
	pword args_array[arity]; // TODO: is this correct? shouldn't it be pword *args_array=new pword[arity];
	for(int j=0;argumentsBegin!=argumentsEnd;++argumentsBegin,++j){
		args_array[j]=argumentsBegin->getPword();
	}
	ref = ec_ref_create_newvar();
	ec_ref_set(ref, ec_term_array(d,args_array));
}

template<class BidirectionalIterator> void EclipseTerm::init(BidirectionalIterator elementsBegin, BidirectionalIterator elementsEnd){
	pword l=ec_nil();
	if(elementsBegin!=elementsEnd){
		--elementsEnd;
		for(;elementsEnd!=elementsBegin;--elementsEnd){
			l=ec_list(elementsEnd->getPword(), l);
		}
		l=ec_list(elementsEnd->getPword(), l);
	}
	ref = ec_ref_create_newvar();
	ec_ref_set(ref, l);
}

#endif
