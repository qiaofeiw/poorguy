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

:- module(message_handler).

:- export handle_message/2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(gdl_parser).
:- use_module(game_description).
:- use_module(match_info).
:- use_module(logger).
:- use_module(time_sync).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- mode handle_message(++, -).
handle_message(MessageString, Type) :-
	(parse_gdl_message_string(MessageString, Message) ->
		handle_message_switch(Message, MessageString, Type)
	;
		writeln("error parsing message!"),
		Type=0
	).

:- mode handle_message_switch(+, ++, -).
handle_message_switch(start(MatchID, Role, Rules, StartClock, PlayClock), MessageString, 1) :- !,
	set_log_prefix(MatchID),
	logln("messages.txt", MessageString),
	
	% remember some information about the match
	set_match_id(MatchID),
	set_startclock(StartClock),
	set_playclock(PlayClock),
	set_our_role(Role),
	
	% load the rules
	load_rules(Rules),
	
	% compute and save the initial state
	initial_state(InitialState),
	set_current_state(InitialState), !.

handle_message_switch(play(_MatchID, Moves), MessageString, 2) :- !,
	logln("messages.txt", MessageString),
	update_current_state(Moves).

handle_message_switch(stop(_MatchID, Moves), MessageString, 3) :- !,
	logln("messages.txt", MessageString),
	update_current_state(Moves),
	closelogs.

handle_message_switch(_, MessageString, 0) :- !,
	logln("messages.txt", MessageString).

:- mode update_current_state(++).
update_current_state(Moves) :-
	(Moves\=[] ->
		get_current_state(LastState),
		state_update(LastState, Moves, CurrentState),
		set_current_state(CurrentState)
	;
		get_current_state(CurrentState)
	),
	logln("gameplayer.log", last_moves(Moves)-new_state(CurrentState)).
