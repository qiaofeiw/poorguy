/*
    Copyright (C) 2009 Stephan Schiffel <stephan.schiffel@gmx.de>

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

:- module(c_connector).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- ensure_loaded([
	"time_sync",
	"logger",
	"gdl_parser",
	"match_info",
	"game_description",
	"message_handler"]).
	
:- import
	true/2,
	terminal/1,
	goal_values/2,
	role2index/2
		from game_description.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% the exported predicates are used by the C++-BasicPlayer
% and must not fail (unless there is a bug in the game
% description)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- reexport
	handle_message/2
		from message_handler.
:- reexport
	roles/1,
	initial_state/1,
	state_update/3,
	legal_moves/3,
	game_rules/1,
	set_game_rules/1
		from game_description.

:- reexport
	get_current_state/1,
	get_match_id/1,
	get_our_role/1,
	get_startclock/1,
	get_playclock/1
		from match_info.

:- reexport
	set_logger_enabled/1
		from logger.

:- reexport
	convert_to_gdl_string/2,
	parse_gdl_term_string/2,
	parse_gdl_term_list_string/2
		from gdl_parser.


:- export is_true/3.
:- mode is_true(++, ++, -).
is_true(Fluent, State, Result) :-
	(true(Fluent, State) ->
		Result=1
	;
		Result=0
	).

:- export is_terminal/2.
:- mode is_terminal(++, -).
is_terminal(State, Result) :-
	(terminal(State) ->
		Result=1
	;
		Result=0
	).

:- export get_goal_values/2.
:- mode get_goal_values(++, -).
get_goal_values(State, Values) :-
	(goal_values(State, Values) ->
		true
	;
		Values=[]
	).

:- export index_of_role/2.
:- mode index_of_role(++, -).
index_of_role(Role, Index) :-
	role2index(Role, Index), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

