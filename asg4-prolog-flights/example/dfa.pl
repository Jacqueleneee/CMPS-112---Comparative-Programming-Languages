% $Id: dfa.pl,v 1.2 2011-05-19 19:53:59-07 - - $ */

%
% DFA simulator.
% Simulates a DFA that accepts the language b*aa*b[ab]*
%

%
% Facts describing the DFA.
% trans( source, label, dest).
%

trans( 0, a, 1).
trans( 0, b, 0).
trans( 1, a, 1).
trans( 1, b, 2).
trans( 2, a, 2).
trans( 2, b, 2).
start( 0).
final( 2).

%
% Functions for determining moves.
%

match( String) :- start( State), move( State, String).

move( From_state, String) :-
	[Head_string | Tail_string] = String, 
	trans( From_state, Head_string, To_state), 
	print_status( From_state, String), 
	move( To_state, Tail_string).

move( From_state, []) :-
	final( From_state), 
	print_status( From_state, []).

print_status( State, String) :-
	write( State), write( ' '), write( String), nl.

% TEST: match( [b, b, a, a, b, a, b]).
% TEST: match( [b, b, b, b]).

