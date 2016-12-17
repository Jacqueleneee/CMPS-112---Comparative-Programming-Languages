% $Id: factorial.pl,v 1.2 2011-05-19 19:53:59-07 - - $ */

%
% Factorial, the old intro to recursion standby.
%

factorial( 0, 1 ) :-
	!.

factorial( N, Nfac ) :-
	M is N - 1,
	factorial( M, Mfac ),
	Nfac is N * Mfac.

% TEST: factorial(5,N).
% TEST: factorial(20,N).
