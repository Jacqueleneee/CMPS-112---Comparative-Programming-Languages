% $Id: viiiqueens.pl,v 1.3 2011-05-19 19:53:59-07 - - $ */

not( X ) :- X, !, fail.
not( _ ).

remove( Item, [ Item | Tail ], Tail ).

remove( Item, [ Head1 | Tail1 ], [ Head1 | Tail2 ] ) :-
   remove( Item, Tail1, Tail2 ).

threatens( Rank1, File1, Rank2, File2 ) :-
   Rankdiff is abs( Rank1 - Rank2 ),
   Filediff is abs( File1 - File2 ),
   Rankdiff =:= Filediff.

place( Rank, File, Rank, [], [ File ] ).

place( Rank1, File1, Rank2, [ File2 | Rest ], [ File2 | Gives ] ) :-
   not( threatens( Rank1, File1, Rank2, File2 )),
   Nextrank2 is Rank2 + 1,
   place( Rank1, File1, Nextrank2, Rest, Gives ).

queens( _, [], Sequence, Sequence ).

queens( Rank, Start, Partial, Answer ) :-
   remove( One, Start, Remaining ),
   place( Rank, One, 1, Partial, New ),
   Nextrank is Rank + 1,
   queens( Nextrank, Remaining, New, Answer ).

queens :-
   queens( 1, [1,2,3,4,5,6,7,8], [], Answer ),
   write( Answer ), nl,
   fail.

% TEST: queens.
