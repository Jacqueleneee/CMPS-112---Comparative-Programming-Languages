% $Id: minimum.pl,v 1.3 2012-03-15 17:51:11-07 - - $

% Find the minimum element of a list.

item( 4, four).
item( 6, six ).
item( 9, nine).
item( 2, two ).
item(10, ten ).

minitem( [Min], Min) :-
   write( 'minitem: '), write( [Min]), nl.
minitem( [Head, Next | Tail], Newmin) :-
   write( 'minitem: '), write( [Head, Next | Tail]), nl,
   Head = item( Headnum, _),
   Next = item( Nextnum, _),
   (Headnum < Nextnum -> Min = Head; Min = Next),
   minitem( [Min | Tail], Newmin).

findminitem( Min) :-
   findall( item( X, Y), item( X, Y), List),
   write( 'findminitem: '), write( List), nl,
   minitem( List, Min),
   write( 'findminitem; '), write( Min), nl.
   
