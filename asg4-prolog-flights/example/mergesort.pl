% $Id: mergesort.pl,v 1.3 2011-05-19 19:53:59-07 - - $ */

%
% Some sorting examples.
%

%
% Insertion sort's top level function accepts the Unsorted list
% and returns the Sorted list.  Insert inserts one element into
% list such that the output list is sorted.
%

insertion_sort( Unsorted, Sorted ) :-
   insertion_sort_gather( Unsorted, [], Sorted ).

insertion_sort_gather( [], Gathered, Gathered ).
insertion_sort_gather( [Head|Tail], Gathered, Sorted ) :-
   insert( Head, Gathered, NewGathered ),
   insertion_sort_gather( Tail, NewGathered, Sorted ).

insert( Item, [], [Item] ).
insert( Item, [Head|Tail], [Item,Head|Tail] ) :-
   Item =< Head.
insert( Item, [Head|Tail], [Head|NewTail] ) :-
   Item > Head,
   insert( Item, Tail, NewTail ).

%
% Merge sort divides the unsorted list into subparts and then
% merges the sublists back again in pairs.
%

mergesort( [], [] ).
mergesort( [Only], [Only] ).
mergesort( Unsorted, Sorted ) :-
   split( Unsorted, UnsortedHalf1, UnsortedHalf2 ),
   mergesort( UnsortedHalf1, SortedHalf1 ),
   mergesort( UnsortedHalf2, SortedHalf2 ),
   merge( SortedHalf1, SortedHalf2, Sorted ).

split( [], [], [] ).
split( [Only], [Only], [] ).
split( [First,Second|Tail], [First|Tail1], [Second|Tail2] ) :-
   split( Tail, Tail1, Tail2 ).

merge( [], List, List ).
merge( List, [], List ).
merge( [Head1|Tail1], [Head2|Tail2], [Head1|NewTail] ) :-
   Head1 =< Head2,
   merge( Tail1, [Head2|Tail2], NewTail ).
merge( [Head1|Tail1], [Head2|Tail2], [Head2|NewTail] ) :-
   Head1 > Head2,
   merge( [Head1|Tail1], Tail2, NewTail ).

