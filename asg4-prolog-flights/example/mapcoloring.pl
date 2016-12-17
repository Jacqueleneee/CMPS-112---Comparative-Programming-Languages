% $Id: mapcoloring.pl,v 1.4 2011-05-19 19:53:59-07 - - $ */

%
% Map coloring.
%
% Given an adjacency matrix, find a coloring of the map such
% that no two adjacent nodes have the same color.  The four
% color theorem says this is always possible with four colors.
%

not( X ) :- X, !, fail.
not( _ ).

%
% Specification of the nodes in the graph and the paths.
%

graph( [1,2,3,4,5] ).
path( 1, 2 ).
path( 1, 3 ).
path( 1, 4 ).
path( 2, 3 ).
path( 2, 4 ).
path( 3, 4 ).
path( 4, 5 ).

%
% Undirected graph, ajacency is bidirectional.
%

adjacent( Node1, Node2 ) :- path( Node1, Node2 ).
adjacent( Node1, Node2 ) :- path( Node2, Node1 ).

%
% Specifications of possible colors for the nodes.
%

color( red ).
color( green ).
color( blue ).
color( white ).

%
% Find a coloring with no conflicts.
%

findcoloring( [], [] ).
findcoloring( [Node | Nodes], [Coloring | Colorings] ) :-
   findcoloring( Nodes, Colorings ),
   Coloring = color( Node, Color ),
   color( Color ),
   noconflict( Coloring, Colorings ).

noconflict( _, [] ).
noconflict( Coloring1, [Coloring2 | Colorings] ) :-
   not( conflict( Coloring1, Coloring2 )),
   noconflict( Coloring1, Colorings ).

conflict( color( Node1, Color ), color( Node2, Color )) :-
   adjacent( Node1, Node2 ).

%
% Trace the relevant relations.
%

traceon :-
   trace( adjacent ),
   trace( color ),
   trace( findcoloring ),
   trace( noconflict ),
   trace( conflict ).

writeallcolorings :-
   writeanycoloring,
   fail.

writeanycoloring :-
   findanycoloring( Coloring ),
   write( Coloring ), nl.

findanycoloring( Coloring ) :-
   graph( Graph ),
   findcoloring( Graph, Coloring ).

% TEST: writeallcolorings.
