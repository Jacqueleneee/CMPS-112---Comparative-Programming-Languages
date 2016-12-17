% $Id: einstein.pl,v 1.4 2016-11-16 14:57:33-08 - - $ */

%
% -----------------------------------------------------------------
% 
%                         Einstein's Riddle
% 
% * General Problem:
%   - There are 5 houses in a row
%   - Each house is a different color.
%   - In each house lives a person with a different nationality.
%   - The 5 owners
%        + drink a certain type of beverage,
%        + smoke a certain brand of cigar, and
%        + keep a certain pet.
%   - No owners have the same pet, smoke the same brand of cigar or
%     drink the same beverage.
% 
% * Specific Facts:
%   - The Brit lives in the red house.
%   - The Swede keeps dogs as pets.
%   - The Dane drinks tea.
%   - The green house is on the left of the white house.
%   - The green house's owner drinks coffee.
%   - The person who smokes Pall Mall rears birds.
%   - The owner of the yellow house smokes Dunhill.
%   - The man living in the center house drinks milk.
%   - The Norwegian lives in the first house.
%   - The man who smokes Blends lives next to the one who keeps cats.
%   - The man with the horse lives next to the man who smokes Dunhill.
%   - The owner who smokes Bluemasters drinks beer.
%   - The German smokes Prince.
%   - The Norwegian lives next to the blue house.
%   - The man who smokes Blends has a neighbor who drinks water.
% 
% * Question:
%   - Who owns the fish?
% 
% -----------------------------------------------------------------
%

%
% Is the first house in a pair next to the second?
%
left_of( Left, Right, [Left, Right | _]).
left_of( Left, Right, [_ | Others]) :- left_of( Left, Right, Others).

%
% Are the two houses in a pair next to each other?
%
next_to( Left, Right, Houses) :- left_of( Left, Right, Houses).
next_to( Left, Right, Houses) :- left_of( Right, Left, Houses).

%
% Are each of the facts true about the houses?
%
map_member( [], _).
map_member( [Fact | Facts], Houses) :-
   member( Fact, Houses),
   map_member( Facts, Houses).

%
% Apply each pair of relations (left_of or next_to) to the houses.
%
map_pairs( _, [], _).
map_pairs( Relation, [First, Second | Rest], Houses) :-
   call_with_args( Relation, First, Second, Houses),
   map_pairs( Relation, Rest, Houses).

%
% Statement of Einstein's riddle in Prolog.
%
einstein( Houses, Fish) :-
   %         HOUSE( NATION   , COLOR , DRINK , SMOKE      , PET  )
   Houses = [house( norwegian, _     , _     , _          , _    ),
             _,
             house( _        , _     , milk  , _          , _    ),
             _,
             _
   ],
   Facts =  [house( brit     , red   , _     , _          , _    ),
             house( swede    , _     , _     , _          , dogs ),
             house( dane     , _     , tea   , _          , _    ),
             house( _        , green , coffee, _          , _    ),
             house( _        , _     , _     , pallmall   , birds),
             house( _        , yellow, _     , dunhill    , _    ),
             house( _        , _     , beer  , bluemasters, _    ),
             house( german   , _     , _     , prince     , _    ),
             house( Fish     , _     , _     , _          , fish )
   ],
   Left =   [house( _        , green , _     , _          , _    ),
             house( _        , white , _     , _          , _    )
   ],
   Next =   [house( _        , _     , _     , blends     , _    ),
             house( _        , _     , _     , _          , cats ),
             house( _        , _     , _     , _          , horse),
             house( _        , _     , _     , dunhill    , _    ),
             house( norwegian, _     , _     , _          , _    ),
             house( _        , blue  , _     , _          , _    ),
             house( _        , _     , _     , blends     , _    ),
             house( _        , _     , water , _          , _    )
   ],
   map_member( Facts, Houses),
   map_pairs( left_of, Left, Houses),
   map_pairs( next_to, Next, Houses).

%
% Code to print out the answer to the riddle.
%

riddle :-
   einstein( Houses, Fish),
   write_houses( Houses), nl,
   write_fish( Fish), nl,
   write( '--------------------------------------------------'),
   nl, nl.

write_fish( Fish) :-
   write( 'The '), write( Fish), write( ' owns the fish.'), nl.

write_houses( []).
write_houses( [House | Houses]) :-
   write_house( House),
   write_houses( Houses).

write_house( house( Nation, Color, Drink, Smoke, Pet)) :-
   write_label( 'House', Nation, comma),
   write_label( 'Color', Color, comma),
   write_label( 'Drink', Drink, comma),
   write_label( 'Smoke', Smoke, comma),
   write_label( 'Pet', Pet, period).

write_label( Label, Object, Punct) :-
   write( Label), write( ': '), write( Object), call( Punct).

comma :- write( ', ').
period :- write( '.'), nl.

%
% Automatically print out the answer to the riddle.
%

% TEST: riddle.

