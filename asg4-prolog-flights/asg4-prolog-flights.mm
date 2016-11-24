.so Tmac.mm-etc
.if t .Newcentury-fonts
.SIZE 12 14
.INITR* \n[.F]
.TITLE CMPS-112 Fall\~2016 Project\~3 "Logic Programming\(:) Prolog""
.RCS "$Id: asg4-prolog-flights.mm,v 1.5 2016-11-15 13:53:11-08 - - $"
.PWD
.URL
.H 1 "Logic Programming in Prolog"
You will use Prolog to write an airline reservation system for
the Twilight Zone Airlines.
Given a request to travel from one city to another,
print out the flight schedule.
For each leg of the trip,
print out the departure airport, airport, city name, and time.
For example\(::
.DS
.TVCODE* 0 "| ?- " "fly( lax, sea)."
.TVCODE* 0 "depart  LAX  Los Angeles     14:22"
.TVCODE* 0 "arrive  SFO  San Francisco   15:29"
.TVCODE* 0 "depart  SFO  San Francisco   16:02"
.TVCODE* 0 "arrive  SEA  Seattle-Tacoma  17:42"
.TVCODE* 0 ""
.TVCODE* 0 "true ?"
.TVCODE* 0 "yes"
.DE
.P
A database has been provided which lists some airports,
their cities,
and the North latitudes and West longitudes of the airports.
A flight schedule has also been provided listing the departure
and arrival airports and the departure times.
.H 1 "Logic"
Some notes about the logic of the program.
.ALX 1
.LI
To compute the distance between airports,
use the 
.E= "haversine formula" .
Note that the database contains degrees and minutes,
which must be converted to radians.
The result must be converted to miles.
.LI
All flight times are in Twilight Zone Standard Time (TZST),
so you don't need to handle PST, PDT, etc.
.LI
Planes fly at a constant speed of 500 miles per hour and 
always using great circle paths,
so the arrival time can be computed from the departure time.
.LI
A flight transfer always takes 30 minutes, 
so during a transfer at a hub,
the departure of a connecting flight must be at least 30
minutes later than the arrival of the incoming flight.
.LI
There are no overnight trips.
The complete trip must depart and arrive in the same day.
.LE
.H 1 "What to Submit"
Submit one file\(::
.V= functions.pl .
Also submit
.V= README ,
and if applicable,
.V= PARTNER .
The grader will copy in the files
.V= \&.score/database.pl
and the test data.
.FINISH
