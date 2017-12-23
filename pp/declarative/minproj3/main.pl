
% --- Problem 1. Introduce appropriate predicates for the entities in the system.
% --- Problem 2. Introduce facts for your choosen predicates.

airline(Name) :- string(Name).
airline(sas).
airline(norwegian).

airport(Code, Country, Weather) :- string(Code), string(Country), weather(Weather).
airport(agb, germany, stormy).
airport(lon, england, thunderstorm).
airport(ruh, "Saudi Arabia", cloudy).
airport(aal, denmark, clear).

aircraft(Reg, Owner, Model) :- number(Reg), string(Owner), string(Model).
aircraft(1, sas, cesna).
aircraft(2, norwegian, "Airbus A380").
seat(Reg, SeatNumber, Class, Type) :- number(Reg), string(SeatNumber), string(Class), string(Type).
seat(1, "1A", economy, window).
seat(1, "1B", economy, aisle).
seat(2, "1A", business, window).
seat(2, "1B", business, other).
seat(2, "1C", business, aisle).
adjacantSeat(Reg, Seat1, Seat2) :- number(Reg), string(Seat1), string(Seat2).
adjacantSeat(1, "1A", "1B").
adjacantSeat(2, "1A", "1B").
adjacantSeat(2, "1B", "1C").

model(Name, Class, Manufactorer) :- string(Name), string(Class), string(Manufactorer).
model(cesna, light, "Airbus Industrie").
model("Airbus A380", heavy, "Textron Aviation").

passenger(Id, First, Last, Birthday) :- number(Id), string(First), string(Last), string(Birthday).
passenger(1, dario, wunsch, "13-7-1990").
passenger(2, jens, aasgaard, "28-7-1990").
passenger(3, grzegorz, komincz, "14-12-1993").
passenger(4, patrick, brix, "4-12-1992").

passport(Owner, Country) :- number(Owner), string(Country).
passport(1, germany).
passport(2, england).
passport(3, "Saudi Arabia").
passport(4, denmark).
passport(4, england).

leg(Origin, Destination, Servicer, Aircraft) :- string(Origin), string(Destination), string(Servicer), number(Aircraft).
leg(aal, lon, sas, 1).
leg(lon, agb, sas, 2).
leg(augs, ruh, norwegian, 1).
leg(ruh, aal, norwegian, 2).

reservation(Code, Passenger, Origin, Destination, Airline, SeatNumber) :- string(Code), number(Passenger), string(Origin), string(Destination), string(Airline), string(SeatNumber).
reservation("R2D2", 1, aal, agb, sas, 2, "1A").
reservation("BB8", 1, agb, lon, sas, 1, "1C").
reservation("C3PO", 2, lon, aal, norwegian, 1, "1A").
reservation("IG88", 3, lon, aal, norwegian, 1, "1A"). % double reservation % illegal reservation
reservation("BB9E", 4, aal, ruh, norwegian, 2, "1B").

itinerary(Code, ReservationCode) :- string(Code), string(ReservationCode).
itinerary("010", "R2D2").
itinerary("010", "BB8").

visaAgreement(CountryA, CountryB) :- string(CountryA), string(CountryB).
visaAgreement(CountryA, CountryB) :- visaA(CountryA, CountryB).
visaAgreement(CountryA, CountryB) :- visaA(CountryB, CountryA).
visaA(denmark, germany).
visaA(denmark, england).
visaA(germany, england).
visaA(england, "Saudi Arabia").

weather(Weather) :- string(Weather).
weather(clear).
weather(cloudy).
weather(stormy).
weather(thunderstorm).


%A passenger is allowed to fly into an airport if he or she holds a passport from
%the country where the airport resides or if there is a visa agreement between
%the country of the passport holder and the country of the airport.
% --- Problem 3. Compute the airports a passenger may fly into.

mayFlyTo(PassengerId, AirportCode) :- passport(PassengerId, Country),
                                    airport(AirportCode, Country, _).
mayFlyTo(PassengerId, AirportCode) :- passport(PassengerId, Origin),
                                    visaAgreement(Origin, Destination),
                                    airport(AirportCode, Destination, _).

%A passenger may have a reservation which is illegal in the sense that he or
%she is not permitted to enter the country of the destination airport.
% --- Problem 4. Compute the passengers that have illegal reservations.

legalReservations(Pid, Rid, Dest) :- reservation(Rid, Pid, _, Dest, _, _, _), mayFlyTo(Pid, Dest).
illegalReservations(Pid) :- reservation(Rid, Pid, _, Dest, _, _, _), not(legalReservations(Pid, Rid, Dest)).

%\+((treeHeight(T, Y), Y > X))

%A double booking occurs when the same seat on the same leg of a flight is
%reserved by two different passe
% --- Problem 5. Compute the booking code of all double bookings.








%An aircraft, i.e. a flight leg, is “cleared for takeoff ” if there are no double
%bookings on it, the weather at the origin and destination is within limits, and
%every passenger is allowed to travel to the destination country.
% --- Problem 6. Compute the aircraft that are permitted to takeoff.



%A passenger can book a flight, i.e. create an itinerary, from one airport to
%another airport if they are connected by one or more legs, if on each leg there
%is a seat which is not reserved, and the passenger is permitted to enter each
%country on the way.
% --- Problem 7. Compute if a passenger can book a flight from one airport to another.
%• As above, but each leg must be on the same airline.
%• As above, but each leg must be on a Boeing aircraft.
%• As above, but each leg must be with a window seat.



% --- Problem 8. (Optional, you do not have to solve this problem to receive full credit.)
%Compute if two passengers can book a flight from one airport to another airport. The
%two passengers must travel on business class, have adjacent seats, and one of the seats
%must be a window seat.

