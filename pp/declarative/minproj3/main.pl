
% --- Problem 1. Introduce appropriate predicates for the entities in the system.

country(Country) :- string(Country).
country(germany).
country(england).
country("Saudi Arabia").
country(denmark).

airline(Name) :- string(Name).
airline(sas).
airline(norwegian).

apCode(Code) :- string(Code).
apCode(agb).
apCode(lon).
apCode(ruh).
apCode(aal).

apModel(Model) :- string(Model).
apModel(cesna).
apModel("Airbus A380").

seatClass(Class) :- string(Class).
seatClass(economy).
seatClass(business).

seatType(Type) :- string(Type).
seatType(aisle).
seatType(other).
seatType(window).

acClass(Class) :- string(Class).
acClass(light).
acClass(heavy).

apManufactorer(Manu) :- string(Manu).
apManufactorer("Textron Aviation").
apManufactorer("Airbus Industrie").

weather(Weather) :- string(Weather).
weather(clear). % light and heavy can fly
weather(cloudy). % light and heavy can fly
weather(stormy). % heavy can fly
weather(thunderstorm). % non can fly


% --- Problem 2. Introduce facts for your choosen predicates.

airport(Code, Country, Weather) :- string(Code), country(Country), weather(Weather).
airport(agb, germany, stormy).
airport(lon, england, thunderstorm).
airport(ruh, "Saudi Arabia", cloudy).
airport(aal, denmark, clear).

aircraft(Reg, Owner, Model) :- number(Reg), airline(Owner), string(Model).
aircraft(1, sas, cesna).
aircraft(2, norwegian, "Airbus A380").

seat(Aircraft, SeatNumber, Class, Type) :- number(Aircraft), string(SeatNumber), string(Class), string(Type).
seat(1, "1A", economy, window).
seat(1, "1B", economy, other).
seat(1, "1C", economy, aisle).
seat(2, "1A", business, window).
seat(2, "1B", business, other).
seat(2, "1C", business, other).
seat(2, "1D", business, aisle).
adjacantSeat(Aircraft, Seat1, Seat2) :- number(Aircraft), string(Seat1), string(Seat2).
adjacantSeat(Aircraft, Seat1, Seat2) :- aSeat(Aircraft, Seat1, Seat2).
adjacantSeat(Aircraft, Seat1, Seat2) :- aSeat(Aircraft, Seat2, Seat1).
aSeat(1, "1A", "1B").
aSeat(1, "1B", "1C").
aSeat(2, "1A", "1B").
aSeat(2, "1B", "1C").
aSeat(2, "1C", "1D").

model(Name, Class, Manufactorer) :- string(Name), string(Class), string(Manufactorer).
model(cesna, light, "Textron Aviation").
model("Airbus A380", heavy, "Airbus Industrie").

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

leg(Origin, Destination, Servicer, Aircraft) :- apCode(Origin), apCode(Destination), string(Servicer), number(Aircraft).
leg(lon, aal, sas, 1).
leg(lon, ruh, sas, 2).
leg(aal, agb, sas, 2).
leg(agb, lon, norwegian, 2).
leg(agb, ruh, norwegian, 1).
leg(ruh, aal, norwegian, 2).

reservation(Code, Passenger, Origin, Destination, Aircraft, SeatNumber) :- string(Code), number(Passenger), string(Origin), string(Destination), string(Aircraft), string(SeatNumber).
reservation("R2D2", 1, aal, agb, 2, "1A").
reservation("002", 2, aal, agb, 2, "1B").
reservation("003", 4, aal, agb, 2, "1C").
reservation("C3PO", 2, lon, aal, 1, "1A"). % double reservation
reservation("IG88", 3, lon, aal, 1, "1A"). % double reservation % illegal reservation
reservation("001", 1, lon, aal, 1, "1B").
reservation("004", 3, lon, ruh, 2, "1A").


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

canFly(Model, Weather) :- apModel(Model), weather(Weather).
canFly(light, clear).
canFly(light, cloudy).
canFly(heavy, clear).
canFly(heavy, cloudy).
canFly(heavy, stormy).


%A passenger is allowed to fly into an airport if he or she holds a passport from
%the country where the airport resides or if there is a visa agreement between
%the country of the passport holder and the country of the airport.
% --- Problem 3. Compute the airports a passenger may fly into.

mayFlyTo(PassengerId, AirportCode) :-
        passport(PassengerId, Country),
        airport(AirportCode, Country, _).
mayFlyTo(PassengerId, AirportCode) :-
        passport(PassengerId, Origin),
        visaAgreement(Origin, Destination),
        airport(AirportCode, Destination, _).


%A passenger may have a reservation which is illegal in the sense that he or
%she is not permitted to enter the country of the destination airport.
% --- Problem 4. Compute the passengers that have illegal reservations.
% illegalReservations(P, R). should return: P = 3, R = "IG88";

legalReservations(Pid, Rid, Dest) :- reservation(Rid, Pid, _, Dest, _, _), mayFlyTo(Pid, Dest).
illegalReservations(Pid, Rid) :- reservation(Rid, Pid, _, Dest, _, _), not(legalReservations(Pid, Rid, Dest)).
illegalReservations(Rid) :- reservation(Rid, Pid, _, Dest, _, _), not(legalReservations(Pid, Rid, Dest)).

%A double booking occurs when the same seat on the same leg of a flight is
%reserved by two different passengers
% --- Problem 5. Compute the booking code of all double bookings.

doubleBookings(Rid) :-
        reservation(Rid, _, Origin, Destination, Aircraft, SeatNumber),
        reservation(Rid2, _, Origin, Destination, Aircraft, SeatNumber),
        Rid \= Rid2.


%An aircraft, i.e. a flight leg, is “cleared for takeoff ” if there are no double
%bookings on it, the weather at the origin and destination is within limits, and
%every passenger is allowed to travel to the destination country.
% --- Problem 6. Compute the aircraft that are permitted to takeoff.
% legCleared(lon, aal, sas, 1). % should be false
% legCleared(aal, agb, sas, 2). % should be true

% no double bookings for leg
bookingsNotDoubleOnLeg(Origin, Dest, Aircraft, Rid) :-
        reservation(Rid, _, Origin, Dest, Aircraft, _),
        not(doubleBookings(Rid)).
noDoubleBookingsForLeg(Origin, Dest, Aircraft) :-
        setof(_, reservation(Rid, _, Origin, Dest, Aircraft, _), ResLegList),
        length(ResLegList, ResLegLen),
        setof(_, bookingsNotDoubleOnLeg(Origin, Dest, Aircraft, Rid), NoDoubleList),
        length(NoDoubleList, NoDoubleLen),
        ResLegLen = NoDoubleLen, !.

% canFly
takeOffConditions(Origin, Dest, Aircraft) :-
        aircraft(Aircraft, _, Model),
        model(Model, Class, _),
        airport(Origin, _, OriginWeather),
        airport(Dest, _, DestWeather),
        canFly(Class, OriginWeather),
        canFly(Class, DestWeather), !.

% no illegal passengers on flight
legalReservationsForLeg(Origin, Dest, Aircraft, Rid) :- reservation(Rid, Pid, Origin, Dest, Aircraft, _), mayFlyTo(Pid, Dest).
noIllegalReservationsForLeg(Origin, Dest, Aircraft) :-
        setof(_, reservation(Rid, _, Origin, Dest, Aircraft, _), ResLegList),
        length(ResLegList, ResLegLen),
        setof(_, legalReservationsForLeg(Origin, Dest, Aircraft, Rid), LegalResList),
        length(LegalResList, LegalResLen),
        ResLegLen = LegalResLen, !.

% leg cleared for takeoff
legCleared(Origin, Dest, Servicer, Aircraft) :-
        leg(Origin, Dest, Servicer, Aircraft), % does leg exist
        noDoubleBookingsForLeg(Origin, Dest, Aircraft),
        takeOffConditions(Origin, Dest, Aircraft),
        noIllegalReservationsForLeg(Origin, Dest, Aircraft).


%A passenger can book a flight, i.e. create an itinerary, from one airport to
%another airport if they are connected by one or more legs, if on each leg there
%is a seat which is not reserved, and the passenger is permitted to enter each
%country on the way.
% --- Problem 7. Compute if a passenger can book a flight from one airport to another.

% find legs connecting airports
findLegs(Origin, Dest, Legs) :-
        leg(Origin, Dest, Servicer, Aircraft),
        append([], [[Origin, Dest, Servicer, Aircraft]], Legs).
findLegs(Origin, Dest, Legs) :-
        not(leg(Origin, Dest, Servicer, Aircraft)),
        leg(Origin, OtherDest, Servicer, Aircraft),
        findLegs(OtherDest, Dest, Res),
        append([[Origin, OtherDest, Servicer, Aircraft]], Res, Legs).
tflt(L) :- findLegs(aal, lon, L).

% calculate if each leg has free seats
freeSeats([]).
freeSeats(Legs) :-
        [[Origin, Dest, _, Aircraft]|Tail] = Legs.


% passenger can enter each country
canEnterCountries(_, []).
canEnterCountries(Pid, Legs) :-
        [[Origin, Dest, _, _]|Tail] = Legs,
        mayFlyTo(Pid, Origin),
        mayFlyTo(Pid, Dest),
        canEnterCountries(Pid, Tail).

% passenger can book flight
canBook(Pid, Origin, Dest) :-
        findLegs(Origin, Dest, Legs),
        freeSeats(Legs),
        canEnterCountries(Pid, Legs).

canBookTest :- canBook(4, aal, lon).





%• As above, but each leg must be on the same airline.
%• As above, but each leg must be on a Boeing aircraft.
%• As above, but each leg must be with a window seat.



% --- Problem 8. (Optional, you do not have to solve this problem to receive full credit.)
%Compute if two passengers can book a flight from one airport to another airport. The
%two passengers must travel on business class, have adjacent seats, and one of the seats
%must be a window seat.




%\+((treeHeight(T, Y), Y > X))


