
% --- Problem 1. Introduce appropriate predicates for the entities in the system.

% airline(name).
% airport(code, city, country).
% aircraft(reg, owner, modelName).
% model(name, class, manufactorer).
% seat(aircraft, number, class, type).
% adjacantSeat(seat1, seat2).
% passenger(first, last, birthday).
% passport(owner, country).
% leg(origin, destination, servicer, aircraft).
% reservation(code, passenger, origin, destination, airline, seatNumber)
% itinerary(code, reservationCode).
% viseAgreement(countryA, countryB).


% --- Problem 2. Introduce facts for your choosen predicates.

airline(sas).
airline(norwegian).

airport(aal, aalborg, denmark).
airport(lon, london, england).
airport(agb, augsburg, germany).
airport(ruh, riyadh, "Saudi Arabia").

aircraft(1, sas, cesna).
seat(1, "1A", economy, window).
seat(1, "1B", economy, aisle).
adjacantSeat(1, "1A", "1B");

aircraft(2, norwegian, "Airbus A380").
seat(2, "1A", business, window).
seat(2, "1B", business, other).
seat(2, "1C", business, aisle).
adjacantSeat(2, "1A", "1B").
adjacantSeat(2, "1B", "1C").

model(cesna, light, "Airbus Industrie").
model("Airbus A380", heavy, "Textron Aviation").

passenger(dario, wunsch, "13-7-1990").
passenger(jens, aasgaard, "28-7-1990").
passenger(grzegorz, komincz, "14-12-1993").
passenger(patrick, brix, "4-12-1992").

passport(dario, wunsch, germany).
passport(jens, aasgaard, norway).
passport(grzegorz, komincz, poland).
passport(patrick, brix, denmark).
passport(patrick, brix, sweden).



weather(clear).
weather(cloudy).
weather(thunderstorm).



%A passenger is allowed to fly into an airport if he or she holds a passport from
%the country where the airport resides or if there is a visa agreement between
%the country of the passport holder and the country of the airport.
% --- Problem 3. Compute the airports a passenger may fly into.







%A passenger may have a reservation which is illegal in the sense that he or
%she is not permitted to enter the country of the destination airport.
% --- Problem 4. Compute the passengers that have illegal reservations.




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

