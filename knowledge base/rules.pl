% Rule for converting a time in HH:MM format to total minutes
time_in_minutes(Time, Minutes) :-
    split_string(Time, ":", "", [HourString, MinutesString]),
    number_string(Hour, HourString),
    number_string(Min, MinutesString),
    Minutes is Hour * 60 + Min.

% Rule for checking whether one time is greater than or equal to another time
equal_major_time(Time1, Time2) :-
    time_in_minutes(Time1, Minutes1),
    time_in_minutes(Time2, Minutes2),
    Minutes1 >= Minutes2.

% Rule for getting trains departing later at the specified time (HH:MM)
trains_after_time(Time, Trains) :-
    findall(TrainID,
            (train(TrainID, _, _, _, DepartureTime, _, _),
             equal_major_time(DepartureTime, Time)),
            Trains).

% Rule for getting trains arriving before at the specified time (HH:MM)
trains_before_time(Time, Trains) :-
    findall(TrainID,
            (train(TrainID, _, _, _, _, ArrivalTime, _),
            \+equal_major_time(ArrivalTime, Time)),
            Trains).

% Rule for calculating travel time 
travel_time(DepartureTime, ArrivalTime, Duration) :- 
    time_in_minutes(DepartureTime, Minutes1),
    time_in_minutes(ArrivalTime, Minutes2),
    Duration is Minutes2 - Minutes1.

% Rule for finding all trains departing from a station
trains_departure_from_station_name(StationName, Trains) :-
    findall(TrainID, (station(DepartureStationID, StationName, _), train(TrainID, _, DepartureStationID, _, _, _, _)), Trains).

% Rule for finding all trains leaving a station after a specific time (HH:MM) 
trains_departure_from_station_name_at_time(StationName, Departure, Trains) :-
    findall(TrainID, (station(DepartureStationID, StationName, _), train(TrainID, _, DepartureStationID, _, DepartureTime, _, _), equal_major_time(DepartureTime, Departure)), Trains).

% Rule for finding all trains between two stations after a specific time (HH:MM)
trains_departure_between_stations_name_at_time(DepartureStationName, ArrivalStationName, Time, Trains) :-
    findall(TrainID, (station(DepartureStationID, DepartureStationName, _), 
        station(ArrivalStationID, ArrivalStationName, _), 
        train(TrainID, _, DepartureStationID, ArrivalStationID, DepartureTime, _, _), 
        equal_major_time(DepartureTime, Time)), 
        Trains).

% Rule for finding all trains between two stations after a specific time (HH:MM)
trains_departure_between_stations_name(DepartureStationName, ArrivalStationName, Trains) :-
    findall(TrainID, (station(DepartureStationID, DepartureStationName, _), 
        station(ArrivalStationID, ArrivalStationName, _), 
        train(TrainID, _, DepartureStationID, ArrivalStationID, _, _, _)), 
        Trains).

% Rule for getting all the information about a train given the ID
get_train_fact(TrainID, Type, DepartureID, ArrivalID, DepartureTime, ArrivalTime) :-
    train(TrainID, Type, DepartureID, ArrivalID, DepartureTime, ArrivalTime, _).
