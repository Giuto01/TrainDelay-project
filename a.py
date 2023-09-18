from pyswip import Prolog

def time_to_minutes(time):
    hour, minutes = time.split(":")
    return int(hour)*60 + int(minutes)


departure_time = []
departure_station = "s11119"
arrival_station = "s05043"
prolog = Prolog()
prolog.consult("Knowledge base/trenitalia.pl")
for sol in list(prolog.query(f"train(_, _, {departure_station}, {arrival_station}, DepartureTime, ArrivalTime, _)")):
    print(sol["DepartureTime"],"---" ,sol["ArrivalTime"])