import PrologEngine
from prettytable import PrettyTable
import re


def search(engine, departure, arrival, time):

    stations = engine.trains_departure_between_stations_name_at_time(departure, arrival, time)

    # Creazione dell'oggetto PrettyTable
    table = PrettyTable()
    table.border = True
    table.padding_width = 3
    # Definizione delle colonne
    table.field_names = ["TrainID", "TrainType", "DepartureStation", "ArrivalStation", "DepartureTime", "ArrivalTime"]
    table.sortby = "ArrivalTime"
    for station in stations:
        train = engine.get_train_fact_by_ID(station)
        trainID = station
        trainType = train["Type"]
        departureStationName = engine.station_name_by_ID(train["DepartureID"])
        arrivalStationName = engine.station_name_by_ID(train["ArrivalID"])
        departureTime = train["DepartureTime"]
        arrivalTime = train["ArrivalTime"]
        table.add_row([trainID, trainType, re.sub("(b|')", "", str(departureStationName)),  re.sub("(b|')", "", str(arrivalStationName)), departureTime, arrivalTime], divider=True)

    print(table)

def main():
    engine = PrologEngine.PrologEngine("Knowledge base/trenitalia.pl", "Knowledge base/rules.pl","Knowledge base/station.pl")
    print("Benvenuto nel sistema di ricerca treni!")

    while True:
        departure_station = input("Inserisci la stazione di partenza: ").upper()
        arrival_station = input("Inserisci la stazione di destinazione: ").upper()
        departure_time = input("Inserisci l'ora di partenza (HH:MM): ").replace(".", ":").replace(" ", "") 

        available_trains = search(engine, departure_station, arrival_station, departure_time)

        if available_trains:
            print("Treni disponibili:")

        else:
            print("Nessun treno disponibile per la tua ricerca.")

        cont = input("Vuoi effettuare un'altra ricerca? (s/n): ")
        if cont.lower() != 's':
            print("Grazie per aver utilizzato il nostro sistema.")
            break

if __name__ == "__main__":
    main()
