import networkx as nx
import re
import prolog_engine
from prettytable import PrettyTable


def search(departure, arrival, time):
    '''
    Search for trains between two stations for a given departure time.

    Args:
        departure (str): The name of the departure station.
        arrival (str): The name of the arrival station.
        time (str): The departure time in the "HH:MM" format.

    Returns:
        PrettyTable or None: A formatted table with details of available trains
        or None if no trains are available.
    '''

    engine = prolog_engine.PrologEngine("knowledge base/trenitalia_schedule.pl", "knowledge base/rules.pl","knowledge base/stations.pl")

    trainsList = engine.trains_departure_between_stations_name_at_time(departure, arrival, time)

    # Creazione dell'oggetto PrettyTable
    table = PrettyTable()
    table.border = True
    table.padding_width = 3
    # Definizione delle colonne
    table.field_names = ["TrainID", "TrainType", "DepartureStation", "ArrivalStation", "DepartureTime", "ArrivalTime"]
    table.sortby = "ArrivalTime"
    table.title = "\033[1m"+ str(departure) + " --> " + str(arrival) + "\033[0m"

    if len(trainsList) != 0:
    
        for train in trainsList:
            trainInfo = engine.get_train_fact_by_ID(train)
            trainID = train
            trainType = trainInfo["Type"]
            departureStationName = re.sub("(b|')", "", str(engine.station_name_by_ID(trainInfo["DepartureID"])))
            arrivalStationName = re.sub("(b|')", "", str(engine.station_name_by_ID(trainInfo["ArrivalID"])))
            departureTime = trainInfo["DepartureTime"]
            arrivalTime = trainInfo["ArrivalTime"]
            table.add_row([trainID, trainType, departureStationName, arrivalStationName, departureTime, arrivalTime], divider=True)

        return table
    else:
        return None

def searchItinerary(graph, departure, arrival):
    '''
    Search for the shortest itinerary between two stations using a railway connections graph.

    Args:
        graph (networkx.Graph): The graph of railway connections between stations.
        departure (str): The name of the departure station.
        arrival (str): The name of the arrival station.

    Returns:
        None: The function prints tables with details of trains for each leg of the journey.
    '''

    engine = prolog_engine.PrologEngine("knowledge base/trenitalia_schedule.pl", "knowledge base/rules.pl","knowledge base/stations.pl")

    departureID = engine.station_ID_by_name(departure)
    arrivalID = engine.station_ID_by_name(arrival)

    path = nx.shortest_path(graph, departureID, arrivalID)

    if len(path) != 0 and path[0] == departureID:
            
            try:
                for index, station in enumerate(path):
                    station_name = re.sub("(b|')", "", str(engine.station_name_by_ID(station)))
                    next_station_name = re.sub("(b|')", "", str(engine.station_name_by_ID(path[index+1])))

                    trains = engine.trains_departure_between_stations_name(station_name, next_station_name)
                    if len(trains) != 0:
                        # Creazione dell'oggetto PrettyTable
                        table = PrettyTable()
                        table.border = True
                        table.padding_width = 3
                        # Definizione delle colonne
                        table.field_names = ["TrainID", "DepartureTime", "ArrivalTime"]
                        table.sortby = "DepartureTime"
                        table.title = "\033[1m"+ station_name + " --> " + next_station_name + "\033[0m"
                        for trainID in trains:
                            tr = engine.get_train_fact_by_ID(trainID)
                            departureTime = tr["DepartureTime"]
                            arrivalTime = tr["ArrivalTime"]
                            table.add_row([trainID, departureTime, arrivalTime], divider=True)
                        print(table)
            except:
                pass
    else:
        print("\nNessun itineriario disponibile per la tua ricerca. 😥\n")
