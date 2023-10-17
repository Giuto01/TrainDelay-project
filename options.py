import networkx as nx
import re
from knowledge_base import knowledge_engine
from prettytable import PrettyTable
import utils

def search(departure, arrival, time, model):
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

    engine = knowledge_engine.PrologEngine(
        "knowledge_base/trenitalia_schedule.pl", "knowledge_base/rules.pl", "knowledge_base/stations.pl")
    # Get train list for knowledge base
    trainsList = engine.trains_departure_between_stations_name_at_time(
        departure, arrival, time)

    table = PrettyTable()
    table.border = True
    table.padding_width = 3
    table.field_names = ["TrainID", "TrainType", "DepartureStation",
                         "ArrivalStation", "DepartureTime", "ArrivalTime", "Predicted delay (AI)"]
    table.sortby = "ArrivalTime"
    table.title = "\033[1m" + str(departure) + \
        " --> " + str(arrival) + "\033[0m"

    if len(trainsList) != 0:

        for train in trainsList:
            trainInfo = engine.get_train_fact_by_ID(train)
            trainID = train
            trainType = trainInfo["Type"]
            departureStationName = re.sub("(b|')", "", str(
                engine.station_name_by_ID(trainInfo["DepartureID"])))
            arrivalStationName = re.sub("(b|')", "", str(
                engine.station_name_by_ID(trainInfo["ArrivalID"])))
            departureTime = trainInfo["DepartureTime"]
            arrivalTime = trainInfo["ArrivalTime"]
            # Make delay prediction
            prediction = utils.prediction_str(model.predict([[trainID, utils.time_to_minutes(
                departureTime), utils.time_to_minutes(arrivalTime), utils.bin_train_type(trainType)]]))
            table.add_row([trainID, trainType, departureStationName, arrivalStationName,
                          departureTime, arrivalTime, prediction], divider=True)

        return table
    else:
        return None


def searchItinerary(graph, departure, arrival, model):
    '''
    Search for the shortest itinerary between two stations using a railway connections graph.

    Args:
        graph (networkx.Graph): The graph of railway connections between stations.
        departure (str): The name of the departure station.
        arrival (str): The name of the arrival station.

    Returns:
        None: The function prints tables with details of trains for each leg of the journey.
    '''

    engine = knowledge_engine.PrologEngine(
        "knowledge_base/trenitalia_schedule.pl", "knowledge_base/rules.pl", "knowledge_base/stations.pl")

    try:
        departureID = engine.station_ID_by_name(departure)
        arrivalID = engine.station_ID_by_name(arrival)
        # Search path between stations
        path = nx.shortest_path(graph, departureID, arrivalID)

        if len(path) != 0 and path[0] == departureID:

            try:
                for index, station in enumerate(path):
                    station_name = re.sub("(b|')", "", str(
                        engine.station_name_by_ID(station)))
                    next_station_name = re.sub("(b|')", "", str(
                        engine.station_name_by_ID(path[index+1])))
                    # Get train list for knowledge base
                    trains = engine.trains_departure_between_stations_name(
                        station_name, next_station_name)
                    if len(trains) != 0:
                        table = PrettyTable()
                        table.border = True
                        table.padding_width = 3
                        table.field_names = [
                            "TrainID", "DepartureTime", "ArrivalTime", "Predicted delay (AI)"]
                        table.sortby = "DepartureTime"
                        table.title = "\033[1m" + station_name + \
                            " --> " + next_station_name + "\033[0m"
                        for trainID in trains:
                            tr = engine.get_train_fact_by_ID(trainID)
                            departureTime = tr["DepartureTime"]
                            arrivalTime = tr["ArrivalTime"]
                            trainType = tr["Type"]
                            if (departureTime == "none" or arrivalTime == "none"):
                                prediction = "Null"
                            else:
                                # Make delay prediction
                                prediction = utils.prediction_str(model.predict([[trainID, utils.time_to_minutes(
                                    departureTime), utils.time_to_minutes(arrivalTime), utils.bin_train_type(trainType)]]))
                            table.add_row(
                                [trainID, departureTime, arrivalTime, prediction], divider=True)
                        print(table)
            except:
                pass
    except:
        print("\nNessun itineriario disponibile per la tua ricerca. 😥\n")
