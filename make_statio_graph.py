import networkx as nx
import pandas as pd
from pyswip import Prolog

def get_stops(departureStationID):
    '''
    Get the list of stops for a train based on the departure station.

    This method utilizes a Prolog knowledge base containing information about trains
    and their stops. It queries the knowledge base to retrieve the stops of a train
    that has the specified departure station. It returns a list of stops.

    Parameters:
    departureStationID (str): The departure station ID of the train.

    Returns:
    list: A list of stops for the train with the specified departure station.
    '''

    prolog = Prolog()
    prolog.consult("Knowledge base/trenitalia_schedule.pl")
    stops = []
    for solution in prolog.query(f"train(_, _, {departureStationID}, _, _, _, Stops)."):
        stops = solution["Stops"]
    return stops

# Create empty graph
station_graph = nx.Graph()

data = pd.read_csv("Dataset/stazioni_regioni.csv")

# Add nodes to graph
for i in data.index:
    station_graph.add_node(data["idStazione"][i].lower())

# Add edge to graph
G_copy = station_graph.copy()
for node in G_copy.nodes():
    print(node)
    stops = get_stops(node)
    for stop in stops:
        if str(stop) != str(node):
            station_graph.add_edge(node, stop)

# Save graph
nx.write_gexf(station_graph, "graphs/station_graph.gexf")
