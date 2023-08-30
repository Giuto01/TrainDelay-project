import networkx as nx

G = nx.read_gexf("station_graph.gexf")
is_connected = nx.has_path(G, "s11502", "s01700")

print(is_connected)
