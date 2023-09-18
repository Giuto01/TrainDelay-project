import networkx as nx

G = nx.read_gexf("station_graph.gexf")

print(nx.shortest_path(G, "s11502", "s01700"))