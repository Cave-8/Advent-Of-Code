import networkx as nx

# Min cut problem, might implement it in Rust, but it's Christmas =(
iterator = open("./input/input.txt").read().strip().split("\n")
G = nx.Graph()

for i in iterator:
    label, c = i.split(": ")
    connected = c.split(" ")
    for c in connected:
        G.add_edge(label, c, capacity=1)

# Find critical edges
critical_edges = nx.minimum_edge_cut(G)
G.remove_edges_from(critical_edges)

# Get two split graphs
first_part, second_part = nx.connected_components(G)

# Result
print(len(first_part) * len(second_part))

