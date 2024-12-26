import networkx as nx

def read_to_2d(path):
    with open(path, 'r') as f:
        return [list(line.strip()) for line in f]

def build_graph(grid, rows, cols):
    G = nx.DiGraph()
    directions = [(0, 1), (1, 0), (0, -1), (-1, 0)]
    start_pos = (0, 0)
    end_pos = (0, 0)

    for r in range(rows):
        for c in range(cols):
            # Wall
            if grid[r][c] == '#':
                continue
            # 4 nodes for each cell as they represent the various orientations
            for dx, dy in directions:
                if grid[r][c] == 'S':
                    start_pos = (r, c)
                if grid[r][c] == 'E':
                    end_pos = (r, c)
                G.add_node((r, c, dx, dy))

    for (curr_x, curr_y, curr_dx, curr_dy) in G.nodes:
        # Step ahead
        if grid[curr_x + curr_dx][curr_y + curr_dy] != '#':
            G.add_edge((curr_x, curr_y, curr_dx, curr_dy), (curr_x + curr_dx, curr_y + curr_dy, curr_dx, curr_dy), weight=1)
        # Rotations
        rot1_dx, rot1_dy = int((complex(curr_dx, curr_dy) * complex(0, 1)).real), int((complex(curr_dx, curr_dy) * complex(0, 1)).imag)
        G.add_edge((curr_x, curr_y, curr_dx, curr_dy), (curr_x, curr_y, rot1_dx, rot1_dy), weight=1000)
        rot2_dx, rot2_dy = int((complex(curr_dx, curr_dy) * complex(0, -1)).real), int((complex(curr_dx, curr_dy) * complex(0, -1)).imag)
        G.add_edge((curr_x, curr_y, curr_dx, curr_dy), (curr_x, curr_y, rot2_dx, rot2_dy), weight=1000)

    return G, start_pos, end_pos

def problem1(graph, start_pos, end_pos):
    ends = [(end_pos[0], end_pos[1], 0, 1), (end_pos[0], end_pos[1], 1, 0), (end_pos[0], end_pos[1], 0, -1), (end_pos[0], end_pos[1], -1, 0)]
    lengths = []
    for end in ends:
        lengths.append(nx.shortest_path_length(graph, (start_pos[0], start_pos[1], 0, 1), end, weight='weight'))
    return min(lengths)

def compute_length(graph, path):
    weight = 0
    for i in range(len(path) - 1):
        weight += graph[path[i]][path[i+1]]['weight']
    return weight

def problem2(graph, start_pos, end_pos, length):
    ends = [(end_pos[0], end_pos[1], 0, 1), (end_pos[0], end_pos[1], 1, 0), (end_pos[0], end_pos[1], 0, -1), (end_pos[0], end_pos[1], -1, 0)]
    traversed = set()
    for end in ends:
        paths = list(nx.all_shortest_paths(graph, (start_pos[0], start_pos[1], 0, 1), end, weight='weight'))
        # Check if it is the actual optimal path
        if compute_length(graph, [p for p in paths[0]]) == length:
            for path in paths:
                for (curr_x, curr_y, _, _) in path:
                  traversed.add((curr_x, curr_y))
    return len(traversed)

content = read_to_2d("./input/input.txt")
rows = len(content)
cols = len(content[0])

graph, start_pos, end_pos = build_graph(content, rows, cols)

part1 = problem1(graph, start_pos, end_pos)
print(part1)

part2 = problem2(graph, start_pos, end_pos, part1)
print(part2)
