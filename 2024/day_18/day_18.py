from collections import deque

def read_line_by_line(path, separator):
    with open(path, 'r') as f:
        return [line.strip() for line in f]

def parse_coordinates(content):
    corrupted_bytes = []
    for line in content:
        row, col = line.split(',')
        corrupted_bytes.append((int(row), int(col)))
    return corrupted_bytes

def in_grid(x, y, rows, cols):
    return 0 <= x < rows and 0 <= y < cols

def problem(corrupted_bytes, rows, cols):
    visited = set()
    directions = [(0, 1), (1, 0), (0, -1), (-1, 0)]
    queue = deque([(0, 0, 0)])

    while queue:
        curr_x, curr_y, cost = queue.popleft()
        if (curr_x, curr_y) == (rows - 1, cols - 1):
            return cost
        if (curr_x, curr_y) not in visited:
            visited.add((curr_x, curr_y))
            for dx, dy in directions:
                nx, ny = curr_x + dx, curr_y + dy
                if (nx, ny) not in visited and in_grid(nx, ny, rows, cols) and (nx, ny) not in corrupted_bytes:
                    queue.append((nx, ny, cost + 1))
    return -1


content = read_line_by_line("./input/input.txt", '\n')
corrupted_bytes = parse_coordinates(content)

rows = 71
cols = 71
print(problem(set(corrupted_bytes[:1024]), rows, cols))

rows = 71
cols = 71
for i in range(len(corrupted_bytes)):
    if problem(set(corrupted_bytes[:i]), rows, cols) == -1:
        print(corrupted_bytes[i-1])
        break
