from collections import deque

def read_to_2d(path):
    with open(path, 'r') as f:
        return [list(line.strip()) for line in f]

def in_grid(r, c, rows, cols):
    return 0 <= r < rows and 0 <= c < cols

def bfs(grid, rows, cols):
    start_pos = (0, 0)
    end_pos = (0, 0)
    walls = set()
    for r in range(rows):
        for c in range(cols):
            if grid[r][c] == 'S':
                start_pos = (r, c)
            if grid[r][c] == 'E':
                end_pos = (r, c)
            if grid[r][c] == '#':
                walls.add((r, c))
    queue = deque([(start_pos, [start_pos])])
    visited = set()
    directions = [(0, 1), (1, 0), (0, -1), (-1, 0)]
    while queue:
        (curr_x, curr_y), curr_path = queue.popleft()
        if (curr_x, curr_y) == end_pos:
            return curr_path + [end_pos], start_pos, end_pos, walls
        if (curr_x, curr_y) not in visited:
            visited.add((curr_x, curr_y))
            for dx, dy in directions:
                nx, ny = curr_x + dx, curr_y + dy
                if in_grid(nx, ny, rows, cols) and (nx, ny) not in walls and (nx, ny) not in visited:
                    queue.append(((nx, ny), curr_path + [(curr_x, curr_y)]))

def problem(grid, optimal_path, threshold, spared_picoseconds, rows, cols):
    cheats = 0
    for i in range(0, len(optimal_path)):
        for j in range(i + spared_picoseconds, len(optimal_path)):
            curr_dist = abs(optimal_path[i][0] - optimal_path[j][0]) + abs(optimal_path[i][1] - optimal_path[j][1])
            if curr_dist <= threshold:
                if j - i - curr_dist >= spared_picoseconds and (j - i- curr_dist) % 2 == 0:
                    cheats += 1
    return cheats

content = read_to_2d("./input/input.txt")
optimal_path, start_pos, end_pos, walls = bfs(content, len(content), len(content[0]))

part1 = problem(content, optimal_path, 2, 100, len(content), len(content[0]))
print(part1)

part2 = problem(content, optimal_path, 20, 100, len(content), len(content[0]))
print(part2)
