from collections import deque

def read_to_2d(path):
    with open(path, "r") as f:
        return [list(map(lambda x: int(x), line.strip())) for line in f]

def find_trailheads(trailmap):
    trailheads = []
    for i in range(0, len(trailmap)):
        for j in range(0, len(trailmap[0])):
            if trailmap[i][j] == 0:
                trailheads.append((i, j))
    return trailheads

def problem(trailmap, trailheads, rows, cols, part2):
    directions = [(-1, 0), (0, 1), (1, 0), (0, -1)]
    score = 0

    for trailhead in trailheads:
        visited = set()
        queue = deque([trailhead])
        visited.add(trailhead)

        if not part2:
            while queue:
                x, y = queue.popleft()
                if trailmap[x][y] == 9:
                    score += 1
                for dx, dy in directions:
                    nx, ny = x + dx, y + dy
                    if 0 <= nx < rows and 0 <= ny < cols and ((nx, ny) not in visited):
                        if trailmap[nx][ny] == trailmap[x][y] + 1:
                            visited.add((nx, ny))
                            queue.append((nx, ny))
        else:
            while queue:
                x, y = queue.popleft()
                if trailmap[x][y] == 9:
                    score += 1
                for dx, dy in directions:
                    nx, ny = x + dx, y + dy
                    if 0 <= nx < rows and 0 <= ny < cols:
                        if trailmap[nx][ny] == trailmap[x][y] + 1:
                            queue.append((nx, ny))
    return score

content = read_to_2d("./input/input.txt")
trailheads = find_trailheads(content)
print(problem(content, trailheads, len(content), len(content[0]), False))
print(problem(content, trailheads, len(content), len(content[0]), True))
