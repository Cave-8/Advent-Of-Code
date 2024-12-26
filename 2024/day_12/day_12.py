from collections import deque

def read_to_2d(path):
    with open(path, 'r') as f:
        return [list(line.strip()) for line in f]

def compute_perimeter(coord, garden, rows, cols):
    perimeter = 4
    directions = [(0, 1), (1, 0), (-1, 0), (0, -1)]

    for dx, dy in directions:
        nx, ny = coord[0] + dx, coord[1] + dy
        if 0 <= nx < rows and 0 <= ny < cols:
            if garden[nx][ny] == garden[coord[0]][coord[1]]:
                perimeter -= 1
    return perimeter

def compute_shape(shape, garden, rows, cols):
    sides = 0

    # Scan by row
    for row in range(rows):
        scanning_up = False
        scanning_down = False
        for col in range(cols):
            if (row, col) in shape:
                if (row-1, col) not in shape and (not scanning_up):
                    scanning_up = True
                    sides += 1
                if (row+1, col) not in shape and (not scanning_down):
                    scanning_down = True
                    sides += 1
                if (row-1, col) in shape:
                    scanning_up = False
                if (row+1, col) in shape:
                    scanning_down = False
            else:
                scanning_up = False
                scanning_down = False

    # Scan by column
    for col in range(cols):
        scanning_left = False
        scanning_right = False
        for row in range(rows):
            if (row, col) in shape:
                if (row, col-1) not in shape and (not scanning_left):
                    scanning_left = True
                    sides += 1
                if (row, col+1) not in shape and (not scanning_right):
                    scanning_right = True
                    sides += 1
                if (row, col-1) in shape:
                    scanning_left = False
                if (row, col+1) in shape:
                    scanning_right = False
            else:
                scanning_left = False
                scanning_right = False
    return sides

def problem(garden, rows, cols, part2):
    directions = [(1, 0), (0, -1), (-1, 0), (0, 1)]
    visited = [[False] * cols for _ in range(rows)]
    result = 0

    for row in range(rows):
        for col in range(cols):
            if not visited[row][col]:
                area = 0
                perimeter = 0
                queue = deque([(row, col)])
                visited[row][col] = True
                shape = set()                                                       # Only for part 2
                while queue:
                    curr_row, curr_col = queue.popleft()
                    area += 1
                    perimeter += compute_perimeter((curr_row, curr_col), garden, rows, cols)
                    shape.add((curr_row, curr_col))                                 # Only for part 2
                    for dr, dc in directions:
                        new_row, new_col = curr_row + dr, curr_col + dc
                        if 0 <= new_row < rows and 0 <= new_col < cols and not visited[new_row][new_col] and garden[new_row][new_col] == garden[row][col]:
                            visited[new_row][new_col] = True
                            queue.append((new_row, new_col))
                if part2:
                    perimeter = compute_shape(shape, garden, rows, cols)            # Only for part 2
                result += area*perimeter
    return result

content = read_to_2d("./input/input.txt")
rows = len(content)
cols = len(content[0])
print(problem(content, rows, cols, False))
print(problem(content, rows, cols, True))
