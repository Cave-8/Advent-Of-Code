from copy import deepcopy


def readAsMatrix(path):
    with open(path, 'r') as f:
        return [list(line.strip()) for line in f]


def parseGrid(matrix):
    guard = (0, 0, 0, 0)
    obstacles = set()
    for row in range(len(matrix)):
        for col in range(len(matrix[0])):
            if matrix[row][col] == '^':
                guard = (row, col, -1, 0)
            else:
                obstacles.add((row, col))
    return guard, obstacles


def problem1(guard, obstacles, rows, cols):
    visited = set()
    while (0 <= guard[0] < rows and 0 <= guard[1] < cols):
        # Add current position
        visited.add((guard[0], guard[1]))
        # New position
        new_r, new_c = guard[0] + guard[2], guard[1] + guard[3]
        # There's an obstacle
        if ((new_r, new_c) in obstacles):
            # A bit overkill, a simple if else it's enough
            rot = complex(guard[2], guard[3])*complex(0, -1)
            guard = (guard[0], guard[1], int(rot.real), int(rot.imag))
            continue
        # No obstacles ahead
        guard = (new_r, new_c, guard[2], guard[3])
    return len(visited)

# Can be optimized by putting an obstacles only where we wisited in part 1


def problem2(guard, obstacles, rows, cols):
    tests = [(x, y) for x in range(rows) for y in range(cols)]
    original_guard = deepcopy(guard)
    loops = 0
    for test in tests:
        # Setup
        visited = set()
        guard = deepcopy(original_guard)
        new_obstacles = obstacles.copy()
        if (test in obstacles or test == (guard[0], guard[1])):
            continue
        new_obstacles.add(test)
        while (0 <= guard[0] < rows and 0 <= guard[1] < cols):
            # Check for loops
            # The state is the position of the guard plus its orientation
            if (guard in visited):
                loops += 1
                break
            visited.add((guard[0], guard[1], guard[2], guard[3]))
            new_r, new_c = guard[0] + guard[2], guard[1] + guard[3]
            if ((new_r, new_c) in new_obstacles):
                rot = complex(guard[2], guard[3])*complex(0, -1)
                guard = (guard[0], guard[1], int(rot.real), int(rot.imag))
                continue
            guard = (new_r, new_c, guard[2], guard[3])
    return loops


content = readAsMatrix('./day_06/input/input.txt')
rows, cols = len(content), len(content[0])
guard, obstacles = parseGrid(content)
print(problem1(guard, obstacles, rows, cols))
print(problem2(guard, obstacles, rows, cols))
