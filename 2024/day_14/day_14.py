import re

def read_line_by_line(path, separator):
    with open(path, 'r') as f:
        return [line.strip() for line in f]

class Robot:
    def __init__(self, x, y, v_x, v_y):
        self.x = x
        self.y = y
        self.v_x = v_x
        self.v_y = v_y
    def __str__(self):
        return f"(x, y): ({self.x}, {self.y}), speed: ({self.v_x}, {self.v_y})"

def read_robots(content):
    robots = []
    for text in content:
        pattern = r'[-]?\d+'
        numbers = [int(num) for num in re.findall(pattern, text)]
        robots.append(Robot(numbers[0], numbers[1], numbers[2], numbers[3]))
    return robots

def problem(robots, seconds, rows, cols, visualize):
    # No visualization
    if not visualize:
        for r in robots:
            r.x += seconds * r.v_x
            r.y += seconds * r.v_y
            r.x %= cols
            r.y %= rows
    # With visualization
    else:
        iteration = 1
        while(True):
            for r in robots:
                r.x += r.v_x
                r.y += r.v_y
                r.x %= cols
                r.y %= rows
            robots_coords = set(list(map(lambda r: (r.x, r.y), robots)))
            print(f"Iteration {iteration}")
            iteration += 1
            curr_row = ""
            for r in range(rows):
                for c in range(cols):
                    if (r, c) in robots_coords:
                        curr_row += '#'
                    else:
                        curr_row += '.'
                curr_row += '\n'
            # Basic heuristic, if the Christmas tree is displayed a contiguous line of # should appear (chosen length is random)
            if "##########################" in curr_row:
                print(curr_row)
                input()
                return iteration

    first_quadrant = [r for r in robots if 0 <= r.x < cols // 2 and 0 <= r.y < rows // 2]
    second_quadrant = [r for r in robots if 1 + cols // 2 <= r.x < cols and 0 <= r.y < rows // 2]
    third_quadrant = [r for r in robots if 0 <= r.x < cols // 2 and 1 + rows // 2 <= r.y < rows]
    fourth_quadrant = [r for r in robots if 1 + cols // 2 <= r.x < cols and 1 + rows // 2 <= r.y < rows]
    return len(first_quadrant) * len(second_quadrant) * len(third_quadrant) * len(fourth_quadrant)


content = read_line_by_line("../day_14/input/input.txt", '\n')
cols = 101
rows = 103

robots = read_robots(content)
part1 = print(problem(robots, 100, rows, cols, False))
robots = read_robots(content)
part2 = print(problem(robots, 0, rows, cols, True))
