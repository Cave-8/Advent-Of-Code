from itertools import combinations


def read_line_by_line(path, separator):
    with open(path, "r") as f:
        return [line.strip() for line in f]


def read_antennas(content, rows, cols):
    antennas = {}
    for i in range(rows):
        for j in range(cols):
            if content[i][j] != ".":
                if content[i][j] in antennas:
                    antennas[content[i][j]].append((i, j))
                else:
                    antennas[content[i][j]] = [(i, j)]
    return antennas


def distance(antenna_1, antenna_2):
    return (antenna_1[0] - antenna_2[0], antenna_1[1] - antenna_2[1])


def in_grid(antinode, rows, cols):
    return 0 <= antinode[0] < rows and 0 <= antinode[1] < cols


def problem1(antennas, rows, cols):
    antinodes = set()
    for antenna_id in antennas:
        for antenna_1, antenna_2 in combinations(antennas.get(antenna_id), 2):
            dx, dy = distance(antenna_1, antenna_2)
            curr_antinodes = [
                (x[0] + y[0], x[1] + y[1])
                for x in [antenna_1, antenna_2]
                for y in [(dx, dy), (-dx, -dy)]
            ]
            curr_antinodes = list(
                filter(
                    lambda x: in_grid(x, rows, cols)
                    and x != antenna_1
                    and x != antenna_2,
                    curr_antinodes,
                )
            )
            for x in curr_antinodes:
                antinodes.add(x)
    return len(antinodes)


def problem2(antennas, rows, cols):
    antinodes = set()
    for antenna_id in antennas:
        for antenna_1, antenna_2 in combinations(antennas.get(antenna_id), 2):
            dx, dy = distance(antenna_1, antenna_2)
            iterations = 1
            while True:
                new_antinodes = 0
                curr_antinodes = [
                    (x[0] + y[0], x[1] + y[1])
                    for x in [antenna_1, antenna_2]
                    for y in [
                        (iterations * dx, iterations * dy),
                        (-dx * iterations, -dy * iterations),
                    ]
                ]
                curr_antinodes = list(
                    filter(lambda x: in_grid(x, rows, cols), curr_antinodes)
                )
                for x in curr_antinodes:
                    antinodes.add(x)
                    new_antinodes += 1
                if new_antinodes == 0:
                    break
                iterations += 1
    return len(antinodes)


content = read_line_by_line("./day_08/input/input.txt", "\n")
rows = len(content)
cols = len(content[0])
antennas = read_antennas(content, rows, cols)

part1 = problem1(antennas, rows, cols)
print(part1)
part2 = problem2(antennas, rows, cols)
print(part2)
