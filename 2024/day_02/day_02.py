def readLines(path):
    with open(path, 'r') as file:
        return [line.strip() for line in file]

def parseReports(lines):
    reports = []
    for line in lines:
        curr_report = []
        for num in line:
            curr_report.append(int(num))
        reports.append(curr_report)
    return reports


def all_sublists(lst):
    return [lst[:i] + lst[i+1:] for i in range(len(lst))]


def criteria(diff_list):
    # First criterion
    first_crit = list(filter(lambda x: abs(x) >= 1 and abs(x) <= 3, diff_list))
    if (len(first_crit) != len(diff_list)):
        return 0

    # Second criterion
    second_crit_1 = list(filter(lambda x: x > 0, diff_list))
    second_crit_2 = list(filter(lambda x: x < 0, diff_list))
    if (len(second_crit_1) != len(diff_list) and len(second_crit_2) != len(diff_list)):
        return 0
    return 1


def problem(reports, first_part):
    safe_lists = 0
    for rep in reports:
        diff_list = []
        for i in range(len(rep) - 1):
            diff_list.append(rep[i] - rep[i+1])

        # No tolerance
        if (first_part):
            safe_lists += criteria(diff_list)
            continue

        # One fault tolerated
        lists = all_sublists(rep)
        compatible_lists = 0
        for lst in lists:
            diff_list = []
            for i in range(len(lst) - 1):
                diff_list.append(lst[i] - lst[i+1])
            compatible_lists += criteria(diff_list)
        if (compatible_lists >= 1):
            safe_lists += 1

    return safe_lists


lines = [line.split(' ') for line in readLines("day_02/input/input.txt")]
reports = parseReports(lines)

print(problem(reports, True))
print(problem(reports, False))
