def read_line_by_line(path, separator):
    with open(path, 'r') as f:
        return [int(line.strip()) for line in f]

def procedure(number):
    res1 = (number ^ number << 6) % 16777216
    res2 = (res1 ^ res1 >> 5) % 16777216
    return (res2 ^ res2 << 11) % 16777216

def problem1(content):
    lists = []
    for num in content:
        lists.append([num := procedure(num) for _ in range(2000)])
    return sum(list(map(lambda x: x[1999], lists)))

def problem2(content):
    sequences = []
    seq_val = []

    # Build sequences
    for num in content:
        res = list(map(lambda x: x % 10, [num := procedure(num) for _ in range(2000)]))
        curr_seq = [(res[i]-res[i-1], res[i+1]-res[i], res[i+2]-res[i+1], res[i+3]-res[i+2]) for i in range(1, 1997)]
        sequences.append(curr_seq)
        temp_seq_val = {}
        for seq, res in zip(curr_seq, [res[i+3] for i in range(1, 1997)]):
            if temp_seq_val.get(seq) is not None:
                continue
            temp_seq_val[seq] = res
        seq_val.append(temp_seq_val)

    # List all common sequences
    common_sequences = set(sequences[0])
    for seq in sequences[1:]:
        common_sequences |= set(seq)

    # Bind each sequence to the total cumulated bananas
    lists = [[dict[seq] if dict.get(seq) is not None else 0 for dict in seq_val] for seq in common_sequences]
    return max(list(map(lambda x: sum(x), lists)))

content = read_line_by_line("./input/input.txt", '\n')

part1 = problem1(content)
print(part1)

part2 = problem2(content)
print(part2)
