from collections import deque, defaultdict
import graphviz

class Instruction:
    def __init__(self, op1, op2, op, op_str, dest):
        self.op1 = op1
        self.op2 = op2
        self.op = op
        self.op_str = op_str
        self.dest = dest
    def __str__(self):
        return f"Instruction: {self.op1} {self.op_str} {self.op2} -> {self.dest}"

class Machine:
    def __init__(self, registers):
        self.registers = registers
    def __str__(self):
        return f"Machine: {self.registers}"

def read_line_by_line(path, separator):
    with open(path, "r") as f:
        return [line.strip() for line in f]

def parse_registers(starting_values):
    registers = {}
    for value in starting_values:
        register, val = value.split(': ')
        registers[register] = int(val)
    return Machine(registers=registers)

def parse_precedences(operations_list):
    # List of tuples, first op must be done before second op
    final_precedences = []
    operations = []
    for op in operations_list:
        lst = op.split(' ')
        match lst[1]:
            case 'OR':
                operations.append(Instruction(lst[0], lst[2], lambda x, y: x | y, 'OR', lst[4]))
            case 'AND':
                operations.append(Instruction(lst[0], lst[2], lambda x, y: x & y, 'AND', lst[4]))
            case 'XOR':
                operations.append(Instruction(lst[0], lst[2], lambda x, y: x ^ y, 'XOR', lst[4]))

    for i in range(len(operations)):
        for j in range(i+1, len(operations)):
            if operations[j].op1 == operations[i].dest or operations[j].op2 == operations[i].dest:
                final_precedences.append((operations[i], operations[j]))
            if operations[i].op1 == operations[j].dest or operations[i].op2 == operations[j].dest:
                final_precedences.append((operations[j], operations[i]))
    return final_precedences, operations

def topological_sort(operations, dependencies):
    graph = defaultdict(list)
    in_degree = defaultdict(int)

    for op in operations:
        in_degree[op] = 0
    for op, dep in dependencies:
        graph[dep].append(op)
        in_degree[op] += 1

    queue = deque([op for op in operations if in_degree[op] == 0])
    result = []

    while queue:
        current = queue.popleft()
        result.append(current)

        for neighbor in graph[current]:
            in_degree[neighbor] -= 1
            if in_degree[neighbor] == 0:
                queue.append(neighbor)

    if len(result) == len(operations):
        return result[::-1]
    else:
        # Should not happen
        raise ValueError("There is a cycle in the dependencies (invalid DAG).")

def problem1(machine, op_ordered):
    for instruction in op_ordered:
        machine.registers[instruction.dest] = instruction.op(machine.registers[instruction.op1], machine.registers[instruction.op2])
    registers_list = [(k, v) for k, v in machine.registers.items() if 'z' in k]
    ordered_values = sorted(registers_list, key=lambda x: x[0])
    output = 0
    for i, val in enumerate(ordered_values):
        output += val[1] << i
    return output

def problem2(machine, op_ordered):
    dot = graphviz.Digraph(comment="Circuit")
    for instruction in op_ordered:
        match instruction.op_str:
            case 'OR':
                dot.edge(instruction.op1 + "="  + str(machine.registers[instruction.op1]), instruction.dest + "=" + str(machine.registers[instruction.dest]), color='green')
                dot.edge(instruction.op2 + "=" + str(machine.registers[instruction.op2]), instruction.dest + "=" + str(machine.registers[instruction.dest]), color='green')
            case 'AND':
                dot.edge(instruction.op1 + "="  + str(machine.registers[instruction.op1]), instruction.dest + "=" + str(machine.registers[instruction.dest]), color='blue')
                dot.edge(instruction.op2 + "=" + str(machine.registers[instruction.op2]), instruction.dest + "=" + str(machine.registers[instruction.dest]), color='blue')
            case 'XOR':
                dot.edge(instruction.op1 + "="  + str(machine.registers[instruction.op1]), instruction.dest + "=" + str(machine.registers[instruction.dest]), color='red')
                dot.edge(instruction.op2 + "=" + str(machine.registers[instruction.op2]), instruction.dest + "=" + str(machine.registers[instruction.dest]), color='red')
    dot.render("Circuit", format="png", view=True)

content = read_line_by_line("./input/input.txt", "\n")
starting_values, precedences = content[:content.index('')], content[content.index('')+1:]

machine = parse_registers(starting_values)
precedences, operations = parse_precedences(precedences)
op_ordered = topological_sort(operations, precedences)

part1 = problem1(machine, op_ordered)
print(part1)

# Manual inspection
part2 = problem2(machine, op_ordered)

# Half adder for x00 and y00 (S = x00 ^ y00 and C = x00 & y00)
# Full adder for the rest (S = x ^ y ^ c_in and C = (x & y) | (c_in & (x ^ y)))
# z45 is carry bit

# XOR = RED
# AND = BLUE
# OR = GREEN

# z08 is generated by an AND                    (switch z08 - ffj)
# z15 is generated by a sequence of AND and XOR (switch dwp - kfm)
# z22 is generated by an AND                    (switch z22 - gjh)
# z31 is generated by an OR                     (switch z31 - jdr)
