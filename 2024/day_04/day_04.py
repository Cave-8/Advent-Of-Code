def readAsMatrix(path):
    with open(path, 'r') as f:
        return [list(line.strip()) for line in f]


def problem1(mat):
    rows = len(mat)
    cols = len(mat[0])
    xmas_counter = 0
    for i in range(rows):
        for j in range(cols):
            strings = []
            # Row left to right
            if j <= cols-4:
                strings.append(mat[i][j] + mat[i][j+1] +
                               mat[i][j+2] + mat[i][j+3])
            # Row right to left
            if j >= 3:
                strings.append(mat[i][j] + mat[i][j-1] +
                               mat[i][j-2] + mat[i][j-3])
            # Column up to down
            if i <= rows-4:
                strings.append(mat[i][j] + mat[i+1][j] +
                               mat[i+2][j] + mat[i+3][j])
            # Column down to up
            if i >= 3:
                strings.append(mat[i][j] + mat[i-1][j] +
                               mat[i-2][j] + mat[i-3][j])
            # Diagonal center to top-left
            if i >= 3 and j >= 3:
                strings.append(mat[i][j] + mat[i-1][j-1] +
                               mat[i-2][j-2] + mat[i-3][j-3])
            # Diagonal center to bottom-left
            if j >= 3 and i <= rows-4:
                strings.append(mat[i][j] + mat[i+1][j-1] +
                               mat[i+2][j-2] + mat[i+3][j-3])
            # Diagonal center to top-right
            if i >= 3 and j <= cols-4:
                strings.append(mat[i][j] + mat[i-1][j+1] +
                               mat[i-2][j+2] + mat[i-3][j+3])
            # Diagonal center to bottom-right
            if i <= rows-4 and j <= cols-4:
                strings.append(mat[i][j] + mat[i+1][j+1] +
                               mat[i+2][j+2] + mat[i+3][j+3])
            xmas_counter += len(list(filter(lambda x: x == 'XMAS', strings)))
    return xmas_counter


def problem2(mat):
    rows = len(mat)
    cols = len(mat[0])
    x_mas_counter = 0
    for i in range(rows):
        for j in range(cols):
            # Inner part of matrix
            if i >= 1 and j >= 1 and i < rows - 1 and j < cols - 1:
                first_diag = mat[i-1][j-1] + mat[i][j] + mat[i+1][j+1]
                second_diag = mat[i+1][j-1] + mat[i][j] + mat[i-1][j+1]
                if (first_diag == 'MAS' or first_diag == 'SAM') and (second_diag == 'MAS' or second_diag == 'SAM'):
                    x_mas_counter += 1
    return x_mas_counter


content = readAsMatrix('./day_04/input/input.txt')
result1 = problem1(content)
print(result1)
result2 = problem2(content)
print(result2)
