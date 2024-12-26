def read_line_by_line(path, separator):
    """
    Given a path and a separator it returns a list containg all lines in the given file.
    The lines are already stripped of any trailing characters.
    """
    with open(path, 'r') as f:
        return [line.strip() for line in f]

def read_to_2d(path):
    """
    Given a path it returns a list of lists (matrix) by the lines in the given file.
    The lines are already stripped of any trailing characters.
    """
    with open(path, 'r') as f:
        return [list(line.strip()) for line in f]
