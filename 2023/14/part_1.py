from os import path
from functools import reduce

data_file = path.join("14", "sample")

def main() -> int:
    global data_file

    with open(data_file, "r") as file:
        data = [*map(lambda x: [*x], file.read().splitlines())]
    
    transposed = [[elem[i] for elem in data] for i in range(len(data[0]))]

    def run_cycle(rows: list) -> list:
        """
        Run a "cycle"
        rolling the rocks around N, W, S, then E
        """
        out = reduce(tilt, [1, 2, -1, -2], rows)

        return(out)

    def tilt(data: list[list], direction: int, moving_block = "O", non_moving_block = "#") -> list[list]:
        """
        'tilt' data, moving some items to the farthest most extent
        :param: data a nested list in column-major order
        :param: direction an int, one of 1 (col), -1 (col, reversed), 2 (row), -2 (row, reversed)
                indicating the axis of operation
        """
        out = []
        
        if (abs(direction) == 2):
            # Evaluate horizontally
            data_gen = [[x[i] for x in data] for i, _ in enumerate(data[0])]
        else:
            data_gen = data

        # Loop over each col or row
        for axis in data_gen:
            new_col = []
            moving_blocks = 0

            # Reverse axis if called
            if direction < 0:
                axis.reverse()

            for elem in axis:
                if elem == moving_block:
                    moving_blocks += 1
                    new_col.append(".")
                elif elem == non_moving_block:
                    # Pop past insertions, insert blocks
                    # up to the non-movable one
                    if moving_blocks > 0:
                        new_col = new_col[:-moving_blocks]
                        new_col.extend([moving_block] * moving_blocks)
                        moving_blocks = 0
                    new_col.append(non_moving_block)
                else:
                    # Just append the empty space
                    new_col.append(".")

        # End of col, check whether we had
        # leftover ones to stack
        if moving_blocks > 0:
            new_col = new_col[:-moving_blocks]
            new_col.extend([moving_block] * moving_blocks)
        out.append(new_col)

        return(out)

    out = transposed
    # Run 1000000000 times
    for _ in range(1):
        out = run_cycle(out)

    print(transposed)
    print(out)
    # Write out solution
    with open(path.join("14", "out"), "w") as file:
        lines = [[elem[i] for elem in out] for i in range(len(data[0]))]
        lines = ["".join(line) + "\n" for line in lines]

        file.writelines(lines)

    col_load = 0
    col_load += sum([i + 1 if el == "O" else 0 for i, el in enumerate(out)])

    return(col_load)

if __name__ == "__main__":
    print(main())
    # 105461
