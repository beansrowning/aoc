from os import path

data_file = path.join("14", "data")

def main() -> int:
    global data_file

    with open(data_file, "r") as file:
        data = [*map(lambda x: [*x], file.read().splitlines())]
    
    transposed = [[elem[i] for elem in data] for i in range(len(data[0]))]

    new_cols = []
    col_load = 0
    for col in transposed:
        new_col = []
        moving_blocks = 0

        # Reverse so they slide north
        col.reverse()

        for elem in col:
            if elem == "O":
                moving_blocks += 1
                new_col.append(".")
            elif elem == "#":
                # Pop past insertions, insert blocks
                # up to the non-movable one
                if moving_blocks > 0:
                    new_col = new_col[:-moving_blocks]
                    new_col.extend(["O"] * moving_blocks)
                    moving_blocks = 0
                new_col.append("#")
            else:
                # Just append the empty space
                new_col.append(".")

        # End of col, check whether we had
        # leftover ones to stack
        if moving_blocks > 0:
            new_col = new_col[:-moving_blocks]
            new_col.extend(["O"] * moving_blocks)

        # Compute col load
        col_load += sum([i + 1 if el == "O" else 0 for i, el in enumerate(new_col)])
        new_col.reverse()
        new_cols.append(new_col)
    
    # Write out solution
    with open(path.join("14", "out"), "w") as file:
        lines = [[elem[i] for elem in new_cols] for i in range(len(data[0]))]
        lines = ["".join(line) + "\n" for line in lines]

        file.writelines(lines)

    return(col_load)

if __name__ == "__main__":
    print(main())
    # 105461
