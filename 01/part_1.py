import os.path as path
import re

data_file = path.join("01", "data")

def main():
    number_re = r"\d{1}"

    with open(data_file, "r") as f:
        raw_data = f.read().splitlines()
    
    all_nums = [re.findall(number_re, st) for st in raw_data]
    filtered_nums = [[num for i, num in enumerate(num_list) if i in [0, len(num_list) - 1]] for num_list in all_nums]
    out = [int("".join(num_list * 2)) if len(num_list) == 1 else int("".join(num_list)) for num_list in filtered_nums]

    print(sum(out))
    # 55816

if __name__ == "__main__":
    main()