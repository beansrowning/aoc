import os.path as path
import re

data_file = path.join("01", "data")

nums = [
    "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "nine",
    r"\d"
]

nums_dict = {val: i + 1 for i, val in enumerate(nums[:-1])}
nums_dict.update({str(i + 1): i + 1 for i in range(9)})

# Learned: Using a lookahead assertion here simplifies greatly
num_reg_str = "(?=(" + "|".join(nums) + "))"
num_reg = re.compile(num_reg_str)

def main():
    with open(data_file, "r") as f:
        raw_data = f.read().splitlines()
    
    matches = [[(mat.start(), mat.group(1)) for mat in num_reg.finditer(string)] for string in raw_data]
    digits = [(10 * int(nums_dict[mat[0][1]])) + int(nums_dict[mat[-1][1]]) for mat in matches]

    print(sum(digits))
    # 54980

if __name__ == "__main__":
    main()