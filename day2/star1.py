import re
from dataclasses import dataclass
from collections import Counter

@dataclass
class Policy:
    letter: str
    min: int
    max: int

@dataclass
class Password:
    password: str
    policy: Policy

    def is_valid(self):
        c = Counter(self.password)
        if length := c.get(self.policy.letter):
            return self.policy.min <= length <= self.policy.max
        else:
            return False

def main():
    with open("day2/input.dat") as f:
        lines = f.readlines()
    passwords = []
    for line in lines:
        res = re.match(r"(\d+)-(\d+) (\w): (\w+)", line)
        passwords.append(Password(res.group(4), Policy(res.group(3), int(res.group(1)), int(res.group(2)))))
    
    valid = 0
    for password in passwords:
        if password.is_valid():
            valid += 1
    return valid


if __name__ == "__main__":
    print(main())
