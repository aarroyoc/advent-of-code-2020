import re
from dataclasses import dataclass
from collections import Counter

@dataclass
class Policy:
    letter: str
    one: int
    two: int

@dataclass
class Password:
    password: str
    policy: Policy

    def is_valid(self):
        a = self.policy.one - 1
        b = self.policy.two - 1
        if a >= len(self.password) or b >= len(self.password):
            return False
        
        if self.password[a] == self.policy.letter and not (self.password[b] == self.policy.letter):
            return True
        if not (self.password[a] == self.policy.letter) and self.password[b] == self.policy.letter:
            return True
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