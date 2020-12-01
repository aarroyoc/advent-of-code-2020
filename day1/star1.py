
def main():
    with open("day1/input.dat") as f:
        expenses = [int(n) for n in f.readlines()]
    result = None
    for a in expenses:
        for b in expenses:
            if a + b == 2020:
                result = a*b
                break
    return result

if __name__ == "__main__":
    print(main())