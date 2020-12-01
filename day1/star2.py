
def main():
    with open("input.dat") as f:
        expenses = [int(n) for n in f.readlines()]
    result = None
    for a in expenses:
        for b in expenses:
            for c in expenses:
                if a + b + c == 2020:
                    result = a*b*c
                    break
    print(result)

if __name__ == "__main__":
    main()