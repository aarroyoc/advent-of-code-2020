# advent-of-code-2020
Solutions of Advent of Code 2020

## Python solutions

Install environment:
```
poetry install
poetry shell
```

Execute a day:
```
python day1/star1.py
```

Execute all tests:
```
make python
```

## Prolog solutions

Execute a day:
```
swipl day1/day1.pl
?- day1:star(1, X).
```

Execute all tests:
```
make prolog
```

## Haskell solutions

Execute a day:
```
cd day1
ghc Day1.hs -main-is Day1 -o Day1
./Day1
```

Execute tests:
```
cd day1
ghc Test.hs
./Test
```