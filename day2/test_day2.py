from star1 import main as star1_main
from star2 import main as star2_main

def test_day2_star1():
    assert star1_main() == 569

def test_day2_star2():
    assert star2_main() == 346