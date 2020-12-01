from star1 import main as star1_main
from star2 import main as star2_main

def test_day1_star1():
    assert star1_main() == 988771

def test_day1_star2():
    assert star2_main() == 171933104