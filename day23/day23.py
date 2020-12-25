from collections import deque

class Node:
    def __init__(self, n, prev_node):
        self.n = n
        self.next = prev_node

    def __str__(self):
        return str(self.n)

    def __repl__(self):
        return self.__str__()

class List:
    def __init__(self, iterable):
        self.map = dict()
        self.first_node = None
        for n in iterable[::-1]:
            node = Node(n, self.first_node)
            self.map[n] = node
            self.first_node = node
        

    def search(self, n):
        return self.map[n]

def main2():
    initial = build_list()
    circle = List(initial)
    circle.search(1000000).next = circle.search(3)
    selection = circle.first_node
    for _ in range(10000000):
        picked = (selection.next.n, selection.next.next.n, selection.next.next.next.n)
        selection.next = selection.next.next.next.next
        destination_n = destination_cup(selection.n, picked)
        destination = circle.search(destination_n)
        circle.search(picked[2]).next = destination.next
        destination.next = circle.search(picked[0])
        selection = selection.next
    one = circle.search(1)
    n = one.next.n
    m = one.next.next.n
    output = n*m
    print(f"Resultado: {output}")


def main():
    initial = build_list()
    circle = deque(initial)
    #circle = deque([3, 8, 9, 1, 2, 5, 4, 6, 7])
    for j in range(10000000):
        circle.rotate(-1)
        picked = (circle.popleft(), circle.popleft(), circle.popleft())
        destination = destination_cup(circle[-1], picked)
        dest_index = (circle.index(destination) + 1)*-1
        circle.rotate(dest_index)
        circle.appendleft(picked[2])
        circle.appendleft(picked[1])
        circle.appendleft(picked[0])
        circle.rotate((dest_index*-1))
        if j % 100000 == 0:
            print(j)
    i = circle.index(1)
    n = circle[i+1]
    m = circle[i+2]
    output = n*m
    print(f"Resultado: {output}")

def build_list():
    l = [3, 1, 8, 9, 4, 6, 5, 7, 2]
    post = [x for x in range(10, 1000001)]
    l.extend(post)
    return l

def destination_cup(n, picked):
    while True:
        n = n - 1
        if n > 0:
            if n not in picked:
                return n
        else:
            n = 1000001
            #n = 10

            

if __name__ == "__main__":
    main2()