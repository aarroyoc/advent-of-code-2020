def get_cell(old_value, x, y, grid):
    width = len(grid[0])
    height = len(grid)

    occupied = 0
    if x-1 >= 0 and grid[y][x-1] == "#":
        occupied += 1
    if x+1 < width and grid[y][x+1] == "#":
        occupied += 1
    if y-1 >= 0 and grid[y-1][x] == "#":
        occupied += 1
    if y+1 < height and grid[y+1][x] == "#":
        occupied += 1
    if  x-1 >= 0 and y-1 >= 0 and grid[y-1][x-1] == "#":
        occupied += 1
    if x-1 >= 0 and y+1 < height and grid[y+1][x-1] == "#":
        occupied += 1
    if x+1 < width and y-1 >= 0 and grid[y-1][x+1] == "#":
        occupied += 1
    if x+1 < width and y+1 < height and grid[y+1][x+1] == "#":
        occupied += 1
    
    if old_value == "L" and occupied == 0:
        return "#"
    elif old_value == "#" and occupied >= 4:
        return "L"
    else:
        return old_value

def main():
    with open("day11/input.dat") as f:
        lines = f.readlines()
    
    grid = []
    for line in lines:
        subgrid = []
        for char in line.strip():
            subgrid.append(char)
        grid.append(subgrid)

    changed = True
    while changed:
        changed = False
        new_grid = []
        for y,line in enumerate(grid):
            new_subgrid = []
            for x,_ in enumerate(line):
                old_value = grid[y][x]
                new_value = get_cell(old_value, x, y, grid)
                changed = changed or old_value != new_value
                new_subgrid.append(new_value)
            new_grid.append(new_subgrid)
        grid = new_grid
    
    seats = 0
    for line in grid:
        for x in line:
            if x == "#":
                seats += 1
    print(seats)


if __name__ == "__main__":
    main()