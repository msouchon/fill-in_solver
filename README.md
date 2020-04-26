# fill-in_solver
Solves [Fill-In](https://en.wikipedia.org/wiki/Fill-In_(puzzle)) Puzzles

Given a blank puzzle words and a list of words this program outputs a solved board file.

To solve the puzzle this program first fills in words of a unique length which therefore have a unique solution. From this point words are attempted to be filled in order of those with the fewest possible satisfiable locations. At each point the constraints are updated.

For example this is puzzle 2:

Board:
```
____
___#
____
```

Words:
```
boat
art
need
ban
ore
ate
```

Solution:
```
boat
art#
need
```
