# 05

In this case the solution for part 1 is overly complicated, but way more efficient than the simple solution.
The reason for that is that I first expected a way bigger input.

Since it wasn't the second part was solved in ~7 lines.
The first part could've been solved in the same way if you just filter the input first like in the first example:

```fsharp
List.filter (fun ((x1, y1), (x2, y2)) -> x1 = x2 || y1 = y2)
```
