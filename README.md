# Linear Algebra
Some linear algebra work done in Haskell.

Main project is smith.hs and smith.exe. The idea is to take in input 

    smith [[1,2],[3,4]]

and output the smith normal-form of such a matrix.

There is a smith.exe program in the \out folder. Place that somewhere and from the cmd, cd to it's location and run commands of the above form to start converting your matrices. E.g. after downloading smith.exe, your next steps may look something like:

    C:\Users\josep>cd C:\Users\josep\Downloads

    C:\Users\josep\Downloads>smith [[1,2],[4,5]]
    Input:
    ┌     ┐
    │ 1 2 │
    │ 4 5 │
    └     ┘
    Corresponding Smith normal-form:
    ┌     ┐
    │ 1 0 │
    │ 0 3 │
    └     ┘

### TODO
- refactor code into a more standard project layout
- maybe use SNF to implement some kind of crazy group counter.
- Redo matrixparser.hs to be both more general and less wonky.
