# Hanoi

Prints moves for towers of Hanoi puzzle. Finds (optimal?) solution for both 3 and 4 peg variants of puzzle.

*Disclaimer: Output hasn't been validated yet - I need a simulator to validate the moves.*

```
*Hanoi> length $ hanoi3 15 "A" "B" "C"
32767
*Hanoi> length $ hanoi4 15 "A" "B" "C" "D"
129
*Hanoi> hanoi4 15 "A" "B" "C" "D"
[("A","B"),("A","D"),("A","C"),("D","C"),("B","C"),("A","B"),("A","D"),("B","D"),("A","B"),("D","A"),("D","B"),("A","B"),("C","A"),("C","D"),("C","B"),("D","B"),("A","B"),("A","D"),("A","C"),("D","C"),("A","D"),("C","A"),("C","D"),("A","D"),("A","C"),("D","C"),("D","A"),("C","A"),("D","C"),("A","D"),("A","C"),("D","C"),("B","C"),("B","D"),("B","A"),("D","A"),("C","A"),("B","C"),("B","D"),("C","D"),("B","C"),("D","B"),("D","C"),("B","C"),("A","B"),("A","D"),("A","C"),("D","C"),("B","C"),("A","B"),("A","D"),("B","D"),("A","B"),("D","A"),("D","B"),("A","B"),("A","D"),("B","D"),("B","A"),("D","A"),("B","D"),("A","B"),("A","D"),("B","D"),("A","B"),("D","A"),("D","B"),("A","B"),("D","A"),("B","D"),("B","A"),("D","A"),("D","B"),("A","B"),("A","D"),("B","D"),("A","B"),("D","A"),("D","B"),("A","B"),("C","A"),("C","D"),("C","B"),("D","B"),("A","B"),("C","A"),("C","D"),("A","D"),("C","A"),("D","C"),("D","A"),("C","A"),("B","C"),("B","D"),("B","A"),("D","A"),("C","A"),("C","D"),("C","B"),("D","B"),("C","D"),("B","C"),("B","D"),("C","D"),("C","B"),("D","B"),("D","C"),("B","C"),("D","B"),("C","D"),("C","B"),("D","B"),("A","B"),("A","D"),("A","C"),("D","C"),("B","C"),("A","B"),("A","D"),("B","D"),("A","B"),("D","A"),("D","B"),("A","B"),("C","A"),("C","D"),("C","B"),("D","B"),("A","B")]
```
