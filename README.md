# Fortran Linked List

An assignment from a Legacy Systems course written in Fortran. The program makes use of linked lists to allow for big number math which exceeds standard integer sizes. With larger input values (factorial and exponential in particular) the inefficiency of the code becomes a bottleneck, so things may take a very long time. On top of the operations addition, subtraction, multiplication and factorials which were required by the assignment, I have added exponents and am considering others.

Valid operations include:  
- Add  
- Subtract  
- Multiply  
- Factorial
- Exponent (Positives only currently)

Potential future actions:

- Division

## How to run

1) Make sure the command `gfortran` is valid (otherwise you will need to install it)
2) Enter: `gfortran -o list.out linkedList.f95`
3) Enter: `./list.out`

## Issues

- For large values the program will take a while, and with no output it may look hung. It is not. Its just grossly inefficient
- Factorial's input must be less than or equal to the max Integer value (2.17 billion, give or take). Although, with the above issue, it will never finish probably
- As with factorial, Exponent's power (input 2) must be less than or equal to the max Integer value. Again, it will take forever anyway, so no real point