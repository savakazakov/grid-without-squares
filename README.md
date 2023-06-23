# Grid Without Squares

***I recently came up*** with this idea while doodling on a piece of paper:

In a N x N grid/matrix what is the proportion of "X"s / "O"s when N goes up and does it converge to some interesting number.
"X"s and "O"s are just the two symbols that can go into the grid/matrix and the key rule is that:

No 4 "X"s can be the vertices of an orthogonal square.

## Quick little example for N = 2:

```
+-+-+                                                                   +-+-+
|X|X| <----- Not a valid solution since it forms a square.              |X|X|
+-+-+                                                                   +-+-+
|X|X|        Valid since it doesn't form any orthogonal squares. -----> |X|O|
+-+-+                                                                   +-+-+
```

You can try and compute the optimal solutions for N = {1, 2, 3}. N = 3 would probably require a couple of seconds.
But as soon as you go to N = 4 things get pretty hard to simulate in your head.

I've had a go with the problem, but it is traitorously simply stated. In fact, it is a fairly tough problem.
I've recently moved my solution to this repository and the history is no lost.
With that I've slightly rephrased the original question:

**What is the maximum number of elements of interest, one could place in a square grid, such that no 4 elements are the vertices of an orthogonal square**

## A quick summary of what has happened so far:

1. My idea was to generate all possible permutations, then validate them and get the best one.
    a. This was a complete nightmare because of the complexity of the problem **O(2 ^ (N ^ 2))**
    b. Therefore I only managed to get the solutions for N = {1, 2, 3, 4}.
    c. This resulted in a heap error since the ADT to hold all of the candidates for N = 4 would need to be with dimensions:
    [2 ^ 25][5][5]
2. Then I decided to generate the candidates with a specific number of elements of interest, which sort of solves the issue of having to store them all at once.
    a. Never the less the time complexity is still **O(2 ^ (N ^ 2))**.
    b. For reference for N = 4, takes few seconds let's say 5 seconds.
        i. If (2 ^ 16) takes 5 seconds, (2 ^ 25) would take (5 ^ 9) seconds or about 3.229 weeks!
3. Now the plan is to solve it properly by integrating the validation process into the generation of candidates.
    a. This has now been done, amongst other optimisations.
4. With this I managed to produce the sequence of {1, 3, 7, 12, 17, ...}
    a. With a quick search in an integer sequence database called OEIS, I found the original problem:
        i. https://oeis.org/A227133
    b. It turns out that this problem is pretty hard and even with modern computers and much more optimised algorithm than mine, people
    have barely confirmed the solutions for N = 10.
    c. I guess this means I should move on... Since the original proposer did that in 2016...
        i. http://inversed.ru/InvMem.htm#InvMem_20:~:text=N20%20/%20Maximal%20density%20subsquare%2Dfree%20arrangements%20/%20%23Optimization%20%23OpenProblem%20/%202016.02.22
5. As of 26/06/2023 I introduced some optimisations. The most notable is that I introduced the following optimisation which solve the problem for N = {6, 7} in a reasonable amount of time. I'm checking if it possible to get to the current max EOI, given the current number of EOI. This is obvious, and I don't know why it took me this long to spot, but it trims the search tree significantly. Simply put, if the number of cells left to iterate trough is not enough to go match or surpass the current max of EOI, then you return immediately, since you cannot possibly find a solution there.
    a. I have few new ideas of how to improve the efficiency:
        i. Make sure all methods are safe and I do parameter checking.
        ii. Implement concurrency using Thread Pools.
        iii. Check assumption for paths in the grid. Clarification: I've noticed that for a lot of solutions there is a graph of orthogonally connected EOIs (that connects all EOIs), if this is somehow intrinsically true as a result of the nature of the problem, then this could be the crack in the problem that makes it possible to go beyond the results, described in the original problem. (https://oeis.org/A227133)
        iv. Optimise my Java code.
        v. Test again with global variables and stop passing them through the stack frames.
        vi. Calculate a hashmap for the uniqueness check.
6. I've also tried to translate my solution to C++, but without a lot of success. For a test with N = 4 and an average over 10 iterations, the average time spent is 289 ms for C++ and with Java it is 15 ms. Even if I use the -O3 flag in C++ the time spent is 38 ms, which is still quite a lot. After the optimisation for the possibility of the solution, I get 3381 us or about 3 ms with Java. I guess the takeaway point is that my C++ is very unoptimised. (I have to read a bunch of books on C++ ...)

## **TODO:**

1. Make sure all methods are safe and I do parameter checking.
2. Implement concurrency using Thread Pools.
3. Check assumption for paths in the grid. Clarification: I've noticed that for a lot of solutions there is a graph of orthogonally connected EOIs (that connects all EOIs), if this is somehow intrinsically true as a result of the nature of the problem, then this could be the crack in the problem that makes it possible to go beyond the results, described in the original problem. (https://oeis.org/A227133)
4. Optimise my Java code.
5. Test again with global variables and stop passing them through the stack frames.
6. Calculate a hashmap for the uniqueness check.

## **NOTES:**

1. I'm absolutely sure this question has been asked before and has an optimal solution.
    a. I've tried to search for it for a while and the closed I've come to it is:
        i. "Moscow Paving Problem", "Squaring the square", "Squaring the Plane", "Maximal square-free subsets of point sets in the plane", "Square-free 2-colorings of the Plane", "Ramsay problem", "Square-free subset problem", "Maximal square-free packing problem", etc.
    b. Please do feel free to refer me to the origin of this question.
2. Java is far away from perfect for this problem, but I really didn't think I'd spend so much time on this...
3. Please feel free to contribute with ideas or even better with algorithms.
4. Ohh, and I presume you are thinking: "There is definitely a pattern here. What is this guy on about solution for N should be dependent on N - 1 or previous Ns. Boom easy recursion".
    a. I've thought about that, but I cannot prove rigorously that making a sub grid not optimal doesn't end up with a candidate grid with more elements of interest that is valid. (I.e., the proper solution)
    b. Update: I know that this is not the case, since the solution for N = 5 contains 51 unique solutions and the solution for N = 4 is only 1, which mean that there will always be solutions that are not dependent on the previous (smaller) problems.
5. Update: The original problem is called: "Maximal density subsquare-free arrangements" - by Peter Karpov, or originally formulated as "Given a square grid with side n consisting of n^2 cells (or points), a(n) is the maximum number of points that can be painted so that no four of the painted ones form a square with sides parallel to the grid." by the author - Heinrich Ludwig, Jul 06 2013.