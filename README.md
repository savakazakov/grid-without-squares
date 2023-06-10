# GridWithoutSquares

***I recently came up*** with this idea while doodling on a piece of paper:

In a N x N grid/matrix what is the proportion of "X"s / "O"s when N goes up and does it converge to some interesting number.
"X"s and "O"s are just the two symbols that can go into the grid/matrix and the key rule is that:

    No 4 "X"s can be the vertices of an orthogonal square.

Quick little example for N = 2:

**
+-+-+                                                                   +-+-+
|X|X| <----- Not a valid solution since. It forms a square.             |X|X|
+-+-+                                                                   +-+-+
|X|X|        Valid since it doesn't form any orthogonal squares. -----> |X|O|
+-+-+                                                                   +-+-+
**

You can try and compute the optimal solutions for N = {1, 2, 3}. N = 3 would probably require a couple of seconds.
But as soon as you go to N = 4 things get pretty hard to simulate in your head.

I've had a go with the problem but it is traitorously simply stated. In fact it is a fairly tough problem.
I've recently moved my solution to this repository and the history is no lost.
With that I've slightly rephrased the original question:

**What is the maximum number of elements of interest, one could place in a square grid, such that no 4 elements are the vertices of an orthogonal square**

A quick summary of what has happened so far:

    1. My idea was to generate all possible permutations, then validate them and get the best one.
        a. This was a complete nightmare because of the complexity of the problem ** O( 2 ^ (N ^ 2)) **
        b. Therefore I only managed to get the solutions for N = {1, 2, 3, 4}.
        c. This resulted in a heap error since the ADT to hold all of the candidates for N = 4 would need to be with dimensions:
        [2 ^ 25][5][5]
    2. Then I decided to generate the candidates with a specific number of elements of interest, which sort of solves the issue of having to store them all at once.
        a. Never the less the time complexity is still ** O( 2 ^ (N ^ 2)) **.
        b. For reference for N = 4, takes few seconds let's say 5 seconds.
            i. If (2 ^ 16) takes 5 seconds, (2 ^ 25) would take (5 ^ 9) seconds or about 3.229 weeks!
    3. Now the plan is to solve it properly by integrating the validation process into the generation of candidates.
        
**NOTES:**

    1. I'm absolutely sure this question has been asked before and has an optimal solution.
        a. I've tried to search for it for a while and the closed I've come to it is:
            i. "Moscow Paving Problem", "Squaring the square", "Squaring the Plane", "Maximal square-free subsets of point sets in the plane", "Square-free 2-colorings of the Plane", "Ramsay problem", "Square-free subset problem", "Maximal square-free packing problem", etc.
        b. Please do feel free to refer me to the origin of this question.
    2. Java is far away from perfect for this problem, but I really didn't think I'd spend so much time on this...
    3. Please feel free to contribute with ideas or even better with algorithms.