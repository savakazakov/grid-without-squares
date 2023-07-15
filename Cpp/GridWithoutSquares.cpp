#include <iostream>
#include <map>
#include <vector>

#include <chrono>
#include <string>
#include <math.h>
#include <algorithm>

using namespace std;
using namespace std::chrono;

#define DEBUG

void gridWithoutSquaresSequence(int maxSize);
vector<vector<bool>*> genSol(int size);
void genSolTR(int ctr, int curElm, int* maxElm, vector<bool>* cand, vector<vector<bool>*>* sol, std::map<int, vector<int *> *> cellToSquares);
std::map<int, vector<int*>*> genCellToSquaresMap(int size);
void printSolution(vector<bool>* sol);
string repeat(string s, int n);
bool chkIdent(vector<bool>* sol, vector<bool>* cand);
bool chkIdentHor(vector<bool>* sol, vector<bool>* cand);
bool chkIdentVert(vector<bool>* sol, vector<bool>* cand);
bool chkIdentDiag(vector<bool>* sol, vector<bool>* cand);
bool chkUnique(vector<bool>* sol, vector<bool>* cand);

/**
 * A class that attempts to tackle the Grid without squares problem.
 * @Note For full explanation of the problem please refer to the README.md file.
 * @Note I'm assuming that the element of interest (EOI) is "O". I.e., no 4 "O"s can to form a square.
 * @Note An "O" corresponds to a boolean value of true.
 * 
 * TODO: Make sure all methods are safe and I do parameter checking.
 * TODO: Fix comments from Java to C++.
 */
int main()
{    
    int size = 4;
    int times = 10;

    // Get starting timepoint.
    auto start = high_resolution_clock::now();

    for (int i = 0; i < times; i++)
    {
        std::vector<vector<bool>*> solutions = genSol(size);
    }

    // Get ending timepoint.
    auto stop = high_resolution_clock::now();
    auto duration = duration_cast<microseconds>(stop - start);

    std::cout << "Testing: N = " << size << ", " << times << " times" << std::endl;
    std::cout << "Average performance: " << duration.count() / times / 1000 << " ms, (" << duration.count() / times << " us)" << std::endl;

    // gridWithoutSquaresSequence(6);
}

/**
 * TODO: Finish this.
 */
void gridWithoutSquaresSequence(int maxSize)
{
    int numOfEOI = 0;

    for (int i = 2; i <= maxSize; i++)
    {
        const int size = i;
        // Generate the solutions.
        std::vector<vector<bool>*> solutions = genSol(i);

        vector<bool>* sol = solutions[0];

        // Reset the numOfEOI
        numOfEOI = 0;

        // Get the number of EOI.
        for (int j = 0; j < sol->size(); j++)
        {
            if ((*sol)[j])
                numOfEOI++;
        }

        // Print the ratio.
        std::cout << "Ratio: " << numOfEOI << " / " << (i * i) << " (" << ((double) numOfEOI / (i * i)) << ")" << std::endl;

        // Print the first solution.
        printSolution(sol);

        // Print how many unique solutions there are.
        if (solutions.size() > 1)
            std::cout << " + " << (solutions.size() - 1) << " more unique solutions";
    }
}

/**
 * Generate a list of 1D flattened arrays, which are solutions for the 
 * "Grid Without Squares" problem for a given size. This integrates the validation
 * of the solution in the generation process.
 * 
 * @param size - The size of the solutions. E.g. size = 5 would give solutions for a 5 x 5 grid.
 * @return - The list containing all valid unique solutions for the give size.
 * @see chkUnique(boolean[], boolean[]) for a definition of unique.
 */
vector<vector<bool>*> genSol(int size)
{
    std::map<int, vector<int*>*> cellToSquares = genCellToSquaresMap(size);
    vector<vector<bool>*> sol;
    int* maxElm = new int(0);

    genSolTR(0, 0, maxElm, new vector<bool>(size * size), &sol, cellToSquares);

    delete maxElm;

    return sol;
}

/**
 * An almost Tail Recursive algorithm, invoked by the genSol method (Middle recursion in reality).
 * @param ctr - The current index in the 1D flattened accumulator array.
 * @param curElm - The current number of EOIs.
 * @param maxElm - The current maximum number of EOIs for a complete solution.
 * @param cand - The accumulator candidate.
 * @param sol - The list of solutions with maxElm EOIs.
 * @param cellToSquares - ADT to hold the mapping between cells and the squares it belongs to.
 */
void genSolTR(int ctr, int curElm, int *maxElm, vector<bool>* cand, vector<vector<bool>*>* sol, std::map<int, vector<int *> *> cellToSquares)
{
    // Recursion end condiditons.
    if (ctr == cand->size())
    {
        if (curElm > *maxElm)
        {
            sol->clear();
            sol->push_back(new vector<bool>(*cand));
            *maxElm = curElm;
        }
        else if (curElm == *maxElm)
        {
            if (std::all_of(sol->begin(), sol->end(), [&](vector<bool>* x){ return chkUnique(x, cand); }))
                sol->push_back(new vector<bool>(*cand));
        }

        return;
    }

    // Check if it is possible to place the EOI in cell[ctr / size][ctr % size].
    if (std::all_of(cellToSquares[ctr]->begin(), cellToSquares[ctr]->end(), [&](int* x){ return *x < 3; }))
    {
        // First, set the cell to the EOI.
        (*cand)[ctr] = 1;

        // Update all squares, which have this cell as a vertex.
        for (int* vert : *cellToSquares[ctr])
            (*vert)++;
        
        // Move on to the next cell.
        genSolTR(++ctr, ++curElm, maxElm, cand, sol, cellToSquares);

        // Second, check if a solution would be optimal with this cell as not an EOI.
        // Reset to the state before the recursive call.
        curElm--;
        (*cand)[--ctr] = 0;

        // Update the squares.
        for (int *vert : *cellToSquares[ctr])
            (*vert)--;
    }

    // If it isn't possible or we already attemped the to have an EOI in this cell,
    // try to set the cell as not an EOI.
    genSolTR(++ctr, curElm, maxElm, cand, sol, cellToSquares);
}

/**
 * Generate the ADT to hold the mapping between cells and the squares it belongs to.
 * I.e. for all cells of the grid build a list of squares it is a vertex of.
 * 
 * Cells are indexed from 0 to @param size * @param size - 1, 
 * from top left corner to bottom right.
 * Squares are indexed from 0 to @param size * (@param size − 1) * (2@param size − 1) / 6.
 * This formula is derived from the following series: 1 ^ 2 + 2 ^ 2 + 3 ^ 2 + ... + (n - 1) ^ 2,
 * because there are (n - 1) ^ 2 squares of size 2 in an n by n grid, (n - 2) ^ 2 squares of size 3
 * and so on.
 * 
 * The exact vertices are unimportant for this application, therefore, only their count is stored.
 * They can be retrieved at the end using the squareID. This massively simplifies the ADT and the Square class.
 * 
 * @param size - The size of the grid.
 * @return - A mapping between the cell ID and the list of square IDs.
 */
std::map<int, vector<int*>*> genCellToSquaresMap(int size)
{
    std::map<int, vector<int*>*> squares;
    int* curSquare;

    // It is more effecient to iterate through the squares, because
    // for each square we add all 4 cell IDs. (the vertices)
    for (int i = 2; i <= size; i++)
    {
        for (int j = 0; j <= size - i; j++)
        {
            for (int k = 0; k <= size - i; k++)
            {
                // Create an instance of the current square.
                curSquare = new int(0);

                // Add the top left cell of the square.
                if (!squares.try_emplace(size * j + k, new vector<int*>(1, curSquare)).second)
                    (*squares[size * j + k]).push_back(curSquare);

                // Add the top right cell of the square.
                if (!squares.try_emplace(size * j + k + i - 1, new vector<int*>(1, curSquare)).second)
                    (*squares[size * j + k + i - 1]).push_back(curSquare);

                // Add the bottom left cell of the square.
                if (!squares.try_emplace(size * (j + i - 1) + k, new vector<int*>(1, curSquare)).second)
                    (*squares[size * (j + i - 1) + k]).push_back(curSquare);
                
                // Add the bottom right cell of the square.
                if (!squares.try_emplace(size * (j + i - 1) + k + i - 1, new vector<int*>(1, curSquare)).second)
                    (*squares[size * (j + i - 1) + k + i - 1]).push_back(curSquare);
            }
        }
    }

    return squares;
}

/**
 * Print the solution as on the console. Where "O" is the EOI.
 * I.e. no 4 elements are the vertices of a square.
 * TODO: This should be rewritten to accept 1D flattened array.
 * @param solution - The 2D array that stores the solution.
 */
void printSolution(vector<bool>* sol)
{
    int size = sqrt(sol->size());

    // Initialise the strings to be used in the printing.
    string line = "+" + repeat("-+", size);
    string middleLine = "";

    for (int i = 0; i < size; i++)
    {
        std::cout << line << std::endl;

        for (int j = 0; j < size; j++)
        {
            middleLine += "|" + (string)((*sol)[i * size + j] ? "O" : "X");
        }

        std::cout << middleLine + "|" << std::endl;
        middleLine = "";
    }

    std::cout << line << std::endl;
}

/**
 * TODO: Finish this!
*/
string repeat(string s, int n)
{
    string repeat;

    for (int i = 0; i < n; i++)
        repeat += s;

    return repeat;
}

/**
 * Method overload of chkIdent(boolean[][], boolean[][]).
 * Check if two 1D flattened arrays are not identical.
 * @Note This assumes the 1D arrays are of the same dimensions.
 * @param sol - The proposed solution.
 * @param cand - The candidate.
 * @return - True if not identical and false otherwise.
 */
bool chkIdent(vector<bool>* sol, vector<bool>* cand)
{
    for (int i = 0; i < sol->size(); i++)
    {
        if ((*sol)[i] != (*cand)[i])
            return true;
    }

    return false;
}

/**
 * Method overload of chkIdentHor(boolean[][], boolean[][]).
 * Check if two 1D flattened arrays not identical, i.e., if the second is flipped horizontally.
 * @Note This assumes the 1D arrays are of the same dimensions.
 * @param sol - The proposed solution.
 * @param cand - The candidate.
 * @return - True if not identical and false otherwise.
 */
bool chkIdentHor(vector<bool>* sol, vector<bool>* cand)
{
    int size = (int) sqrt(sol->size());

    for (int i = 0; i < sol->size(); i++)
    {
        if ((*sol)[i] != (*cand)[size - 1 - (i % size) + (i / size) * size])
            return true;
    }

    return false;
}

/**
 * Method overload of chkIdentVert(boolean[][], boolean[][]).
 * Check if two 1D flattened arrays not identical, i.e., if the second is flipped vertically.
 * @Note This assumes the 1D arrays are of the same dimensions.
 * @param sol - The proposed solution.
 * @param cand - The candidate.
 * @return - True if not identical and false otherwise.
 */
bool chkIdentVert(vector<bool>* sol, vector<bool>* cand)
{
    int size = (int) sqrt(sol->size());

    for (int i = 0; i < sol->size(); i++)
    {
        for (int j = 0; j < sol->size(); j++)
        {
            if ((*sol)[i] != (*cand)[(size - 1 - i / size) * size + (i % size)])
                return true;
        }
    }

    return false;
}

/**
 * Method overload of chkIdentDiag(boolean[][], boolean[][]).
 * Check if two 1D flattened arrays not identical, i.e., if the second flipped along the diagonal.
 * @Note This assumes the 1D arrays are of the same dimensions.
 * @param sol - The proposed solution.
 * @param cand - The candidate.
 * @return - True if not identical and false otherwise.
 */
bool chkIdentDiag(vector<bool>* sol, vector<bool>* cand)
{        
    for (int i = 0; i < sol->size(); i++)
    {
        if ((*sol)[i] != (*cand)[sol->size() - i - 1])
            return true;
    }

    return false;
}

/**
 * Check if two 1D arrays are unique.
 * This means the second is not a flipped or rotated version of the first.
 * @Note This assumes the 1D arrays are of the same dimensions.
 * @param sol - The proposed solution.
 * @param cand - The candidate.
 * @return - True if unique and false otherwise.
 */
bool chkUnique(vector<bool>* sol, vector<bool>* cand)
{
    return (chkIdent(sol, cand) && chkIdentHor(sol, cand) && chkIdentVert(sol, cand) && chkIdentDiag(sol, cand));
}