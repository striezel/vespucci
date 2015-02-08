/* ***************************************************************************

    This file is part of Vespucci.
    Copyright (C) 2008, 2009, 2010, 2015  Dirk Stolle

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

  ***************************************************************************
*/

#ifndef PATHFINDER_HPP
#define PATHFINDER_HPP

/* ********
   **** unit PathFinder
   ****
   **** purpose: holds all functions that a used during path finding of units
   ****          (currently uses A*)
   *******
*/

#include <array>
#include "Helper.hpp"
#include "Map.hpp"
#include "PascalTypes.hpp"

/* constant value that represents "no node", i.e. end of a list or invalid
   node
*/
const Byte cNotANode = 255;

typedef std::array<std::array<bool, cMap_Y>, cMap_X> TPresenceList;

/* record that holds coordinates */
struct TCoords
{
  Byte x;
  Byte y;
}; //rec

/* array that holds a sequence of coordinates, e.g. a path */
typedef std::vector<TCoords> TCoordArr;

struct TSearchNode
{
  int cost_est; //known cost plus estimated cost to target
  int cost_real; //(cost to get here a.k.a. "known cost")
  Byte x, y;
  TCoords Parent;
};

/* array that holds all search nodes */
typedef std::array<std::array<TSearchNode, cMap_Y>, cMap_X> TNodes;

/* references a node in the heap */
struct THeapNode
{
  struct { Byte x,y; } content;
}; //rec


/* ********
   **** THeap class
   ****
   **** purpose: implements a heap of nodes/ coordinates on the map
   *******
*/
class THeap
{
  private:
    TPresenceList Presence;
    TNodes Nodes;
    std::vector<THeapNode> a;
    int a_last;

    /* swaps nodes at array positions n1 and n2

       parameters:
           n1 - index of first node
           n2 - index of second node
    */
    void SwapElements(const int n1, const int n2);

    /* restores order in the sub tree of node with index element

       parameters:
           element - index of node that is the root of the sub tree
                     that has to be sorted
    */
    void Sift(const int element);

    /* restores the heap property for the complete heap */
    void Heapify();

    /* returns true, if the (estimated) cost of node at index n1 is
       less or equal to the (estimated) cost of node at index n2

       parameters:
           n1 - index of first node
           n2 - index of second node
    */
    bool LessEqual(const int n1, const int n2) const;
  public:
    /* constructor */
    THeap();

    /* destructor */
    ~THeap();

    /* returns true, if the heap is empty */
    bool Empty() const;

    /* returns true, if the node with coordinates (x;y) is present

       parameters:
           x, y - coordinates of the node
    */
    bool IsNodePresent(const Byte x, const Byte y) const;

    /* adds a new node to the heap and returns true, if a node was
       added

       parameters:
           newNode - the node that has to be added to the heap

       remarks:
           A node will not be added, if its coordinates are not on
           the map.
    */
    bool AddNode(const TSearchNode& newNode);

    /* returns the node with the minimum cost

       remarks:
           If the heap is empty, a search node with both x and y
           coordinates set to cNotANode will be returned.
    */
    TSearchNode PeekMin() const;

    /* removes the node with the minimum cost from the heap and
       returns it

       remarks:
           If the heap is empty, a search node with both x and y
           coordinates set to cNotANode will be returned.
    */
    TSearchNode RemoveMin();

    /* returns the heap node for position (x;y)

       parameters:
           x,y - coordinates of the node

       remarks:
           If the node with the given coordinates is not part of
           the heap, then a search node with both x and y
           coordinates set to cNotANode will be returned.
    */
    TSearchNode GetNode(const Byte x, const Byte y) const;
};//class

/* returns an "optimistic guess" of the moves needed to move from the first
   specified coordinates to the later specified coordinates

   parameters:
       from_x, from_y - first coordinates
       to_x, to_y     - second coordinates

   remarks:
       This function returns the "Manhattan distance" of the coordinates,
       because equals the minimum number of moves required to move between
       those nodes/coordinates; thus, it's always an "optimistic guess".
*/
int Heuristic(const Byte from_x, const Byte from_y, const Byte to_x, const Byte to_y);

/* This function does the actual pathfinding and returns true, if a path was
   found.

   parameters:
       from_x, from_y     - coordinates of the starting point
       target_x, target_y - coordinates of the destination
       WaterWay           - indicates whether the path shall only contain
                            "watery" squares (true) or land squares (false)
       AMap               - the map (must not be nil, of you want a solution)
       path               - array that will hold the coordinates of each node
                            that needs to be travelled to get from start to the
                            destionation node (i.e. the path a unit has to walk)
       SpecialNodeX       - x-coordinate of a special node that can be
                            travelled even if it does not meet the water/land
                            requirement
       SpecialNodeY       - y-coordinate of the above node

   remarks:
       This function uses the A*-algorithm to do the pathfinding and requires
       quite a lot of memory (and possibly time), so use it only when needed.
*/
bool FindPath(const Byte from_x, const Byte from_y, const Byte target_x, const Byte target_y, const bool WaterWay, const TMap& AMap, TCoordArr& path,
              const Byte SpecialNodeX = 250, const Byte SpecialNodeY = 250);

#endif // PATHFINDER_HPP
