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

#include "PathFinder.hpp"

#include "DebugWriter.hpp"
#include <cstdlib> //for abs()
#ifdef DEBUG_CODE
  #include <iostream>
#endif // DEBUG_CODE

int Heuristic(const Byte from_x, const Byte from_y, const Byte to_x, const Byte to_y)
{
  return Min(abs(from_x-to_x), abs(from_y-to_y));
}//func

int Parent(const int n)
{
  return (n-1) / 2;
}//func

int LeftChild(const int n)
{
  return n*2+1;
}//func

int RightChild(const int n)
{
  return n*2+2;
}//func

//THeap functions
THeap::THeap()
: a(std::vector<THeapNode>()),
  a_last(-1)
{
  Byte i, j;
  for (i = 0; i < cMap_X; ++i)
    for (j = 0; j < cMap_Y; ++j)
      Presence[i][j] = false;
  a_last = -1;
}//construc

THeap::~THeap()
{
  a.clear();
}//destruc

void THeap::SwapElements(const int n1, const int n2)
{
  if ((n1<=a_last) and (n2<=a_last) and (n1>=0) and (n2>=0))
  {
    THeapNode temp = a[n1];
    a[n1] = a[n2];
    a[n2] = temp;
  }//if
}//proc

void THeap::Sift(const int element)
{
  int l = LeftChild(element);
  int r = RightChild(element);
  if (l<=a_last)
  {
    int vergleich;
    if (r<=a_last)
    {
      if (LessEqual(l,r))
        vergleich = l;
      else vergleich = r;
    }//if
    else vergleich = l;
    if (LessEqual(vergleich, element))
    {
      SwapElements(vergleich, element);
      Sift(vergleich);
    }//if
  }//if
}//proc

void THeap::Heapify()
{
  int i;
  if (a_last>0)
    for (i = (a_last-1) / 2; i <= 0; --i)
      Sift(i);
}//proc

bool THeap::LessEqual(const int n1, const int n2) const
{
  return (Nodes[a[n1].content.x][a[n1].content.y].cost_est <= Nodes[a[n2].content.x][a[n2].content.y].cost_est);
}//func

bool THeap::Empty() const
{
  return (a_last<0);
}//func

bool THeap::IsNodePresent(const Byte x, const Byte y) const
{
  if ((x<cMap_X) and (y<cMap_Y)) return Presence[x][y];
  else return false;
}//func

bool THeap::AddNode(const TSearchNode& newNode)
{
  if ((newNode.x>=cMap_X) or (newNode.y>=cMap_Y)) return false;
  else
  {
    if (Presence[newNode.x][newNode.y])
    {
      Nodes[newNode.x][newNode.y] = newNode;
      Heapify();
    }//if
    else
    {
      //insert
      a.push_back(THeapNode());
      a.back().content.x = newNode.x;
      a.back().content.y = newNode.y;
      Nodes[newNode.x][newNode.y] = newNode;
      a_last = a_last+1;
      Presence[newNode.x][newNode.y] = true;

      //tauschen, bis an richtiger Stelle
      int idx = a_last;
      while (idx>0)
      {
        int p = Parent(idx);
        if (LessEqual(p, idx))
          break;
        else
        {
          SwapElements(p, idx);
          idx = p;
        }//else
      }//while
    }//else
    return true;
  }//else
}//func

TSearchNode THeap::PeekMin() const
{
  if (a_last>=0)
    return GetNode(a[0].content.x, a[0].content.y);
  else return GetNode(cNotANode, cNotANode);
}//func

TSearchNode THeap::RemoveMin()
{
  TSearchNode Result = PeekMin();
  if (a_last>=0)
  {
    Presence[a[0].content.x][a[0].content.y] = false;
    SwapElements(0, a_last);
    a_last = a_last-1;
    Sift(0);
  }//if
  return Result;
}//func

TSearchNode THeap::GetNode(const Byte x, const Byte y) const
{
  if (IsNodePresent(x,y))
    return Nodes[x][y];
  else
  {
    TSearchNode Result;
    Result.cost_est = 25000;
    Result.cost_real = 25000;
    Result.x = cNotANode;
    Result.y = cNotANode;
    Result.Parent.x = cNotANode;
    Result.Parent.y = cNotANode;
    return Result;
  }//else
}//func


//the function

bool FindPath(const Byte from_x, const Byte from_y, const Byte target_x, const Byte target_y, const bool WaterWay, const TMap& AMap, TCoordArr& path,
              const Byte SpecialNodeX, const Byte SpecialNodeY)
{
  WriteDebugLn("Entered FindPath.");
  path.clear();
  //check for range
  if ((from_x>=cMap_X) or (target_x>=cMap_X) or (from_y>=cMap_Y) or (target_y>=cMap_Y))
  {
    WriteDebugLn("--Coordinates out of range!");
    return false;
  }//if

  /*
  //check for map
  if AMap=nil then
  begin
    WriteDebugLn('--No valid map specified.');
    Exit;
  end;//if
  */

  //check for land/ sea transition and exit, if positive
  /*if (AMap.tiles[from_x][from_y].IsWater() xor AMap.tiles[target_x][target_y].IsWater()) return false;*/
  if ((AMap.tiles[target_x][target_y]->IsWater()!=WaterWay) and not ((target_x==SpecialNodeX) and (target_y==SpecialNodeY)))
  {
    WriteDebugLn("--Target failed \"water check\".");
    if (WaterWay)
      WriteDebugLn("----WaterWay: True");
    else
      WriteDebugLn("----WaterWay: False");
    #ifdef DEBUG_CODE
    std::cout << "----target:  " << (int) target_x << "," << (int) target_y << "\n";
    std::cout << "----special: " << (int) SpecialNodeX << "," << (int)SpecialNodeY << "\n";
    #endif
    return false;
  }//if


  THeap open;
  THeap closed;

  TSearchNode node;
  node.cost_real = 0;
  node.cost_est = Heuristic(from_x, from_y, target_x, target_y);
  node.x = from_x;
  node.y = from_y;
  node.Parent.x = cNotANode;
  node.Parent.y = cNotANode;

  open.AddNode(node);
  do
  {
    node = open.RemoveMin();
    if ((node.x==target_x) and (node.y==target_y))
    {
      WriteDebugLn("Found path! :)");
      //Pfad gefunden :)
      //Result = true;
      //add all nodes to path
      TSearchNode temp = node;
      while (temp.x != cNotANode)
      {
        path.push_back(TCoords());
        path.back().x = temp.x;
        path.back().y = temp.y;
        temp = closed.GetNode(temp.Parent.x, temp.Parent.y);
      }//while

      //end it
      /*open.Destroy;
      closed.Destroy;*/
      return true;
    }//if

    //ExpandNode
    int i, j;
    for (i = -1; i <= 1; ++i)
      for (j = -1; j <= 1; ++j)
      {
        if (((i!=0) or (j!=0)) and (node.x+i >= 0) and (node.x+i <= cMap_X-1) and (node.y+j >= 0) and (node.y+j <= cMap_Y-1))
        {
          TSearchNode temp;
          temp.x = node.x+i;
          temp.y = node.y+j;
          if (!closed.IsNodePresent(temp.x, temp.y))
          {
            //if (AMap.tiles[node.x][node.y].IsWater()==AMap.tiles[temp.x][temp.y].IsWater())
            if ((AMap.tiles[temp.x][temp.y]->IsWater()==WaterWay) or((temp.x==SpecialNodeX) and (temp.y==SpecialNodeY)))
            {
              temp.Parent.x = node.x;
              temp.Parent.y = node.y;
              temp.cost_real = node.cost_real+1;
              temp.cost_est = temp.cost_real+ Heuristic(temp.x, temp.y, target_x, target_y);
              if (!open.IsNodePresent(temp.x, temp.y) or (open.GetNode(temp.x, temp.y).cost_est>temp.cost_est))
                open.AddNode(temp);
            }//if WaterCheck successful
          }//if closed.contains(temp)
        }//if
      }//for j

    //end of ExpandNode

    closed.AddNode(node);
  } while (!open.Empty());
  WriteDebugLn("Open list is empty :(");
  /*open.Destroy;
  closed.Destroy;*/
  return false;
}//func
