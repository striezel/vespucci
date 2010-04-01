#include "PathFinder.h"
#include <iostream>

LongInt Heuristic(const Byte from_x, const Byte from_y, const Byte to_x, const Byte to_y)
{
  return Min(abs(from_x-to_x), abs(from_y-to_y));
}//func

LongInt Parent(const LongInt n)
{
  return (n-1) / 2;
}//func

LongInt LeftChild(const LongInt n)
{
  return n*2+1;
}//func

LongInt RightChild(const LongInt n)
{
  return n*2+2;
}//func

//THeap functions
THeap::THeap()
{
  Byte i,j;
  for (i= 0; i<cMap_X; ++i)
    for (j= 0; j<cMap_Y; ++j)
      Presence[i][j] = false;
  a.clear();
  a_last =-1;
}//construc

THeap::~THeap()
{
  a.clear();
  //inherited Destroy;
}//destruc

void THeap::SwapElements(const LongInt n1, const LongInt n2)
{
  if ((n1<=a_last) and (n2<=a_last) and (n1>=0) and (n2>=0))
  {
    const THeapNode temp = a[n1];
    a[n1] = a[n2];
    a[n2] = temp;
  }//if
}//proc

void THeap::Sift(const LongInt element)
{
  //var vergleich, l,r: Integer;
  const LongInt l = LeftChild(element);
  const LongInt r = RightChild(element);
  if (l<=a_last)
  {
    LongInt vergleich;
    if (r<=a_last)
    {
      if (LessEqual(l,r)) vergleich = l;
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
  if (a_last>0)
  {
    LongInt i;
    for (i= (a_last-1) / 2; i>=0; --i)
      Sift(i);
  }
}//proc

bool THeap::LessEqual(const LongInt n1, const LongInt n2) const
{
  return (Nodes[a[n1].x][a[n1].y].cost_est <= Nodes[a[n2].x][a[n2].y].cost_est);
}//func

bool THeap::Empty() const
{
  return (a_last<0);
}//func

bool THeap::IsNodePresent(const Byte x, const Byte y) const
{
  if ((x<cMap_X) and (y<cMap_Y)) return Presence[x][y];
  return false;
}//func

bool THeap::AddNode(const TSearchNode& newNode)
{
  if ((newNode.x>=cMap_X) or (newNode.y>=cMap_Y)) return false;

  if (Presence[newNode.x][newNode.y])
  {
    Nodes[newNode.x][newNode.y] = newNode;
    Heapify();
  }//if
  else
  {
    //insert
    /*a[a_last+1].content.x:= newNode.x;
    a[a_last+1].content.y:= newNode.y;*/
    THeapNode tempHeapNode;
    tempHeapNode.x = newNode.x;
    tempHeapNode.y = newNode.y;
    a.push_back(tempHeapNode);

    Nodes[newNode.x][newNode.y] = newNode;
    a_last = a_last+1;
    Presence[newNode.x][newNode.y] = true;

    //tauschen, bis an richtiger Stelle
    LongInt idx = a_last;
    while (idx>0)
    {
      const LongInt p = Parent(idx);
      if (LessEqual(p, idx)) break;
      else
      {
        SwapElements(p, idx);
        idx = p;
      }//else
    }//while
  }//else
  return true;
}//func

TSearchNode THeap::PeekMin() const
{
  if (a_last>=0) return GetNode(a[0].x, a[0].y);
  return GetNode(cNotANode, cNotANode);
}//func

TSearchNode THeap::RemoveMin()
{
  const TSearchNode Result = PeekMin();
  if (a_last>=0)
  {
    Presence[a[0].x][a[0].y] = false;
    SwapElements(0,a_last);
    a_last = a_last-1;
    Sift(0);
  }//if
  return Result;
}//func

TSearchNode THeap::GetNode(const Byte x, const Byte y) const
{
  if (IsNodePresent(x,y)) return Nodes[x][y];
  TSearchNode Result;
  Result.cost_est = 25000;
  Result.cost_real = 25000;
  Result.x = cNotANode;
  Result.y = cNotANode;
  Result.Parent.x = cNotANode;
  Result.Parent.y = cNotANode;
  return Result;
}//func


//the function

bool FindPath(const Byte from_x, const Byte from_y, const Byte target_x, const Byte target_y,
              const bool WaterWay, const TMap& AMap, TCoordArr& path,
              const Byte SpecialNodeX, const Byte SpecialNodeY)
{
  std::cout<< "Entered FindPath.\n";
  path.clear();
  //check for range
  if ((from_x>=cMap_X) or (target_x>=cMap_X) or (from_y>=cMap_Y) or (target_y>=cMap_Y))
  {
    std::cout << "--Coordinates out of range!\n";
    return false;
  }//if

  /*//check for map
  if AMap=nil then
  begin
    WriteLn('--No valid map specified.');
    Exit;
  end;//if*/

  //check for land/ sea transition and exit, if positive
  /*if (AMap.tiles[from_x, from_y].IsWater xor AMap.tiles[target_x, target_y].IsWater) then Exit;*/
  if ((AMap.tiles[target_x][target_y]->IsWater()!=WaterWay) and not ((target_x==SpecialNodeX) and(target_y==SpecialNodeY)))
  {
    std::cout <<"--Target failed \"water check\".\n";
    if (WaterWay) std::cout<< "----WaterWay: True\n"; else std::cout<<"----WaterWay: False\n";
    std::cout <<"----target:  "<< (int)target_x<<","<< (int)target_y<<"\n";
    std::cout <<"----special: "<<(int)SpecialNodeX<<","<<(int)SpecialNodeY<<"\n";
    return false;
  }//if

  THeap* open;
  THeap* closed;

  open = new THeap;
  closed = new THeap;

  TSearchNode node;
  TSearchNode temp;
  node.cost_real = 0;
  node.cost_est = Heuristic(from_x, from_y, target_x, target_y);
  node.x = from_x;
  node.y = from_y;
  node.Parent.x = cNotANode;
  node.Parent.y = cNotANode;

  open->AddNode(node);
  LongInt i, j;
  do
  {
    node = open->RemoveMin();
    if ((node.x==target_x) and (node.y==target_y))
    {
      std::cout<<"Found path! :)\n";
      //Pfad gefunden :)
      //add all nodes to path
      temp = node;
      while (temp.x!=cNotANode)
      {
        TCoords ttt; ttt.x = temp.x; ttt.y = temp.y; //node for push_back
        path.push_back(ttt);
        temp = closed->GetNode(temp.Parent.x, temp.Parent.y);
      }//while

      //end it
      delete open;
      open = NULL;
      delete closed;
      closed = NULL;
      return true;
    }//if

    //ExpandNode
    for (i=-1; i<=1; ++i)
      for (j= -1; j<=1; ++j)
        if (((i!=0) or (j!=0)) and (node.x+i>=0) and (node.x+i<cMap_X) and (node.y+j>=0) and (node.y+j<cMap_Y-1))
        {
          temp.x = node.x+i;
          temp.y = node.y+j;
          if (!closed->IsNodePresent(temp.x, temp.y))
          {
            //if AMap.tiles[node.x,node.y].IsWater=AMap.tiles[temp.x,temp.y].IsWater then
            if ((AMap.tiles[temp.x][temp.y]->IsWater()==WaterWay) or((temp.x==SpecialNodeX) and (temp.y==SpecialNodeY)))
            {
              temp.Parent.x = node.x;
              temp.Parent.y = node.y;
              temp.cost_real = node.cost_real+1;
              temp.cost_est = temp.cost_real+ Heuristic(temp.x, temp.y, target_x, target_y);
              if (!open->IsNodePresent(temp.x, temp.y) or (open->GetNode(temp.x, temp.y).cost_est>temp.cost_est))
                open->AddNode(temp);
            }//if WaterCheck successful
          }//if closed.contains(temp)
        }//if

    //end of ExpandNode

    closed->AddNode(node);
  } while (!open->Empty());
  std::cout << "Open list is empty :(\n";
  delete open;
  open = NULL;
  delete closed;
  closed = NULL;
  return false;
}//func

