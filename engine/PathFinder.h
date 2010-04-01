#ifndef PATHFINDER_H
#define PATHFINDER_H

#include "Map.h"
#include "Helper.h"
#include <vector>

const Byte cNotANode = 255;

typedef bool TPresenceList[cMap_X][cMap_Y];

struct TCoords
{
  Byte x;
  Byte y;
};//rec

typedef std::vector<TCoords> TCoordArr;


struct TSearchNode
{
  LongInt cost_est; //known cost plus estimated cost to target
  LongInt cost_real; //(cost to get here a.k.a. "known cost")
  Byte x;
  Byte y;
  TCoords Parent;
};//rec

typedef TSearchNode TNodes[cMap_X][cMap_Y];

/*  THeapNode = record
                content: record
                           x,y: Byte;
                         end;
              end;//rec*/

struct THeapNode
{
  Byte x;
  Byte y;
};

class THeap
{
  private:
    TPresenceList Presence;
    TNodes Nodes;
    std::vector<THeapNode> a;
    LongInt a_last;
    void SwapElements(const LongInt n1, const LongInt n2);
    void Sift(const LongInt element);
    void Heapify();
    bool LessEqual(const LongInt n1, const LongInt n2) const;
  public:
    THeap();
    ~THeap();
    bool Empty() const;
    bool IsNodePresent(const Byte x, const Byte y) const;
    bool AddNode(const TSearchNode& newNode);
    TSearchNode PeekMin() const;
    TSearchNode RemoveMin();
    TSearchNode GetNode(const Byte x, const Byte y) const;
};//class

LongInt Heuristic(const Byte from_x, const Byte from_y, const Byte to_x, const Byte to_y);

bool FindPath(const Byte from_x, const Byte from_y, const Byte target_x, const Byte target_y,
              const bool WaterWay, const TMap& AMap, TCoordArr& path,
              const Byte SpecialNodeX=250, const Byte SpecialNodeY=250);
#endif

