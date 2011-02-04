{ ***************************************************************************

    This file is part of Vespucci.
    Copyright (C) 2008, 2009, 2010  Thoronador

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
}

{ ********
  **** unit PathFinder
  ****
  **** purpose: holds all functions that a used during path finding of units
  ****          (currently uses A*)
  *******
}
unit PathFinder;

interface

uses
  Map, Helper;

const
  { constant value that represents "no node", i.e. end of a list or invalid
    node
  }
  cNotANode = 255;

type
  TPresenceList = array [0..cMap_X-1, 0..cMap_Y-1] of Boolean;

  { record that holds coordinates }
  TCoords = record
              x,y: Byte;
            end;//rec

  { array that holds a sequence of coordinates, e.g. a path }
  TCoordArr = array of TCoords;
  TSearchNode = record
                  cost_est: Integer; //known cost plus estimated cost to target
                  cost_real: Integer; //(cost to get here a.k.a. "known cost")
                  x,y: Byte;
                  Parent: TCoords;
                end;//rec

  { array that holds all search nodes }
  TNodes = array [0..cMap_X-1, 0..cMap_Y-1] of TSearchNode;

  { references a node in the heap }
  THeapNode = record
                content: record
                           x,y: Byte;
                         end;
              end;//rec

  { ********
    **** THeap class
    ****
    **** purpose: implements a heap of nodes/ coordinates on the map
    *******
  }
  THeap = class
                private
                  Presence: TPresenceList;
                  Nodes: TNodes;
                  a: array of THeapNode;
                  a_last: Integer;

                  { swaps nodes at array positions n1 and n2

                    parameters:
                        n1 - index of first node
                        n2 - index of second node
                  }
                  procedure SwapElements(const n1, n2: Integer);

                  { restores order in the sub tree of node with index element

                    parameters:
                        element - index of node that is the root of the sub tree
                                  that has to be sorted
                  }
                  procedure Sift(const element: Integer);

                  { restores the heap property for the complete heap }
                  procedure Heapify;

                  { returns true, if the (estimated) cost of node at index n1 is
                    less or equal to the (estimated) cost of node at index n2

                    parameters:
                        n1 - index of first node
                        n2 - index of second node
                  }
                  function  LessEqual(const n1, n2: Integer): Boolean;
                public
                  { constructor }
                  constructor Create;

                  { destructor }
                  destructor Destroy; override;

                  { returns true, if the heap is empty }
                  function Empty: Boolean;

                  { returns true, if the node with coordinates (x;y) is present

                    parameters:
                        x, y - coordinates of the node
                  }
                  function IsNodePresent(const x,y: Byte): Boolean;

                  { adds a new node to the heap and returns true, if a node was
                    added

                    parameters:
                        newNode - the node that has to be added to the heap

                    remarks:
                        A node will not be added, if its coordinates are not on
                        the map.
                  }
                  function AddNode(const newNode: TSearchNode): Boolean;

                  { returns the node with the minimum cost

                    remarks:
                        If the heap is empty, a search node with both x and y
                        coordinates set to cNotANode will be returned.
                  }
                  function PeekMin: TSearchNode;

                  { removes the node with the minimum cost from the heap and
                    returns it

                    remarks:
                        If the heap is empty, a search node with both x and y
                        coordinates set to cNotANode will be returned.
                  }
                  function RemoveMin: TSearchNode;

                  { returns the heap node for position (x;y)

                    parameters:
                        x,y - coordinates of the node

                    remarks:
                        If the node with the given coordinates is not part of
                        the heap, then a search node with both x and y
                        coordinates set to cNotANode will be returned.
                  }
                  function GetNode(const x,y: Byte): TSearchNode;
              end;//class

  { returns an "optimistic guess" of the moves needed to move from the first
    specified coordinates to the later specified coordinates

    parameters:
        from_x, from_y - first coordinates
        to_x, to_y     - second coordinates

    remarks:
        This function returns the "Manhattan distance" of the coordinates,
        because equals the minimum number of moves required to move between
        those nodes/coordinates; thus, it's always an "optimistic guess".
  }
  function Heuristic(const from_x, from_y, to_x, to_y: Byte): Integer;

  { This function does the actual pathfinding and returns true, if a path was
    found

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
  }
  function FindPath(const from_x, from_y, target_x, target_y: Byte; const WaterWay: Boolean; AMap: TMap; var path: TCoordArr;
                    const SpecialNodeX: Byte=250; const SpecialNodeY: Byte=250): Boolean;

implementation

function Heuristic(const from_x, from_y, to_x, to_y: Byte): Integer;
begin
  Result:= Min(abs(from_x-to_x), abs(from_y-to_y));
end;//func

function Parent(const n: Integer): Integer;
begin
  Result:= (n-1) div 2;
end;//func

function LeftChild(const n: Integer): Integer;
begin
  Result:= n*2+1;
end;//func

function RightChild(const n: Integer): Integer;
begin
  Result:= n*2+2;
end;//func

//THeap functions
constructor THeap.Create;
var i,j: Byte;
begin
  for i:= 0 to cMap_X-1 do
    for j:= 0  to cMap_Y-1 do
      Presence[i,j]:= False;
  SetLength(a,0);
  a_last:=-1
end;//construc

destructor THeap.Destroy;
begin
  SetLength(a, 0);
  inherited Destroy;
end;//destruc

procedure THeap.SwapElements(const n1, n2: Integer);
var temp: THeapNode;
begin
  if ((n1<=a_last) and (n2<=a_last) and (n1>=0) and (n2>=0)) then
  begin
    temp:= a[n1];
    a[n1]:= a[n2];
    a[n2]:= temp;
  end;//if
end;//proc

procedure THeap.Sift(const element: Integer);
var vergleich, l,r: Integer;
begin
  l:= LeftChild(element);
  r:= RightChild(element);
  if l<=a_last then
  begin
    if r<=a_last then
    begin
      if LessEqual(l,r) then vergleich:= l
      else vergleich:= r;
    end//if
    else vergleich:= l;
    if LessEqual(vergleich, element) then
    begin
      SwapElements(vergleich, element);
      Sift(vergleich);
    end;//if
  end;//if
end;//proc

procedure THeap.Heapify;
var i: Integer;
begin
  if a_last>0 then
    for i:= (a_last-1) div 2 downto 0 do
      Sift(i);
end;//proc

function THeap.LessEqual(const n1, n2: Integer): Boolean;
begin
  Result:= (Nodes[a[n1].content.x, a[n1].content.y].cost_est <= Nodes[a[n2].content.x, a[n2].content.y].cost_est);
end;//func

function THeap.Empty: Boolean;
begin
  Result:= (a_last<0);
end;//func

function THeap.IsNodePresent(const x,y: Byte): Boolean;
begin
  if ((x<cMap_X) and (y<cMap_Y)) then Result:= Presence[x,y]
  else Result:= False;
end;//func

function THeap.AddNode(const newNode: TSearchNode): Boolean;
var idx, p: Integer;
begin
  if ((newNode.x>=cMap_X) or (newNode.y>=cMap_Y)) then Result:= False
  else begin
    if Presence[newNode.x,newNode.y] then
    begin
      Nodes[newNode.x,newNode.y]:= newNode;
      Heapify;
    end//if
    else begin
      //make sure that there is enough space
      if High(a)<a_last+1 then SetLength(a, length(a)+5);

      //insert
      a[a_last+1].content.x:= newNode.x;
      a[a_last+1].content.y:= newNode.y;
      Nodes[newNode.x,newNode.y]:= newNode;
      a_last:= a_last+1;
      Presence[newNode.x,newNode.y]:= True;

      //tauschen, bis an richtiger Stelle
      idx:= a_last;
      while idx>0 do
      begin
        p:= Parent(idx);
        if LessEqual(p, idx) then break
        else begin
          SwapElements(p, idx);
          idx:= p;
        end;//else
      end;//while
    end;//else
    Result:= True;
  end;//else
end;//func

function THeap.PeekMin: TSearchNode;
begin
  if a_last>=0 then Result:= GetNode(a[0].content.x, a[0].content.y)
  else Result:= GetNode(cNotANode, cNotANode);
end;//func

function THeap.RemoveMin: TSearchNode;
begin
  Result:= PeekMin;
  if a_last>=0 then
  begin
    Presence[a[0].content.x,a[0].content.y]:= False;
    SwapElements(0,a_last);
    a_last:= a_last-1;
    Sift(0);
  end;//if
end;//func

function THeap.GetNode(const x,y: Byte): TSearchNode;
begin
  if IsNodePresent(x,y) then Result:= Nodes[x,y]
  else begin
    Result.cost_est:= 25000;
    Result.cost_real:= 25000;
    Result.x:= cNotANode;
    Result.y:= cNotANode;
    Result.Parent.x:= cNotANode;
    Result.Parent.y:= cNotANode;
  end;//if
end;//func


//the function

function FindPath(const from_x, from_y, target_x, target_y: Byte; const WaterWay: Boolean; AMap: TMap; var path: TCoordArr;
                  const SpecialNodeX: Byte=250; const SpecialNodeY: Byte=250): Boolean;
var open, closed: THeap;
    node, temp: TSearchNode;
    i,j: Integer;
begin
  WriteLn('Entered FindPath.');
  SetLength(path, 0);
  Result:= False;
  //check for range
  if ((from_x>=cMap_X) or (target_x>=cMap_X) or (from_y>=cMap_Y) or (target_y>=cMap_Y)) then
  begin
    WriteLn('--Coordinates out of range!');
    Exit;
  end;//if

  //check for map
  if AMap=nil then
  begin
    WriteLn('--No valid map specified.');
    Exit;
  end;//if

  //check for land/ sea transition and exit, if positive
  {if (AMap.tiles[from_x, from_y].IsWater xor AMap.tiles[target_x, target_y].IsWater) then Exit;}
  if (AMap.tiles[target_x, target_y].IsWater<>WaterWay) and not ((target_x=SpecialNodeX) and(target_y=SpecialNodeY)) then
  begin
    WriteLn('--Target failed "water check".');
    if WaterWay then WriteLn('----WaterWay: True') else WriteLn('----WaterWay: False');
    WriteLn('----target:  ', target_x, ',', target_y);
    WriteLn('----special: ', SpecialNodeX, ',', SpecialNodeY);
    Exit;
  end;//if


  open:= THeap.Create;
  closed:= THeap.Create;

  node.cost_real:= 0;
  node.cost_est:= Heuristic(from_x, from_y, target_x, target_y);
  node.x:= from_x;
  node.y:= from_y;
  node.Parent.x:= cNotANode;
  node.Parent.y:= cNotANode;

  open.AddNode(node);
  repeat
    node:= open.RemoveMin;
    if ((node.x=target_x) and (node.y=target_y)) then
    begin
      WriteLn('Found path! :)');
      //Pfad gefunden :)
      Result:= True;
      //add all nodes to path
      temp:= node;
      while temp.x<>cNotANode do
      begin
        SetLength(path, length(path)+1);
        path[High(path)].x:= temp.x;
        path[High(path)].y:= temp.y;
        temp:= closed.GetNode(temp.Parent.x, temp.Parent.y);
      end;//while

      //end it
      open.Destroy;
      closed.Destroy;
      Exit;
    end;

    //ExpandNode
    for i:= -1 to 1 do
      for j:= -1 to 1 do
        if (((i<>0) or (j<>0)) and (node.x+i in [0..cMap_X-1]) and (node.y+j in [0..cMap_Y-1])) then
        begin
          temp.x:= node.x+i;
          temp.y:= node.y+j;
          if not closed.IsNodePresent(temp.x, temp.y) then
          begin
            //if AMap.tiles[node.x,node.y].IsWater=AMap.tiles[temp.x,temp.y].IsWater then
            if (AMap.tiles[temp.x,temp.y].IsWater=WaterWay) or((temp.x=SpecialNodeX) and (temp.y=SpecialNodeY)) then
            begin
              temp.Parent.x:= node.x;
              temp.Parent.y:= node.y;
              temp.cost_real:= node.cost_real+1;
              temp.cost_est:= temp.cost_real+ Heuristic(temp.x, temp.y, target_x, target_y);
              if not open.IsNodePresent(temp.x, temp.y) or (open.GetNode(temp.x, temp.y).cost_est>temp.cost_est) then
                open.AddNode(temp);
            end;//if WaterCheck successful
          end;//if closed.contains(temp)
        end;//if

    //end of ExpandNode

    closed.AddNode(node);
  until open.Empty;
  WriteLn('Open list is empty :(');
  open.Destroy;
  closed.Destroy;
  Result:= False;
end;//func

end.