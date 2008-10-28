unit PathFinder;

interface

uses
  Map, Helper;

const
  cNotANode = 255;

type
  TPresenceList = array [0..cMap_X-1, 0..cMap_Y-1] of Boolean;
  TCoords = record
              x,y: Byte;
            end;//rec
  TCoordArr = array of TCoords;
  TSearchNode = record
                  cost_est: Integer; //known cost plus estimated cost to target
                  cost_real: Integer; //(cost to get here a.k.a. "known cost")
                  x,y: Byte;
                  Parent: TCoords;
                end;//rec
  TNodes = array [0..cMap_X-1, 0..cMap_Y-1] of TSearchNode;
  THeapNode = record
                content: record
                           x,y: Byte;
                         end;
              end;//rec

  THeap = class
                private
                  Presence: TPresenceList;
                  Nodes: TNodes;
                  a: array of THeapNode;
                  a_last: Integer;
                  procedure SwapElements(const n1, n2: Integer);
                  procedure Sift(const element: Integer);
                  function  LessEqual(const n1, n2: Integer): Boolean;
                public
                  constructor Create;
                  destructor Destroy;
                  function Empty: Boolean;
                  function IsNodePresent(const x,y: Byte): Boolean;
                  function AddNode(const newNode: TSearchNode): Boolean;
                  function PeekMin: TSearchNode;
                  function RemoveMin: TSearchNode;
                  function GetNode(const x,y: Byte): TSearchNode;
              end;//class

  function Heuristic(const from_x, from_y, to_x, to_y: Byte): Integer;
  
  function FindPath(const from_x, from_y, target_x, target_y: Byte; AMap: TMap; var path: TCoordArr): Boolean;

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
    Result:= True;
  end;//else
end;//func

function THeap.PeekMin: TSearchNode;
begin
  if a_last>=0 then Result:= GetNode(a[0].content.x, a[0].content.y)
  else GetNode(cNotANode, cNotANode);
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

function FindPath(const from_x, from_y, target_x, target_y: Byte; AMap: TMap; var path: TCoordArr): Boolean;
var open, closed: THeap;
    node, temp: TSearchNode;
    i,j: Integer;
begin
  SetLength(path, 0);
  //check for land/ sea transition and exit, if positive
  if (AMap.tiles[from_x, from_y].IsWater xor AMap.tiles[target_x, target_y].IsWater) then Exit;
  
  
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
      //Pfad gefunden :)
      Result:= True;
      //to do: add all nodes to path!
      //adding them
      temp:= node;
      while temp.x<>cNotANode do
      begin
        SetLength(path, length(path)+1);
        path[High(path)].x:= temp.x;
        path[High(path)].y:= temp.y;
        temp:= closed.GetNode(temp.Parent.x, temp.Parent.y);
      end;//while
      
      //end it
      
      break;
    end;

    //ExpandNode

    for i:= -1 to 1 do
      for j:= -1 to 1 do
        if (((i<>0) or (j<>0)) and (node.x+i in [0..cMap_X-1]) and (node.y+j in [0..cMap_Y-1])) then
        begin
          temp.x:= node.x+i;
          temp.y:= node.y+i;
          if AMap.tiles[node.x,node.y].IsWater=AMap.tiles[temp.x,temp.y].IsWater then
          begin
            temp.Parent.x:= node.x;
            temp.Parent.y:= node.y;
            temp.cost_real:= node.cost_real+1;
            temp.cost_est:= temp.cost_real+ Heuristic(temp.x, temp.y, target_x, target_y);
            if not open.IsNodePresent(temp.x, temp.y) or (open.GetNode(temp.x, temp.y).cost_est>temp.cost_est) then
              open.AddNode(temp);
          end;//if WaterCheck successful
        end;//if

    //end of ExpandNode

    closed.AddNode(node);
  until open.Empty;
  open.Destroy;
  closed.Destroy;
  Result:= False;
end;//func

end.