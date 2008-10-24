unit PathFinder;

interface

uses
  Map, Helper;

type
  TPresenceList = array [0..cMap_X-1, 0..cMap_Y-1] of Boolean;
  TSearchNode = record
                  cost: Integer;
                  Parent: record
                            x,y: Byte;
                          end;//rec
                end;//rec
  TNodes = array [0..cMap_X-1, 0..cMap_Y-1] of TSearchNode;
  PHeapNode = ^THeapNode;
  THeapNode = record
                content: record
                           x,y: Byte;
                         end;
                left, right: PHeapNode;
              end;//rec

  THeap = class
                private
                  Presence: TPresenceList;
                  Nodes: TNodes;
                  rootNode: PHeapNode;
                public
                  constructor Create;
                  destructor Destroy;
                  function IsNodePresent(const x,y: Byte): Boolean;
                  function AddNode(const x,y: Byte; const newNode: TSearchNode): Boolean;
                  function RemoveNode(const x,y: Byte): Boolean;
              end;//class

  function Heuristic(const from_x, from_y, to_x, to_y: Byte): Double;
  procedure SwapContent(var a, b: PHeapNode);
  procedure Heapify(var ANode: PHeapNode);


implementation

function Heuristic(const from_x, from_y, to_x, to_y: Byte): Double;
begin
  Result:= Min(abs(from_x-to_x), abs(from_y-to_y));
end;//func

procedure SwapContent(var a, b: PHeapNode);
var temp: TSearchNode;
begin
  temp:= a^.content:
  a^.content:= b^.content;
  b^.content:= temp;
end;//proc

procedure Heapify(var ANode: PHeapNode);
begin
  if (ANode=nil) then Exit;
  if ((ANode^.left=nil) and (ANode^.right=nil)) then Exit;
  Heapify(ANode^.left);
  Heapify(ANode^.right);
  if ANode^.left<>nil then
    if ANode^.left^.content.cost<ANode^.cost then
    begin
      SwapContent(ANode, ANode^.left);
      Heapify(ANode^.left);
    end;//if
  if ANode^.right<>nil then
    if ANode^.right^.content.cost<ANode^.cost then
    begin
      SwapContent(ANode, ANode^.right);
      Heapify(ANode^.right);
    end;//if
end;//proc

//THeap functions
constructor THeap.Create;
var i,j: Byte;
begin
  for i:= 0 to cMap_X-1 do
    for j:= 0  to cMap_Y-1 do
      Presence[i,j]:= False;
  rootNode:= nil;
end;//construc

destructor THeap.Destroy;
begin
  inherited Destroy;
end;//destruc

function THeap.IsNodePresent(const x,y: Byte): Boolean;
begin
  if ((x<cMap_X) and (y<cMap_Y)) then Result:= Presence[x,y]
  else Result:= False;
end;//func

function THeap.AddNode(const x,y: Byte; const newNode: TSearchNode): Boolean;
var temp, trace: PHeapNode;
begin
  if ((x>=cMap_X) or (y>=cMap_Y)) then Result:= False
  else begin
    New(temp);
    temp^.content.x:=x;
    temp^.content.y:=y;
    Nodes[x,y].cost:= newNode.cost;
    Nodes[x,y].Parent.x:= newNode.Parent.x;
    Nodes[x,y].Parent.y:= newNode.Parent.y;
    temp^.left:= nil;
    temp^.right:= nil;
    if rootNode=nil then rootNode:= temp
    else begin
      trace:= rootNode;
      while trace^.left<>nil do
        trace:= trace^.left;
      trace^.left:= temp;
      Heapify(rootNode);
    end;//else
  end;//else
end;//func

end.