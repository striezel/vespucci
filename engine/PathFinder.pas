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
  
  TNodeHeap = class
                private
                  Presence: TPresenceList;
                  Nodes: TNodes;
                public
                  constructor Create;
                  destructor Destroy;
                  function IsNodePresent(const x,y: Byte): Boolean;
                  function AddNode(const newNode: TSearchNode): Boolean;
                  function RemoveNode(const x,y: Byte): Boolean;
              end;//class
  
  function Heuristic(const from_x, from_y, to_x, to_y: Byte): Double;
  

implementation

function Heuristic(const from_x, from_y, to_x, to_y: Byte): Double;
begin
  Result:= Min(abs(from_x-to_x), abs(from_y-to_y));
end;//func

//TNodeHeap functions
constructor TNodeHeap.Create;
var i,j: Byte;
begin
  for i:= 0 to cMap_X-1 do
    for j:= 0  to cMap_Y-1 do
      Presence[i,j]:= False;
end;//construc

destructor TNodeHeap.Destroy;
begin
  inherited Destroy;
end;//destruc

function TNodeHeap.IsNodePresent(const x,y: Byte): Boolean;
begin
  if ((x<cMap_X) and (y<cMap_Y)) then Result:= Presence[x,y]
  else Result:= False;
end;//func

end.
