{ ***************************************************************************

    This file is part of Vespucci.
    Copyright (C) 2008, 2010, 2011  Dirk Stolle

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

unit MessageSystem;

interface

uses
  BasicCallback, Helper;

type
  { pointer type for elements of the message queue }
  PQueueElem = ^TQueueElem;
  { record that hold an element of the message queue }
  TQueueElem = record
                 txt: AnsiString;
                 options: TShortStrArr;
                 inputCaption, inputText: ShortString;
                 cbRec: TBasicCallback;
                 next: PQueueElem;
               end;//rec


  { ********
    **** TMessageSystem class
    ****
    **** purpose: holds data of currently shown text messages in game and
    ****          provides interface to add new messages
    ****
    **** TODO:
    **** =====
    ****
    **** - Class TCallbackRec should be completely removed from this class and
    ****   its dependencies and be replaced with another neutral, minimalistic
    ****   callback class.
    *******
  }
  TMessageSystem = class
    private
      msg_queue: record
                   first: PQueueElem;
                   last: PQueueElem;
                 end;//rec

      { pushes a new message at the end of the message queue

        parameters:
            msg_txt   - text of the message
            opts      - array that contains the answer options to that message
            inCaption - caption for the input field
            inText    - preset text of the input field
            the_cbRec - callback record; used after the message window was
                        closed to handle input/ selection

        remarks:
            If opts contains more than zero options, the player can choose from
            the specified number of options in the message window.
            If opts contains no options, but inCaption is not an empty string,
            the player will get a one-line field to enter a text.
      }
      procedure EnqueueNewMessage(const msg_txt: AnsiString; const opts: TShortStrArr; const inCaption, inText: ShortString; the_cb: TBasicCallback);
    public
      //text messages
      //msg: record
             txt: AnsiString;
             options: TShortStrArr;
             selected_option: Integer;
             inputCaption, inputText: ShortString;
             cbRec: TBasicCallback;
      //     end;//rec

      { constructor }
      constructor Create;

      { destructor }
      destructor Destroy;

      { adds a new message with the given text (or better: puts it into the
        message queue)

        paramaters:
            msg_text - the text of the message
      }
      procedure AddMessageSimple(const msg_txt: AnsiString);

      { adds a new message with the given text and options (or better: puts it
        into the message queue)

        paramaters:
            msg_text  - the text of the message
            opts      - the options the player can choose from
            the_cbRec - callback record for callback after player has chosen an
                        option
      }
      procedure AddMessageOptions(const msg_txt: AnsiString; const opts: TShortStrArr; the_cb: TBasicCallback);

      { adds a new message with the given text and an input field (or better:
        puts the message into the message queue)

        paramaters:
            msg_text  - the text of the message
            inCaption - caption of the input field
            inDefault - default (preset) text for the input field
            the_cbRec - callback record for callback after player has made his
                        input
      }
      procedure AddMessageInput(const msg_txt: AnsiString; const inCaption: ShortString; const inDefault: ShortString; the_cb: TBasicCallback);

      { deletes the current message and replaces it with the next message in
        the message queue
      }
      procedure DequeueMessage;//de-facto dequeue
  end;//class

var
  msg: TMessageSystem;

implementation

constructor TMessageSystem.Create;
begin
  inherited Create;
  //msg part
  txt:= '';
  SetLength(options, 0);
  selected_option:= 0;
  inputCaption:= '';
  inputText:= '';
  cbRec:= nil;
  //queue
  msg_queue.first:= nil;
  msg_queue.last:= nil;
end;//construc

destructor TMessageSystem.Destroy;
begin
  while (txt<>'') or (msg_queue.first<>nil) do
  begin
    DequeueMessage;
  end;//while
  inherited Destroy;
end;//destruc

procedure TMessageSystem.AddMessageSimple(const msg_txt: AnsiString);
var null_opts: TShortStrArr;
begin
  {$IFDEF DEBUG_CODE}
    WriteLn('Entered TMessageSystem.AddMessageSimple');
  {$ENDIF}
  if txt='' then
  begin
    txt:= Trim(msg_txt);
    SetLength(msg.options, 0);
    inputCaption:= '';
    inputText:= '';
    cbRec:= nil;
  end
  else begin
    //enqueue new message
    SetLength(null_opts, 0);
    EnqueueNewMessage(msg_txt, null_opts, '', '', nil);
  end;//else
  {$IFDEF DEBUG_CODE}
    WriteLn('Leaving TMessageSystem.AddMessageSimple');
  {$ENDIF}
end;//proc

procedure TMessageSystem.AddMessageOptions(const msg_txt: AnsiString; const opts: TShortStrArr; the_cb: TBasicCallback);
var i: Integer;
begin
  {$IFDEF DEBUG_CODE}
    WriteLn('Entered TMessageSystem.AddMessageOptions');
  {$ENDIF}
  if msg.txt='' then
  begin
    txt:= Trim(msg_txt)+cSpace60;
    SetLength(options, length(opts));
    for i:= 0 to High(opts) do
      options[i]:= copy(Trim(opts[i]),1,59);
    inputCaption:= '';
    inputText:= '';
    selected_option:= 0;
    cbRec:= the_cb;
  end
  else begin
    //enqueue new message
    EnqueueNewMessage(Trim(msg_txt)+cSpace60, opts, '', '', the_cb);
  end;//else
  {$IFDEF DEBUG_CODE}
    WriteLn('Leaving TMessageSystem.AddMessageOptions');
  {$ENDIF}
end;//proc

procedure TMessageSystem.AddMessageInput(const msg_txt: AnsiString; const inCaption: ShortString; const inDefault: ShortString; the_cb: TBasicCallback);
var null_opts: TShortStrArr;
begin
  {$IFDEF DEBUG_CODE}
    WriteLn('Entered TMessageSystem.AddMessageInput');
  {$ENDIF}
  if txt='' then
  begin
    txt:= Trim(msg_txt)+cSpace60;
    SetLength(msg.options, 0);
    //input caption maximum is half the line (i.e. 30 characters)
    inputCaption:= copy(Trim(inCaption),1, 30);
    inputText:= Trim(inDefault);
    selected_option:= 0;
    cbRec:= the_cb;
  end//if
  else begin
    //enqueue new message
    SetLength(null_opts, 0);
    EnqueueNewMessage(Trim(msg_txt)+cSpace60, null_opts, inCaption, inDefault, the_cb);
  end;//else
  {$IFDEF DEBUG_CODE}
    WriteLn('Leaving TMessageSystem.AddMessageInput');
  {$ENDIF}
end;//proc

procedure TMessageSystem.EnqueueNewMessage(const msg_txt: AnsiString; const opts: TShortStrArr; const inCaption, inText: ShortString; the_cb: TBasicCallback);
var temp: PQueueElem;
    i: Integer;
begin
  {$IFDEF DEBUG_CODE}
    WriteLn('Entered TMessageSystem.EnqueueNewMessage');
  {$ENDIF}
  New(temp);
  temp^.txt:= msg_txt;
  SetLength(temp^.options, length(opts));
  for i:= 0 to High(opts) do temp^.options[i]:= copy(Trim(opts[i]),1,59);
  //maximum caption is half the line long (i.e. 30 characters)
  temp^.inputCaption:= copy(Trim(inCaption),1, 30);
  temp^.inputText:= Trim(inText);
  temp^.cbRec:= the_cb;
  temp^.next:= nil;
  if msg_queue.first=nil then
  begin
    msg_queue.first:= temp;
    msg_queue.last:= temp;
  end//if
  else begin
    msg_queue.last^.next:= temp;
    msg_queue.last:= temp;
  end;//else
  {$IFDEF DEBUG_CODE}
    WriteLn('Leaving TMessageSystem.EnqueueNewMessage');
  {$ENDIF}
end;//proc

procedure TMessageSystem.DequeueMessage;
var i: Integer;
    temp: PQueueElem;
begin
  if msg_queue.first<>nil then
  begin
    txt:= msg_queue.first^.txt;
    SetLength(options, length(msg_queue.first^.options));
    for i:=0 to High(msg_queue.first^.options) do
      options[i]:= msg_queue.first^.options[i];
    inputCaption:= msg_queue.first^.inputCaption;
    inputText:= msg_queue.first^.inputText;
    cbRec:= msg_queue.first^.cbRec;
    //move first pointer to new first element
    temp:= msg_queue.first;
    msg_queue.first:= msg_queue.first^.next;
    if msg_queue.first=nil then msg_queue.last:= nil;
    //shorten queue (and thus free former first element)
    Dispose(temp);
    selected_option:=0;
  end//if-then-branch
  else begin
    //no new messages in queue; clear msg.
    txt:= '';
    SetLength(options, 0);
    selected_option:=0;
    inputCaption:= '';
    inputText:= '';
    cbRec:= nil;
  end;//else branch
end;//proc


initialization
  msg:= TMessageSystem.Create;

finalization
  msg.Destroy;
  msg:= nil;

end.