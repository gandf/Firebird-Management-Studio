{
 * The contents of this file are subject to the InterBase Public License
 * Version 1.0 (the "License"); you may not use this file except in
 * compliance with the License.
 * 
 * You may obtain a copy of the License at http://www.Inprise.com/IPL.html.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.  The Original Code was created by Inprise
 * Corporation and its predecessors.
 * 
 * Portions created by Inprise Corporation are Copyright (C) Inprise
 * Corporation. All Rights Reserved.
 *
 * Contributor(s): Krzysztof Golko, Jeff Overcash.
}

unit MemoLists;

{$MODE Delphi}

interface
uses
  Classes, Contnrs;

type
  TMemoList = class
  private
    FCurrent: integer;
    // A flag indicating that pointer is "just past" current position
    // if set to true GetPrev clears the flag and doesn't move back and
    // GetNext moves forward and clears the flag
    FJustPast: boolean;
    FMemos: TObjectList;
    function GetItem(Index: Integer): TStrings;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function GetCurrent: TStrings;
    function GetNext: TStrings;
    function GetPrev: TStrings;
    function BOC: boolean;  // at the beginning
    function EOC: boolean;  // at the end
    procedure Clear;
    procedure Add(const Memo: TStrings);
    // sets the FJustPast flag
    procedure MovePast;
    property Items[Index: Integer]: TStrings read GetItem; default;
    property Current : Integer read FCurrent write FCurrent;
    // kris new property
    property JustPast: boolean read FJustPast;
    property Count : Integer read GetCount;
  end;


implementation

{ TMemoList }
constructor TMemoList.Create;
begin
  FCurrent := -1;
  FMemos := TObjectList.Create;
end;

destructor TMemoList.Destroy;
begin
  FMemos.Clear;
  FMemos.Free;
  inherited;
end;

procedure TMemoList.Clear;
begin
  FCurrent := -1;
  FJustPast := FALSE;
  FMemos.Clear;
end;

procedure TMemoList.Add(const Memo: TStrings);
var
  t : TStringList;
begin
  t := TStringList.Create;
  t.AddStrings(Memo);
  FMemos.Add(t);
  // Kris new line of code
  FJustPast := FALSE;
  FCurrent := FMemos.Count - 1; // Index is zero based so FCurrent is looking one before count
end;

procedure TMemoList.MovePast;
begin
  if FMemos.Count > 0 then
    FJustPast := TRUE;
end;

function TMemoList.GetCurrent: TStrings;
begin
  if (FCurrent >= 0) and (FCurrent < FMemos.Count) then
    Result := TStrings(FMemos[FCurrent])
  else
    Result := nil;
end;

function TMemoList.GetNext: TStrings;
begin
  if FCurrent < FMemos.Count - 1 then
  begin
    Inc(FCurrent);
    Result := TStrings(FMemos[FCurrent]);
    FJustPast := FALSE;
  end
  else
  begin
    Result := nil;
    // kris new two lines of code
    if Fmemos.Count > 0 then
      FJustPast := TRUE;
  end;
end;

function TMemoList.GetPrev: TStrings;
begin
  if FCurrent > 0 then
  begin
    if FJustPast then
      FJustPast := FALSE
    else
      Dec(FCurrent);
    Result := TStrings(FMemos[FCurrent]);
  end
  else
  begin
    // kris new line of code
    FJustPast := FALSE;
    if FCurrent = 0 then
      Result := TStrings(FMemos[FCurrent])
    else
      Result := nil;
  end;
end;

function TMemoList.BOC: boolean;
begin
  // kris slight change
  Result := (FCurrent < 0)                           // list empty
            or ((FCurrent = 0) and not FJustPast);   // or at first
end;

function TMemoList.EOC: boolean;
begin
  Result := FCurrent = (FMemos.Count - 1);
end;

function TMemoList.GetItem(Index: Integer): TStrings;
begin
  If Index > FMemos.Count then
    Index := FMemos.Count
  else
    if Index < 0 then
      Index := 0;
  Result := TStrings(FMemos[Index]);
end;

function TMemoList.GetCount: Integer;
begin
  Result := FMemos.Count;
end;

end.
