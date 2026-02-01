program Example;

{$mode objfpc}{$H+}{$linklib elementary}

uses
  elementary;

procedure OnBtnClick(data, obj, event: Pointer); cdecl;
var
  btn: TElmButton;
  lbl: TElmLabel;
begin
  btn.ptr := obj;
  lbl.ptr := data;
  lbl.SetText(btn.Text);
end;

procedure OnWinDelete(data, obj, event: Pointer); cdecl;
begin
  WriteLn('Exit');
  Exit;
end;

function Setup: TElmWindow; cdecl;
var
  box: TElmBox;
  lbl: TElmLabel;
  btn: TElmButton;
  ent: TElmEntry;
begin
  Result.Setup('demo', 'FreePascal + EFL');
  Result.OnDelete(@OnWinDelete, nil);
  box.Setup(Result);
  lbl.Setup(Result);
  lbl.Move(100, 100);
  lbl.Resize(200, 50);
  ent.Setup(Result);
  ent.Move(100, 100);
  ent.Resize(200, 50);
  btn.Setup(Result);
  btn.Move(100, 200);
  btn.Resize(200, 50);
  btn.SetText('BBUUUT');
  btn.OnClicked(@OnBtnClick, lbl.ptr);
end;

begin
  ElmApplication(@Setup);
end.
