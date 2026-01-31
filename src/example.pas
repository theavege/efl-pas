program Example;

{$mode objfpc}{$H+}{$linklib elementary}

uses
  elementary;

procedure OnButtonClick(data, obj, event: Pointer); cdecl;
var
  btn: TElmButton;
  lbl: TElmLabel;
begin
  btn.ptr := obj;
  lbl.ptr := data;
  lbl.SetText(btn.Text);
end;

function Setup: TElmWindow; cdecl;
var
  lbl: TElmLabel;
  btn: TElmButton;
  ent: TElmEntry;
begin
  Result.Setup('demo', 'FreePascal + EFL');
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
  btn.OnClicked(@OnButtonClick, lbl.ptr);
end;

begin
  ElmApplication(@Setup);
end.
