program Example;

{$mode objfpc}{$H+}{$linklib elementary}

uses
  elementary;

procedure OnButtonClick(data, obj, event: Pointer); cdecl;
var
  btn: TElmButton;
  lbl: TElmLabel;
begin
  btn.obj := obj;
  lbl.obj := data;
  lbl.SetText(btn.Text);
end;

function Setup: TElmWindow; cdecl;
var
  btn: TElmButton;
  lbl: TElmLabel;
begin
  Result.Setup('demo', 'FreePascal + EFL');
  Result.Resize(400, 300);
  Result.SetAutodel(1);
  Result.Show;

  lbl.Setup(Result);
  lbl.Move(100, 100);
  lbl.Resize(200, 50);
  lbl.Show;

  btn.Setup(Result);
  btn.Move(100, 200);
  btn.Resize(200, 50);
  btn.SetText('BBUUUT');
  btn.OnClicked(@OnButtonClick, lbl.obj);
  btn.Show;
end;

begin
  ElmApplication(@Setup);
end.
