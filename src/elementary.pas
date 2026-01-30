unit elementary;

{$mode objfpc}{$H+}{$linklib elementary}

interface

uses
  ctypes,
  evas;

type

  TElmWindow = object(TEvasObject)
  public
    procedure Setup(const id, title: PChar); cdecl;
    procedure SetAutodel(const value: Integer); cdecl;
  end;

  TElmButton = object(TEvasObject)
  public
    procedure Setup(const parent: TEvasObject); cdecl;
    procedure OnClicked(const cb: Elm_Button_Cb; data: Pointer); cdecl;
  end;

  TElmLabel = object(TEvasObject)
  public
    procedure Setup(const parent: TEvasObject); cdecl;
  end;

  ElmAppCallback = function: TElmWindow; cdecl;

procedure ElmApplication(cb: ElmAppCallback); cdecl;

implementation
  function elm_init(argc: cint; argv: PPChar): cint; cdecl; external;
  function elm_shutdown: cint; cdecl; external;
  function elm_run: cint; cdecl; external;
  procedure ElmApplication(cb: ElmAppCallback); cdecl;
  begin
    elm_init(0, nil);
    cb;
    elm_run;
    elm_shutdown;
  end;

  function elm_win_util_standard_add(id, title: PChar): Pointer; cdecl; external;
  procedure TElmWindow.Setup(const id, title: PChar); cdecl;
  begin
    Self.obj := elm_win_util_standard_add(id, title);
  end;

  procedure elm_win_autodel_set(obj: Pointer; value: cint); cdecl; external;
  procedure TElmWindow.SetAutodel(const value: Integer); cdecl;
  begin
    elm_win_autodel_set(Self.obj, value);
  end;

  function elm_button_add(parent: Pointer): Pointer; cdecl; external;
  procedure TElmButton.Setup(const PARENT: TEvasObject); cdecl;
  begin
    Self.obj := elm_button_add(PARENT.obj);
  end;

  procedure TElmButton.OnClicked(const cb: Elm_Button_Cb; data: Pointer); cdecl;
  begin
    Self.SetCallback('clicked', cb, data);
  end;

  function elm_label_add(parent: Pointer): Pointer; cdecl; external;
  procedure TElmLabel.Setup(const PARENT: TEvasObject); cdecl;
  begin
    Self.obj := elm_label_add(PARENT.obj);
  end;
end.
