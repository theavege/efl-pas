unit elementary;

{$mode objfpc}{$H+}{$linklib elementary}

interface

uses
  ctypes,
  evas;

type

  TElmWindow = object(TEvasObject)
  public
    procedure Add(const child: TEvasObject); cdecl; virtual;
    procedure ResizeObject(const child: TEvasObject); cdecl;
    procedure SetAutodel(const value: Integer); cdecl;
    Constructor Setup(const id, title: PChar);
  end;

  TElmBox = object(TEvasObject)
  public
    procedure Add(const child: TEvasObject); cdecl; virtual;
    procedure SetHorizontal(const value: integer); cdecl;
    procedure SetHomogeneous(const value: integer); cdecl;
    procedure SetPadding(const H, V: integer); cdecl;
    procedure Pack(const child: TEvasObject); cdecl;
    procedure Recalculate; cdecl;
    Constructor Setup(const parent: TEvasObject);
  end;

  TElmLabel = object(TEvasObject)
  public
    Constructor Setup(const parent: TEvasObject);
  end;

  TElmButton = object(TEvasObject)
  public
    procedure OnClicked(const cb: Elm_Button_Cb; data: Pointer); cdecl;
    Constructor Setup(const parent: TEvasObject);
  end;

  TElmEntry = object(TEvasObject)
  public
    Constructor Setup(const parent: TEvasObject);
    procedure SetLine(const value: Integer); cdecl;
    procedure SetScrollable(const value: Integer); cdecl;
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
    cb.Show();
    elm_run;
    elm_shutdown;
  end;

  function elm_win_util_standard_add(id, title: PChar): Pointer; cdecl; external;
  Constructor TElmWindow.Setup(const id, title: PChar);
  begin
    Self.ptr := elm_win_util_standard_add(id, title);
    Self.Resize(400, 300);
    Self.SetAutodel(1);
  end;

  procedure elm_win_resize_object_add(parent, child: Pointer); cdecl; external;
  procedure TElmWindow.ResizeObject(const child: TEvasObject); cdecl;
  begin
    elm_win_resize_object_add(Self.ptr, child.ptr);
  end;

  procedure TElmWindow.Add(const child: TEvasObject); cdecl;
  begin
    //Self.ResizeObject(child);
    inherited;
  end;

  procedure elm_win_autodel_set(obj: Pointer; value: cint); cdecl; external;
  procedure TElmWindow.SetAutodel(const value: Integer); cdecl;
  begin
    elm_win_autodel_set(Self.ptr, value);
  end;

  function elm_box_add(parent: Pointer): Pointer; cdecl; external;
  Constructor TElmBox.Setup(const parent: TEvasObject);
  begin
    Self.ptr := elm_box_add(parent.ptr);
    Self.SetHomogeneous(1);
    Self.SetHorizontal(0);
    parent.Add(Self);
  end;

  function elm_box_horizontal_set(obj: Pointer; value: cint): Pointer; cdecl; external;
  procedure TElmBox.SetHorizontal(const value: integer); cdecl;
  begin
    elm_box_horizontal_set(Self.ptr, value);
  end;

  function elm_box_homogeneous_set(obj: Pointer; value: cint): Pointer; cdecl; external;
  procedure TElmBox.SetHomogeneous(const value: integer); cdecl;
  begin
    elm_box_homogeneous_set(Self.ptr, value);
  end;

  function elm_box_padding_set(obj: Pointer; h, v: cint): Pointer; cdecl; external;
  procedure TElmBox.SetPadding(const H, V: integer); cdecl;
  begin
    elm_box_padding_set(Self.ptr, H, V);
  end;

  procedure elm_box_pack_end(obj, subobj: Pointer); cdecl; external;
  procedure TElmBox.Pack(const child: TEvasObject); cdecl;
  begin
    elm_box_pack_end(Self.ptr, child.ptr);
  end;

  procedure elm_box_recalculate(obj: Pointer); cdecl; external;
  procedure TElmBox.Recalculate; cdecl;
  begin
    elm_box_recalculate(Self.ptr);
  end;

  procedure TElmBox.Add(const child: TEvasObject); cdecl;
  begin
    Self.Pack(child);
    Self.Recalculate;
    inherited;
  end;

  function elm_button_add(parent: Pointer): Pointer; cdecl; external;
  Constructor TElmButton.Setup(const parent: TEvasObject);
  begin
    Self.ptr := elm_button_add(parent.ptr);
    parent.Add(Self);
  end;

  procedure TElmButton.OnClicked(const cb: Elm_Button_Cb; data: Pointer); cdecl;
  begin
    Self.SetCallback('clicked', cb, data);
  end;

  function elm_label_add(parent: Pointer): Pointer; cdecl; external;
  Constructor TElmLabel.Setup(const parent: TEvasObject);
  begin
    Self.ptr := elm_label_add(parent.ptr);
    parent.Add(Self);
  end;

  function elm_entry_add(parent: Pointer): Pointer; cdecl; external;
  Constructor TElmEntry.Setup(const parent: TEvasObject);
  begin
    Self.ptr := elm_entry_add(parent.ptr);
    Self.SetLine(1);
    Self.SetScrollable(1);
    parent.Add(Self);
  end;

  function elm_entry_single_line_set(obj: Pointer; value: cint): Pointer; cdecl; external;
  procedure TElmEntry.SetLine(const value: Integer); cdecl;
  begin
    elm_entry_single_line_set(Self.ptr, value);
  end;

  function elm_entry_scrollable_set(obj: Pointer; value: cint): Pointer; cdecl; external;
  procedure TElmEntry.SetScrollable(const value: Integer); cdecl;
  begin
    elm_entry_scrollable_set(Self.ptr, value);
  end;
end.
