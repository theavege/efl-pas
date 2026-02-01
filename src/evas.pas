unit evas;

{$mode objfpc}{$H+}{$linklib evas}

interface

uses
  ctypes;

type

  Elm_Button_Cb = procedure(data, obj, event: Pointer); cdecl;

  TEvasObject = object
  public
    ptr: Pointer;
    procedure Add(const child: TEvasObject); cdecl; virtual;
    procedure Resize(const W, H: Integer); cdecl;
    procedure Move(const X, Y: Integer); cdecl;
    procedure SetText(const TEXT: PChar); cdecl;
    procedure SetAlign(const v, h: Single); cdecl;
    procedure SetWeight(const x, y: Integer); cdecl;
    function Text: PChar; cdecl;
    procedure Show; cdecl;
    procedure Conf; cdecl;
    procedure SetCallback(const event: PChar; cb: Elm_Button_Cb; data: Pointer); cdecl;
  end;

implementation
  procedure evas_object_show(ptr: Pointer); cdecl; external;
  procedure TEvasObject.Show; cdecl;
  begin
    evas_object_show(Self.ptr);
  end;

  procedure TEvasObject.Conf; cdecl;
  begin
    Self.SetAlign(-1.0, -1.0);
    Self.SetWeight(1, 1);
  end;

  procedure TEvasObject.Add(const child: TEvasObject); cdecl;
  begin
    child.Show;
  end;

  procedure evas_object_smart_callback_add(obj: Pointer; const event: PChar; cb: Elm_Button_Cb; data: Pointer); cdecl; external;
  procedure TEvasObject.SetCallback(const event: PChar; cb: Elm_Button_Cb; data: Pointer); cdecl;
  begin
    evas_object_smart_callback_add(Self.ptr, event, cb, data);
  end;

  procedure evas_object_resize(obj: Pointer; w, h: cint); cdecl; external;
  procedure TEvasObject.Resize(const W, H: Integer); cdecl;
  begin
    evas_object_resize(Self.ptr, W, H);
  end;

  procedure evas_object_move(obj: Pointer; x, y: cint); cdecl; external;
  procedure TEvasObject.Move(const X, Y: Integer); cdecl;
  begin
    evas_object_move(Self.ptr, X, Y);
  end;

  procedure elm_object_part_text_set(obj: Pointer; const PART, TEXT: PChar); cdecl; external;
  procedure TEvasObject.SetText(const TEXT: PChar); cdecl;
  begin
    elm_object_part_text_set(Self.ptr, 'default', TEXT);
  end;

  function elm_object_part_text_get(obj: Pointer; part: PChar): PChar; cdecl; external;
  function TEvasObject.Text: PChar; cdecl;
  begin
    Result := elm_object_part_text_get(Self.ptr, nil);
  end;

  procedure evas_object_size_hint_align_set(obj: Pointer; v, h: cfloat); cdecl; external;
  procedure TEvasObject.SetAlign(const v, h: Single); cdecl;
  begin
    evas_object_size_hint_align_set(Self.ptr, v, h);
  end;

  procedure evas_object_size_hint_weight_set(obj: Pointer; x, y: cint); cdecl; external;
  procedure TEvasObject.SetWeight(const x, y: Integer); cdecl;
  begin
    evas_object_size_hint_weight_set(Self.ptr, x, y);
  end;
end.
