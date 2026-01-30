unit evas;

{$mode objfpc}{$H+}{$linklib evas}

interface

uses
  ctypes;

type

  Elm_Button_Cb = procedure(data, obj, event: Pointer); cdecl;

  TEvasObject = object
  public
    obj: Pointer;
    procedure Resize(const W, H: Integer); cdecl;
    procedure Move(const X, Y: Integer); cdecl;
    procedure SetText(const TEXT: PChar); cdecl;
    function Text: PChar; cdecl;
    procedure Show; cdecl;
    procedure SetCallback(const event: PChar; cb: Elm_Button_Cb; data: Pointer); cdecl;
  end;

implementation
  procedure evas_object_show(obj: Pointer); cdecl; external;
  procedure TEvasObject.Show; cdecl;
  begin
    evas_object_show(Self.obj);
  end;

  procedure evas_object_smart_callback_add(obj: Pointer; const event: PChar; cb: Elm_Button_Cb; data: Pointer); cdecl; external;
  procedure TEvasObject.SetCallback(const event: PChar; cb: Elm_Button_Cb; data: Pointer); cdecl;
  begin
    evas_object_smart_callback_add(Self.obj, event, cb, data);
  end;

  procedure evas_object_resize(obj: Pointer; w, h: cint); cdecl; external;
  procedure TEvasObject.Resize(const W, H: Integer); cdecl;
  begin
    evas_object_resize(Self.obj, W, H);
  end;

  procedure evas_object_move(obj: Pointer; x, y: cint); cdecl; external;
  procedure TEvasObject.Move(const X, Y: Integer); cdecl;
  begin
    evas_object_move(Self.obj, X, Y);
  end;

  procedure elm_object_part_text_set(obj: Pointer; const PART, TEXT: PChar); cdecl; external;
  procedure TEvasObject.SetText(const TEXT: PChar); cdecl;
  begin
    elm_object_part_text_set(Self.obj, 'default', TEXT);
  end;

  function elm_object_part_text_get(obj: Pointer; part: PChar): PChar; cdecl; external;
  function TEvasObject.Text: PChar; cdecl;
  begin
    Result := elm_object_part_text_get(Self.obj, nil);
  end;
end.
