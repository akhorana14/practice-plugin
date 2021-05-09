{

FL Studio generator/effect plugins SDK
Delphi object extension
Handy functions for easier implementation of a FL plugin under Delphi

Notes:
- define UseCriticalSection if you need it

!!! warning: DO NOT ever set the ShowHint property to True for *any* windowed component in an FL plugin !!!

}

{{$DEFINE UseCriticalSection}

unit FP_DelphiPlug;

interface

uses
  Windows, Forms, Controls, SysUtils, ActiveX, Classes, Menus, FP_PlugClass;

type
  TDelphiFruityPlug = class(TFruityPlug)
    PlugHost: TFruityPlugHost;
    EditorForm: TForm;
    SmpRate: Integer;
    MaxPoly: Integer;
    PitchMul: Single;
    LastOnHint: TNotifyEvent;
    ProgPath: string;

    {$IFDEF UseCriticalSection}
    ThreadSafeCS: TRTLCriticalSection;
    {$ENDIF}

    procedure DestroyObject; override;
    function Dispatcher(ID, Index, Value: IntPtr): IntPtr; override;
    procedure Idle_Public; override; // obsolete, the plugin now does its own idling
    function ProcessEvent(EventID, EventValue, Flags: Integer): Integer; override;

    // internal
    function GetStep_Cents(Pitch: Integer): Integer;
    function GetStep_Cents_S(Pitch: Single): Integer;
    function GetStep_Freq(Freq: Integer): Integer;
    procedure ProcessAllParams; inline;
    procedure SkipRendering(SourceBuffer, DestBuffer: Pointer; Length: Integer);
    constructor Create(SetHostTag: Integer; SetPlugHost: TFruityPlugHost);

    // replaces PlugHost.LockMix_Shared
    procedure LockMix_Shared; inline;
    procedure UnlockMix_Shared; inline;
    procedure LockPlugin; inline;
    procedure UnlockPlugin; inline;

    // hints
    procedure OnHint(Sender: TObject);

    procedure ShowHintMsg(const Msg: string); virtual;
    procedure ShowHintMsg_Direct(const Msg: string);
    procedure ShowHintMsg_Percent(Value, Max: Integer);
    procedure ShowHintMsg_Pitch(Value, PitchType: Integer); overload;
    procedure ShowHintMsg_Pitch(Value: Single; PitchType: Integer; Digits: Integer = 2); overload;
    procedure ShowHintMsg_Pan(Value: Integer);
    procedure ShowHintMsg_Time(Value: Integer);
    procedure ShowHintMsg_Gauge(const Msg: string; Value, Max: Integer);

    // param popup
    procedure AdjustParamPopup(PopupItem: TMenuItem; ParamNum, FirstItemIndex: Integer; SetOnClick: TNotifyEvent);
    // in/out
    procedure AdjustInOutPopup(PopupItem: TMenuItem; Flags, CurrentIndex, FirstItemIndex: Integer; SetOnClick: TNotifyEvent);

    {$IFDEF UseCriticalSection}
    // thread synchronisation / safety
    function TryLock: Boolean; inline;
    procedure Lock; inline;
    procedure Unlock; inline;
    {$ENDIF}

    procedure Idle; virtual;
  end;

var
  PluginPath: string; // path to this DLL plugin (for loading of resources)

const
  AbsPPN = 192 div 4; // 192 PPQ

  // see ShowHintMsg_Pitch
  PitchType_Octaves = -1;
  PitchType_Semitones = 0;
  PitchType_Cents = 1;
  PitchType_Hz = 2;

procedure GetPluginPath;
procedure Stream_ReadString(const Stream: IStream; var s: string);
procedure Stream_ReadUString(const Stream: IStream; var s: string);
procedure Stream_StoreString(const Stream: IStream; const s: string);
procedure Stream_StoreUString(const Stream: IStream; const s: string);
procedure TStream_ReadUString(const Stream: TStream; var s: string);
procedure TStream_StoreUString(const Stream: TStream; const s: string);
function TranslateMIDI(Value, Min, Max: Integer): Integer;
function CleanHintStr(const s: string): string;

implementation

uses
  FP_Extra, FP_Def;

{ TDelphiFruityPlug }

// create the object
constructor TDelphiFruityPlug.Create;
begin
  inherited Create;

  HostTag := SetHostTag;
  PlugHost := SetPlugHost;

  with Application do
  begin
    Handle := PlugHost.AppHandle;
    LastOnHint := OnHint;
    OnHint := Self.OnHint;
  end;

  {$IFDEF UseCriticalSection}
  InitializeCriticalSection(ThreadSafeCS);
  {$ENDIF}

  SmpRate := MixSmpRate_Default;
  ProgPath := UTF8ToString(PAnsiChar(PlugHost.Dispatcher(HostTag, FHD_GetPath, GP_ProgPath, 0)));
end;

procedure TDelphiFruityPlug.DestroyObject;
begin
  {$IFDEF UseCriticalSection}
  DeleteCriticalSection(ThreadSafeCS);
  {$ENDIF}
  Application.OnHint := LastOnHint;
  if EditorForm <> nil then
  begin
    EditorForm.Free;
    EditorForm := nil;
  end;
  inherited;
end;

function TDelphiFruityPlug.Dispatcher(ID, Index, Value: IntPtr): IntPtr;
begin
  Result := 0;

  case ID of
    FPD_UseIncreasedMIDIResolution: Result := 1;
  end;
end;

// events
function TDelphiFruityPlug.ProcessEvent(EventID, EventValue, Flags: Integer): Integer;
begin
  Result := 0;
  case EventID of
    FPE_MaxPoly: MaxPoly := EventValue;
  end;
end;

procedure TDelphiFruityPlug.ProcessAllParams;
var
  n: Integer;
begin
  for n := 0 to Info^.NumParams - 1 do
    ProcessParam(n, ProcessParam(n, 0, REC_GetValue), REC_UpdateValue or REC_UpdateControl);
end;

// to safely skip a rendering (for plugins that don't render)
procedure TDelphiFruityPlug.SkipRendering(SourceBuffer, DestBuffer: Pointer; Length: Integer);
begin
  if SourceBuffer <> DestBuffer then MoveMemory(DestBuffer, SourceBuffer, Length shl 3);
end;

// get the speed step to go through the LFO table at a given pitch (cents) (tuned around C5)
function TDelphiFruityPlug.GetStep_Cents(Pitch: Integer): Integer;
begin
  Result := Round(PitchMul * Exp(Pitch * NoteMul));
end;

// get the speed step to go through the LFO table at a given pitch (cents) (tuned around C5)
function TDelphiFruityPlug.GetStep_Cents_S(Pitch: Single): Integer;
begin
  Result := Round(PitchMul * Exp(Pitch * NoteMul));
end;

{$IFDEF CPUX64}
function GetStep_Freq_Asm1(n, freq: integer): integer;
asm
    XOR   EAX,EAX
    DIV   ECX
end;
{$ENDIF}

function TDelphiFruityPlug.GetStep_Freq(Freq: Integer): Integer;
var
  n: Integer;
begin
  n := SmpRate;
  {$IFDEF CPUX64}
  n := GetStep_Freq_Asm1(n, freq);
  {$ELSE}
  asm
    MOV   ECX,n
    XOR   EAX,EAX
    MOV   EDX,Freq
    DIV   ECX
    MOV   n,EAX
  end;
  {$ENDIF}
  Result := n;
end;

{$IFDEF UseCriticalSection}
// thread synchronization for safety
function TDelphiFruityPlug.TryLock: Boolean;
begin
  Result := TryEnterCriticalSection(ThreadSafeCS);
end;

procedure TDelphiFruityPlug.Lock;
begin
  EnterCriticalSection(ThreadSafeCS);
end;

procedure TDelphiFruityPlug.Unlock;
begin
  LeaveCriticalSection(ThreadSafeCS);
end;
{$ENDIF}

// hints
procedure TDelphiFruityPlug.ShowHintMsg(const Msg: string);
begin
  PlugHost.OnHint(HostTag, PAnsiChar(UTF8Encode(Msg)));
end;

procedure TDelphiFruityPlug.ShowHintMsg_Direct(const Msg: string);
begin
  PlugHost.Dispatcher(HostTag, FHD_OnHint_Direct, 0, IntPtr(PAnsiChar(UTF8Encode(Msg))));
end;

procedure TDelphiFruityPlug.ShowHintMsg_Percent(Value, Max: Integer);
begin
  ShowHintMsg(IntToStr(Value * 100 div Max) + '%');
end;

const
  PitchTypeT: array[-1..2] of AnsiString = (' octave', ' semitone', ' cent', ' Hz');

procedure TDelphiFruityPlug.ShowHintMsg_Pitch(Value, PitchType: Integer);
var
  Msg: string;
begin
  Msg := IntToStr(Value) + PitchTypeT[PitchType];
  if Value >= 0 then Msg := '+' + Msg;
  if (Abs(Value) > 1) and (PitchType < 2) then Msg := Msg + 's';
  ShowHintMsg(Msg);
end;

procedure TDelphiFruityPlug.ShowHintMsg_Pitch(Value: Single; PitchType: Integer; Digits: Integer = 2);
var
  Msg: string;
begin
  Msg := FloatToStrF(Value, ffFixed, 15, Digits) + PitchTypeT[PitchType];
  if Value >= 0 then Msg := '+' + Msg;
  if (Abs(Value) > 1) and (PitchType < 2) then Msg := Msg + 's';
  ShowHintMsg(Msg);
end;

procedure TDelphiFruityPlug.ShowHintMsg_Pan(Value: Integer);
var
  Msg: string;
  n: Integer;
begin
  n := Round(Value * (100 / 64));
  if n = 0 then
    Msg := 'Centered'
  else
  begin
    Msg := IntToStr(Abs(n)) + '% ';
    if n < 0 then
      Msg := Msg + 'left'
    else
      Msg := Msg + 'right';
  end;
  ShowHintMsg(Msg);
end;

// show the time in 0:00 format at 192 PPQ
procedure TDelphiFruityPlug.ShowHintMsg_Time(Value: Integer);
var
  Msg: string;
begin
  Msg := IntToStr(Value div AbsPPN) + ':' + Zeros(Value mod AbsPPN, 2);
  ShowHintMsg(Msg);
end;

// show a progression gauge
const
  HintPBMax = 20; // 20 steps for the hint progress bar
  HintPBFirst = Ord('a');
  HintPBLast = HintPBFirst + HintPBMax;

procedure TDelphiFruityPlug.ShowHintMsg_Gauge(const Msg: string; Value, Max: Integer);
var
  s: string;
begin
  s := '^.' + Char(HintPBFirst + MulDiv64(Value, HintPBMax, MaxOf(Max, 1))) + Msg;
  ShowHintMsg_Direct(s);
end;

// adjust a common param popup
// changed in SDK version 1
procedure TDelphiFruityPlug.AdjustParamPopup(PopupItem: TMenuItem; ParamNum, FirstItemIndex: Integer; SetOnClick: TNotifyEvent);
var
  n: Integer;
  MenuEntry: PParamMenuEntry;
  NewItem: TMenuItem;
begin
  with PopupItem do
  begin
    // we don't need the first separator anymore
    if (FirstItemIndex > 0) and Items[FirstItemIndex - 1].IsLine then Items[FirstItemIndex - 1].Visible := False;

    // delete the old entries
    while Count > FirstItemIndex do Delete(Count - 1);
    // add (append) new ones
    n := 0;
    repeat
      // get menu entry
      MenuEntry := PParamMenuEntry(PlugHost.Dispatcher(HostTag, FHD_GetParamMenuEntry, ParamNum, n));
      if Assigned(MenuEntry) then
        with MenuEntry^ do
        begin
          // create, fill & add item
          NewItem := TMenuItem.Create(PopupItem);
          NewItem.Caption := Name;
          NewItem.Tag := n;
          NewItem.Checked := Flags and FHP_Checked <> 0;
          NewItem.Enabled := Flags and FHP_Disabled = 0;
          NewItem.OnClick := SetOnClick;
          Add(NewItem);
          inc(n);
        end;
    until not Assigned(MenuEntry);
  end;
end;

// adjust an input/output popup
// flags=0 for input, 1 for output
// CurrentIndex is existing in/out index, or zero if none
procedure TDelphiFruityPlug.AdjustInOutPopup(PopupItem: TMenuItem; Flags, CurrentIndex, FirstItemIndex: Integer; SetOnClick: TNotifyEvent);
var
  n, m, nMax: Integer;
  NameColor: TNameColor;
  NewItem: TMenuItem;
begin
  with PopupItem do
  begin
    // delete the old entries
    while Count > FirstItemIndex do Delete(Count - 1);
    // add (append) new ones
    nMax := PlugHost.Dispatcher(HostTag, FHD_GetNumInOut, Flags, 0);
    for n := -nMax to nMax do
    begin
      // get in/out info
      if n = 0 then
      begin
        NameColor.Name := '(none)';
        NameColor.VisName := NameColor.Name;
        m := 1;
      end
      else
        m := PlugHost.Dispatcher(HostTag, FHD_GetInName + Flags, n, IntPtr(@NameColor));
      if m > 0 then
      begin
        // create, fill & add item
        NewItem := TMenuItem.Create(PopupItem);
        NewItem.Caption := NameColor.VisName;
        NewItem.Tag := n;
        NewItem.Default := n = CurrentIndex;
        NewItem.OnClick := SetOnClick;
        Add(NewItem);
      end;
    end;
  end;
end;

procedure TDelphiFruityPlug.Idle_Public;
begin
end;

procedure TDelphiFruityPlug.Idle;
begin
end;

procedure TDelphiFruityPlug.OnHint(Sender: TObject);
var
  s: string;
begin
  s := GetLongHint(Application.Hint);
  PlugHost.OnHint(HostTag, PAnsiChar(UTF8Encode(s)));
end;

procedure TDelphiFruityPlug.LockMix_Shared;
var
  OBuffer: TIOBuffer;
begin
  OBuffer.Flags := IO_Lock;
  PlugHost.GetOutBuffer(HostTag, 0, @OBuffer);
end;

procedure TDelphiFruityPlug.LockPlugin;
begin
  PlugHost.LockPlugin(HostTag);
end;

procedure TDelphiFruityPlug.UnlockMix_Shared;
var
  OBuffer: TIOBuffer;
begin
  OBuffer.Flags := IO_Unlock;
  PlugHost.GetOutBuffer(HostTag, 0, @OBuffer);
end;

procedure TDelphiFruityPlug.UnlockPlugin;
begin
  PlugHost.UnlockPlugin(HostTag);
end;

// getting the current plugin's path
procedure GetPluginPath;
var
  p: array[0..Max_Path] of char;
begin
  GetModuleFileName(HInstance, @p, Max_Path);
  PluginPath := ExtractFilePath(p);
end;

// storing strings in the plugin's state
procedure Stream_ReadString(const Stream: IStream; var s: string);
var
  l: DWORD;
  b: Byte;
  s_ansi: AnsiString;
begin
  Stream.Read(@b, 1, nil);
  if b = 255 then
    Stream.Read(@l, 4, nil)
  else
    l := b;
  SetLength(s_ansi, l);
  if s_ansi <> '' then
  begin
    Stream.Read(Pointer(s_ansi), l, nil);
    s_ansi[l + 1] := #0; // ensure null-terminated (yes s[l+1] is valid)
    // because of an old bug, check the integrity of the string
    while (Length(s_ansi) > 0) and (s_ansi[Length(s_ansi)] < #32) do
      SetLength(s_ansi, Length(s_ansi) - 1);
  end;
  s := UTF8ToString(s_ansi);
end;

procedure Stream_ReadUString(const Stream: IStream; var s: string);
var
  l: DWORD;
  b: Byte;
begin
  Stream.Read(@b, 1, nil);
  if b = 255 then
    Stream.Read(@l, 4, nil)
  else
    l := b;
  SetLength(s, l);
  if s <> '' then
  begin
    Stream.Read(Pointer(s), l * SizeOf(Char), nil);
    s[l + 1] := #0; // ensure null-terminated (yes s[l+1] is valid)
  end;
end;

procedure Stream_StoreString(const Stream: IStream; const s: string);
var
  l: DWORD;
  b: Byte;
  s_ansi: AnsiString;
begin
  s_ansi := UTF8Encode(s);
  l := Length(s_ansi);
  if l >= 255 then
  begin
    b := 255;
    Stream.Write(@b, 1, nil); // tells that a 32Bit length follows
    Stream.Write(@l, 4, nil); // 32Bit length
  end
  else
    Stream.Write(@l, 1, nil); // 8Bit length
  Stream.Write(Pointer(s_ansi), l, nil);
end;

procedure Stream_StoreUString(const Stream: IStream; const s: string);
var
  l: DWORD;
  b: Byte;
begin
  l := Length(s);
  if l >= 255 then
  begin
    b := 255;
    Stream.Write(@b, 1, nil); // tells that a 32Bit length follows
    Stream.Write(@l, 4, nil); // 32Bit length
  end
  else
    Stream.Write(@l, 1, nil); // 8Bit length
  Stream.Write(Pointer(s), l * SizeOf(Char), nil);
end;

procedure TStream_ReadUString(const Stream: TStream; var s: string);
var
  l: DWORD;
  b: Byte;
begin
  Stream.Read(b, 1);
  if b = 255 then
    Stream.Read(l, 4)
  else
    l := b;
  SetLength(s, l);
  if s <> '' then
  begin
    Stream.Read(s[1], l * SizeOf(Char));
    s[l + 1] := #0; // ensure null-terminated (yes s[l+1] is valid)
  end;
end;

procedure TStream_StoreUString(const Stream: TStream; const s: string);
var
  l: DWORD;
  b: Byte;
begin
  l := Length(s);
  if l >= 255 then
  begin
    b := 255;
    Stream.Write(b, 1); // tells that a 32Bit length follows
    Stream.Write(l, 4); // 32Bit length
  end
  else
    Stream.Write(l, 1); // 8Bit length
  Stream.Write(s[1], l * SizeOf(Char));
end;

// translate a controller value from 0..FromMIDI_Max to Min..Max
function TranslateMIDI(Value, Min, Max: Integer): Integer;
begin
  Result := Min + Round(Value * FromMIDI_Div * (Max - Min));
end;

// clean the control chars in a hint string
function CleanHintStr(const s: string): string;
var
  c, p: Integer;
const
  SCChar = '^'; // special command (followed by one of the chars listed below)
  KSChar = '^'; // keyboard shortcut
  BMChar = '_'; // big hint bar message (will not appear on the normal hint bar)
  LMChar = '`'; // long hint bar message (will not appear on the normal hint bar)
begin
  Result := s;
  while (Length(Result) >= 2) and (Result[1] = SCChar) do
  begin
    c := Ord(Result[2]);
    case c of
      // keyboard shortcut
      Ord(KSChar), Ord(BMChar), Ord(LMChar):
        begin
          Delete(Result, 1, 2);
          p := Pos(SCChar, Result);
          if p > 0 then Delete(Result, 1, p);
        end;
      // icons
    else
      Delete(Result, 1, 2);
    end;
  end;
end;

initialization
  IsMultiThread := True;
  SetMinimumBlockAlignment(mba16Byte); // needed for some SSE stuff
  GetPluginPath;
  FormatSettings.DecimalSeparator := '.';

finalization
  Application.Handle := 0; // does something weird (minimizes the host app) without this

end.

