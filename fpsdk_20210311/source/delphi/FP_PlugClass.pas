{

FL Studio generator/effect plugins SDK
plugin & host classes

(99-13) gol

}

unit FP_PlugClass;

interface

uses
  Winapi.Windows, Winapi.ActiveX, FP_Def, GenericTransport;

{$A4}

type // plugin info, common to all instances of the same plugin
  TFruityPlugInfo = record
    SDKVersion: Integer; // =CurrentSDKVersion
    LongName, ShortName: PAnsiChar; // full plugin name (should be the same as DLL name) & short version (for labels)
    Flags: Integer; // see FPF_Generator
    NumParams: Integer; // (maximum) number of parameters, can be overridden using FHD_SetNumParams
    DefPoly: Integer; // preferred (default) max polyphony (Fruity manages polyphony) (0=infinite)
    NumOutCtrls: Integer; // number of internal output controllers
    NumOutVoices: Integer; // number of internal output voices

    Reserved: array[2..31] of Integer; // set to zero
  end;
  PFruityPlugInfo = ^TFruityPlugInfo;

  // voice handle (can be an index or a memory pointer (must be unique, that is *not* just the semitone #))
  TVoiceHandle = IntPtr;
  TOutVoiceHandle = TVoiceHandle;
  TPluginTag = IntPtr;

  // sample handle
  TSampleHandle = IntPtr;

  // sample region
  TSampleRegion = record
    SampleStart, SampleEnd: Integer;
    Name, Info: array[0..255] of AnsiChar;
    Time: Single; // beat position, mainly for loop dumping (-1 if not supported)
    KeyNum: Integer; // linked MIDI note number (-1 if not supported)
    Reserved: array[0..3] of Integer;
  end;
  PSampleRegion = ^TSampleRegion;

  // sample info, FILL CORRECTLY
  TSampleInfo = packed record
    Size: Integer; // size of this structure, MUST BE SET BY THE PLUGIN
    Data: Pointer; // pointer to the samples
    Length: Integer; // length in samples
    SolidLength: Integer; // length without ending silence
    LoopStart, LoopEnd: Integer; // loop points (LoopStart=-1 if no loop points)
    SmpRateConv: Double; // host sample rate*SmpRateConv = sample rate
    NumRegions: Integer; // number of regions in the sample (see GetSampleRegion)
    NumBeats: Single; // length in beats
    Tempo: Single;
    NumChans: Integer; // 1=mono, 2=stereo, MUST BE SET BY THE PLUGIN, to -1 if all formats are accepted
    Format: Integer; // 0=16I, 1=32F, MUST BE SET BY THE PLUGIN, to -1 if all formats are accepted
    Reserved: array[0..12] of Integer; // future use
  end;
  PSampleInfo = ^TSampleInfo;

  // see FPV_GetInfo
  TVoiceInfo = record
    Length: Integer;
    Color: Integer;
    Velocity: Single;
    Flags: Integer;
    FinePitch: Single;
    Reserved: array[0..6] of Integer;
  end;
  PVoiceInfo = ^TVoiceInfo;

  // see FHD_GetMixingTime
  TFPTime = packed record
    t, t2: Double;
  end;
  PFPTime = ^TFPTime;

  // see FHD_GetInName
  TNameColor = packed record
    Name, VisName: array[0..255] of AnsiChar; // user-defined name (can be empty), visible name (can be guessed)
    Color: Integer;
    Index: Integer; // real index of the item (can be used to translate plugin's own in/out into real mixer track #)
  end;
  PNameColor = ^TNameColor;

  // see GetInBuffer/GetOutBuffer
  TIOBuffer = packed record
    Buffer: Pointer;
    //Filled:LongBool;  // only valid for GetInBuffer, indicates if buffer is not empty
    Flags: DWORD; // see IO_Filled
  end;
  PIOBuffer = ^TIOBuffer;

  // level params, used both for final voice levels (voice levels+parent channel levels) & original voice levels
  // note: all params can go outside their defined range

  // OLD, OBSOLETE VERSION, DO NOT USE!!!
  TLevelParams_Old = record
    Pan: Integer; // panning (-64..64)
    Vol: Single; // volume/velocity (0..1)
    Pitch: Integer; // pitch (in cents) (semitone=Pitch/100)
    FCut, FRes: Single; // filter cutoff & Q (0..1)
  end;
  PLevelParams_Old = ^TLevelParams_Old;
  TVoiceParams_Old = record
    InitLevels, FinalLevels: TLevelParams_Old;
  end;

  // NEW VERSION (all floats), USE THESE
  TLevelParams = record
    Pan: Single; // panning (-1..1)
    Vol: Single; // volume/velocity (0.0 = -inf dB .. 1.0 = 0 dB) - note: can go above 1.0!
    Pitch: Single; // pitch (in cents) (semitone=Pitch/100)
    FCut, FRes: Single; // filter cutoff & Q (0..1)
  end;
  PLevelParams = ^TLevelParams;
  TVoiceParams = record
    InitLevels, FinalLevels: TLevelParams;
  end;
  PVoiceParams = ^TVoiceParams;

  // to add notes to the piano roll (current pattern)
  TNoteParams = packed record
    Position, Length: Integer; // in PPQ
    // levels
    Pan: Integer; // default=0
    Vol: Single; // default=100/128
    Note: SmallInt; // default=60
    Color: SmallInt; // 0..15 (=MIDI channel)
    Pitch: Integer; // default=0
    FCut, FRes: Single; // default=0
  end;
  TNotesParams = record
    Target: Integer; // 0=step seq (not supported yet), 1=piano roll
    Flags: Integer; // see NPF_EmptyFirst
    PatNum: Integer; // -1 for current
    ChanNum: Integer; // -1 for plugin's channel, or selected channel if plugin is an effect
    Count: Integer; // the # of notes in the structure
    NoteParams: array[0..0] of TNoteParams; // array of notes
  end;
  PNotesParams = ^TNotesParams;

  // param menu entry
  TParamMenuEntry = record
    Name: PAnsiChar; // name of the menu entry (or menu separator if '-')
    Flags: Integer; // checked or disabled, see FHP_Disabled
  end;
  PParamMenuEntry = ^TParamMenuEntry;

  PRenderWindowBitmapInfo = ^TRenderWindowBitmapInfo;
  TRenderWindowBitmapInfo = record
    DestP: pointer;
    MaxWidth, MaxHeight: integer;
    Id: integer; //0=mixer, 1=playlist
    ReturnedWidth, ReturnedHeight: integer;
    NamedPlugin: array[0..127] of char;
  end;

  // options to send along with FHD_RenderProject when RPF_IsVideoExport is set
  TFinalAudioCallback = procedure(Data : pointer; Length : integer); stdcall;
  PRenderProjectVideoInfo = ^TRenderProjectVideoInfo;
  TRenderProjectVideoInfo = record
    VideoFileName: PAnsiChar;
    VideoDescription: PAnsiChar;
    FinalAudioCallback : TFinalAudioCallback; //To allow ZGE to capture post master audio
  end;

  // see FHD_SetContextInfo and CI_ constants ... allows changes that require additional info
  PContextInfo = ^TContextInfo;
  TContextInfo = record
    Param: IntPtr;
    IntValue: IntPtr;
    FloatValue: single;
  end;

  PConvertStringToValueData = ^TConvertStringToValueData;
  TConvertStringToValueData = record
    ParamIndex: integer;
    StringValue: PAnsiChar;
    ConvertedData: Double;
  end;

  // plugin class
  TFruityPlug = class

    // *** params ***

    HostTag: TPluginTag; // free for the host to use (parent object reference, ...), passed as 'Sender' to the host
    Info: PFruityPlugInfo;
    EditorHandle: HWnd; // handle to the editor window panel (created by the plugin)

    MonoRender: LongBool; // last rendered voice rendered mono data (not used yet)

    Reserved: array[1..32] of Integer; // for future use, set to zero

    // *** functions ***
    // (G) = called from GUI thread, (M) = called from mixer thread, (GM) = both, (S) = called from MIDI synchronization thread
    // (M) calls are done inside the plugin lock (LockPlugin / UnlockPlugin)
    // + TriggerVoice and Voice_ functions are also called inside the plugin lock
    // + assume that any other call is not locked! (so call LockPlugin / UnlockPlugin where necessary, but no more than that)
    // + don't call back to the host while inside a LockPlugin / UnlockPlugin block

    // messages (to the plugin)
    procedure DestroyObject; virtual; stdcall; // (G)
    function Dispatcher(ID, Index, Value: IntPtr): IntPtr; virtual; stdcall; abstract; // (GM)
    procedure Idle_Public; virtual; stdcall; abstract; // (G) (used to be Idle)
    procedure SaveRestoreState(const Stream: IStream; Save: LongBool); virtual; stdcall; abstract; // (G)

    // names (see FPN_Param) (Name has room for 256 chars)
    procedure GetName(Section, Index, Value: Integer; Name: PAnsiChar); virtual; stdcall; abstract; // (GM)

    // events
    function ProcessEvent(EventID, EventValue, Flags: Integer): Integer; virtual; stdcall; abstract; // (GM)
    function ProcessParam(Index, Value, RECFlags: Integer): Integer; virtual; stdcall; abstract; // (GM)

    // effect processing (source & dest can be the same)
    procedure Eff_Render(SourceBuffer, DestBuffer: PWAV32FS; Length: Integer); virtual; stdcall; abstract; // (M)
    // generator processing (can render less than length)
    procedure Gen_Render(DestBuffer: PWAV32FS; var Length: Integer); virtual; stdcall; abstract; // (M)

    // voice handling
    function TriggerVoice(VoiceParams: PVoiceParams; SetTag: IntPtr): TVoiceHandle; virtual; stdcall; abstract; // (GM)
    procedure Voice_Release(Handle: TVoiceHandle); virtual; stdcall; abstract; // (GM)
    procedure Voice_Kill(Handle: TVoiceHandle); virtual; stdcall; abstract; // (GM)
    function Voice_ProcessEvent(Handle: TVoiceHandle; EventID, EventValue, Flags: IntPtr): Integer; virtual; stdcall; abstract; // (GM)
    function Voice_Render(Handle: TVoiceHandle; DestBuffer: PWAV32FS; var Length: Integer): Integer; virtual; stdcall; abstract; // FPF_UseSampler only (GM)

    // (see FPF_WantNewTick) called before a new tick is mixed (not played)
    // internal controller plugins should call OnControllerChanged from here
    procedure NewTick; virtual; stdcall; abstract; // (M)

    // (see FHD_WantMIDITick) called when a tick is being played (not mixed) (not used yet)
    procedure MIDITick; virtual; stdcall; abstract; // (S)

    // MIDI input message (see FHD_WantMIDIInput & TMIDIOutMsg) (set Msg to MIDIMsg_Null if it has to be killed)
    procedure MIDIIn(var Msg: Integer); virtual; stdcall; abstract; // (GM)

    // buffered messages to itself (see PlugMsg_Delayed)
    procedure MsgIn(Msg: IntPtr); virtual; stdcall; abstract; // (S)

    // voice handling
    function OutputVoice_ProcessEvent(Handle: TOutVoiceHandle; EventID, EventValue, Flags: IntPtr): Integer; virtual; stdcall; abstract; // (GM)
    procedure OutputVoice_Kill(Handle: TOutVoiceHandle); virtual; stdcall; abstract; // (GM)
  end;

  // plugin host class
  TFruityPlugHost = class

    // *** params ***

    HostVersion: Integer; // current FruityLoops version stored as 01002003 (integer) for 1.2.3
    Flags: Integer; // reserved

    // windows
    AppHandle: THandle; // application handle, for slaving windows

    // handy wavetables (32Bit float (-1..1), 16384 samples each)
    // 6 are currently defined (sine, triangle, square, saw, analog saw, noise)
    // those pointers are fixed
    // (obsolete, avoid)
    WaveTables: array[0..9] of PWaveT;

    // handy free buffers, guaranteed to be at least the size of the buffer to be rendered (float stereo)
    // those pointers are variable, please read & use while rendering only
    // those buffers are contiguous, so you can see TempBuffer[0] as a huge buffer
    TempBuffers: array[0..3] of PWAV32FS;

    // reserved for future use
    Reserved: array[1..30] of Integer; // set to zero

    // *** functions ***

    // messages (to the host) (Sender=plugin tag)
    function Dispatcher(Sender: TPluginTag; ID, Index, Value: IntPtr): IntPtr; virtual; stdcall; abstract;
    // for the host to store changes
    procedure OnParamChanged(Sender: TPluginTag; Index, Value: Integer); virtual; stdcall; abstract;
    // for the host to display hints (call from GUI thread!)
    procedure OnHint(Sender: TPluginTag; Text: PAnsiChar); virtual; stdcall; abstract;

    // compute left & right levels using pan & volume info (OLD, OBSOLETE VERSION, USE ComputeLRVol INSTEAD)
    procedure ComputeLRVol_Old(var LVol, RVol: Single; Pan: Integer; Volume: Single); virtual; stdcall; abstract;

    // voice handling (Sender=voice tag)
    procedure Voice_Release(Sender: IntPtr); virtual; stdcall; abstract;
    procedure Voice_Kill(Sender: IntPtr; KillHandle: LongBool); virtual; stdcall; abstract;
    function Voice_ProcessEvent(Sender: IntPtr; EventID, EventValue, Flags: IntPtr): Integer; virtual; stdcall; abstract;

    // thread synchronisation / safety
    procedure LockMix_Old; virtual; stdcall; abstract; // will prevent any new voice creation & rendering
    procedure UnlockMix_Old; virtual; stdcall; abstract;

    // delayed MIDI out message (see TMIDIOutMsg) (will be sent once the MIDI tick has reached the current mixer tick
    procedure MIDIOut_Delayed(Sender: TPluginTag; Msg: IntPtr); virtual; stdcall; abstract;
    // direct MIDI out message
    procedure MIDIOut(Sender: TPluginTag; Msg: IntPtr); virtual; stdcall; abstract;

    // handy macro functions

    // adds a mono float buffer to a stereo float buffer, with left/right levels & ramping if needed
    // how it works: define 2 float params for each voice: LastLVol & LastRVol. Make them match LVol & RVol before the *first* rendering of that voice (unless ramping will occur from 0 to LVol at the beginning).
    // then, don't touch them anymore, just pass them to the function.
    // the level will ramp from the last ones (LastLVol) to the new ones (LVol) & will adjust LastLVol accordingly
    // LVol & RVol are the result of the ComputeLRVol function
    // for a quick & safe fade out, you can set LVol & RVol to zero, & kill the voice when both LastLVol & LastRVol will reach zero
    procedure AddWave_32FM_32FS_Ramp(SourceBuffer, DestBuffer: Pointer; Length: Integer; LVol, RVol: Single; var LastLVol, LastRVol: Single); virtual; stdcall; abstract;
    // same, but takes a stereo source
    // note that left & right channels are not mixed (not a true panning), but might be later
    procedure AddWave_32FS_32FS_Ramp(SourceBuffer, DestBuffer: Pointer; Length: Integer; LVol, RVol: Single; var LastLVol, LastRVol: Single); virtual; stdcall; abstract;

    // sample loading functions (FruityLoops 3.1.1 & over)
    // load a sample (creates one if necessary)
    // FileName must have room for 256 chars, since it gets written with the file that has been 'located'
    // only 16Bit 44Khz Stereo is supported right now, but fill the format correctly!
    // see FHLS_ShowDialog
    function LoadSample(var Handle: TSampleHandle; FileName: PAnsiChar; NeededFormat: PWaveFormatExtensible; Flags: Integer): Boolean; virtual; stdcall; abstract;
    function GetSampleData(Handle: TSampleHandle; var Length: Integer): Pointer; virtual; stdcall; abstract;
    procedure CloseSample(Handle: TSampleHandle); virtual; stdcall; abstract;

    // time info
    // obsolete, use FHD_GetMixingTime & FHD_GetPlaybackTime
    // get the current mixing time, in ticks (integer result)
    function GetSongMixingTime: Integer; virtual; stdcall; abstract;
    // get the current mixing time, in ticks (more accurate, with decimals)
    function GetSongMixingTime_A: Double; virtual; stdcall; abstract;
    // get the current playing time, in ticks (with decimals)
    function GetSongPlayingTime: Double; virtual; stdcall; abstract;

    // internal controller
    procedure OnControllerChanged(Sender: TPluginTag; Index, Value: IntPtr); virtual; stdcall; abstract;

    // get a pointer to one of the send buffers (see FPD_SetNumSends)
    // those pointers are variable, please read & use while processing only
    // the size of those buffers is the same as the size of the rendering buffer requested to be rendered
    function GetSendBuffer(Num: IntPtr): Pointer; virtual; stdcall; abstract;

    // ask for a message to be dispatched to itself when the current mixing tick will be played (to synchronize stuff) (see MsgIn)
    // the message is guaranteed to be dispatched, however it could be sent immediately if it couldn't be buffered (it's only buffered when playing)
    procedure PlugMsg_Delayed(Sender: TPluginTag; Msg: IntPtr); virtual; stdcall; abstract;
    // remove a buffered message, so that it will never be dispatched
    procedure PlugMsg_Kill(Sender: TPluginTag; Msg: IntPtr); virtual; stdcall; abstract;

    // get more details about a sample
    procedure GetSampleInfo(Handle: TSampleHandle; Info: PSampleInfo); virtual; stdcall; abstract;

    // distortion (same as TS404) on a piece of mono or stereo buffer
    // DistType in 0..1, DistThres in 1..10
    procedure DistWave_32FM(DistType, DistThres: Integer; SourceBuffer: Pointer; Length: Integer; DryVol, WetVol, Mul: Single); virtual; stdcall; abstract;

    // same as GetSendBuffer, but Num is an offset to the mixer track assigned to the generator (Num=0 will then return the current rendering buffer)
    // to be used by generators ONLY, & only while processing
    function GetMixBuffer(Num: Integer): Pointer; virtual; stdcall; abstract;

    // get a pointer to the insert (add-only) buffer following the buffer a generator is currently processing in
    // Ofs is the offset to the current buffer, +1 means next insert track, -1 means previous one, 0 is forbidden
    // only valid during Gen_Render
    // protect using LockMix_Shared
    function GetInsBuffer(Sender: TPluginTag; Ofs: Integer): Pointer; virtual; stdcall; abstract;

    // ask the host to prompt the user for a piece of text (s has room for 256 chars)
    // set x & y to -1 to have the popup screen-centered
    // if false is returned, ignore the results
    // set c to -1 if you don't want the user to select a color
    function PromptEdit(x, y: Integer; SetCaption, s: PAnsiChar; var c: Integer): LongBool; virtual; stdcall; abstract;

    // deprecated, use SuspendOutput and ResumeOutput instead
    procedure SuspendOutput_Old; virtual; stdcall; abstract;
    procedure ResumeOutput_Old; virtual; stdcall; abstract;

    // get the region of a sample
    procedure GetSampleRegion(Handle: TSampleHandle; RegionNum: Integer; Region: PSampleRegion); virtual; stdcall; abstract;

    // compute left & right levels using pan & volume info (USE THIS AFTER YOU DEFINED FPF_NewVoiceParams
    procedure ComputeLRVol(var LVol, RVol: Single; Pan, Volume: Single); virtual; stdcall; abstract;

    // use this instead of PlugHost.LockMix
    procedure LockPlugin(Sender: TPluginTag); virtual; stdcall; abstract;
    procedure UnlockPlugin(Sender: TPluginTag); virtual; stdcall; abstract;

    // multithread processing synchronisation / safety
    procedure LockMix_Shared_Old; virtual; stdcall; abstract;
    procedure UnlockMix_Shared_Old; virtual; stdcall; abstract;

    // multi-in/output (for generators & effects) (only valid during Gen/Eff_Render)
    // !!! Index starts at 1, to be compatible with GetInsBuffer (Index 0 would be Eff_Render's own buffer)
    procedure GetInBuffer(Sender: TPluginTag; Index: IntPtr; IBuffer: PIOBuffer); virtual; stdcall; abstract; // returns (read-only) input buffer Index (or Nil if not available).
    procedure GetOutBuffer(Sender: TPluginTag; Index: IntPtr; OBuffer: PIOBuffer); virtual; stdcall; abstract; // returns (add-only) output buffer Index (or Nil if not available). Use LockMix_Shared when adding to this buffer.

    // output voices (VFX "voice effects")
    function TriggerOutputVoice(VoiceParams: PVoiceParams; SetIndex, SetTag: IntPtr): TOutVoiceHandle; virtual; stdcall; abstract; // (GM)
    procedure OutputVoice_Release(Handle: TOutVoiceHandle); virtual; stdcall; abstract; // (GM)
    procedure OutputVoice_Kill(Handle: TOutVoiceHandle); virtual; stdcall; abstract; // (GM)
    function OutputVoice_ProcessEvent(Handle: TOutVoiceHandle; EventID, EventValue, Flags: IntPtr): Integer; virtual; stdcall; abstract; // (GM)

    // ask the host to prompt the user for a piece of text, color, icon ... See PEO_ constants for SetOptions. Text should be nil or a pointer to an allocated buffer with at least 255 characters!
    function PromptEdit_Ex(x, y: Integer; SetCaption, Text: PAnsiChar; var Color1, Color2, IconIndex: Integer; FontHeight, SetOptions: Integer): LongBool; virtual; stdcall; abstract;

    // SuspendOutput removes the plugin from all processing lists, so Eff/Gen_Render and voice functions will no longer be called.
    // To be used around lengthy operations (instead of straightforward locking)
    procedure SuspendOutput(Sender: TPluginTag); virtual; stdcall; abstract;
    procedure ResumeOutput(Sender: TPluginTag); virtual; stdcall; abstract;
  end;

const // history:
  // 0: original version
  // 1: new popup menu system
  CurrentSDKVersion = 1;

  // plugin flags
  FPF_Generator = 1; // plugin is a generator (not effect)
  FPF_RenderVoice = 1 shl 1; // generator will render voices separately (Voice_Render) (not used yet)
  FPF_UseSampler = 1 shl 2; // 'hybrid' generator that will stream voices into the host sampler (Voice_Render)
  FPF_GetChanCustomShape = 1 shl 3; // generator will use the extra shape sample loaded in its parent channel (see FPD_ChanSampleChanged)
  FPF_GetNoteInput = 1 shl 4; // plugin accepts note events (not used yet, but effects might also get note input later)
  FPF_WantNewTick = 1 shl 5; // plugin will be notified before each mixed tick (& be able to control params (like a built-in MIDI controller) (see NewTick))
  FPF_NoProcess = 1 shl 6; // plugin won't process buffers at all (FPF_WantNewTick, or special visual plugins (Fruity NoteBook))
  FPF_NoWindow = 1 shl 10; // plugin will show in the channel settings window & not in its own floating window
  FPF_Interfaceless = 1 shl 11; // plugin doesn't provide its own interface (not used yet)
  FPF_TimeWarp = 1 shl 13; // supports timewarps, that is, can be told to change the playing position in a voice (direct from disk music tracks, ...) (not used yet)
  FPF_MIDIOut = 1 shl 14; // plugin will send MIDI out messages (only those will be enabled when rendering to a MIDI file)
  FPF_DemoVersion = 1 shl 15; // plugin is a trial version, & the host won't save its automation
  FPF_CanSend = 1 shl 16; // plugin has access to the send tracks, so it can't be dropped into a send track or into the master
  FPF_MsgOut = 1 shl 17; // plugin will send delayed messages to itself (will require the internal sync clock to be enabled)
  FPF_HybridCanRelease = 1 shl 18; // plugin is a hybrid generator & can release its envelope by itself. If the host's volume envelope is disabled, then the sound will keep going when the voice is stopped, until the plugin has finished its own release
  FPF_GetChanSample = 1 shl 19; // generator will use the sample loaded in its parent channel (see FPD_ChanSampleChanged)
  FPF_WantFitTime = 1 shl 20; // fit to time selector will appear in channel settings window (see FPD_SetFitTime)
  FPF_NewVoiceParams = 1 shl 21; // MUST BE USED - tell the host to use TVoiceParams instead of TVoiceParams_Old
  FPF_IsDelphi = 1 shl 22; // tell if EditorHandle is a Delphi-made window, that can receive Delphi special messages (like CN_KeyDown for popup menus)
  FPF_CantSmartDisable = 1 shl 23; // plugin can't be smart disabled
  FPF_WantSettingsBtn = 1 shl 24; // plugin wants a settings button on the titlebar (mainly for the wrapper)
  FPF_CanStealKBFocus = 1 shl 25; // plugin can steal keyboard focus away from FL
  FPF_VFX = 1 shl 26; // is VFX plugin
  FPF_MacNeedsNSView = 1 shl 27; //On Mac: This plugin requires a NSView parent

  // useful combo's
  FPF_Type_Effect = 0; // for an effect (Eff_Render)
  FPF_Type_FullGen = FPF_Generator or FPF_GetNoteInput or FPF_NewVoiceParams; // for a full standalone generator (Gen_Render)
  FPF_Type_HybridGen = FPF_Type_FullGen or FPF_UseSampler or FPF_NewVoiceParams; // for an hybrid generator (Voice_Render)
  FPF_Type_Visual = FPF_NoProcess; // for a visual plugin that doesn't use the wave data

  // plugin dispatcher ID's
  // called from GUI thread unless specified
  FPD_ShowEditor = 0; // shows the editor (ParentHandle in Value)
  FPD_ProcessMode = 1; // sets processing mode flags (flags in value) (see PM_Normal) (can be ignored)
  FPD_Flush = 2; // breaks continuity (empty delay buffers, filter mem, etc.) (warning: can be called from the mixing thread) (GM)
  FPD_SetBlockSize = 3; // max processing length (samples) (in value)
  FPD_SetSampleRate = 4; // sample rate in Value
  FPD_WindowMinMax = 5; // allows the plugin to set the editor window resizable (min/max PRect in index, sizing snap PPoint in value)
  FPD_KillAVoice = 6; // (in case the mixer was eating way too much CPU) the plugin is asked to kill its weakest voice & return 1 if it did something (not used yet)
  FPD_UseVoiceLevels = 7; // return 0 if the plugin doesn't support the default per-voice level Index
  // return 1 if the plugin supports the default per-voice level Index (filter cutoff (0) or filter resonance (1))
  // return 2 if the plugin supports the per-voice level Index, but for another function (then check FPN_VoiceLevel)
  FPD_WrapPlugin = 8; // (private message to the plugin wrapper) ask to open the plugin given in Value (PWrappedPluginID). Return one of the PE_ constants as a result.
  FPD_SetPreset = 9; // set internal preset Index (mainly for wrapper)
  FPD_ChanSampleChanged = 10; // (see FPF_GetChanCustomShape) sample has been loaded into the parent channel, & given to the plugin
  // either as a wavetable (FPF_GetChanCustomshape) (pointer to shape in Value, same format as WaveTables)
  // or as a sample (FPF_GetChanSample) (TSampleHandle in Index)
  FPD_SetEnabled = 11; // the host has enabled/disabled the plugin (state in Value) (warning: can be called from the mixing thread) (GM)
  FPD_SetPlaying = 12; // the host is playing (song pos info is valid when playing) (state in Value) (warning: can be called from the mixing thread) (GM)
  FPD_SongPosChanged = 13; // song position has been relocated (by other means than by playing of course) (warning: can be called from the mixing thread) (GM)
  FPD_SetTimeSig = 14; // PTimeSigInfo in Value (GM)
  FPD_CollectFile = 15; // let the plugin tell which files need to be collected or put in zip files. File # in Index, starts from 0 until no more filenames are returned (PAnsiChar in Result).
  FPD_SetInternalParam = 16; // (private message to known plugins, ignore) tells the plugin to update a specific, non-automated param
  FPD_SetNumSends = 17; // tells the plugin how many send tracks there are (fixed to 4, but could be set by the user at any time in a future update) (number in Value) (!!! will be 0 if the plugin is in the master or a send track, since it can't access sends)
  FPD_LoadFile = 18; // when a file has been dropped onto the parent channel's button (LFT_ type in Index, filename in Value). Result should be 0 if not handled, 1 if handled and 2 if a dropped file should be rejected
                     //LFT_DownloadDataPack option is used to download Flex packs: Result is -1 if failed, or Pack index on success
  FPD_SetFitTime = 19; // set fit to time in beats (FLOAT time in value (need to typecast))
  FPD_SetSamplesPerTick = 20; // # of samples per tick (changes when tempo, PPQ or sample rate changes) (FLOAT in Value (need to typecast)) (warning: can be called from the mixing thread) (GM)
  FPD_SetIdleTime = 21; // set the freq at which Idle is called (can vary), ms time in Value
  FPD_SetFocus = 22; // the host has focused/unfocused the editor (focused in Value) (plugin can use this to steal keyboard focus ... also see FPD_StealKBFocus)
  FPD_Transport = 23; // special transport messages, from a controller. See GenericTransport.pas for Index. Must return 1 if handled.
  FPD_MIDIIn = 24; // live MIDI input preview, allows the plugin to steal messages (mostly for transport purposes). Must return 1 if handled. Packed message (only note on/off for now) in Value.
  FPD_RoutingChanged = 25; // mixer routing changed, must check FHD_GetNumInOut, FHD_GetInName and FHD_GetOutName if necessary. See RCV_ constants for the meaning of the Value parameter.
  FPD_GetParamInfo = 26; // retrieves info about a parameter. Param number in Index, see PI_Float for the result
  FPD_ProjLoaded = 27; // called after a project has been loaded, to leave a chance to kill automation (that could be loaded after the plugin is created) if necessary
  FPD_WrapperLoadState = 28; // (private message to the plugin wrapper) load a (VST1, DX) plugin state, pointer in Index, length in Value
  FPD_ShowSettings = 29; // called when the settings button on the titlebar is switched. On/off in Value (1=active). See FPF_WantSettingsBtn
  FPD_SetIOLatency = 30; // input/output latency (Index,Value) of the output, in samples (only for information)
  FPD_WallpaperChanged = 31; // sent on opening & whenever the host's background wallpaper has changed, window handle in Value, invalid if 0
  FPD_PreferredNumIO = 32; // (message from Patcher) retrieves the preferred number (0=default, -1=none) of audio inputs (Index=0), audio outputs (Index=1) or voice outputs (Index=2)
  FPD_GetGUIColor = 33; // retrieves the darkest background color of the GUI (Index=0 for background), for a nicer border around it
  FPD_CloseAllWindows = 34; // hide all windows opened by the plugin (except the plugin editor window)
  FPD_RenderWindowBitmap = 35; // used by ZgeViz
  FPD_StealKBFocus = 36; // switch stealing keyboard focus off or on (Value = 0 or 1)
  FPD_GetHelpContext = 37; // for plugins that want to show specific help pages, like Patcher. Return the context as a UTF-8 encoded PAnsiChar as the result. Return 0 or an empty string for the default context.
  FPD_RegChanged = 38; // notify plugin about registration change
  FPD_ArrangeWindows = 39; // arrange subwindows into the workspace (Value = workspace PRect)
  FPD_PluginLoaded = 40; // done opening the plugin - note that SaveRestoreState is called before this!
  FPD_ContextInfoChanged = 41; // Index holds the type of information (see CI_ constants), call FHD_GetContextInfo for the new value(s)
  FPD_ProjectInfoChanged = 42; // Index holds the value that changed (see GPI_ contants)
  FPD_GetDemoPlugins = 43; //returns ; delimited list (formatted as "productCode|name") of plugins in demo mode. If Value is 1, it should only list plugins that were saved as a demo.
  FPD_UnLockDemoPlugins = 44; //tells patcher to recheck demo mode and unlock purchased plugins
  FPD_ColorWasPicked = 46; // called after FHD_PickVoiceColor finishes. The new color value (an index, not RGB) is passed in Value.
  FPD_IsInDebugMode = 47; // return 0 for no, 1 for yes
  FPD_ColorsHaveChanged = 48; // some shared colors have changed. Index indicates the palette (see CP_ constants).
  FPD_GetStateSizeEstimate = 49; //get plugin estimated state size
  FPD_UseIncreasedMIDIResolution = 50; // return 1 if increased MIDI resolution is supported
  FPD_ConvertStringToValue = 51;  //let plugin do string to value conversion, value is pointer to TConvertStringToValueData record , used for custom type in value
  FPD_GetParamType = 52; //return control (Index) param type, see //FPD_GetParamType options below

  // GetName sections
  FPN_Param = 0; // retrieve name of param Index
  FPN_ParamValue = 1; // retrieve text label of param Index for value Value (used in event editor)
  FPN_Semitone = 2; // retrieve name of note Index (used in piano roll), for color (=MIDI channel) Value
  FPN_Patch = 3; // retrieve name of patch Index (not used yet)
  FPN_VoiceLevel = 4; // retrieve name of per-voice param Index (default is filter cutoff (0) & resonance (1)) (optional)
  FPN_VoiceLevelHint = 5; // longer description for per-voice param (works like FPN_VoiceLevels)
  FPN_Preset = 6; // for plugins that support internal presets (mainly for the wrapper plugin), retrieve the name for program Index
  FPN_OutCtrl = 7; // for plugins that output controllers, retrieve the name of output controller Index
  FPN_VoiceColor = 8; // retrieve name of per-voice color (MIDI channel) Index
  FPN_OutVoice = 9; // for plugins that output voices, retrieve the name of output voice Index

  // processing mode flags
  PM_Normal = 0; // realtime processing (default)
  PM_HQ_Realtime = 1; // high quality, but still realtime processing
  PM_HQ_NonRealtime = 2; // non realtime processing (CPU does not matter, quality does) (normally set when rendering only)
  PM_IsRendering = 16; // is rendering if this flag is set
  //PM_IPMask             =7 shl 8;  // 3 bits value for interpolation quality (0=none (obsolete), 1=linear, 2=6 point hermite (default), 3=32 points sinc, 4=64 points sinc, 5=128 points sinc, 6=256 points sinc)
  PM_IPMask = $FFFF shl 8; // 16 bits value for interpolation number of points

  // ProcessParam flags
  REC_UpdateValue = 1; // update the value
  REC_GetValue = 2; // retrieves the value
  REC_ShowHint = 4; // updates the hint (if any)
  REC_UpdateControl = 16; // updates the wheel/knob
  REC_FromMIDI = 32; // value from 0 to FromMIDI_Max has to be translated (& always returned, even if REC_GetValue isn't set)
  REC_NoLink = 1024; // don't check if wheels are linked (internal to plugins, useful for linked controls)
  REC_InternalCtrl = 2048; // sent by an internal controller - internal controllers should pay attention to those, to avoid nasty feedbacks
  REC_PlugReserved = 4096; // free to use by plugins

  // event ID's
  FPE_Tempo = 0; // FLOAT tempo in value (need to typecast), & average samples per tick in Flags (DWORD) (warning: can be called from the mixing thread) (GM)
  FPE_MaxPoly = 1; // max poly in value (infinite if <=0) (only interesting for standalone generators)
  // since MIDI plugins, or other plugin wrappers won't support the voice system, they should be notified about channel pan, vol & pitch changes
  FPE_MIDI_Pan = 2; // MIDI channel panning (0..127) in EventValue, FL panning in -64..+64 in Flags (warning: can be called from the mixing thread) (GM)
  FPE_MIDI_Vol = 3; // MIDI channel volume (0..127) in EventValue + volume as normalized float in Flags (need to typecast) (warning: can be called from the mixing thread) (GM)
  FPE_MIDI_Pitch = 4; // MIDI channel pitch in *cents* (to be translated according to current pitch bend range) in EventValue (warning: can be called from the mixing thread) (GM)

  // voice handles
  FVH_Null = -1;

  // TFruityPlug.Voice_ProcessEvent ID's
  FPV_Retrigger = 0; // monophonic mode can retrigger releasing voices (not used yet)

  // TFruityPlugHost.Voice_ProcessEvent ID's
  FPV_GetLength = 1; // retrieve length in ticks (not reliable) in Result (-1 if undefined)
  FPV_GetColor = 2; // retrieve color (0..15) in Result, can be mapped to MIDI channel
  FPV_GetVelocity = 3; // retrieve note on velocity (0..1) in Result (typecast as a float) (this is computed from InitLevels.Vol)
  FPV_GetRelVelocity = 4; // retrieve release velocity (0..1) in Result (typecast as a float) (to be called from Voice_Release) (use this if some release velocity mapping is involved)
  FPV_GetRelTime = 5; // retrieve release time multiplicator (0..2) in Result (typecast as a float) (to be called from Voice_Release) (use this for direct release multiplicator)
  FPV_SetLinkVelocity = 6; // set if velocity is linked to volume or not (in EventValue)
  FPV_GetInfo = 7; // retrieve info about the voice (some of which also available above) (PVoiceInfo in EventValue)

  // TVoiceInfo.Flags
  VoiceInfo_FromPattern = 1; // voice is received from score, not played live

  // Voice_Render function results
  FVR_Ok = 0;
  FVR_NoMoreData = 1; // for sample streaming, when there's no more sample data to fill any further buffer (the voice will then be killed by the host)

  // Tool indexes for FHD_OpenTool
  OTI_ControlCreator = 0; // open Control Creator, no params
  OTI_Diagnostics = 1; // open Diagnostics, no params

  // FPD_LoadFile types
  LFT_Generic = 0;
  LFT_Patcherize = 1;
  LFT_DroppedFile = 2;
  LFT_DownloadDataPack = 3;

  // FHD_GetMixingTime / FHD_GetLocalTime flags
  GT_Beats = 0; // beats
  GT_AbsoluteMS = 1; // absolute milliseconds
  GT_RunningMS = 2; // running milliseconds
  GT_MSSinceStart = 3; // milliseconds since soundcard restart
  GT_Ticks = 4; // ticks
  GT_LocalTime = 1 shl 31; // time relative to song start

  GT_FlagsMask = $FFFFFF00;
  GT_TimeFormatMask = $000000FF;

  // PromptEdit_Ex option values
  PEO_Text = 1 shl 0;
  PEO_Icon = 1 shl 1;
  PEO_Color = 1 shl 2;
  PEO_Gradient = 1 shl 3;
  PEO_Presets = 1 shl 4;
  PEO_Password = 1 shl 5;

  //external media dialogs flags
  EMD_SearchImages = 1;
  EMD_SearchVideos = 2;
  EMD_DownloadFile = 4;

  //FPD_GetParamType options
  PT_Default = 0;
  PT_Db = 1;
  PT_Hz = 2;
  PT_Centered = 3;
  PT_Ms = 4;
  PT_Percent = 5;
  PT_Time = 6;
  PT_Value = 7;
  PT_Number = 8;
  PT_Text = 9;

  PT_Last = PT_Text;

  // host dispatcher ID's
  FHD_ParamMenu = 0; // the popup menu for each control (Index=param index, Value=popup item index (see FHP_EditEvents))
  FHD_GetParamMenuFlags = 1; // [OBSOLETE, see FHD_GetParamMenuEntry] before the popup menu is shown, you must ask the host to tell if items are checked or disabled (Index=param index, Value=popup item index, Result=flags (see FHP_Disabled))
  FHD_EditorResized = 2; // to notify the host that the editor (EditorHandle) has been resized
  FHD_NamesChanged = 3; // to notify the host that names (GetName function) have changed, with the type of names in Value (see FPN_Semitone)
  FHD_ActivateMIDI = 4; // makes the host enable its MIDI output, useful when a MIDI out plugin is created (but not useful for plugin wrappers)
  FHD_WantMIDIInput = 5; // plugin wants to be notified about MIDI messages (for processing or filtering) (switch in Value)
  FHD_WantMIDITick = 6; // plugin wants to receive MIDITick events, allowing MIDI out plugins (not used yet)
  FHD_LocatePlugin = 7; // ask the host to find a plugin, pass the simple filename in Value, full path is returned as Result (both PAnsiChar). Set Index to 1 if you want host to show a warning if plugin could not be found.
  FHD_KillAutomation = 8; // ask the host to kill the automation linked to the plugin, for params # between Index & Value (included) (can be used for a trial version of the plugin)
  FHD_SetNumPresets = 9; // tell the host how many (Value) internal presets the plugin supports (mainly for wrapper)
  FHD_SetNewName = 10; // sets a new short name for the parent (PAnsiChar in Value)
  FHD_VSTiIdle = 11; // used by the VSTi wrapper, because the dumb VSTGUI needs idling for its knobs
  FHD_SelectChanSample = 12; // ask the parent to open a selector for its channel sample (see FPF_UseChanSample)
  FHD_WantIdle = 13; // plugin wants to receive the idle message (enabled by default) (Value=0 for disabled, 1 for enabled when UI is visible, 2 for always enabled)
  FHD_LocateDataFile = 14; // ask the host to search for a file in its search paths, pass the simple filename in Value, full path is returned as Result (both PAnsiChar) (Result doesn't live long, please copy it asap). Set Index to 1 if you don't want FL to buffer the filename if it couldn't be found.
  FHD_ShowPlugSelector = 15; // ask the host to show the plugin selector (Index: see SPSF flags)
  FHD_TicksToTime = 16; // translate tick time (Value) into Bar:Step:Tick (PSongTime in Index) (warning: it's *not* Bar:Beat:Tick)
  FHD_AddNotesToPR = 17; // add a note to the piano roll, PNotesParams in Value
  FHD_GetParamMenuEntry = 18; // before the popup menu is shown, you must fill it with the entries set by the host (Index=param index, Value=popup item index (starting from 0), Result=PParamMenuEntry, or null pointer if no more entry)
  FHD_MsgBox = 19; // make FL show a message box (PAnsiChar in Index [formatted as 'Title|Message'], flags in Value (MB_OkCancel, MB_IconWarning, etc.), result in IDOk, IDCancel format (as in TApplication.MessageBox)
  FHD_NoteOn = 20; // preview note on (semitone in Index low word, color in index high word (0=default), velocity in Value)
  FHD_NoteOff = 21; // preview note off (semitone in Index, color in index high word, velocity in Value (-1=default otherwise 0..127))
  FHD_OnHint_Direct = 22; // same as OnHint, but show it immediately (to show a progress while you're doing something) (PAnsiChar in Value)
  FHD_SetNewColor = 23; // sets a new color for the parent (color in Value) (see FHD_SetNewName);
  FHD_GetInstance = 24; // (Windows) returns the module instance of the host (could be an exe or a DLL, so not the process itself)
  FHD_KillIntCtrl = 25; // ask the host to kill anything linked to an internal controller, for # between Index & Value (included) (used when undeclaring internal controllers)
  FHD_CheckProdCode = 26; // reserved
  FHD_SetNumParams = 27; // override the # of parameters (for plugins that have a different set of parameters per instance) (number of parameters in Value)
  FHD_PackDataFile = 28; // ask the host to pack an absolute filename into a local filemane, pass the simple filename in Value, packed path is returned as Result (both PAnsiChar) (Result doesn't live long, please copy it asap)
  FHD_GetPath = 29; // ask the host for a path specified by Index (see GP_ constants) (returned as Result)
  FHD_SetLatency = 30; // set plugin latency, if any (samples in Value)
  FHD_CallDownloader = 31; // call the presets downloader (optional plugin name PAnsiChar in Value)
  FHD_EditSample = 32; // edits sample in Edison (PAnsiChar in Value, Index=1 means an existing Edison can be re-used)
  FHD_SetThreadSafe = 33; // plugin is thread-safe, doing its own thread-sync using LockMix_Shared (switch in Value)
  FHD_SmartDisable = 34; // plugin asks FL to exit or enter smart disabling (if currently active), mainly for generators when they get MIDI input (switch in Value), set Value to -1 if the FPF_CantSmartDisable flag was added to or removed from Info.Flags
  FHD_SetUID = 35; // sets a unique identifying string for this plugin. This will be used to save/restore custom data related to this plugin. Handy for wrapper plugins. (PAnsiChar in Value)
  FHD_GetMixingTime = 36; // get mixer time, Index is the time format required (see GT_... constants). Value is a pointer to a TFPTime, which is filled with an optional offset in samples
  FHD_GetPlaybackTime = 37; // get playback time, same as above
  FHD_GetSelTime = 38; // get selection time in t & t2, same as above. Returns 0 if no selection (t & t2 are then filled with full song length).
  FHD_GetTimeMul = 39; // get current tempo multiplicator, that's not part of the song but used for fast-forward
  FHD_Captionize = 40; // captionize the plugin (useful when dragging) (captionized in Value)
  FHD_SendSysEx = 41; // send a SysEx AnsiString (pointer to array in Value, the first integer being the length of the AnsiString, the rest being the AnsiString), through port Index, immediately (do not abuse)
  FHD_LoadAudioClip = 42; // send an audio file to the playlist as an audio clip, starting at the playlist selection. Options in Index (see LAC_ constants). FileName as PAnsiChar in Value.
  FHD_LoadInChannel = 43; // send a file to the selected channel(s) (mainly for Edison), FileName as PAnsiChar in Value
  FHD_ShowInBrowser = 44; // locates the file in the browser & jumps to it (Index is one of SIB_ constants, PAnsiChar filename in Value)
  FHD_DebugLogMsg = 45; // adds message to the debug log (PAnsiChar in Value)
  FHD_GetMainFormHandle = 46; // gets the handle of the main form (HWND in Value, 0 if none)
  FHD_GetProjDataPath = 47; // [OBSOLETE - use FHD_GetPath instead] ask the host where the project data is, to store project data (returned as Result)
  FHD_SetDirty = 48; // mark project as dirty (not required for automatable parameters, only for tweaks the host can't be aware of)
  FHD_AddToRecent = 49; // add file to recent files (PAnsiChar in Value)
  FHD_GetNumInOut = 50; // ask the host how many inputs (Index=0) are routed to this effect (see GetInBuffer), or how many outputs (Index=1) this effect is routed to (see GetOutBuffer)
  FHD_GetInName = 51; // ask the host the name of the input Index (!!! first = 1), in Value as a PNameColor, Result=0 if failed (Index out of range)
  FHD_GetOutName = 52; // ask the host the name of the ouput Index (!!! first = 1), in Value as a PNameColor, Result=0 if failed (Index out of range)
  FHD_ShowEditor = 53; // make host bring plugin's editor (visibility in Value, -1 to toggle)
  FHD_FloatAutomation = 54; // (for the plugin wrapper only) ask the host to turn 0..FromMIDI_Max automation into 0..1 float, for params # between Index & Value (included)
  FHD_ShowSettings = 55; // called when the settings button on the titlebar should be updated switched. On/off in Value (1=active). See FPF_WantSettingsBtn
  FHD_NoteOnOff = 56; // generators only! note on/off (semitone in Index low word, color in index high word, NOT recorded in bit 30, velocity in Value (<=0 = note off))
  FHD_ShowPicker = 57; // show picker (mode [0=plugins, 1=project] in Index, categories [gen=0/FX=1/both=-1/Patcher (includes VFX)=-2] in Value)
  FHD_GetIdleOverflow = 58; // ask the host for the # of extra frames Idle should process, generally 0 if no overflow/frameskip occured
  FHD_ModalIdle = 59; // used by FL plugins, when idling from a modal window, mainly for the smoothness hack
  FHD_RenderProject = 60; // prompt the rendering dialog in song mode
  FHD_GetProjectInfo = 61; // get info about project (see GPI constants for value of Index), (returned as Result as a PAnsiChar with UTF-8 encoding)
  FHD_ForceDetached = 62; // used by Wrapper in OSX to force the plugin form to be detached
  FHD_StartDrag = 63; // sent by Patcher when starting dragging a preset
  FHD_EndDrag = 64; // sent by Patcher when finished dragging a preset
  FHD_PreviewKey = 65; // chance for host to handle keyboard messages, Index=flags in lower 16 bits (see KUD constants) and virtual key in second 16 bits, Value=KeyData from WM_KeyUp or WM_KeyDown message (0 if not available), returns 1 if handled and 0 if not
  FHD_RenderWindowBitmap = 66; // used by ZgeViz
  FHD_UpdateStealKBFocus = 67; // the plugin will steal kb input or not (Value is 1 or 0)
  FHD_GetPluginFavPath = 68; // [OBSOLETE - use FHD_GetPath instead] returns pchar (not PAnsiChar!) of the full path to generators (Index=1) or FX (Index=0) favorite presets path (plugin database)
  FHD_GetPluginMenuMode = 69; // returns the view mode of the favorite plugin menus in FL: 0=categories 1=tree 2=flat
  FHD_OpenTool = 70; // open application in System\Tools folder. Index=tool to start (see OTI_ControlCreator), Value=PAnsiChar with command line params
  FHD_GetPathManager = 71; // returns IPathManager instance (pointer)
  FHD_RegisterSideInput = 72; // let the host know that you intend to use a sidechained input, so latency can be calculated. Index=input index (first=1), Value=see RSIO_ constants
  FHD_RegisterSideOutput = 73; // let the host know that you intend to use a sidechained output, so latency can be calculated. Index=output index (depends on use of GetInsBuffer or GetOutBuffer), Value=see RSIO_ constants
  FHD_ReportError = 74; // report error during plugin load (will show combined dialog for all missing plugins after project is loaded or MsgBox in case we are adding plugin to project)
  FHD_ShowStandardParamMenu = 75; // ask FL to pop up a parameter menu, so the plugin doesn't have to implement it itself. Index is the parameter index.
  FHD_GetContextInfo = 76; // get information about various things. Index is the information type (see CI_ constants), Value and result depend on the type
  FHD_SetContextInfo = 77; // change some piece of context information. Index is the information type (see CI_ constants), Value and result depend on the type
  FHD_GetExternalMedia = 78; // set Flags (bits) as index, for example : EMD_SearchImages or EMD_DownloadFile to search and download images
  FHD_Transport = 79; // allows the plugin to control FL through some of the messages in GenericTransport.pas. Index=message, Value=release/switch/hold value. Currently only FPT_Play and FPT_Stop are supported. Returns -1 if can't be handled, 0 if not handled, 1 if handled by focused plugin, 2 if handled by focused form, 4 if handled by menu, 5 if delayed, 8 if handled globally.
  FHD_DownloadMissing = 80; // notify FL about missing data pack
  FHD_DownloadFinished = 81; // notify FL about missing pack download is finished
  FHD_DebugBuild = 82; // tell FL to show a [DEBUG] warning in the plugin window caption. Value is 0 (release) or 1 (debug)
  FHD_PickVoiceColor = 83; // Show the piano roll's color picker. Index = screen co-ordinates with x in first 2 bytes and y in next 2 bytes, Value = current color number (not an RGB value). Will call FPD_ColorWasPicked when the user selects a color.
  FHD_GetColorRGBValue = 84; // Get the RGB value for a color in a palette. Index is the color palette (see CP_ constants for available palettes). Value is the index in the palette. If Value is -1, this returns the count of colors in the palette.
  FHD_ShowException = 85; // Show application exception. Index is Exception.Message string. Value is Stack-trace string.
  FHD_GetTranslationMoFile = 86; // Get the current translation object (for Plugins)
  FHD_PresetSelected = 87;  // tell the host internal preset is changed

  // param popup menu item indexes (same order as param menu in FL)
  // note that it can be a Windows popup menu or anything else
  // OBSOLETE (compatibility only): now the plugin doesn't know about those menu entries, that can be freely changed by the host
  {
  FHP_Edit              =0;     // Edit events
  FHP_EditNewWindow     =1;     // Edit events in new window
  FHP_Init              =2;     // Init with this position
  FHP_Link              =3;     // Link to MIDI controller
  }

  // param popup menu item flags
  FHP_Disabled = 1;
  FHP_Checked = 2;

  // sample loading flags
  FHLS_ShowDialog = 1; // tells the sample loader to show an open box, for the user to select a sample
  FHLS_ForceReload = 2; // force it to be reloaded, even if the filename is the same (in case you modified the sample)
  FHLS_GetName = 4; // don't load the sample, instead get its filename & make sure that the format is correct (useful after FPD_ChanSampleChanged)
  FHLS_NoResampling = 8; // don't resample to the host sample rate

  // TNotesParams flags
  NPF_EmptyFirst = 1; // delete everything before adding the notes
  NPF_UseSelection = 2; // dump inside piano roll selection if any

  // param flags (see FPD_GetParamInfo)
  PI_CantInterpolate = 1; // makes no sense to interpolate parameter values (when values are not levels)
  PI_Float = 2; // parameter is a normalized (0..1) single float. (Integer otherwise)
  PI_Centered = 4; // parameter appears centered in event editors

  // GetInBuffer / GetOutBuffer flags
  // input
  IO_Lock = 0; // GetOutBuffer, before adding to the buffer
  IO_Unlock = 1; // GetOutBuffer, after adding to the buffer
  // output
  IO_Filled = 1; // GetInBuffer, tells if the buffer is filled

  // FHD_KeyUpDown flags
  KUD_Down = 1 shl 0;
  KUD_Shift = 1 shl 1;
  KUD_Control = 1 shl 2;
  KUD_Alt = 1 shl 3;
  KUD_Command = 1 shl 4;

  // FHD_ShowPlugSelector flags (for Index parameter)
  SPSF_Effects = 1 shl 0;
  SPSF_Generators = 1 shl 1;
  SPSF_MoreWindow = 1 shl 2;

  // FHD_ShowInBrowser flags (for Index parameter)
  SIB_FileName = 0; // Value parameter is a filename to show
  SIB_GenPluginPresets = 1; // Value parameter is the name of a generator plugin for which to show the presets
  SIB_EffPluginPresets = 2; // Value parameter is the name of an effect plugin for which to show the presets
  SIB_GenPluginFileName = 3; // Value parameter is a filename to show, plugin is generator
  SIB_EffPluginFileName = 4; // Value parameter is a filename to show, plugin is effect

  // FHD_GetPath values (for Index parameter)
  GP_ProgPath = 0; // the location of the engine, which may NOT be where the executable is, but where the data path will be
  GP_ProjDataPath = 1; // the location of the project data, to store project data
  GP_PluginFavPath = 2; // the full path to generators (Value=1) or effect (Value=0) favorite presets path (plugin database)
  GP_PluginPresetPath = 3; // the path containing the plugin's own presets
  GP_GenPluginPresetPath = 4; // the preset path for a specific generator plugin, plugin UID in Value
  GP_EffPluginPresetPath = 5; // the preset path for a specific effect plugin, plugin UID in Value
  GP_PluginDatabase = 6; // the root of the plugin database folder
  GP_SharedDlls = 7; // the location of the shared dlls

  // FHD_GetProjectInfo, FPD_ProjectInfoChanged Index values
  GPI_Title = 0;
  GPI_Author = 1;
  GPI_Comments = 2;
  GPI_URL = 3;
  GPI_FileName = 4;

  // FHD_RenderProject Index flags
  RPF_UsesFLWindows = 1;
  RPF_IsVideoExport = 2;
  RPF_IsVideoUpload = 4;

  // path indexes for the host IPathManager (see FHD_GetPathManager)
  PI_User = 0;
  PI_Factory = 1;

  PathIndexesConst: array[0..1] of integer = (PI_User, PI_Factory);
  PathIndexNamesConst: array[0..1] of string = ('User', 'Factory');

  // data path identifiers for the host IPathManager (see FHD_GetPathManager)
  FLData_Loops = 1000;
  FLData_Backup = 1001;
  FLData_AutoSave = 1002;
  FLData_Logs = 1003;
  FLData_Settings = 1004;
  FLData_AboutURL = 1005;
  FLData_Screenshots = 1006;
  FLData_Recorded = 1007;
  FLData_Rendered = 1008;
  FLData_SlicedBeats = 1009;
  FLData_ClipboardFiles = 1010;
  FLData_PluginPresets = 1011;
  FLData_Thumbnails = 1012;
  FLData_Templates = 1013;
  FLData_ChannelPresets = 1015;
  FLData_MixerPresets = 1016;
  FLData_Scores = 1017;
  FLData_ProjectBones = 1018;
  FLData_EffectPlugins = 1019; // use the value returned by GetSettingsSubPath as the first sub folder!
  FLData_GeneratorPlugins = 1020; // use the value returned by GetSettingsSubPath as the first sub folder!
  FLData_Internet = 1021;
  FLData_PluginDatabase = 1022;
  FLData_Tools = 1023;
  FLData_BrowserConfig = 1024;
  FLData_SystemData = 1025;
  FLData_NameLists = 1026;
  FLData_TypingToPiano = 1027;
  FLData_ToolbarPresets = 1028;
  FLData_TouchKbPresets = 1029;
  FLData_HardwareSpecific = 1030;
  FLData_Mapping = 1031;
  FLData_GenericMapping = 1032;
  FLData_Styles = 1033;
  FLData_Help = 1034;
  FLData_Speech = 1035;
  FLData_Envelopes = 1036;
  FLData_Packs = 1037;
  FLData_NewPluginArchive = 1038;
  FLData_Languages = 1039;
  FLData_Impulses = 1040;
  FLData_NoteColorPresets = 1041;

  // values for the Value parameter of FHD_RegisterSideInput and FHD_RegisterSideOutput
  RSIO_Unregister = 0; // unregister a previously registered I/O
  RSIO_Register = 1 shl 0; // register a new I/O as used

  // flags for the Value parameter of FPD_RoutingChanged
  RCV_NamesChanged = 1 shl 0; // only one or more track names have changed
  RCV_SideIOCleared = 1 shl 1; // if the plugin uses any side I/O, the host has unregistered them all and the plugin should re-register them.

  RCV_Default = RCV_NamesChanged;

  //values for FHD_ReportError
  RE_OpeningUnknown = 0;
  RE_OpeningEffectIntoChannel = 1;
  RE_UnknownFormat = 2;
  RE_DxPlugNotFound = 3;
  RE_UnknownPluginType = 4;
  RE_PluginCrashOnSettings = 5;

  // result values (errors) for FPD_WrapPlugin
  PE_None = 0;
  PE_UnknownError = -1;
  PE_ExceptionWhenOpening = -2;
  PE_NotStereo = 10;
  //PE_VST_NoReplacing=11;
  //PE_VST_BadVersion =12;
  //PE_VST_VST2       =13;
  //PE_DX_NotSupported=20;
  PE_NotFound = 30;
  PE_NotSupported = 40; // invalid PlugClass
  PE_VFXinFL = 50;
  PE_FileNotFound = 60;
  // values from FHD_ReportError mapped to PE messages
  PE_RE_OpeningUnknown = 70;
  PE_RE_OpeningEffectIntoChannel = 71;
  PE_RE_UnknownFormat = 72;
  PE_RE_DxPlugNotFound = 73;
  PE_RE_UnknownPluginType = 74;
  PE_RE_PluginCrashOnSettings = 75;

  // context info types
  // requested info is returned as the result of the function and passed in Value to set it (unless specified otherwise)
  CI_TrackName = 0; // (R/W) PAnsiChar encoded as UTF-8
  CI_TrackIndex = 1; // (R)
  CI_TrackColor = 2; // (R/W) color is RGBA
  CI_TrackSelected = 3; // (R/W) the track is selected (0=false 1=true, 2=selected with other tracks)
  CI_TrackFocused = 4; // (R) the track is focused for user input (0=false 1=true)
  CI_TrackIsOutput = 5; // (R) the track sends directly to an audio device output (0=false, 1=true)
  CI_TrackVolume = 6; // (R/W) (float+string) the value of the tracks' volume slider. Info is floating point (single / float) cast to an int32
  CI_TrackPan = 7; // (R/W) (float+string) the value of the track's panning knob, as a single / float (-1..1) cast to int32
  CI_TrackMuteSolo = 8; // (R/W) flags indicate mute and solo state for a track (see CIMS_ constants)
  CI_TrackSendCount = 9; // (R) returns the send count for the plugin's track
  CI_TrackSendLevel = 10; // (R/W) (float+string) get or set the level for a specific send of this track. On read, Value holds the send index. On write, Value holds a pointer to a TContextInfo record with the new value in FloatValue.
  CI_TrackMaxVolume = 11; // (R) get the maximum value for mixer track volume
  CI_TrackMaxSendLevel = 12; // (R) get the maximum value for mixer track send level
  // flags to be included with info type (e.g. CI_TrackName or CI_AsString), for info that can be both a string and an int or float value
  CI_AsValue = 1 shl 0;
  CI_AsString = 1 shl 31;
  // flags for CI_TrackMuteSolo
  CIMS_Muted = 1 shl 0;
  CIMS_Solo = 1 shl 1;

  // color palettes for FHD_GetColorValue and other palette functions
  CP_VoiceColors = 0;

  // options for FHD_LoadAudioClip
  LAC_Stretch_Default = 0;
  LAC_Stretch_Auto = 1;
  LAC_Stretch_Resample = 2;
  LAC_Stretch_Stretching = 3;
  LAC_Stretch_E3ProDefault = 4;
  LAC_Stretch_E3Mono = 5;
  LAC_Stretch_Slices = 6;
  LAC_Stretch_SliceMap = 7;
  LAC_StretchMask = $0000000F; // stretch options are in the first 4 bits

implementation

// destroy the object
procedure TFruityPlug.DestroyObject;
begin
  Destroy;
end;

end.

