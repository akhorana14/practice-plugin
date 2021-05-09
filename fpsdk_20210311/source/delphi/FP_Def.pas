{

a couple of def for the Fruity Plugin SDK

(00) gol

}

unit FP_Def;

interface

uses
  Winapi.MMSystem;

const
  WaveT_Bits = 14; // 14 bits for the length of the wavetable
  WaveT_Size = 1 shl WaveT_Bits; // length of the wavetable
  WaveT_Shift = 32 - WaveT_Bits; // shift for full DWORD conversion
  WaveT_Step = 1 shl WaveT_Shift; // speed for 1 sample in the wavetable
  WaveT_PMask = $FFFFFFFF shr WaveT_Shift; // mask to limit the position to the range of the wavetable
  WaveT_FMask = $FFFFFFFF shr WaveT_Bits; // mask to get the frac part of the position

  MIDIMsg_PortMask = $FFFFFF;
  MIDIMsg_Null = $FFFFFFFF;

  // see REC_FromMIDI
  FromMIDI_Max = 1 shl 30; // #9142
  FromMIDI_Half = FromMIDI_Max shr 1;
  FromMIDI_Div = 1 / FromMIDI_Max;

type // published wavetables
  TWaveT = array[0..WaveT_Size - 1] of Single;
  PWaveT = ^TWaveT;

  // interlaced stereo 32Bit float buffer
  TWAV32FS = array[0..0, 0..1] of Single;
  PWAV32FS = ^TWAV32FS;

  TWAV32FM = array[0..0] of Single;
  PWAV32FM = ^TWAV32FM;

  // MIDI out message structure (3 bytes standard MIDI message + port)
  TMIDIOutMsg = packed record
    Status, Data1, Data2, Port: Byte;
  end;
  PMIDIOutMsg = ^TMIDIOutMsg;

  // for the wrapper only
  TWrappedPluginID = packed record
    PlugClass: Integer;
    Name, FileName: PAnsiChar;
    ID: PGUID;
  end;
  PWrappedPluginID = ^TWrappedPluginID;

  // extended wav format
  PWaveFormatExtensible = ^TWaveFormatExtensible;
  TWaveFormatExtensible = packed record
    WaveFormatEx: TWaveFormatEx;
    case Integer of
      0: (
        wValidBitsPerSample: Word; // bits of precision
        dwChannelMask: LongWord; // which channels are present in stream
        SubFormat: TGUID;
        );
      1: (wSamplesPerBlock: Word); // valid if wBitsPerSample==0
      2: (wReserved: Word); // if neither applies, set to zero
  end;

  // Bar:Step:Tick
  TSongTime = record
    Bar, Step, Tick: Integer;
  end;
  PSongTime = ^TSongTime;

  // time sig info (easily converted to standard x/x time sig, but more powerful)
  TTimeSigInfo = record
    StepsPerBar, StepsPerBeat, PPQ: Integer;
  end;
  PTimeSigInfo = ^TTimeSigInfo;

implementation

end.

