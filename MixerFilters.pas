// audinx
// ultimate internet-radio stream maker
//
// (c) by sergey korowkin, 2008-2014

unit MixerFilters;

interface

uses Common, Tokenizer, SysUtils, Classes, RegExp,
     dspConst, 
     dspTrebleEnhancer, dspEqualizer, dspCompressor, dspTrueBass, dspSound3D, 
     dspFlanger, dspLowPass, dspHighpass, dspPhaser, dspDynamicAmplify;

type
 TMixerFilter = class
 public
  ID: String;
  Enabled: Boolean;
  Active: Longint;
  constructor Create(const AID: String; var Command, rc: String; Output: TSocketStream);
  procedure Reset; virtual; abstract;
  procedure Perform(var Buffer: TMixerBuffer; var UnitedBuffer: TUnitedMixerBuffer); virtual; abstract;
  function HaveParameter(const Parameter: String): Boolean; virtual;
  function GetParameter(const Parameter: String): Double; virtual;
  procedure SetParameter(const Parameter: String; const AValue: Double); virtual;
  function ExplainStatus: String; virtual;
  destructor Destroy; override;
 end;

 TMixerFilterEnhancer = class(TMixerFilter)
 public
  Enhancer: TDCTrebleEnhancer;
  constructor Create(const AID: String; var Command, rc: String; Output: TSocketStream);
  procedure Reset; override;
  procedure Perform(var Buffer: TMixerBuffer; var UnitedBuffer: TUnitedMixerBuffer); override;
  function HaveParameter(const Parameter: String): Boolean; override;
  function GetParameter(const Parameter: String): Double; override;
  procedure SetParameter(const Parameter: String; const AValue: Double); override;
  function ExplainStatus: String; override;
  destructor Destroy; override;
 end;

 TMixerFilterCompressor = class(TMixerFilter)
 public
  Compressor: TDCCompressor;
  constructor Create(const AID: String; var Command, rc: String; Output: TSocketStream);
  procedure Reset; override;
  procedure Perform(var Buffer: TMixerBuffer; var UnitedBuffer: TUnitedMixerBuffer); override;
  function HaveParameter(const Parameter: String): Boolean; override;
  function GetParameter(const Parameter: String): Double; override;
  procedure SetParameter(const Parameter: String; const AValue: Double); override;
  function ExplainStatus: String; override;
  destructor Destroy; override;
 end;

 TMixerFilterEqualizer = class(TMixerFilter)
 public
  Eq: TDCEqualizer;
  Bands: Integer;
  constructor Create(const AID: String; var Command, rc: String; Output: TSocketStream);
  procedure Reset; override;
  procedure Perform(var Buffer: TMixerBuffer; var UnitedBuffer: TUnitedMixerBuffer); override;
  function HaveParameter(const Parameter: String): Boolean; override;
  function GetParameter(const Parameter: String): Double; override;
  procedure SetParameter(const Parameter: String; const AValue: Double); override;
  function ExplainStatus: String; override;
  destructor Destroy; override;
 end;

 TMixerFilterTrueBass = class(TMixerFilter)
 public
  TrueBass: TDCTrueBass;
  constructor Create(const AID: String; var Command, rc: String; Output: TSocketStream);
  procedure Reset; override;
  procedure Perform(var Buffer: TMixerBuffer; var UnitedBuffer: TUnitedMixerBuffer); override;
  function HaveParameter(const Parameter: String): Boolean; override;
  function GetParameter(const Parameter: String): Double; override;
  procedure SetParameter(const Parameter: String; const AValue: Double); override;
  function ExplainStatus: String; override;
  destructor Destroy; override;
 end;

 TMixerFilterSound3D = class(TMixerFilter)
 public
  Sound3D: TDCSound3D;
  constructor Create(const AID: String; var Command, rc: String; Output: TSocketStream);
  procedure Reset; override;
  procedure Perform(var Buffer: TMixerBuffer; var UnitedBuffer: TUnitedMixerBuffer); override;
  function HaveParameter(const Parameter: String): Boolean; override;
  function GetParameter(const Parameter: String): Double; override;
  procedure SetParameter(const Parameter: String; const AValue: Double); override;
  function ExplainStatus: String; override;
  destructor Destroy; override;
 end;

 TMixerFilterLowPass = class(TMixerFilter)
 public
  LowPass: TDCLowPass;
  constructor Create(const AID: String; var Command, rc: String; Output: TSocketStream);
  procedure Reset; override;
  procedure Perform(var Buffer: TMixerBuffer; var UnitedBuffer: TUnitedMixerBuffer); override;
  function HaveParameter(const Parameter: String): Boolean; override;
  function GetParameter(const Parameter: String): Double; override;
  procedure SetParameter(const Parameter: String; const AValue: Double); override;
  function ExplainStatus: String; override;
  destructor Destroy; override;
 end;

 TMixerFilterHighPass = class(TMixerFilter)
 public
  HighPass: TDCHighPass;
  constructor Create(const AID: String; var Command, rc: String; Output: TSocketStream);
  procedure Reset; override;
  procedure Perform(var Buffer: TMixerBuffer; var UnitedBuffer: TUnitedMixerBuffer); override;
  function HaveParameter(const Parameter: String): Boolean; override;
  function GetParameter(const Parameter: String): Double; override;
  procedure SetParameter(const Parameter: String; const AValue: Double); override;
  function ExplainStatus: String; override;
  destructor Destroy; override;
 end;

 TMixerFilterPhaser = class(TMixerFilter)
 public
  Phaser: TDCPhaser;
  constructor Create(const AID: String; var Command, rc: String; Output: TSocketStream);
  procedure Reset; override;
  procedure Perform(var Buffer: TMixerBuffer; var UnitedBuffer: TUnitedMixerBuffer); override;
  function HaveParameter(const Parameter: String): Boolean; override;
  function GetParameter(const Parameter: String): Double; override;
  procedure SetParameter(const Parameter: String; const AValue: Double); override;
  function ExplainStatus: String; override;
  destructor Destroy; override;
 end;

 TMixerFilterFlanger = class(TMixerFilter)
 public
  Flanger: TDCFlanger;
  constructor Create(const AID: String; var Command, rc: String; Output: TSocketStream);
  procedure Reset; override;
  procedure Perform(var Buffer: TMixerBuffer; var UnitedBuffer: TUnitedMixerBuffer); override;
  function HaveParameter(const Parameter: String): Boolean; override;
  function GetParameter(const Parameter: String): Double; override;
  procedure SetParameter(const Parameter: String; const AValue: Double); override;
  function ExplainStatus: String; override;
  destructor Destroy; override;
 end;

 TMixerFilterDynAmp = class(TMixerFilter)
 public
  DynAmp: TDCDynamicAmplify;
  constructor Create(const AID: String; var Command, rc: String; Output: TSocketStream);
  procedure Reset; override;
  procedure Perform(var Buffer: TMixerBuffer; var UnitedBuffer: TUnitedMixerBuffer); override;
  function HaveParameter(const Parameter: String): Boolean; override;
  function GetParameter(const Parameter: String): Double; override;
  procedure SetParameter(const Parameter: String; const AValue: Double); override;
  function ExplainStatus: String; override;
  destructor Destroy; override;
 end;

 TMixerFiltersPool = class
 public
  Filters: packed array of TMixerFilter;
  Lock: TRTLCriticalSection;
  constructor Create;
  procedure Add(Filter: TMixerFilter);
  function Remove(Filter: TMixerFilter): Boolean;
  procedure Perform(var Buffer: TMixerBuffer);
  function Empty: Boolean;
  function List: String;
  destructor Destroy; override;
 end;

implementation

uses Mixer, Daemon;

// TMixerFilter: ENABLED "1"

constructor TMixerFilter.Create(const AID: String; var Command, rc: String; Output: TSocketStream);
 var
  S: String;
 begin
  inherited Create;

  ID:=AID;

  if not TokenizeToken(Command, 'ENABLED') then
   begin
    Output.Report(400, Command, rc, 'ENABLED expected');
    Destroy;
    Fail;
   end;

  if not TokenizeValue(Command, S) then 
   begin
    Output.Report(400, Command, rc, 'ENABLED "1/0" expected');
    Destroy;
    Fail;
   end;

  Enabled:=S = '1';
  Active:=0;
 end;

function TMixerFilter.HaveParameter(const Parameter: String): Boolean;
 begin
  if Parameter = 'ENABLED' then Exit(True)
  else Exit(False);
 end;

function TMixerFilter.GetParameter(const Parameter: String): Double;
 begin
  if Parameter = 'ENABLED' then Exit(Longint(Enabled))
  else Exit(0.0);
 end;

procedure TMixerFilter.SetParameter(const Parameter: String; const AValue: Double);
 begin
  if Parameter = 'ENABLED' then Enabled:=AValue > 0.0;
 end;

function TMixerFilter.ExplainStatus: String;
 begin
  if Enabled then
   Result:=', enabled'
  else
   Result:=', disabled';

  Result:=Result + ', used=' + IntToStr(Longint(Active));
 end;

destructor TMixerFilter.Destroy; 
 var
  K: Integer;
 begin
  SetLength(ID, 0);

  TheMixer.MasterFilters.Remove(Self);

  for K:=0 to Length(TheMixer.Sources) - 1 do
   TheMixer.Sources[K].Filters.Remove(Self);

  inherited Destroy;
 end;

// TMixerFilterEnhancer: ENABLED "1" FREQUENCY "16000" VOLUME "125"

constructor TMixerFilterEnhancer.Create(const AID: String; var Command, rc: String; Output: TSocketStream);
 var
  Freq, Vol: String;
 begin
  inherited Create(AID, Command, rc, Output);

  Enhancer:=TDCTrebleEnhancer.Create(nil);

  if not TokenizeToken(Command, 'FREQUENCY') then
   begin
    Output.Report(400, Command, rc, 'FREQUENCY expected');
    Destroy;
    Fail;
   end;

  if not TokenizeValue(Command, Freq) then 
   begin
    Output.Report(400, Command, rc, 'FREQUENCY "16000" expected');
    Destroy;
    Fail;
   end;

  if not TokenizeToken(Command, 'VOLUME') then
   begin
    Output.Report(400, Command, rc, 'VOLUME expected');
    Destroy;
    Fail;
   end;

  if not TokenizeValue(Command, Vol) then 
   begin
    Output.Report(400, Command, rc, 'VOLUME "100" expected');
    Destroy;
    Fail;
   end;

  Enhancer.Enabled:=Enabled;
  Enhancer.Seperate:=False;
  Enhancer.SampleSize:=1152;
  Enhancer.Frequency:=StrToIntDef(Freq, 16000);
  Enhancer.Volume[0]:=StrToIntDef(Vol, 0);
 end;

procedure TMixerFilterEnhancer.Reset;
 begin
  if Enhancer <> nil then
   Enhancer.Flush;
 end;

procedure TMixerFilterEnhancer.Perform(var Buffer: TMixerBuffer; var UnitedBuffer: TUnitedMixerBuffer);
 begin
  if (Enhancer <> nil) and Enabled then
   Enhancer.Process(@UnitedBuffer, Buffer.Samples * 4, 44100, 16, 2, False);
 end;

function TMixerFilterEnhancer.HaveParameter(const Parameter: String): Boolean;
 begin
  if Parameter = 'FREQUENCY' then Exit(True)
  else if Parameter = 'VOLUME' then Exit(True)
  else Exit(inherited HaveParameter(Parameter));
 end;

function TMixerFilterEnhancer.GetParameter(const Parameter: String): Double;
 begin
  if Parameter = 'FREQUENCY' then Exit(Enhancer.Frequency)
  else if Parameter = 'VOLUME' then Exit(Enhancer.Volume[0])
  else Exit(inherited GetParameter(Parameter));
 end;

procedure TMixerFilterEnhancer.SetParameter(const Parameter: String; const AValue: Double);
 begin
  if Parameter = 'FREQUENCY' then Enhancer.Frequency:=Trunc(AValue)
  else if Parameter = 'VOLUME' then Enhancer.Volume[0]:=Trunc(AValue)
  else inherited SetParameter(Parameter, AValue);
 end;

function TMixerFilterEnhancer.ExplainStatus: String;
 begin
  Result:='type=enhancer' +
          inherited ExplainStatus + 
          ', freq=' + IntToStr(Enhancer.Frequency) +
          ', volume=' + IntToStr(Enhancer.Volume[0]);
 end;

destructor TMixerFilterEnhancer.Destroy;
 begin
  if Enhancer <> nil then
   Enhancer.Free;

  inherited Destroy;
 end;

// TMixerFilterCompressor: ENABLED "1" THRESHOLD "-20.0" ATTACK "1.0" DECAY "25.0" RATIO "2.0" GAIN "6.0"

constructor TMixerFilterCompressor.Create(const AID: String; var Command, rc: String; Output: TSocketStream);
 var
  Treshold, Attack, Decay, Ratio, Gain: String;
 begin
  inherited Create(AID, Command, rc, Output);

  Compressor:=TDCCompressor.Create(nil);

  if not TokenizeToken(Command, 'THRESHOLD') then begin Output.Report(400, Command, rc, 'THRESHOLD expected'); Destroy; Fail; end;
  if not TokenizeValue(Command, Treshold) then begin Output.Report(400, Command, rc, 'THRESHOLD "-12.0" expected'); Destroy; Fail; end;

  if not TokenizeToken(Command, 'ATTACK') then begin Output.Report(400, Command, rc, 'ATTACK expected'); Destroy; Fail; end;
  if not TokenizeValue(Command, Attack) then begin Output.Report(400, Command, rc, 'ATTACK "0.05" expected'); Destroy; Fail; end;

  if not TokenizeToken(Command, 'DECAY') then begin Output.Report(400, Command, rc, 'DECAY expected'); Destroy; Fail; end;
  if not TokenizeValue(Command, Decay) then begin Output.Report(400, Command, rc, 'DECAY "1.0" expected'); Destroy; Fail; end;

  if not TokenizeToken(Command, 'RATIO') then begin Output.Report(400, Command, rc, 'RATIO expected'); Destroy; Fail; end;
  if not TokenizeValue(Command, Ratio) then begin Output.Report(400, Command, rc, 'RATIO "4.0" expected'); Destroy; Fail; end;

  if not TokenizeToken(Command, 'GAIN') then begin Output.Report(400, Command, rc, 'GAIN expected'); Destroy; Fail; end;
  if not TokenizeValue(Command, Gain) then begin Output.Report(400, Command, rc, 'GAIN "0.0" expected'); Destroy; Fail; end;

  Compressor.Enabled:=Enabled;
  Compressor.SampleSize:=1152;
  Compressor.ThresholdDB:=StrToFloatDef(Treshold, -12.0);
  Compressor.AttackTime:=StrToFloatDef(Attack, 0.05);
  Compressor.DecayTime:=StrToFloatDef(Decay, 1.0);
  Compressor.Ratio:=StrToFloatDef(Ratio, 4.0);
  Compressor.GainDB:=StrToFloatDef(Gain, 0.0);
 end;

procedure TMixerFilterCompressor.Reset;
 begin
 end;

procedure TMixerFilterCompressor.Perform(var Buffer: TMixerBuffer; var UnitedBuffer: TUnitedMixerBuffer);
 begin
  if (Compressor <> nil) and Enabled then
   Compressor.Process(@UnitedBuffer, Buffer.Samples * 4, 44100, 16, 2, False);
 end;

function TMixerFilterCompressor.HaveParameter(const Parameter: String): Boolean;
 begin
  if Parameter = 'THRESHOLD' then Exit(True)
  else if Parameter = 'ATTACK' then Exit(True)
  else if Parameter = 'DECAY' then Exit(True)
  else if Parameter = 'RATIO' then Exit(True)
  else if Parameter = 'GAIN' then Exit(True)
  else Exit(inherited HaveParameter(Parameter));
 end;

function TMixerFilterCompressor.GetParameter(const Parameter: String): Double;
 begin
  if Parameter = 'THRESHOLD' then Exit(Compressor.ThresholdDB)
  else if Parameter = 'ATTACK' then Exit(Compressor.AttackTime)
  else if Parameter = 'DECAY' then Exit(Compressor.DecayTime)
  else if Parameter = 'RATIO' then Exit(Compressor.Ratio)
  else if Parameter = 'GAIN' then Exit(Compressor.GainDB)
  else Exit(inherited GetParameter(Parameter));
 end;

procedure TMixerFilterCompressor.SetParameter(const Parameter: String; const AValue: Double);
 begin
  if Parameter = 'THRESHOLD' then Compressor.ThresholdDB:=AValue
  else if Parameter = 'ATTACK' then Compressor.AttackTime:=AValue
  else if Parameter = 'DECAY' then Compressor.DecayTime:=AValue
  else if Parameter = 'RATIO' then Compressor.Ratio:=AValue
  else if Parameter = 'GAIN' then Compressor.GainDB:=AValue
  else inherited SetParameter(Parameter, AValue);
 end;

function TMixerFilterCompressor.ExplainStatus: String;
 begin
  Result:='type=compressor' +
          inherited ExplainStatus + 
          ', threshold=' + FloatToStrF(Compressor.ThresholdDB, ffFixed, 15, 2) +
          ', attack=' + FloatToStrF(Compressor.AttackTime, ffFixed, 15, 2) +
          ', decay=' + FloatToStrF(Compressor.DecayTime, ffFixed, 15, 2) +
          ', ratio=' + FloatToStrF(Compressor.Ratio, ffFixed, 15, 2) +
          ', gain=' + FloatToStrF(Compressor.GainDB, ffFixed, 15, 2);
 end;

destructor TMixerFilterCompressor.Destroy;
 begin
  if Compressor <> nil then
   Compressor.Free;

  inherited Destroy;
 end;

// TMixerFilterEqualizer: ENABLED "1" BANDS "16" VALUES "-5,1,12,10,8,6,4,2,0,-4,-8,-12,-16,-18,-20,-22"

constructor TMixerFilterEqualizer.Create(const AID: String; var Command, rc: String; Output: TSocketStream);
 var
  Bands, Values: String;
  List: TStringList;
  K: Longint;
 begin
  inherited Create(AID, Command, rc, Output);

  Eq:=TDCEqualizer.Create(nil);

  if not TokenizeToken(Command, 'BANDS') then begin Output.Report(400, Command, rc, 'BANDS expected'); Destroy; Fail; end;
  if not TokenizeValue(Command, Bands) then begin Output.Report(400, Command, rc, 'BANDS "4/8/16/32" expected'); Destroy; Fail; end;

  if not TokenizeToken(Command, 'VALUES') then begin Output.Report(400, Command, rc, 'VALUES expected'); Destroy; Fail; end;
  if not TokenizeValue(Command, Values) then begin Output.Report(400, Command, rc, 'VALUES "0,0,0..." expected'); Destroy; Fail; end;

  Eq.Enabled:=Enabled;
  Eq.Seperate:=False;
  Eq.SampleSize:=1152;

  if Bands = '4' then Eq.FFTSize:=fts8
  else if Bands = '8' then Eq.FFTSize:=fts16
  else if Bands = '16' then Eq.FFTSize:=fts32
  else if Bands = '32' then Eq.FFTSize:=fts64
  else begin Output.Report(400, Command, rc, 'Only 4/8/16/32 BANDS supported'); Destroy; Fail; end;

  Self.Bands:=StrToIntDef(Bands, 0);

  List:=TStringList.Create;

  SplitRegExpr('[ ,]+', Values, List);

  for K:=0 to List.Count - 1 do
   Eq.Band[0, K]:=StrToIntDef(List[K], 0);

  List.Free;
 end;

procedure TMixerFilterEqualizer.Reset;
 begin
  if Eq <> nil then
   Eq.Flush;
 end;

procedure TMixerFilterEqualizer.Perform(var Buffer: TMixerBuffer; var UnitedBuffer: TUnitedMixerBuffer);
 begin
  if (Eq <> nil) and Enabled then
   begin
    try
     Eq.Process(@UnitedBuffer, Buffer.Samples * 4, 16, 2, False);
    except
     on E: Exception do Log('TMixerFilterEqualizer.Perform: exception ' + E.Message);
    end;
   end;
 end;

function TMixerFilterEqualizer.HaveParameter(const Parameter: String): Boolean;
 begin
       if Parameter = 'BAND0' then Exit(True)
  else if Parameter = 'BAND1' then Exit(True)
  else if Parameter = 'BAND2' then Exit(True)
  else if Parameter = 'BAND3' then Exit(True)
  else if Parameter = 'BAND4' then Exit(True)
  else if Parameter = 'BAND5' then Exit(True)
  else if Parameter = 'BAND6' then Exit(True)
  else if Parameter = 'BAND7' then Exit(True)
  else if Parameter = 'BAND8' then Exit(True)
  else if Parameter = 'BAND9' then Exit(True)
  else if Parameter = 'BAND10' then Exit(True)
  else if Parameter = 'BAND11' then Exit(True)
  else if Parameter = 'BAND12' then Exit(True)
  else if Parameter = 'BAND13' then Exit(True)
  else if Parameter = 'BAND14' then Exit(True)
  else if Parameter = 'BAND15' then Exit(True)
  else if Parameter = 'BAND16' then Exit(True)
  else if Parameter = 'BAND17' then Exit(True)
  else if Parameter = 'BAND18' then Exit(True)
  else if Parameter = 'BAND19' then Exit(True)
  else if Parameter = 'BAND20' then Exit(True)
  else if Parameter = 'BAND21' then Exit(True)
  else if Parameter = 'BAND22' then Exit(True)
  else if Parameter = 'BAND23' then Exit(True)
  else if Parameter = 'BAND24' then Exit(True)
  else if Parameter = 'BAND25' then Exit(True)
  else if Parameter = 'BAND26' then Exit(True)
  else if Parameter = 'BAND27' then Exit(True)
  else if Parameter = 'BAND28' then Exit(True)
  else if Parameter = 'BAND29' then Exit(True)
  else if Parameter = 'BAND30' then Exit(True)
  else Exit(inherited HaveParameter(Parameter));
 end;

function TMixerFilterEqualizer.GetParameter(const Parameter: String): Double;
 begin
       if Parameter = 'BAND0' then Exit(Eq.Band[0, 0])
  else if Parameter = 'BAND1' then Exit(Eq.Band[0, 1])
  else if Parameter = 'BAND2' then Exit(Eq.Band[0, 2])
  else if Parameter = 'BAND3' then Exit(Eq.Band[0, 3])
  else if Parameter = 'BAND4' then Exit(Eq.Band[0, 4])
  else if Parameter = 'BAND5' then Exit(Eq.Band[0, 5])
  else if Parameter = 'BAND6' then Exit(Eq.Band[0, 6])
  else if Parameter = 'BAND7' then Exit(Eq.Band[0, 7])
  else if Parameter = 'BAND8' then Exit(Eq.Band[0, 8])
  else if Parameter = 'BAND9' then Exit(Eq.Band[0, 9])
  else if Parameter = 'BAND10' then Exit(Eq.Band[0, 10])
  else if Parameter = 'BAND11' then Exit(Eq.Band[0, 11])
  else if Parameter = 'BAND12' then Exit(Eq.Band[0, 12])
  else if Parameter = 'BAND13' then Exit(Eq.Band[0, 13])
  else if Parameter = 'BAND14' then Exit(Eq.Band[0, 14])
  else if Parameter = 'BAND15' then Exit(Eq.Band[0, 15])
  else if Parameter = 'BAND16' then Exit(Eq.Band[0, 16])
  else if Parameter = 'BAND17' then Exit(Eq.Band[0, 17])
  else if Parameter = 'BAND18' then Exit(Eq.Band[0, 18])
  else if Parameter = 'BAND19' then Exit(Eq.Band[0, 19])
  else if Parameter = 'BAND20' then Exit(Eq.Band[0, 20])
  else if Parameter = 'BAND21' then Exit(Eq.Band[0, 21])
  else if Parameter = 'BAND22' then Exit(Eq.Band[0, 22])
  else if Parameter = 'BAND23' then Exit(Eq.Band[0, 23])
  else if Parameter = 'BAND24' then Exit(Eq.Band[0, 24])
  else if Parameter = 'BAND25' then Exit(Eq.Band[0, 25])
  else if Parameter = 'BAND26' then Exit(Eq.Band[0, 26])
  else if Parameter = 'BAND27' then Exit(Eq.Band[0, 27])
  else if Parameter = 'BAND28' then Exit(Eq.Band[0, 28])
  else if Parameter = 'BAND29' then Exit(Eq.Band[0, 29])
  else if Parameter = 'BAND30' then Exit(Eq.Band[0, 30])
  else Exit(inherited GetParameter(Parameter));
 end;

procedure TMixerFilterEqualizer.SetParameter(const Parameter: String; const AValue: Double);
 begin
       if Parameter = 'BAND0' then Eq.Band[0, 0]:=Trunc(AValue)
  else if Parameter = 'BAND1' then Eq.Band[0, 1]:=Trunc(AValue)
  else if Parameter = 'BAND2' then Eq.Band[0, 2]:=Trunc(AValue)
  else if Parameter = 'BAND3' then Eq.Band[0, 3]:=Trunc(AValue)
  else if Parameter = 'BAND4' then Eq.Band[0, 4]:=Trunc(AValue)
  else if Parameter = 'BAND5' then Eq.Band[0, 5]:=Trunc(AValue)
  else if Parameter = 'BAND6' then Eq.Band[0, 6]:=Trunc(AValue)
  else if Parameter = 'BAND7' then Eq.Band[0, 7]:=Trunc(AValue)
  else if Parameter = 'BAND8' then Eq.Band[0, 8]:=Trunc(AValue)
  else if Parameter = 'BAND9' then Eq.Band[0, 9]:=Trunc(AValue)
  else if Parameter = 'BAND10' then Eq.Band[0, 10]:=Trunc(AValue)
  else if Parameter = 'BAND11' then Eq.Band[0, 11]:=Trunc(AValue)
  else if Parameter = 'BAND12' then Eq.Band[0, 12]:=Trunc(AValue)
  else if Parameter = 'BAND13' then Eq.Band[0, 13]:=Trunc(AValue)
  else if Parameter = 'BAND14' then Eq.Band[0, 14]:=Trunc(AValue)
  else if Parameter = 'BAND15' then Eq.Band[0, 15]:=Trunc(AValue)
  else if Parameter = 'BAND16' then Eq.Band[0, 16]:=Trunc(AValue)
  else if Parameter = 'BAND17' then Eq.Band[0, 17]:=Trunc(AValue)
  else if Parameter = 'BAND18' then Eq.Band[0, 18]:=Trunc(AValue)
  else if Parameter = 'BAND19' then Eq.Band[0, 19]:=Trunc(AValue)
  else if Parameter = 'BAND20' then Eq.Band[0, 20]:=Trunc(AValue)
  else if Parameter = 'BAND21' then Eq.Band[0, 21]:=Trunc(AValue)
  else if Parameter = 'BAND22' then Eq.Band[0, 22]:=Trunc(AValue)
  else if Parameter = 'BAND23' then Eq.Band[0, 23]:=Trunc(AValue)
  else if Parameter = 'BAND24' then Eq.Band[0, 24]:=Trunc(AValue)
  else if Parameter = 'BAND25' then Eq.Band[0, 25]:=Trunc(AValue)
  else if Parameter = 'BAND26' then Eq.Band[0, 26]:=Trunc(AValue)
  else if Parameter = 'BAND27' then Eq.Band[0, 27]:=Trunc(AValue)
  else if Parameter = 'BAND28' then Eq.Band[0, 28]:=Trunc(AValue)
  else if Parameter = 'BAND29' then Eq.Band[0, 29]:=Trunc(AValue)
  else if Parameter = 'BAND30' then Eq.Band[0, 30]:=Trunc(AValue)
  else inherited SetParameter(Parameter, AValue);
 end;

function TMixerFilterEqualizer.ExplainStatus: String;
 var
  K: Integer;
 begin
  Result:='type=equalizer' +
          inherited ExplainStatus + 
          ', bands=' + IntToStr(Bands) +
          ', values=';

  for K:=0 to Bands - 1 do
   if K = 0 then
    Result:=Result + IntToStr(Eq.Band[0, K])
   else
    Result:=Result + ',' + IntToStr(Eq.Band[0, K]);
 end;

destructor TMixerFilterEqualizer.Destroy;
 begin
  if Eq <> nil then
   Eq.Free;

  inherited Destroy;
 end;

// TMixerFilterTrueBass: ENABLED "1" FREQUENCY "300" VOLUME "125"

constructor TMixerFilterTrueBass.Create(const AID: String; var Command, rc: String; Output: TSocketStream);
 var
  Freq, Vol: String;
 begin
  inherited Create(AID, Command, rc, Output);

  TrueBass:=TDCTrueBass.Create(nil);

  if not TokenizeToken(Command, 'FREQUENCY') then
   begin
    Output.Report(400, Command, rc, 'FREQUENCY expected');
    Destroy;
    Fail;
   end;

  if not TokenizeValue(Command, Freq) then 
   begin
    Output.Report(400, Command, rc, 'FREQUENCY "16000" expected');
    Destroy;
    Fail;
   end;

  if not TokenizeToken(Command, 'VOLUME') then
   begin
    Output.Report(400, Command, rc, 'VOLUME expected');
    Destroy;
    Fail;
   end;

  if not TokenizeValue(Command, Vol) then 
   begin
    Output.Report(400, Command, rc, 'VOLUME "100" expected');
    Destroy;
    Fail;
   end;

  TrueBass.Enabled:=Enabled;
  TrueBass.Seperate:=False;
  TrueBass.SampleSize:=1152;
  TrueBass.Frequency:=StrToIntDef(Freq, 300);
  TrueBass.Volume[0]:=StrToIntDef(Vol, 0);

  TrueBass.Init(44100);
 end;

procedure TMixerFilterTrueBass.Reset;
 begin
  if TrueBass <> nil then
   TrueBass.Flush;
 end;

procedure TMixerFilterTrueBass.Perform(var Buffer: TMixerBuffer; var UnitedBuffer: TUnitedMixerBuffer);
 begin
  if (TrueBass <> nil) and Enabled then
   TrueBass.Process(@UnitedBuffer, Buffer.Samples * 4, 44100, 16, 2, False);
 end;

function TMixerFilterTrueBass.HaveParameter(const Parameter: String): Boolean;
 begin
  if Parameter = 'FREQUENCY' then Exit(True)
  else if Parameter = 'VOLUME' then Exit(True)
  else Exit(inherited HaveParameter(Parameter));
 end;

function TMixerFilterTrueBass.GetParameter(const Parameter: String): Double;
 begin
  if Parameter = 'FREQUENCY' then Exit(TrueBass.Frequency)
  else if Parameter = 'VOLUME' then Exit(TrueBass.Volume[0])
  else Exit(inherited GetParameter(Parameter));
 end;

procedure TMixerFilterTrueBass.SetParameter(const Parameter: String; const AValue: Double);
 begin
  if Parameter = 'FREQUENCY' then TrueBass.Frequency:=Trunc(AValue)
  else if Parameter = 'VOLUME' then TrueBass.Volume[0]:=Trunc(AValue)
  else inherited SetParameter(Parameter, AValue);
 end;

function TMixerFilterTrueBass.ExplainStatus: String;
 begin
  Result:='type=truebass' +
          inherited ExplainStatus + 
          ', freq=' + IntToStr(TrueBass.Frequency) +
          ', volume=' + IntToStr(TrueBass.Volume[0]);
 end;

destructor TMixerFilterTrueBass.Destroy;
 begin
  if TrueBass <> nil then
   TrueBass.Free;

  inherited Destroy;
 end;

// TMixerFilterSound3D: ENABLED "1" VOLUME "1000"

constructor TMixerFilterSound3D.Create(const AID: String; var Command, rc: String; Output: TSocketStream);
 var
  Vol: String;
 begin
  inherited Create(AID, Command, rc, Output);

  Sound3D:=TDCSound3D.Create(nil);

  if not TokenizeToken(Command, 'VOLUME') then
   begin
    Output.Report(400, Command, rc, 'VOLUME expected');
    Destroy;
    Fail;
   end;

  if not TokenizeValue(Command, Vol) then 
   begin
    Output.Report(400, Command, rc, 'VOLUME "1000" expected');
    Destroy;
    Fail;
   end;

  Sound3D.Enabled:=Enabled;
  Sound3D.SampleSize:=1152;
  Sound3D.Volume:=StrToIntDef(Vol, 1000);
 end;

procedure TMixerFilterSound3D.Reset;
 begin
 end;

procedure TMixerFilterSound3D.Perform(var Buffer: TMixerBuffer; var UnitedBuffer: TUnitedMixerBuffer);
 begin
  if (Sound3D <> nil) and Enabled then
   Sound3D.Process(@UnitedBuffer, Buffer.Samples * 4, 16, 2, False);
 end;

function TMixerFilterSound3D.HaveParameter(const Parameter: String): Boolean;
 begin
  if Parameter = 'VOLUME' then Exit(True)
  else Exit(inherited HaveParameter(Parameter));
 end;

function TMixerFilterSound3D.GetParameter(const Parameter: String): Double;
 begin
  if Parameter = 'VOLUME' then Exit(Sound3D.Volume)
  else Exit(inherited GetParameter(Parameter));
 end;

procedure TMixerFilterSound3D.SetParameter(const Parameter: String; const AValue: Double);
 begin
  if Parameter = 'VOLUME' then Sound3D.Volume:=Trunc(AValue)
  else inherited SetParameter(Parameter, AValue);
 end;

function TMixerFilterSound3D.ExplainStatus: String;
 begin
  Result:='type=sound3d' +
          inherited ExplainStatus + 
          ', volume=' + IntToStr(Sound3D.Volume);
 end;

destructor TMixerFilterSound3D.Destroy;
 begin
  if Sound3D <> nil then
   Sound3D.Free;

  inherited Destroy;
 end;

// TMixerFilterLowPass: ENABLED "1" CUTOFF "50"

constructor TMixerFilterLowPass.Create(const AID: String; var Command, rc: String; Output: TSocketStream);
 var
  CutOff: String;
 begin
  inherited Create(AID, Command, rc, Output);

  LowPass:=TDCLowPass.Create(nil);

  if not TokenizeToken(Command, 'CUTOFF') then
   begin
    Output.Report(400, Command, rc, 'CUTOFF expected');
    Destroy;
    Fail;
   end;

  if not TokenizeValue(Command, CutOff) then 
   begin
    Output.Report(400, Command, rc, 'CUTOFF "1000" expected');
    Destroy;
    Fail;
   end;

  LowPass.Enabled:=Enabled;
  LowPass.Seperate:=False;
  LowPass.SampleSize:=1152;
  LowPass.CutOff[0]:=StrToIntDef(CutOff, 50);
 end;

procedure TMixerFilterLowPass.Reset;
 begin
  if LowPass <> nil then
   LowPass.Flush;
 end;

procedure TMixerFilterLowPass.Perform(var Buffer: TMixerBuffer; var UnitedBuffer: TUnitedMixerBuffer);
 begin
  if (LowPass <> nil) and Enabled then
   LowPass.Process(@UnitedBuffer, Buffer.Samples * 4, 44100, 16, 2, False);
 end;

function TMixerFilterLowPass.HaveParameter(const Parameter: String): Boolean;
 begin
  if Parameter = 'CUTOFF' then Exit(True)
  else Exit(inherited HaveParameter(Parameter));
 end;

function TMixerFilterLowPass.GetParameter(const Parameter: String): Double;
 begin
  if Parameter = 'CUTOFF' then Exit(LowPass.CutOff[0])
  else Exit(inherited GetParameter(Parameter));
 end;

procedure TMixerFilterLowPass.SetParameter(const Parameter: String; const AValue: Double);
 begin
  if Parameter = 'CUTOFF' then LowPass.CutOff[0]:=Trunc(AValue)
  else inherited SetParameter(Parameter, AValue);
 end;

function TMixerFilterLowPass.ExplainStatus: String;
 begin
  Result:='type=lowpass' +
          inherited ExplainStatus + 
          ', cutoff=' + IntToStr(LowPass.CutOff[0]);
 end;

destructor TMixerFilterLowPass.Destroy;
 begin
  if LowPass <> nil then
   LowPass.Free;

  inherited Destroy;
 end;

// TMixerFilterHighPass: ENABLED "1" CUTOFF "10000"

constructor TMixerFilterHighPass.Create(const AID: String; var Command, rc: String; Output: TSocketStream);
 var
  CutOff: String;
 begin
  inherited Create(AID, Command, rc, Output);

  HighPass:=TDCHighPass.Create(nil);

  if not TokenizeToken(Command, 'CUTOFF') then
   begin
    Output.Report(400, Command, rc, 'CUTOFF expected');
    Destroy;
    Fail;
   end;

  if not TokenizeValue(Command, CutOff) then 
   begin
    Output.Report(400, Command, rc, 'CUTOFF "10000" expected');
    Destroy;
    Fail;
   end;

  HighPass.Enabled:=Enabled;
  HighPass.Seperate:=False;
  HighPass.SampleSize:=1152;
  HighPass.CutOff[0]:=StrToIntDef(CutOff, 10000);
 end;

procedure TMixerFilterHighPass.Reset;
 begin
  if HighPass <> nil then
   HighPass.Flush;
 end;

procedure TMixerFilterHighPass.Perform(var Buffer: TMixerBuffer; var UnitedBuffer: TUnitedMixerBuffer);
 begin
  if (HighPass <> nil) and Enabled then
   HighPass.Process(@UnitedBuffer, Buffer.Samples * 4, 44100, 16, 2, False);
 end;

function TMixerFilterHighPass.HaveParameter(const Parameter: String): Boolean;
 begin
  if Parameter = 'CUTOFF' then Exit(True)
  else Exit(inherited HaveParameter(Parameter));
 end;

function TMixerFilterHighPass.GetParameter(const Parameter: String): Double;
 begin
  if Parameter = 'CUTOFF' then Exit(HighPass.CutOff[0])
  else Exit(inherited GetParameter(Parameter));
 end;

procedure TMixerFilterHighPass.SetParameter(const Parameter: String; const AValue: Double);
 begin
  if Parameter = 'CUTOFF' then HighPass.CutOff[0]:=Trunc(AValue)
  else inherited SetParameter(Parameter, AValue);
 end;

function TMixerFilterHighPass.ExplainStatus: String;
 begin
  Result:='type=highpass' +
          inherited ExplainStatus + 
          ', cutoff=' + IntToStr(HighPass.CutOff[0]);
 end;

destructor TMixerFilterHighPass.Destroy;
 begin
  if HighPass <> nil then
   HighPass.Free;

  inherited Destroy;
 end;

// TMixerFilterPhaser: ENABLED "1" DRYWET "128" FEEDBACK "40" STAGES "24" DEPTH "128" STARTPHASE "34" FREQUENCY "1.0"

constructor TMixerFilterPhaser.Create(const AID: String; var Command, rc: String; Output: TSocketStream);
 var
  DryWet, Feedback, Stages, Depth, StartPhase, Frequency: String;
 begin
  inherited Create(AID, Command, rc, Output);

  Phaser:=TDCPhaser.Create(nil);

  if (not TokenizeToken(Command, 'DRYWET')) or (not TokenizeValue(Command, DryWet)) then begin Output.Report(400, Command, rc, 'DRYWET "128" expected'); Destroy; Fail; end;
  if (not TokenizeToken(Command, 'FEEDBACK')) or (not TokenizeValue(Command, Feedback)) then begin Output.Report(400, Command, rc, 'FEEDBACK "40" expected'); Destroy; Fail; end;
  if (not TokenizeToken(Command, 'STAGES')) or (not TokenizeValue(Command, Stages)) then begin Output.Report(400, Command, rc, 'STAGES "24" expected'); Destroy; Fail; end;
  if (not TokenizeToken(Command, 'DEPTH')) or (not TokenizeValue(Command, Depth)) then begin Output.Report(400, Command, rc, 'DEPTH "128" expected'); Destroy; Fail; end;
  if (not TokenizeToken(Command, 'STARTPHASE')) or (not TokenizeValue(Command, StartPhase)) then begin Output.Report(400, Command, rc, 'STARTPHASE "34" expected'); Destroy; Fail; end;
  if (not TokenizeToken(Command, 'FREQUENCY')) or (not TokenizeValue(Command, Frequency)) then begin Output.Report(400, Command, rc, 'FREQUENCY "1.0" expected'); Destroy; Fail; end;

  Phaser.Enabled:=Enabled;
  Phaser.Seperate:=False;
  Phaser.SampleSize:=1152;
  Phaser.DryWetRatio[0]:=StrToIntDef(DryWet, 128);
  Phaser.Feedback[0]:=StrToIntDef(Feedback, 40);
  Phaser.Stages[0]:=StrToIntDef(Stages, 24);
  Phaser.Depth[0]:=StrToIntDef(Depth, 128);
  Phaser.StartPhase[0]:=StrToFloatDef(StartPhase, 34);
  Phaser.Frequency[0]:=StrToFloatDef(Frequency, 1.0);
 end;

procedure TMixerFilterPhaser.Reset;
 begin
  if Phaser <> nil then
   Phaser.Flush;
 end;

procedure TMixerFilterPhaser.Perform(var Buffer: TMixerBuffer; var UnitedBuffer: TUnitedMixerBuffer);
 begin
  if (Phaser <> nil) and Enabled then
   Phaser.Process(@UnitedBuffer, Buffer.Samples * 4, 44100, 16, 2, False);
 end;

function TMixerFilterPhaser.HaveParameter(const Parameter: String): Boolean;
 begin
  if Parameter = 'DRYWET' then Exit(True)
  else if Parameter = 'FEEDBACK' then Exit(True)
  else if Parameter = 'STAGES' then Exit(True)
  else if Parameter = 'DEPTH' then Exit(True)
  else if Parameter = 'STARTPHASE' then Exit(True)
  else if Parameter = 'FREQUENCY' then Exit(True)
  else Exit(inherited HaveParameter(Parameter));
 end;

function TMixerFilterPhaser.GetParameter(const Parameter: String): Double;
 begin
  if Parameter = 'DRYWET' then Exit(Phaser.DryWetRatio[0])
  else if Parameter = 'FEEDBACK' then Exit(Phaser.Feedback[0])
  else if Parameter = 'STAGES' then Exit(Phaser.Stages[0])
  else if Parameter = 'DEPTH' then Exit(Phaser.Depth[0])
  else if Parameter = 'STARTPHASE' then Exit(Phaser.StartPhase[0])
  else if Parameter = 'FREQUENCY' then Exit(Phaser.Frequency[0])
  else Exit(inherited GetParameter(Parameter));
 end;

procedure TMixerFilterPhaser.SetParameter(const Parameter: String; const AValue: Double);
 begin
  if Parameter = 'DRYWET' then Phaser.DryWetRatio[0]:=Trunc(AValue)
  else if Parameter = 'FEEDBACK' then Phaser.Feedback[0]:=Trunc(AValue)
  else if Parameter = 'STAGES' then Phaser.Stages[0]:=Trunc(AValue)
  else if Parameter = 'DEPTH' then Phaser.Depth[0]:=Trunc(AValue)
  else if Parameter = 'STARTPHASE' then Phaser.StartPhase[0]:=AValue
  else if Parameter = 'FREQUENCY' then Phaser.Frequency[0]:=AValue
  else inherited SetParameter(Parameter, AValue);
 end;

function TMixerFilterPhaser.ExplainStatus: String;
 begin
  Result:='type=phaser' +
          inherited ExplainStatus + 
          ', drywet=' + IntToStr(Phaser.DryWetRatio[0]) +
          ', feedback=' + IntToStr(Phaser.Feedback[0]) +
          ', stages=' + IntToStr(Phaser.Stages[0]) +
          ', depth=' + IntToStr(Phaser.Depth[0]) +
          ', startphase=' + FloatToStrF(Phaser.StartPhase[0], ffFixed, 15, 2) +
          ', frequency=' + FloatToStrF(Phaser.Frequency[0], ffFixed, 15, 2);
 end;

destructor TMixerFilterPhaser.Destroy;
 begin
  if Phaser <> nil then
   Phaser.Free;

  inherited Destroy;
 end;

// TMixerFilterFlanger: ENABLED "1" LEFT FREQUENCY "0.125" DELAY "0.005" PHASEINVERT "1"
//                                  RIGHT FREQUENCY "0.125" DELAY "0.005" PHASEINVERT "1"

constructor TMixerFilterFlanger.Create(const AID: String; var Command, rc: String; Output: TSocketStream);
 var
  Frequency1, Delay1, PhaseInvert1, 
  Frequency2, Delay2, PhaseInvert2: String;
 begin
  inherited Create(AID, Command, rc, Output);

  Flanger:=TDCFlanger.Create(nil);

  if not TokenizeToken(Command, 'LEFT') then begin Output.Report(400, Command, rc, 'LEFT expected'); Destroy; Fail; end;
  if (not TokenizeToken(Command, 'FREQUENCY')) or (not TokenizeValue(Command, Frequency1)) then begin Output.Report(400, Command, rc, 'FREQUENCY "0.125" expected'); Destroy; Fail; end;
  if (not TokenizeToken(Command, 'DELAY')) or (not TokenizeValue(Command, Delay1)) then begin Output.Report(400, Command, rc, 'DELAY "0.005" expected'); Destroy; Fail; end;
  if (not TokenizeToken(Command, 'PHASEINVERT')) or (not TokenizeValue(Command, PhaseInvert1)) then begin Output.Report(400, Command, rc, 'PHASEINVERT "1" expected'); Destroy; Fail; end;

  if not TokenizeToken(Command, 'RIGHT') then begin Output.Report(400, Command, rc, 'RIGHT expected'); Destroy; Fail; end;
  if (not TokenizeToken(Command, 'FREQUENCY')) or (not TokenizeValue(Command, Frequency2)) then begin Output.Report(400, Command, rc, 'FREQUENCY "0.125" expected'); Destroy; Fail; end;
  if (not TokenizeToken(Command, 'DELAY')) or (not TokenizeValue(Command, Delay2)) then begin Output.Report(400, Command, rc, 'DELAY "0.005" expected'); Destroy; Fail; end;
  if (not TokenizeToken(Command, 'PHASEINVERT')) or (not TokenizeValue(Command, PhaseInvert2)) then begin Output.Report(400, Command, rc, 'PHASEINVERT "1" expected'); Destroy; Fail; end;

  Flanger.Enabled:=Enabled;
  Flanger.Seperate:=True;
  Flanger.SampleSize:=1152;

  Flanger.Frequency[0]:=StrToFloatDef(Frequency1, 0.125);
  Flanger.Delay[0]:=StrToFloatDef(Delay1, 0.005);
  Flanger.PhaseInvert[0]:=StrToFloatDef(PhaseInvert1, 0.0) > 0.0;

  Flanger.Frequency[1]:=StrToFloatDef(Frequency2, 0.125);
  Flanger.Delay[1]:=StrToFloatDef(Delay2, 0.005);
  Flanger.PhaseInvert[1]:=StrToFloatDef(PhaseInvert2, 0.0) > 0.0;

  Flanger.Init(2, 44100, 2);
 end;

procedure TMixerFilterFlanger.Reset;
 begin
  if Flanger <> nil then
   Flanger.Flush;
 end;

procedure TMixerFilterFlanger.Perform(var Buffer: TMixerBuffer; var UnitedBuffer: TUnitedMixerBuffer);
 begin
  if (Flanger <> nil) and Enabled then
   Flanger.Process(@UnitedBuffer, Buffer.Samples * 4, 16, False);
 end;

function TMixerFilterFlanger.HaveParameter(const Parameter: String): Boolean;
 begin
  if Parameter = 'LEFTFREQUENCY' then Exit(True)
  else if Parameter = 'LEFTDELAY' then Exit(True)
  else if Parameter = 'LEFTPHASEINVERT' then Exit(True)
  else if Parameter = 'RIGHTFREQUENCY' then Exit(True)
  else if Parameter = 'RIGHTDELAY' then Exit(True)
  else if Parameter = 'RIGHTPHASEINVERT' then Exit(True)
  else Exit(inherited HaveParameter(Parameter));
 end;

function TMixerFilterFlanger.GetParameter(const Parameter: String): Double;
 begin
  if Parameter = 'LEFTFREQUENCY' then Exit(Flanger.Frequency[0])
  else if Parameter = 'LEFTDELAY' then Exit(Flanger.Delay[0])
  else if Parameter = 'LEFTPHASEINVERT' then Exit(Integer(Flanger.PhaseInvert[0]))
  else if Parameter = 'RIGHTFREQUENCY' then Exit(Flanger.Frequency[1])
  else if Parameter = 'RIGHTDELAY' then Exit(Flanger.Delay[1])
  else if Parameter = 'RIGHTPHASEINVERT' then Exit(Integer(Flanger.PhaseInvert[1]))
  else Exit(inherited GetParameter(Parameter));
 end;

procedure TMixerFilterFlanger.SetParameter(const Parameter: String; const AValue: Double);
 begin
  if Parameter = 'LEFTFREQUENCY' then Flanger.Frequency[0]:=AValue
  else if Parameter = 'LEFTDELAY' then Flanger.Delay[0]:=AValue
  else if Parameter = 'LEFTPHASEINVERT' then Flanger.PhaseInvert[0]:=AValue > 0.0
  else if Parameter = 'RIGHTFREQUENCY' then Flanger.Frequency[1]:=Trunc(AValue)
  else if Parameter = 'RIGHTDELAY' then Flanger.Delay[1]:=AValue
  else if Parameter = 'RIGHTPHASEINVERT' then Flanger.PhaseInvert[1]:=AValue > 0.0
  else inherited SetParameter(Parameter, AValue);
 end;

function TMixerFilterFlanger.ExplainStatus: String;
 begin
  Result:='type=flanger' +
          inherited ExplainStatus + 
          ', leftfrequency=' + FloatToStrF(Flanger.Frequency[0], ffFixed, 15, 4) +
          ', leftdelay=' + FloatToStrF(Flanger.Delay[0], ffFixed, 15, 4) +
          ', leftphaseinvert=' + IntToStr(Integer(Flanger.PhaseInvert[0])) +
          ', rightfrequency=' + FloatToStrF(Flanger.Frequency[1], ffFixed, 15, 4) +
          ', rightdelay=' + FloatToStrF(Flanger.Delay[1], ffFixed, 15, 4) +
          ', rightphaseinvert=' + IntToStr(Integer(Flanger.PhaseInvert[1]));
 end;

destructor TMixerFilterFlanger.Destroy;
 begin
  if Flanger <> nil then
   Flanger.Free;

  inherited Destroy;
 end;

// TMixerFilterDynAmp: ENABLED "1" ATTACK "1000" RELEASE "3000" AMPLIFY "10000"

constructor TMixerFilterDynAmp.Create(const AID: String; var Command, rc: String; Output: TSocketStream);
 var
  AttackTime, ReleaseTime, MaxAmp: String;
 begin
  inherited Create(AID, Command, rc, Output);

  DynAmp:=TDCDynamicAmplify.Create(nil);

  if (not TokenizeToken(Command, 'ATTACK')) or (not TokenizeValue(Command, AttackTime)) then begin Output.Report(400, Command, rc, 'ATTACK "1000" expected'); Destroy; Fail; end;
  if (not TokenizeToken(Command, 'RELEASE')) or (not TokenizeValue(Command, ReleaseTime)) then begin Output.Report(400, Command, rc, 'RELEASE "3000" expected'); Destroy; Fail; end;
  if (not TokenizeToken(Command, 'AMPLIFY')) or (not TokenizeValue(Command, MaxAmp)) then begin Output.Report(400, Command, rc, 'AMPLIFY "10000" expected'); Destroy; Fail; end;

  DynAmp.Enabled:=Enabled;
  DynAmp.SampleSize:=1152;

  DynAmp.AttackTime:=StrToIntDef(AttackTime, 1000);
  DynAmp.ReleaseTime:=StrToIntDef(ReleaseTime, 3000);
  DynAmp.MaxAmplification:=StrToIntDef(MaxAmp, 10000);
 end;

procedure TMixerFilterDynAmp.Reset;
 begin
 end;

procedure TMixerFilterDynAmp.Perform(var Buffer: TMixerBuffer; var UnitedBuffer: TUnitedMixerBuffer);
 begin
  if (DynAmp <> nil) and Enabled then
   try
    DynAmp.Process(@UnitedBuffer, Buffer.Samples * 4, 44100, 16, 2, False);
   except
   end;
 end;

function TMixerFilterDynAmp.HaveParameter(const Parameter: String): Boolean;
 begin
  if Parameter = 'ATTACK' then Exit(True)
  else if Parameter = 'RELEASE' then Exit(True)
  else if Parameter = 'AMPLIFY' then Exit(True)
  else Exit(inherited HaveParameter(Parameter));
 end;

function TMixerFilterDynAmp.GetParameter(const Parameter: String): Double;
 begin
  if Parameter = 'ATTACK' then Exit(DynAmp.AttackTime)
  else if Parameter = 'RELEASE' then Exit(DynAmp.ReleaseTime)
  else if Parameter = 'AMPLIFY' then Exit(DynAmp.MaxAmplification)
  else Exit(inherited GetParameter(Parameter));
 end;

procedure TMixerFilterDynAmp.SetParameter(const Parameter: String; const AValue: Double);
 begin
  if Parameter = 'ATTACK' then DynAmp.AttackTime:=Trunc(AValue)
  else if Parameter = 'RELEASE' then DynAmp.ReleaseTime:=Trunc(AValue)
  else if Parameter = 'AMPLIFY' then DynAmp.MaxAmplification:=Trunc(AValue)
  else inherited SetParameter(Parameter, AValue);
 end;

function TMixerFilterDynAmp.ExplainStatus: String;
 begin
  Result:='type=dynamp' +
          inherited ExplainStatus + 
          ', attack=' + IntToStr(DynAmp.AttackTime) +
          ', release=' + IntToStr(DynAmp.ReleaseTime) +
          ', amplify=' + IntToStr(DynAmp.MaxAmplification);
 end;

destructor TMixerFilterDynAmp.Destroy;
 begin
  if DynAmp <> nil then
   DynAmp.Free;

  inherited Destroy;
 end;

// TMixerFiltersPool

constructor TMixerFiltersPool.Create;
 begin
  inherited Create;

  InitCriticalSection(Lock, 'TMixerFiltersPool.Create');

  SetLength(Filters, 0);
 end;

procedure TMixerFiltersPool.Add(Filter: TMixerFilter);
 begin
  Remove(Filter);

  EnterCriticalSection(Lock, 'TMixerFiltersPool.Add');

  SetLength(Filters, Length(Filters) + 1);
  Filters[Length(Filters) - 1]:=Filter;

  Inc(Filter.Active);

  LeaveCriticalSection(Lock, 'TMixerFiltersPool.Add');
 end;

function TMixerFiltersPool.Remove(Filter: TMixerFilter): Boolean;
 var
  Got, K: Longint;
 begin
  EnterCriticalSection(Lock, 'TMixerFiltersPool.Remove');

  Got:=-1;

  for K:=0 to Length(Filters) - 1 do
   if Filters[K] = Filter then
    begin
     Got:=K;
     Break;
    end;

  if Got = -1 then
   Result:=False
  else
   begin
    Dec(Filters[K].Active);

    for K:=Got + 1 to Length(Filters) - 1 do
     Filters[K - 1]:=Filters[K];

    SetLength(Filters, Length(Filters) - 1);

    Result:=True;
   end;

  LeaveCriticalSection(Lock, 'TMixerFiltersPool.Remove');
 end;

procedure TMixerFiltersPool.Perform(var Buffer: TMixerBuffer);
 var
  UnitedBuffer: TVirtualUnitedMixerBuffer;
  K: Longint;
 begin
  EnterCriticalSection(Lock, 'TMixerFiltersPool.Perform');

  if Length(Filters) > 0 then
   begin
    CreateVirtualUnitedMixerBuffer(UnitedBuffer);

    MixerBufferToUnitedBuffer(Buffer, UnitedBuffer.UnitedBuffer^);

    for K:=0 to Length(Filters) - 1 do
     Filters[K].Perform(Buffer, UnitedBuffer.UnitedBuffer^);

    UnitedBufferToMixerBuffer(UnitedBuffer.UnitedBuffer^, Buffer);

    DestroyVirtualUnitedMixerBuffer(UnitedBuffer);
   end;

  LeaveCriticalSection(Lock, 'TMixerFiltersPool.Perform');
 end;

function TMixerFiltersPool.Empty: Boolean;
 begin
  EnterCriticalSection(Lock, 'TMixerFiltersPool.Perform');

  Result:=Length(Filters) = 0;

  LeaveCriticalSection(Lock, 'TMixerFiltersPool.Perform');
 end;

function TMixerFiltersPool.List: String;
 var
  K: Integer;
 begin
  EnterCriticalSection(Lock, 'TMixerFiltersPool.List');

  Result:='';

  for K:=0 to Length(Filters) - 1 do
   if K = 0 then
    Result:=Result + Filters[K].ID
   else
    Result:=Result + ',' + Filters[K].ID;

  LeaveCriticalSection(Lock, 'TMixerFiltersPool.List');
 end;

destructor TMixerFiltersPool.Destroy;
 var
  F: TMixerFilter;
 begin
  while True do
   begin
    EnterCriticalSection(Lock, 'TMixerFiltersPool.Destroy');

    if Length(Filters) > 0 then
     F:=Filters[0]
    else
     F:=nil;

    LeaveCriticalSection(Lock, 'TMixerFiltersPool.Destroy');

    if F = nil then
     Break
    else
     Remove(F);
   end;

  DoneCriticalSection(Lock, 'TMixerFiltersPool.Destroy');

  inherited Destroy;
 end;

end.