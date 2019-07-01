// audinx
// ultimate internet-radio stream maker
//
// (c) by sergey korowkin, 2008-2014

unit MixerSetters;

interface

uses MixerSources, MixerFilters, TimeFixer, Common;

type
 TMixerSetterSourceKind = (sskSource, sskFilter);

 TMixerSetterMode = (smLinear);

 TMixerSetter = class
 public
  SourceKind: TMixerSetterSourceKind;
  Mode: TMixerSetterMode;
  ID, Parameter, Group: String;
  StartedAt, StartAt, FinishAt: TTimeFixer;
  ValueDelayDefined, ValueFromDefined, ValueToDefined, ValueDuringDefined: Boolean;
  ValueDelay, ValueFrom, ValueTo, ValueDuring: Double;
  Finished, First: Boolean;
  constructor Create(const ASourceKind: TMixerSetterSourceKind; const AID, AParameter: String);
  procedure Startup(Now: TTimeFixer); virtual;
  procedure Tick(Now: TTimeFixer); virtual;
  destructor Destroy; override;
 end;

implementation

uses Mixer, Daemon;

constructor TMixerSetter.Create(const ASourceKind: TMixerSetterSourceKind; const AID, AParameter: String);
 begin
  inherited Create;

  SourceKind:=ASourceKind;
  ID:=AID;
  Parameter:=AParameter;
  Mode:=smLinear;

  ValueDelayDefined:=False;
  ValueDelay:=0.0;

  ValueFromDefined:=False;
  ValueFrom:=0.0;

  ValueToDefined:=False;
  ValueTo:=0.0;

  ValueDuringDefined:=False;
  ValueDuring:=0.0;

  Group:='';

  Finished:=False;
  First:=True;
 end;

procedure TMixerSetter.Startup(Now: TTimeFixer);
 begin
  StartedAt:=Now;

  StartAt:=StartedAt;

  if ValueDelayDefined then
   StartAt:=StartAt + Round(ValueDelay * 1000);

  FinishAt:=StartAt;

  if ValueDuringDefined then
   FinishAt:=FinishAt + Round(ValueDuring * 1000);
 end;

procedure TMixerSetter.Tick(Now: TTimeFixer);
 var
  Value: Double;
  Source: TMixerSource;
  Filter: TMixerFilter;
 begin
  if (not Finished) and (Now >= StartAt) then 
   begin
    if First and (not ValueFromDefined) then
     if SourceKind = sskSource then
      begin
       Source:=TheMixer.FindSource(ID, False);

       if Source = nil then 
        Finished:=True
       else
        ValueFrom:=Source.GetParameter(Parameter);
      end
     else if SourceKind = sskFilter then
      begin
       Filter:=TheMixer.FindFilter(ID, False);

       if Filter = nil then 
        Finished:=True
       else
        ValueFrom:=Filter.GetParameter(Parameter);
      end;

    First:=False;

    if not Finished then
     begin
      Value:=CalcValueLinear(ValueFrom, ValueTo, StartAt, Now, FinishAt);

      if SourceKind = sskSource then
       begin
        Source:=TheMixer.FindSource(ID);

        if Source = nil then 
         Finished:=True
        else
         Source.SetParameter(Parameter, Value);
       end
      else if SourceKind = sskFilter then
       begin
        Filter:=TheMixer.FindFilter(ID);

        if Filter = nil then 
         Finished:=True
        else
         Filter.SetParameter(Parameter, Value);
       end;

      if Now >= FinishAt then
       Finished:=True;
     end;
   end;
 end;

destructor TMixerSetter.Destroy; 
 begin
  SetLength(ID, 0);
  SetLength(Parameter, 0);

  inherited Destroy;
 end;

end.