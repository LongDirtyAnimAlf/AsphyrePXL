library TestSQL3;
(*
 * This file is part of Asphyre Framework, also known as Platform eXtended Library (PXL).
 * Copyright (c) 2015 - 2017 Yuriy Kotsarenko. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is
 * distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and limitations under the License.
 *)

{$INCLUDE PXL.Config.inc}

{$INCLUDE Android.Config.inc}

{$I mormot.defines.inc}

uses
  {$I mormot.uses.inc}
  {$ifdef UNIX}
  cwstring, // needed as fallback if ICU is not available
  {$endif UNIX}
  Classes,
  SysUtils,
  Android.AppGlue,
  Android.PThread,
  {$IFDEF ANDROID_DEBUG}
  PXL.Logs,
  {$ENDIF}
  PXL.Classes, PXL.TypeDef, PXL.Types, PXL.Devices, PXL.Canvas, PXL.Images, PXL.Fonts,
  PXL.Providers, PXL.Devices.Android, PXL.ImageFormats, PXL.ImageFormats.FCL,
  Engine.Landscape, Engine.Scrolling,
  mormot.core.base,
  mormot.core.os,
  mormot.core.log,
  mormot.core.test,
  mormot.db.raw.sqlite3, // for the SQLite3 version below
  {$ifdef USEZEOS}
  mormot.db.sql.zeos,
  {$endif USEZEOS}
  mormot.tools.ecc,
  test.core.base,
  test.core.data,
  test.core.crypt,
  test.core.ecc,
  test.core.collections,
  {$ifdef LIBQUICKJSSTATIC}
  test.core.script,
  {$endif LIBQUICKJSSTATIC}
  test.net.proto,
  test.orm.core,
  test.orm.sqlite3,
  test.orm.extdb,
  test.orm.threads,
  test.orm.network,
  test.soa.core,
  test.soa.network;

{ TIntegrationTests }

type
  TIntegrationTests = class(TSynTestsLogged)
  published
    procedure CoreUnits;
    procedure ORM;
    procedure SOA;
  end;

procedure TIntegrationTests.CoreUnits;
begin
  //
  AddCase([
    //TTestCoreBase,
    //TTestCoreProcess
  ]);
  //exit;
  AddCase([
  //
    TTestCoreBase, TTestCoreProcess,
    {$ifdef HASGENERICS} // do-nothing on oldest compilers (e.g. <= Delphi XE7)
    TTestCoreCollections,
    {$endif HASGENERICS}
    TTestCoreCrypto, TTestCoreEcc,
    TTestCoreCompression, TNetworkProtocols
  ]);
end;

procedure TIntegrationTests.ORM;
begin
  //exit;
  AddCase([
    //
    TTestOrmCore, TTestSqliteFile, TTestSqliteFileWAL, TTestSqliteFileMemoryMap,
    TTestSqliteMemory, TTestExternalDatabase,
    TTestClientServerAccess, TTestMultiThreadProcess
  ]);
end;

procedure TIntegrationTests.SOA;
begin
  //exit;
  {$ifdef LIBQUICKJSSTATIC}
  AddCase(TTestCoreScript);
  {$endif LIBQUICKJSSTATIC}
  //exit;
  AddCase([
    //
    TTestServiceOrientedArchitecture,
    TTestBidirectionalRemoteConnection
  ]);
end;

var
  ImageFormatHandler: TCustomImageFormatHandler = nil;

  EngineCanvas: TCustomCanvas = nil;
  EngineImages: TAtlasImages = nil;
  EngineFonts: TBitmapFonts = nil;

  DisplaySize: TPoint2i = (X: 0; Y: 0);

  FontSegoe: Integer = -1;
  ImageTerrain: Integer = -1;

  Landscape: TLandscape = nil;
  DragScroll: TDragScroll = nil;
  MapTileSize: TPoint2i;

  DrawPosTextBase: TPoint2i;

  mORMotResult:TStringList;

  FmyThreadID : TThreadID;
  mORMotThread: pthread_t;

const
  IOBufferLength = 512;

threadvar
  IOBuf : array[0..IOBufferLength] of char;
  IOLen : SizeInt;

type
  TData = record
      List: TStringList;
      {$ifdef CPU64}
      Filler: array [1..8] of char;
      {$else}
      Filler: array [1..12] of char;
      {$endif}
    end;
    PData = ^TData;

var
  IORedirected: boolean;

procedure OutputIOBuffer(Var F: TextRec);
begin
  if (PData(@F.UserData)^.List<>nil) then
  with (PData(@F.UserData)^.List) do
  begin
    pthread_mutex_lock(@Application.AndroidApp^.mutex);
    Add(StrPas(PChar(IOBuf)));
    pthread_mutex_unlock(@Application.AndroidApp^.mutex);
  end;
  IOLen:=0;
end;

procedure IOWrite(Var F: TextRec);
var
  i, len : SizeInt;
  pIOBuf: PAnsiChar;
  pIOLen: ^SizeInt;
Begin
  pIOBuf:=@IOBuf;
  pIOLen:=@IOLen;
  while F.BufPos>0 do
    begin
      begin
        if F.BufPos + pIOLen^ > IOBufferLength then
          len:=IOBufferLength - pIOLen^
        else
          len:=F.BufPos;
        i:=0;
        while i < len do
          begin
            if F.bufptr^[i] in [#10, #13] then
              begin
                pIOBuf[pIOLen^]:=#0;
                OutputIOBuffer(F);
                Inc(i);
                if (i < len) and (F.bufptr^[i - 1] = #13) and (F.bufptr^[i] = #10) then
                  Inc(i);
              end
            else
              begin
                pIOBuf[pIOLen^]:=F.bufptr^[i];
                Inc(pIOLen^);
                Inc(i);
              end;
          end;
        pIOBuf[pIOLen^]:=#0;
      end;
      if pIOLen^ = IOBufferLength then
        OutputIOBuffer(F);
      Dec(F.BufPos, len);
    end;
End;

procedure IOClose(Var F: TextRec);
begin
  if IOLen > 0 then
    OutputIOBuffer(F);
end;

procedure IOOpen(Var F: TextRec);
Begin
  TextRec(F).InOutFunc:=@IOWrite;
  TextRec(F).FlushFunc:=@IOWrite;
  TextRec(F).CloseFunc:=@IOClose;
  IOLen:=0;
End;

procedure RedirectFile(Var T: Text; NewList:TStringList);
begin
  System.Assign(T,'');
  with TextRec(T) do
  begin
    OpenFunc:=@IOOpen;
    PData(@UserData)^.List:= NewList;
  end;
  System.Rewrite(T);
end;

procedure RedirectOutputToSysLog(NewList:TStringList);
begin
  if IORedirected AND Assigned(NewList) then exit;
  if Assigned(NewList) then
  begin
    RedirectFile(Output,NewList);
    RedirectFile(System.StdOut,NewList);
    RedirectFile(ErrOutput,NewList);
    RedirectFile(System.StdErr,NewList);
  end
  else
  begin
    SysInitStdIO;
    //if TTextRec(Output).Handle<>0 then Close(Output);
  end;
  IORedirected:=Assigned(NewList);
end;

function FloatTest(a,b,c,d,e,f,g,h,i,j:double):double;
begin
  result:=a*b*c*d*e*f*g*h*i*j;
end;

function FloatTest2(a,b:double):double;
var
  x:double;
begin
  x:=a+b;
  result:=x*a*b;
end;


function mORMotRunner({%H-}param: Pointer): ptrint;// cdecl;
var
  myText:Text;
  tests: TIntegrationTests;
begin
  Result := 0;
{$IFDEF APPGLUE_DEBUG}
  LOGV('AppGlue: mORMotRunner entered');
{$ENDIF}
  Sleep(2000);

  FillChar({%H-}myText,sizeof(myText),0);
  System.Assign(myText,'');
  Rewrite(myText);
  RedirectFile(myText,mORMotResult);

  tests := TIntegrationTests.Create('mORMot2 on Android');
  try
    //tests.WorkDir := StrPas(Application.AndroidApp^.activity^.externalDataPath)+'/';
    tests.WorkDir := StrPas(Application.AndroidApp^.activity^.internalDataPath)+'/';
    tests.SaveToText(myText);
    tests.Run;
  finally
    tests.Free;
  end;

  if Assigned(param) then Boolean(param^) := true;

  EndThread(0);
end;

procedure ApplicationCreate;
var
  attr: pthread_attr_t;
begin
  Application.PresentationAttributes := [TPresentationAttribute.KeepScreenOn, TPresentationAttribute.FullScreen];
  mORMotResult:=TStringList.Create;

  mORMotResult.Append('Welcome !');
  mORMotResult.Append('This is mORMot2 on Android.');
  mORMotResult.Append('The app will run the self-test.');
  mORMotResult.Append('Stand back.');
  mORMotResult.Append('Be patient.');
  mORMotResult.Append('');
  mORMotResult.Append('');


  //pthread_create(@mORMotThread,nil,@mORMotRunner,nil);

  //pthread_attr_init(@attr);
  //pthread_attr_setdetachstate(@attr, PTHREAD_CREATE_DETACHED);
  //pthread_create(@mORMotThread,@attr,@mORMotRunner,nil);

  //pthread_detach(mORMotThread);

  FmyThreadID := BeginThread(@mORMotRunner);
end;

procedure ApplicationDestroy;
begin
  //RedirectOutputToSysLog(nil);
  if (mORMotThread<>0) then
  begin
    //pthread_kill(mORMotThread, 0);
  end;
  if (FmyThreadID<>0) then
  begin
    //KillThread(FmyThreadID);
    CloseThread(FmyThreadID);
  end;
  mORMotResult.Free;
  //CloseHandle(FmyThreadID);
end;

{$ifndef USELIBCURL}
procedure LoadAndSaveFileFromAsset(const aSource, aTarget: StdString);
var
  InStream: TAssetStream;
  OutStream: TFileStream;
begin
  if FileExists(aTarget) then exit;
  try
    OutStream := TFileStream.Create(aTarget, fmCreate{ or fmShareDenyWrite});
    try
      InStream := TAssetStream.Create(aSource);
      try
        OutStream.CopyFrom(InStream,InStream.Size);
      finally
        InStream.Free;
      end;
    finally
      OutStream.Free
    end;
  except
  end;
end;
{$endif}

procedure CreateResources;
{$ifndef USELIBCURL}
const
  MUSTACHE_SPECS: array[0..4] of TFileName =
    ('interpolation','comments','sections','inverted','partials');
  zendframeworkFileName = 'zendframework.json';
  discogsFileName = 'discogs.json';
var
  i:integer;
  aFile,aPath:string;
{$endif}
begin
  {$ifndef USELIBCURL}
  aPath:=StrPas(Application.AndroidApp^.activity^.externalDataPath)+'/';
  for i := 0 to High(MUSTACHE_SPECS) do
  begin
    aFile := MUSTACHE_SPECS[i]+'.json';
    LoadAndSaveFileFromAsset(aFile,aPath+aFile);
  end;
  LoadAndSaveFileFromAsset(zendframeworkFileName,aPath+zendframeworkFileName);
  LoadAndSaveFileFromAsset(discogsFileName,aPath+discogsFileName);
  {$endif}

  ImageFormatHandler := TFCLImageFormatHandler.Create(Application.ImageFormatManager);

  EngineCanvas := (Application.Provider as TGraphicsDeviceProvider).CreateCanvas(Application);
  if not EngineCanvas.Initialize then
    raise Exception.Create('Failed to initialize PXL Canvas.');

  EngineImages := TAtlasImages.Create(Application);

  //ImageTerrain := EngineImages.AddFromAsset('terrain.png');
  ImageTerrain := EngineImages.AddFromAsset('smallmormot.png');

  if ImageTerrain = -1 then
    raise Exception.Create('Could not load Terrain image.');

  EngineFonts := TBitmapFonts.Create(Application);
  EngineFonts.Canvas := EngineCanvas;

  FontSegoe := EngineFonts.AddFromBinaryAsset('Segoe10.font');
  if FontSegoe = -1 then
    raise Exception.Create('Could not load Tahoma font.');

  Landscape := TLandscape.Create;
  DragScroll := TDragScroll.Create;

  DragScroll.DeviceScale := Application.DisplayScale;
  DragScroll.SetPosition(Point2f(MapTileSize.X * 20.0, MapTileSize.Y * 24.0));

  DrawPosTextBase := (Point2f(8.0, 4.0) * Application.DisplayScale).ToInt;
end;

procedure DestroyResources;
begin
  DragScroll.Free;
  Landscape.Free;
  EngineFonts.Free;
  EngineImages.Free;
  EngineCanvas.Free;          
  ImageFormatHandler.Free;
end;

procedure DeviceChange;
begin
  DisplaySize := Application.ContentRect.Size;

  MapTileSize.X := Round(64.0 * Application.DisplayScale);
  MapTileSize.Y := MapTileSize.X div 2;

  if DragScroll <> nil then
    DragScroll.DeviceScale := Application.DisplayScale;
end;

procedure DrawLandscape;
var
  I, J, DeltaX: Integer;
  ViewPos, IsoPos, DrawPos: TPoint2i;
  Heights: array[0..3] of Integer;
begin
  ViewPos.X := DragScroll.ViewPos.X;
  ViewPos.Y := 0;
  //ViewPos := DragScroll.ViewPos;

  for J := -1 to (DisplaySize.Y div (MapTileSize.Y div 2)) + 20 do
  begin
    IsoPos.Y := (ViewPos.Y div (MapTileSize.Y div 2)) + J;
    DrawPos.Y := (IsoPos.Y * (MapTileSize.Y div 2)) - ViewPos.Y - (MapTileSize.Y div 2);
    DeltaX := ((IsoPos.Y mod 2) * (MapTileSize.X div 2)) - ViewPos.X - (MapTileSize.X div 2);

    for I := -1 to (DisplaySize.X div MapTileSize.X) + 2 do
    begin
      IsoPos.X := (ViewPos.X div MapTileSize.X) + I;
      DrawPos.X := (IsoPos.X * MapTileSize.X) + DeltaX;

      if (IsoPos.X >= 0) and
        (IsoPos.Y >= 0) and
        (IsoPos.X < TLandscape.MapWidth) and
        (IsoPos.Y < TLandscape.MapHeight) and
        (Landscape.Entries[IsoPos.X, IsoPos.Y].Corners[0].Light > 0) and
        (Landscape.Entries[IsoPos.X, IsoPos.Y].Corners[1].Light > 0) and
        (Landscape.Entries[IsoPos.X, IsoPos.Y].Corners[2].Light > 0) and
        (Landscape.Entries[IsoPos.X, IsoPos.Y].Corners[3].Light > 0) then
      begin
        // Premultiply the heights so they are in same scale as tile size.
        Heights[0] := Round(Landscape.GetTileHeightSafe(IsoPos, 0) * Application.DisplayScale);
        Heights[1] := Round(Landscape.GetTileHeightSafe(IsoPos, 1) * Application.DisplayScale);
        Heights[2] := Round(Landscape.GetTileHeightSafe(IsoPos, 2) * Application.DisplayScale);
        Heights[3] := Round(Landscape.GetTileHeightSafe(IsoPos, 3) * Application.DisplayScale);

        EngineCanvas.UseImage(EngineImages[ImageTerrain]);

        EngineCanvas.TexQuad(Quad(
          { Isometric Tile corner positions }
          Point2f(DrawPos.X, (DrawPos.Y + (MapTileSize.Y div 2)) - Heights[0]),
          Point2f(DrawPos.X + (MapTileSize.X div 2), DrawPos.Y - Heights[1]),
          Point2f(DrawPos.X + MapTileSize.X, (DrawPos.Y + (MapTileSize.Y div 2)) - Heights[3]),
          Point2f(DrawPos.X + (MapTileSize.X div 2), (DrawPos.Y + MapTileSize.Y) - Heights[2])),
          { Isometric Tile corner lights }
          ColorRect(
            IntColorGray(Landscape.GetTileLightSafe(IsoPos, 0)),
            IntColorGray(Landscape.GetTileLightSafe(IsoPos, 1)),
            IntColorGray(Landscape.GetTileLightSafe(IsoPos, 3)),
            IntColorGray(Landscape.GetTileLightSafe(IsoPos, 2))));
      end;
    end;
  end;
end;

procedure PaintScreen;
var
  DrawPosText: TPoint2i;
  VertShift: VectorInt;
  LineIndex:integer;
  aLine:string;
  aColor:TColorPair;
begin
  DrawLandscape;

  EngineFonts[FontSegoe].Scale := Application.DisplayScale / 2.0;

  //EngineFonts[FontSegoe].Scale := FloatTest(1,2,3,4,5,6,7,8,9);
  //EngineFonts[FontSegoe].Scale := FloatTest2(1,2);

  VertShift := Round(20.0 * Application.DisplayScale);

  DrawPosTextBase.Y:=Round(DisplaySize.Y*1.2)-DragScroll.ViewPos.Y;

  DrawPosText := DrawPosTextBase;

  for LineIndex:=0 to Pred(mORMotResult.Count) do
  begin
    aLine:=mORMotResult[LineIndex];
    aColor:=ColorPair($FFFFE887, $FF12C312);
    if (Length(aLine)>0) then
    begin
      if (Pos('Total failed',aLine)>0) AND (Pos('Total failed: 0',aLine)<1) then aColor:=ColorPair($FFFFE887, $FFFFA020);
      if (Pos('!',aLine)=1) then
      begin
        if (Pos('All tests passed',aLine)>0) then
          aColor:=ColorPair($FFFFE887, $FF005DFF)
        else
          aColor:=ColorPair($FFFFE887, $FFFF1080);
      end;
      if (aLine[1] in ['1'..'9']) then aColor:=ColorPair($FFFFE887, $FF4EBDD5);
      if (Length(aLine)>1) then if (aLine[2] in ['1'..'9']) then aColor:=ColorPair($FFFFE887, $FF4E5DD5);
      EngineFonts[FontSegoe].DrawText(DrawPosText,UniString(aLine),aColor);
    end;
    Inc(DrawPosText.Y, VertShift);
  end;

  DrawPosText := (Point2f(8.0, 4.0) * Application.DisplayScale).ToInt;

  Inc(DrawPosText.X, Round(DisplaySize.X * 0.7));

  EngineFonts[FontSegoe].DrawText(
    DrawPosText,
    'FPS: ' + UniString(IntToStr(Application.Timer.FrameRate)),
    ColorPair($FFFFE887, $FF005DFF));

  Inc(DrawPosText.Y, VertShift);

  EngineFonts[FontSegoe].DrawText(
    DrawPosText,
    'Tech: ' + UniString(GetFullDeviceTechString(Application)),
    ColorPair($FFE8FFAA, $FFE20382));

  Inc(DrawPosText.Y, VertShift);

  EngineFonts[FontSegoe].DrawText(
    DrawPosText,
    'Scale: ' + UniString(FloatToStr(Application.DisplayScale)),
    ColorPair($FFDAF5FF, $FF4E9DE5));

  EngineFonts[FontSegoe].DrawTextAligned(
    Point2f(DisplaySize.X * 0.5, DisplaySize.Y * 0.95),
    'Touch to drag and scroll.',
    ColorPair($FFFFFFFF, $FF808080), TTextAlignment.Middle, TTextAlignment.Final);
end;

procedure ApplicationBeforeMainLoop;
begin
  //RedirectOutputToSysLog(mORMotResult);
  FmyThreadID := BeginThread(@mORMotRunner);
end;

procedure ApplicationPaint;
begin
  Application.Clear([TClearType.Color], 0);
  if EngineCanvas.BeginScene then
  try
    PaintScreen;
  finally
    EngineCanvas.EndScene;
  end;
end;

procedure ApplicationProcess;
begin
  DragScroll.Update;
  Landscape.AnimateHeights;
end;

procedure HandleTactileEvent(const EventType: TTactileEventType; const EventInfo: TTactileEventInfo);
begin
  case EventType of
    TTactileEventType.TouchDown:
      if EventInfo.PointerIndex = 0 then
        DragScroll.TouchDown(EventInfo.Positions[0]);

    TTactileEventType.TouchMove:
      DragScroll.TouchMove(EventInfo.Positions[0]);

    TTactileEventType.TouchUp:
      if EventInfo.PointerCount < 2 then
        DragScroll.TouchUp(EventInfo.Positions[0]);
  end;
end;

// Android Native Activity export: do not edit.
exports
  ANativeActivity_onCreate;

begin
  // Note that this code is executed in a thread that is different than the main thread used by AppGlue.
  // It is recommended to keep this section as short as possible to avoid any threading conflicts.
{$IFDEF ANDROID_DEBUG}
  LogText('Library Load');
{$ENDIF}

  // Assign user's hooks that will be called by PXL application manager.
  HookApplicationCreate := ApplicationCreate;
  HookApplicationDestroy := ApplicationDestroy;

  //HookApplicationBeforeMainLoop := ApplicationBeforeMainLoop;

  HookApplicationPaint := ApplicationPaint;
  HookApplicationProcess := ApplicationProcess;

  HookApplicationCreateResources := CreateResources;
  HookApplicationDestroyResources := DestroyResources;
  HookApplicationDeviceChange := DeviceChange;

  HookApplicationTactileEvent := HandleTactileEvent;

  // Default Application Entry: do not edit.
  android_main := DefaultApplicationEntry;
end.
