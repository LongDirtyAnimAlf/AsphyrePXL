/// minimal REST server for a list of Persons stored in SQlite3
program RESTBenchmark;

{
  run the Server executable then e.g.
  - ab -n 1000 -c 100 http://localhost:8888/root/abc
      for latency measure (return the current timestamp as a few bytes)
  - ab -n 1000 -c 100 http://localhost:8888/root/xyz
      for bandwidth measure (returns some ORM query as 77KB of JSON)
}
{$ifndef UNIX}
{$APPTYPE CONSOLE}
{$endif}

uses
  {$I SynDprUses.inc}  // use FastMM4 on older Delphi, or set FPC threads
  SysUtils,
  SynCommons,          // framework core
  SynTable,
  SynCrtSock,          // direct access to HTTP server
  SynLog,              // logging features
  mORMot,              // RESTful server & ORM
  mORMotSQLite3,       // SQLite3 engine as ORM core
  SynSQLite3Static,    // staticaly linked SQLite3 engine
  mORMotHttpServer;    // HTTP server for RESTful server

type
  TSQLPerson = class(TSQLRecordNoCase)
  private
    fFirstName: RawUTF8;
    fName: RawUTF8;
    fBirth: TDateTime;
  published
    property Name: RawUTF8 read fName write fName;
    property FirstName: RawUTF8 read fFirstName write fFirstName;
    property Birth: TDateTime read fBirth write fBirth;
  end;

  TMyServices = class(TSynPersistent)
  protected
    fDb: TSQLRestServer;
    procedure PopulateWithRandom;
  public
    constructor Create(aDB: TSQLRestServer); reintroduce;
  published
    procedure abc(ctxt: TSQLRestServerURIContext);
    procedure xyz(ctxt: TSQLRestServerURIContext);
  end;

{ TMyServices }

constructor TMyServices.Create(aDB: TSQLRestServer);
begin
  inherited Create;
  fDB := aDB;
  fDB.ServiceMethodRegisterPublishedMethods('', self); // root/abc and root/xyz
  if not fDb.TableHasRows(TSQLPerson) then
    PopulateWithRandom;
end;

procedure TMyServices.PopulateWithRandom;
var
  aBatch: TSQLRestBatch;
  aPerson: TSQLPerson;
  aTimer: TPrecisionTimer;
  i: integer;
begin
  aTimer.Start;
  aPerson := TSQLPerson.Create;
  try
    aBatch := TSQLRestBatch.Create(fDB, TSQLPerson, 10000);
    try
      for i := 1 to 1000 do begin
        aPerson.Name := FormatUTF8('% name', [CardinalToHexShort(i * 777777)]);
        aPerson.FirstName := FormatUTF8('first %', [i]);
        aPerson.Birth := i + 40000;
        aBatch.Add(aPerson, true);
      end;
      fDb.BatchSend(aBatch);
    finally
      aBatch.Free;
    end;
  finally
    aPerson.Free;
  end;
  writeln('Created 1000 entries in ', aTimer.Stop);
end;

procedure TMyServices.xyz(ctxt: TSQLRestServerURIContext);
var
  s: RawUTF8;
begin
  FormatUTF8('xyz %', [NowUTCToString], s);
  ctxt.Results([s]);
end;

procedure TMyServices.abc(ctxt: TSQLRestServerURIContext);
var
  s: RawUTF8;
begin
  s := fDB.RetrieveListJSON(TSQLPerson, '', '', true);
  ctxt.Returns(s);
end;

procedure DoTest(const url: AnsiString; keepAlive: boolean);
var
  aRestServer: TSQLRestServerDB;
  aHttpServer: TSQLHttpServer;
  aServices: TMyServices;
begin
  // create the main mORMot server
  aRestServer := TSQLRestServerDB.CreateWithOwnModel([TSQLPerson],'test.db',False,'root'); // authentication=false
  try
    // create tables or fields if missing
    aRestServer.CreateMissingTables;
    // initialize the services implementation class
    aServices := TMyServices.Create(aRestServer);
    try
      // serve aRestServer data over HTTP
      aHttpServer := TSQLHttpServer.Create(url,[aRestServer]);
      if not keepAlive and (aHttpServer.HttpServer is THttpServer) then
        THttpServer(aHttpServer.HttpServer).ServerKeepAliveTimeOut := 0;
      try
        aHttpServer.AccessControlAllowOrigin := '*'; // allow cross-site AJAX queries
        write('Background server is running on ', url, ' keepAlive ');
        if (keepAlive) then
          writeLn('is enabled') else
          writeLn('is disabled');
        write('Press [Enter] to close the server.');
        readln;
      finally
        aHttpServer.Free;
      end;
    finally
      aServices.Free;
    end;
  finally
    aRestServer.Free;
  end;
end;

const
  UNIX_SOCK_PATH = '/tmp/rest-bench.socket';

var
  url: AnsiString;
  keepAlive: boolean;

begin
  // set logging abilities
  SQLite3Log.Family.Level := LOG_VERBOSE;
  //SQLite3Log.Family.EchoToConsole := LOG_VERBOSE;
  SQLite3Log.Family.PerThreadLog := ptIdentifiedInOnFile;
  SQLite3Log.Family.NoFile := true; // do not create log files for benchmark
  {$ifdef UNIX}
  if (ParamCount>0) and (ParamStr(1)='unix') then begin
    url := 'unix:' + UNIX_SOCK_PATH;
    if FileExists(UNIX_SOCK_PATH) then
      DeleteFile(UNIX_SOCK_PATH); // remove socket file
  end else
  {$endif}
    url := '8888';
  if (ParamCount>1) and (ParamStr(2)='false') then
    keepAlive := false else
    keepAlive := true;
  DoTest(url, keepAlive);
end.

