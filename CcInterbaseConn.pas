unit CcInterbaseConn;

{$I CC.INC}

interface

uses Classes, Sysutils, DB, CcProviders, CcInterbase;

type

//This is an abstract class, serving as an ancestor for all Interbase/Firebird specific
//connectors, which have a number of properties in common.
TCcInterbaseConnection = class(TCcConnection)
  private
    FTRParams: TStrings;
    procedure SetTRParams(const Value: TStrings);
    function GetCharSet: String;
//\ \ 
    function GetRoleName: String;
    function GetSQLDialect: Integer;
    function GetUserLogin: String;
    function GetUserPassword: String;
    procedure SetCharSet(const Value: String);
//\ \ 
    procedure SetRoleName(const Value: String);
    procedure SetSQLDialect(const Value: Integer);
    procedure SetUserLogin(const Value: String);
    procedure SetUserPassword(const Value: String);
  public
    procedure Assign(Source: TPersistent);override;
    constructor Create(AOwner: TComponent);override;
    destructor Destroy; override;
  published
    //Character set to use when connecting to the database.
    property CharSet: String read GetCharSet write SetCharSet;

    property RoleName: String read GetRoleName write SetRoleName;
    property UserLogin : String read GetUserLogin write SetUserLogin;
    property UserPassword: String read GetUserPassword write SetUserPassword;
    property SQLDialect :Integer read GetSQLDialect write SetSQLDialect default 1;

    property DBType;
    property DBVersion;
    property DBName;// read GetDBName write SetDBName;

    //Transaction parameters to specify when starting a transaction
    property TRParams: TStrings read FTRParams write SetTRParams;
end;

implementation

constructor TCcInterbaseConnection.Create(AOwner: TComponent);
begin
  inherited;
  FTRParams := TStringList.Create;
  FTRParams.Add('write');
  FTRParams.Add('nowait');
  FTRParams.Add('concurrency');

  AddDBAdaptor(TCcInterbaseAdaptor);
  SQLDialect := 1;
end;

destructor TCcInterbaseConnection.Destroy;
begin
   FreeAndNil(FTRParams);
   inherited;
end;

procedure TCcInterbaseConnection.SetTRParams(const Value: TStrings);
begin
  if FTRParams.Text <> Value.Text then begin
    FTRParams.BeginUpdate;
    try
      FTRParams.Assign(Value);
    finally
      FTRParams.EndUpdate;
    end;
  end;
end;

procedure TCcInterbaseConnection.Assign(Source: TPersistent);
begin
  if Source is TCcInterbaseConnection then with TCcInterbaseConnection(Source) do begin
    //Copy all published properties from source to Self
    //The standard TCcConnection properties are handled by our fore-father
    Self.SQLDialect := SQLDialect;
    Self.CharSet := CharSet;
    Self.DBName := DBName;
    Self.RoleName := RoleName;
    Self.UserLogin := UserLogin;
    Self.UserPassword := UserPassword;
    Self.TRParams.Assign(TRParams);
  end;
  inherited;
end;

function TCcInterbaseConnection.GetCharSet: String;
begin
  Result := ConnectionParams.Values['CHARSET'];
end;

//function TCcInterbaseConnection.GetDBName: TFileName;
//begin
//  Result := ConnectionParams.Values['DBNAME'];
//end;

function TCcInterbaseConnection.GetRoleName: String;
begin
  Result := ConnectionParams.Values['ROLE_NAME'];
end;

function TCcInterbaseConnection.GetSQLDialect: Integer;
begin
  Result := StrToIntDef(ConnectionParams.Values['SQLDIALECT'], 1);
end;

function TCcInterbaseConnection.GetUserLogin: String;
begin
  Result := ConnectionParams.Values['USER_NAME'];
end;

function TCcInterbaseConnection.GetUserPassword: String;
begin
  Result := ConnectionParams.Values['PASSWORD'];
end;

procedure TCcInterbaseConnection.SetCharSet(const Value: String);
begin
  ConnectionParams.Values['CHARSET'] := Value;
end;

//procedure TCcInterbaseConnection.SetDBName(const Value: TFileName);
//begin
//  ConnectionParams.Values['DBNAME'] := Value;
//end;

procedure TCcInterbaseConnection.SetRoleName(const Value: String);
begin
  ConnectionParams.Values['ROLE_NAME'] := Value;
end;

procedure TCcInterbaseConnection.SetSQLDialect(const Value: Integer);
begin
  ConnectionParams.Values['SQLDIALECT'] := IntToStr(Value);
end;

procedure TCcInterbaseConnection.SetUserLogin(const Value: String);
begin
  ConnectionParams.Values['USER_NAME'] := Value;
end;

procedure TCcInterbaseConnection.SetUserPassword(const Value: String);
begin
  ConnectionParams.Values['PASSWORD'] := Value;
end;

end.
