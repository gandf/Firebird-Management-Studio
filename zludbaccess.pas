unit zluDBAccess;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, FileUtil, zluDBConnection;

type

  { TDMDBAccess }

  TDMDBAccess = class(TDataModule)
    DataSource: TDataSource;
    SQLQuery: TSQLQuery;
    SQLScript: TSQLScript;
  private

  public
    constructor Create (AComponent: TComponent; Connection: TDMDBConnection);

  end;

var
  DMDBAccess: TDMDBAccess;

implementation

{$R *.lfm}

constructor TDMDBAccess.Create(AComponent: TComponent; Connection: TDMDBConnection);
begin
  inherited Create(AComponent);
  SQLScript.DataBase := Connection.IBConnection;
  SQLScript.Transaction := Connection.SQLTransaction;
  SQLQuery.DataBase := Connection.IBConnection;
  DataSource.DataSet := SQLQuery;
end;

end.

