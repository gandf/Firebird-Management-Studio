unit zluDBConnection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBConnection, sqldb,
  FileUtil;

type

  { TDMDBConnection }

  TDMDBConnection = class(TDataModule)
    IBConnection: TIBConnection;
    SQLTransaction: TSQLTransaction;
  private

  public
    constructor Create (AComponent: TComponent); override;

  end;

var
  DMDBConnection: TDMDBConnection;

implementation

{$R *.lfm}

constructor TDMDBConnection.Create(AComponent: TComponent);
begin
  inherited;
  IBConnection.Transaction := SQLTransaction;
  SQLTransaction.DataBase := IBConnection;
end;

end.

