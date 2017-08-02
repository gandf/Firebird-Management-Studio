unit zluDBAdmin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FBAdmin, FBEventMonitor, FileUtil, zluDBConnection;

type

  { TDMDBAdmin }

  TDMDBAdmin = class(TDataModule)
    FBAdmin: TFBAdmin;
    FBEventMonitor: TFBEventMonitor;
  private

  public
    constructor Create (AComponent: TComponent; Connection: TDMDBConnection);

  end;

var
  DMDBAdmin: TDMDBAdmin;

implementation

{$R *.lfm}

constructor TDMDBAdmin.Create(AComponent: TComponent; Connection: TDMDBConnection);
begin
  inherited Create(AComponent);
  FBEventMonitor.Connection := Connection.IBConnection;
end;

end.

