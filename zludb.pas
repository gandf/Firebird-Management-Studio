unit zluDB;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, zluDBConnection, zluDBAccess, zluDBAdmin;

type

  TDB = class (TComponent)
  private
    FConnection: TDMDBConnection;
    FAccess: TDMDBAccess;
    FAdmin: TDMDBAdmin;

  published

  public
    constructor Create (AComponent: TComponent); override;
    destructor Destroy; override;
  end;

implementation

constructor TDB.Create(AComponent: TComponent);
begin
  inherited;
  FConnection := TDMDBConnection.Create(self);
  FAccess := TDMDBAccess.Create(self, FConnection);
  FAdmin := TDMDBAdmin.Create(self, FConnection);
end;

destructor TDB.Destroy;
begin
  FAdmin.Free;
  FAccess.Free;
  FConnection.Free;
  inherited;
end;


end.

