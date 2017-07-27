{
 * The contents of this file are subject to the InterBase Public License
 * Version 1.0 (the "License"); you may not use this file except in
 * compliance with the License.
 * 
 * You may obtain a copy of the License at http://www.Inprise.com/IPL.html.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.  The Original Code was created by Inprise
 * Corporation and its predecessors.
 * 
 * Portions created by Inprise Corporation are Copyright (C) Inprise
 * Corporation. All Rights Reserved.
 * 
 * Contributor(s): ______________________________________.
}

unit frmuDBConnections;

{$MODE Delphi}

interface

uses
  Forms, ExtCtrls, StdCtrls, Classes, Controls, zluibcClasses, ComCtrls,
  IBDatabase, SysUtils, IBDatabaseInfo, LCLIntf, LCLType, LMessages,
  IBServices, IB, frmuMessage, Messages, frmuDlgClass;

type
  TfrmDBConnections = class(TDialog)
    lvConnections: TListView;
    btnOK: TButton;
    procedure btnOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function ViewDBConnections(const CurrSelServer: TibcServerNode; const CurrDatabase: TIBDatabase): boolean;

implementation

uses
  zluGlobal, zluUtility;

{$R *.lfm}

function ViewDBConnections(const CurrSelServer: TibcServerNode; const CurrDatabase: TIBDatabase): boolean;
var
  frmDBConnections: TfrmDBConnections;
  lIBDBInfo: TIBDatabaseInfo;
  lUserName: TListItem;
  i: integer;
  lDatabase : TIBDatabase;
begin
  lDatabase := nil;
  lIBDBInfo := nil;
  frmDBConnections := nil;
  try
    Screen.Cursor := crHourGlass;
    frmDBConnections := TfrmDBConnections.Create(Application.MainForm);
    lDatabase := TIBDatabase.Create(frmDBConnections);
    lIBDBInfo := TIBDatabaseInfo.Create(frmDBConnections);
    try
      case CurrSelServer.Server.Protocol of
        TCP: lDatabase.DatabaseName := Format('%s:%s',[CurrSelServer.ServerName,CurrDatabase.DatabaseName]);
        NamedPipe: lDatabase.DatabaseName := Format('\\%s\%s',[CurrSelServer.ServerName,CurrDatabase.DatabaseName]);
        SPX: lDatabase.DatabaseName := Format('%s@%s',[CurrSelServer.ServerName,CurrDatabase.DatabaseName]);
        Local:  lDatabase.DatabaseName := CurrDatabase.DatabaseName;
      end;

      lDatabase.LoginPrompt := false;
      lDatabase.Params.Clear;
      lDatabase.Params.Add(Format('isc_dpb_user_name=%s',[CurrSelServer.UserName]));
      lDatabase.Params.Add(Format('isc_dpb_password=%s',[CurrSelServer.Password]));
      lDatabase.Connected := true;
      Application.ProcessMessages;
      result := true;
    except
      on E:EIBError do
      begin
        DisplayMsg(ERR_DB_CONNECT,E.Message);
        result := false;
        exit;
      end;
    end;

    lIBDBInfo.Database := lDatabase;

    for i := 1 to lIBDBInfo.UserNames.Count - 1 do
    begin
      lUserName := frmDBConnections.lvConnections.Items.Add;
      lUserName.Caption := lIBDBInfo.UserNames[i];
    end;

    frmDBConnections.ShowModal;
    result := true;

  finally
    if lDatabase.Connected then
      lDatabase.Connected := false;
    Application.ProcessMessages;
    lDatabase.Free;
    lIBDBInfo.Free;
    frmDBConnections.Free;
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmDBConnections.btnOKClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

end.
