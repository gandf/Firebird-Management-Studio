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
 * Contributor(s): Krzysztof Golko, Sergey Gavrilev.
}

unit frmuDBConnect;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, SysUtils, Forms, ExtCtrls, StdCtrls, Classes, Controls,
  zluibcClasses, IB, frmuDlgClass, resstring;

type
  TfrmDBConnect = class(TDialog)
    lblDatabaseName: TLabel;
    bvlLine1: TBevel;
    lblUsername: TLabel;
    lblPassword: TLabel;
    lblRole: TLabel;
    Label1: TLabel;
    cbDialect: TComboBox;
    btnConnect: TButton;
    btnCancel: TButton;
    edtRole: TEdit;
    edtPassword: TEdit;
    edtUsername: TEdit;
    stxDatabaseName: TStaticText;
    cbCaseSensitive: TCheckBox;
    cbCharacterSet: TComboBox;
    Label2: TLabel;
    procedure btnCancelClick(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure edtRoleChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    Procedure TranslateVisual;override;
  private
    { Private declarations }
    function VerifyInputData(): boolean;
  public
    { Public declarations }
  end;

function DBConnect(CurrSelDatabase: TibcDatabaseNode; const CurrSelServer: TibcServerNode; const SilentConnect: boolean): boolean;

implementation

uses
  IBServices, IBDatabase, frmuMessage, zluGlobal;

{$R *.lfm}

function DoDBConnect(const CurrSelServer: TibcServerNode; CurrSelDatabase: TibcDatabaseNode): boolean;
begin
  // check if not already connected
  if CurrSelDatabase.Database.Connected then
  begin
    Result := TRUE;
    Exit;
  end;

  try
      // check if a database file has been specified
    if CurrSelDatabase.DatabaseFiles.Count > 0 then
    begin
      // construct UNC path to database file
      case CurrSelServer.Server.Protocol of
        TCP: CurrSelDatabase.Database.DatabaseName := Format('%s:%s',[CurrSelServer.ServerName,CurrSelDatabase.DatabaseFiles.Strings[0]]);
        NamedPipe: CurrSelDatabase.Database.DatabaseName := Format('\\%s\%s',[CurrSelServer.ServerName,CurrSelDatabase.DatabaseFiles.Strings[0]]);
        SPX: CurrSelDatabase.Database.DatabaseName := Format('%s@%s',[CurrSelServer.ServerName,CurrSelDatabase.DatabaseFiles.Strings[0]]);
        Local:  CurrSelDatabase.Database.DatabaseName := CurrSelDatabase.DatabaseFiles.Strings[0];
      end;
    end;

    // clear database parameters and submit login details
    CurrSelDatabase.Database.Params.Clear;
    CurrSelDatabase.Database.Params.Add(Format('isc_dpb_user_name=%s',[CurrSelDatabase.UserName]));
    CurrSelDatabase.Database.Params.Add(Format('isc_dpb_password=%s',[CurrSelDatabase.Password]));
    if CurrSelDatabase.CaseSensitiveRole then
      CurrSelDatabase.Database.Params.Add(Format('isc_dpb_sql_role_name="%s"',[CurrSelDatabase.Role]))
    else
      CurrSelDatabase.Database.Params.Add(Format('isc_dpb_sql_role_name=%s',[CurrSelDatabase.Role]));

    if (CurrSelDatabase.CharacterSet <> '') and (CurrSelDatabase.CharacterSet <> 'None') then
      CurrSelDatabase.Database.Params.Add('isc_dpb_lc_ctype=' + CurrSelDatabase.CharacterSet);

    Screen.Cursor := crHourGlass;
    CurrSelDatabase.Database.Connected := true;
    Screen.Cursor := crDefault;
  except                               // if an exception occurs then trap it
    on E:EIBError do                   // and show error message
    begin
      Screen.Cursor := crDefault;
      DisplayMsg(ERR_DB_CONNECT, E.Message);
    end;
  end;
  Result := CurrSelDatabase.Database.Connected;
end;

function DBConnect(CurrSelDatabase: TibcDatabaseNode;
  const CurrSelServer: TibcServerNode;
  const SilentConnect: boolean): boolean;
var
  frmDBConnect: TfrmDBConnect;
begin
  frmDBConnect := nil;
  CurrSelDatabase.UserName := CurrSelServer.UserName;
  CurrSelDatabase.Password := CurrSelServer.Password;
  if CurrSelDatabase.CaseSensitiveRole then
    CurrSelDatabase.Database.SQLDialect := 3;

  // check if a SilentConnect is specified
  if not SilentConnect then
  begin
    try
      frmDBConnect:= TfrmDBConnect.Create(Application.MainForm);

      frmDBConnect.stxDatabaseName.Caption := CurrSelDatabase.NodeName;
      frmDBConnect.edtUsername.Text := CurrSelDatabase.UserName;
      frmDBConnect.edtRole.Text := CurrSelDatabase.Role;
      frmDBConnect.cbDialect.ItemIndex := 2; // default to dialect 3

      frmDBConnect.cbCharacterSet.ItemIndex := frmDBConnect.cbCharacterSet.Items.IndexOf(CurrSelDatabase.CharacterSet);

      Result := FALSE;
      while frmDBConnect.ShowModal = mrOK do
      begin
        // set username, password and role
        CurrSelDatabase.UserName := frmDBConnect.edtUsername.Text;
        CurrSelDatabase.Password := frmDBConnect.edtPassword.Text;
        CurrSelDatabase.CharacterSet := frmDBConnect.cbCharacterSet.Text;

        CurrSelDatabase.Role := frmDBConnect.edtRole.Text;
        // get SQL dialect
        CurrSelDatabase.Database.SQLDialect := frmDBConnect.cbDialect.ItemIndex + 1;
        // upgrade SQL dialect if case sensitive role name
        if frmDBConnect.cbCaseSensitive.Checked then
          CurrSelDatabase.Database.SQLDialect := 3;

        if DODBConnect(CurrSelServer, CurrSelDatabase) then
        begin
          Result := true;
          Break;
        end;
      end
    finally
      frmDBConnect.Free;
    end;
  end
  else
  begin
    Result := DoDBConnect(CurrSelServer, CurrSelDatabase);
  end;

  if Result then
  begin
    gAppSettings[DEFAULT_DIALECT].Setting := CurrSelDatabase.Database.SQLDialect;
    gAppSettings[CHARACTER_SET].Setting := CurrSelDatabase.CharacterSet;
  end;
end;

procedure TfrmDBConnect.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrmDBConnect.btnConnectClick(Sender: TObject);
begin
  if VerifyInputData() then
    ModalResult := mrOK;
end;

function TfrmDBConnect.VerifyInputData(): boolean;
begin
  result := true;

  // if username was not specified
  if (edtUsername.Text = '') or (edtUsername.Text = ' ') then
  begin
    DisplayMsg(ERR_USERNAME,'');       // show error message
    edtUsername.SetFocus;              // give focus to control
    result := false;
    Exit;
  end;

  // if password was not specified
  if (edtPassword.Text = '') or (edtPassword.Text = ' ') then
  begin
    DisplayMsg(ERR_PASSWORD,'');       // show error message
    edtPassword.SetFocus;              // give focus to control
    result := false;
    Exit;
  end;
end;

procedure TfrmDBConnect.edtRoleChange(Sender: TObject);
begin
  inherited;
  cbCaseSensitive.Enabled := (edtRole.GetTextLen > 0);
end;

procedure TfrmDBConnect.FormCreate(Sender: TObject);
begin
  inherited;
  with cbCharacterSet do
  begin
    // This is temporary, list of vailable character sets is set in IBX
    // and in another place in IBConsole, it should be provided by IBX
    Items.Add('None');
    Items.Add('ASCII');
    Items.Add('BIG_5');
    Items.Add('CP943C');
    Items.Add('CYRL');
    Items.Add('DOS437');
    Items.Add('DOS737');
    Items.Add('DOS850');
    Items.Add('DOS852');
    Items.Add('DOS857');
    Items.Add('DOS858');
    Items.Add('DOS860');
    Items.Add('DOS861');
    Items.Add('DOS862');
    Items.Add('DOS863');
    Items.Add('DOS864');
    Items.Add('DOS865');
    Items.Add('DOS866');
    Items.Add('DOS869');
    Items.Add('EUCJ_0208');
    Items.Add('GB_2312');
    Items.Add('ISO8859_1');
    Items.Add('ISO8859_2');
    Items.Add('ISO8859_3');
    Items.Add('ISO8859_4');
    Items.Add('ISO8859_5');
    Items.Add('ISO8859_6');
    Items.Add('ISO8859_7');
    Items.Add('ISO8859_8');
    Items.Add('ISO8859_9');
    Items.Add('ISO8859_13');
    Items.Add('KOI8R');
    Items.Add('KOI8U');
    Items.Add('KSC_5601');
    Items.Add('NEXT');
    Items.Add('OCTETS');
    Items.Add('SJIS_0208');
    Items.Add('TIS620');
    Items.Add('UNICODE_FSS');
    Items.Add('UTF8');
    Items.Add('WIN1250');
    Items.Add('WIN1251');
    Items.Add('WIN1252');
    Items.Add('WIN1253');
    Items.Add('WIN1254');
    Items.Add('WIN1255');
    Items.Add('WIN1256');
    Items.Add('WIN1257');
    Items.Add('WIN1258');
  end;
end;

Procedure TfrmDBConnect.TranslateVisual;
Begin
  lblDatabaseName.Caption := LZTDBConnectlblDatabaseNameCaption;
  lblUsername.Caption := LZTDBConnectlblUsernameCaption;
  lblPassword.Caption := LZTDBConnectlblPasswordCaption;
  lblRole.Caption := LZTDBConnectlblRoleCaption;
  Label1.Caption := LZTDBConnectLabel1Caption;
  Label2.Caption := LZTDBConnectLabel2Caption;
  cbCaseSensitive.Caption := LZTDBConnectcbCaseSensitiveCaption;
  btnConnect.Caption := LZTDBConnectbtnConnectCaption;
  btnCancel.Caption := LZTDBConnectbtnCancelCaption;
  Self.Caption := LZTDBConnectFormTitle;
End;

end.
