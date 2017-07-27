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
 * Contributor(s): Gavrilev Sergey.
}

unit frmuDBRegister;

{$MODE Delphi}

interface

uses
  SysUtils, Forms, ExtCtrls, StdCtrls, Classes, Controls, Dialogs,
  LCLIntf, LCLType, LMessages, zluibcClasses, Messages, frmuDlgClass, IB;

type
  TfrmDBRegister = class(TDialog)
    lblServerName: TLabel;
    stxServerName: TStaticText;
    bvlLine1: TBevel;
    gbDatabase: TGroupBox;
    lblDBAlias: TLabel;
    lblDBFile: TLabel;
    btnSelDBFile: TButton;
    edtDBFile: TEdit;
    edtDBAlias: TEdit;
    chkSaveAlias: TCheckBox;
    gbLoginInfo: TGroupBox;
    lblUsername: TLabel;
    lblPassword: TLabel;
    lblRole: TLabel;
    edtUsername: TEdit;
    edtPassword: TEdit;
    edtRole: TEdit;
    btnOK: TButton;
    btnCancel: TButton;
    cbCaseSensitive: TCheckBox;
    cbCharacterSet: TComboBox;
    Label2: TLabel;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnSelDBFileClick(Sender: TObject);
    procedure edtDBFileChange(Sender: TObject);
    procedure edtDBFileExit(Sender: TObject);
    procedure edtRoleChange(Sender: TObject);
    Procedure TranslateVisual;override;
  private
    { Private declarations }
    FCurrSelServer : TibcServerNode;
    function VerifyInputData(): boolean;
  public
    { Public declarations }
  end;

function RegisterDB(var DBAlias,Username,Password,Role, CharacterSet: string;
                        DatabaseFiles: TStringList;
                        const SelServer: TibcServerNode;
                        var SaveAlias, CaseSensitive: boolean): boolean;


implementation

uses
   IBServices, frmuMessage, zluGlobal, zluUtility, zluPersistent, resstring;

{$R *.lfm}

function RegisterDB(var DBAlias, Username, Password, Role, CharacterSet: string;
  DatabaseFiles: TStringList; const SelServer: TibcServerNode; var SaveAlias, CaseSensitive: boolean): boolean;

var
  frmDBRegister: TfrmDBRegister;
begin
  frmDBRegister := TfrmDBRegister.Create(Application.MainForm);
  try
    // show servername
    frmDBRegister.stxServerName.Caption := SelServer.NodeName;
    frmDBRegister.FCurrSelServer := SelServer;
    // disable browse button if remote server
    if SelServer.Server.Protocol <> TProtocol(Local) then
      frmDBRegister.btnSelDBFile.Enabled := false;
    frmDBRegister.ShowModal;
    if frmDBRegister.ModalResult = mrOK then
    begin
      // set database information
      DBAlias := frmDBRegister.edtDBAlias.Text;

      { Force a path for all databases if the current protocol is local }
      if SelServer.Server.Protocol = TProtocol(Local) then
      begin
        if ExtractFilePath(frmDBRegister.edtDBFile.Text) = '' then
          frmDBRegister.edtDBFile.Text := ExtractFilePath(Application.ExeName)+
           frmDBRegister.edtDBFile.Text;
      end;
      DatabaseFiles.Add(frmDBRegister.edtDBFile.Text);
      Username := frmDBRegister.edtUsername.Text;
      Password := frmDBRegister.edtPassword.Text;
      Role := frmDBRegister.edtRole.Text;

      CharacterSet := frmDBRegister.cbCharacterSet.Text;

      SaveAlias := frmDBRegister.chkSaveAlias.Checked;
      CaseSensitive := frmDBRegister.cbCaseSensitive.Checked;
      result := true;
    end
    else
      result := false;
  finally
    // deallocate memory
    frmDBRegister.Free;
  end;
end;

procedure TfrmDBRegister.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrmDBRegister.btnOKClick(Sender: TObject);
begin
  if VerifyInputData() then
    ModalResult := mrOK;
end;

procedure TfrmDBRegister.btnSelDBFileClick(Sender: TObject);
var
  lOpenDialog: TOpenDialog;
begin
  lOpenDialog := nil;
  try
  begin
    lOpenDialog := TOpenDialog.Create(self);
    // setup Open Dialog title, extension, filters and options
    lOpenDialog.Title := LZTDBRegisterlOpenDialogTitle;
    lOpenDialog.DefaultExt := 'fdb';
    lOpenDialog.Filter := LZTDBRegisterDatabaseFile;
    lOpenDialog.Options := [ofHideReadOnly,ofNoNetworkButton, ofEnableSizing];
    if lOpenDialog.Execute then
    begin
      // get filename
      edtDBFile.Text := lOpenDialog.FileName;
      // if no dbalias is specified then make it the name of the file
      if (edtDBAlias.Text = '') or (edtDBAlias.Text = ' ') then
      begin
        edtDBAlias.Text := ExtractFileName(edtDbFile.Text);
        if (edtDBAlias.Text = '') or (edtDBAlias.Text = ' ') then
        begin
          edtDBAlias.Text := ExtractFileName(edtDbFile.Text);
        end;
      end;
    end;
  end
  finally
    // deallocate memory
    lOpenDialog.free;
  end;
end;

procedure TfrmDBRegister.edtDBFileChange(Sender: TObject);
begin
  edtDBFile.hint := edtDBFile.text;
end;

procedure TfrmDBRegister.edtDBFileExit(Sender: TObject);
begin
  // if no dbalias is specified then make filename the dbalias
  if (edtDBAlias.Text = '') or (edtDBAlias.Text = ' ') then
  begin
    edtDBAlias.Text := ExtractFileName(edtDbFile.Text);
  end;
  if not (IsValidDBName(edtDBFile.Text)) then
     DisplayMsg(WAR_REMOTE_FILENAME, Format(LZTDBRegisterFile, [edtDBFile.Text]));
end;

function TfrmDBRegister.VerifyInputData(): boolean;
begin
  result := true;

  // if no dbalias is specified
  if (edtDBAlias.Text = '') or (edtDBAlias.Text = ' ') then
  begin
    DisplayMsg(ERR_DB_ALIAS,'');       // show error message
    edtDBAlias.SetFocus;               // give focus to control
    result := false;
    Exit;
  end;

  // check for backslash in dbalias
  // If backslashes are used (i.e. for a path), then the registry
  // key will not be created properly
  if Pos('\',edtDBAlias.Text) <> 0 then
  begin
    DisplayMsg(ERR_DB_ALIAS,'');       // show error message
    edtDBAlias.SetFocus;               // give focus to control
    result := false;
    Exit;
  end;

  // if no dbfile is specified
  if (edtDBFile.GetTextLen = 0) then
  begin
    DisplayMsg(ERR_DB_FILE,edtDBFile.Text);        // show error message
    edtDBFile.SetFocus;                            // give focus to control
    result := false;
    Exit;
  end;

  if PersistentInfo.DatabaseAliasExists(FCurrSelServer.NodeName, edtDBAlias.Text) then // kris changed from DBAliasExists to DatabaseAliasExists
  begin                                // show error message
    DisplayMsg(ERR_DB_ALIAS,LZTDBRegisterDatabaseAlreadyExist);
    edtDBAlias.SetFocus;               // give focus to control
    result := false;
  end;
end;

procedure TfrmDBRegister.edtRoleChange(Sender: TObject);
begin
  inherited;
  cbCaseSensitive.Enabled := (edtRole.GetTextLen > 0);
end;

Procedure TfrmDBRegister.TranslateVisual;
Begin
  lblServerName.Caption := LZTDBRegisterlblServerName;
  gbDatabase.Caption := LZTDBRegistergbDatabase;
  lblDBFile.Caption := LZTDBRegisterlblDBFile;
  lblDBAlias.Caption := LZTDBRegisterlblDBAlias;
  chkSaveAlias.Caption := LZTDBRegisterchkSaveAlias;
  gbLoginInfo.Caption := LZTDBRegistergbLoginInfo;
  lblUsername.Caption := LZTDBRegisterlblUsername;
  lblPassword.Caption := LZTDBRegisterlblPassword;
  lblRole.Caption := LZTDBRegisterlblRole;
  cbCaseSensitive.Caption := LZTDBRegistercbCaseSensitive;
  Label2.Caption := LZTDBRegisterLabel2;
  btnSelDBFile.Hint := LZTDBRegisterbtnSelDBFileHint;
  Self.Caption := LZTDBRegisterFormTitle;
End;

end.
