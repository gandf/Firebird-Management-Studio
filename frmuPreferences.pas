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

unit frmuPreferences;

interface

uses
  SysUtils, Forms, ExtCtrls, StdCtrls, Classes, Controls, ComCtrls, Grids,
  Graphics, Windows, Dialogs, Registry, Messages, ShlObj, frmuDlgClass;

type
  TfrmPreferences = class(TDialog)
    btnApplTempDir: TButton;
    btnSelExternalEditorFilename: TButton;
    chkUseDefaultEditor: TCheckBox;
    edtApplTempDir: TEdit;
    edtExternalEditorFilename: TEdit;
    edtExternalEditorParams: TEdit;
    gbEditorSettings: TGroupBox;
    lblEditorFilename: TLabel;
    lblEditorParameters: TLabel;
    lblApplTempDir: TLabel;
    pgcMain: TPageControl;
    TabGeneral: TTabSheet;
    btnApply: TButton;
    btnOK: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure btnSelExternalEditorFilenameClick(Sender: TObject);
    procedure chkUseDefaultEditorClick(Sender: TObject);
    procedure edtApplTempDirExit(Sender: TObject);
    procedure btnApplTempDirClick(Sender: TObject);
    procedure edtDataChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure edtExternalEditorFilenameExit(Sender: TObject);
  private
    { Private declarations }
    function VerifyInputData(): boolean;
    procedure SetEditorEnabled(const enable: boolean);
  public
    { Public declarations }
  end;

function EditPreferences(): integer;

implementation

uses
  zluGlobal,frmuMessage, zluUtility;

{$R *.DFM}

procedure TfrmPreferences.FormCreate(Sender: TObject);
begin
  inherited;
{  edtApplTempDir.Text := gAppSettings[TEMP_PATH].Setting;
  chkUseDefaultEditor.checked := gAppSettings[USE_DEFAULT_EDITOR].Setting;
  edtExternalEditorFilename.Text := gAppSettings[EXT_EDITOR_FILENAME].Setting;
  edtExternalEditorParams.Text := gAppSettings[EXT_EDITOR_PARAMS].Setting;
}
end;

procedure TfrmPreferences.btnOKClick(Sender: TObject);
begin
  if btnApply.Enabled then
    btnApplyClick (Sender);
  ModalResult := mrOK;
end;

procedure TfrmPreferences.btnApplyClick(Sender: TObject);
begin
{
  if VerifyInputData() then begin
    gAppSettings[TEMP_PATH].Setting := edtApplTempDir.Text;
    gAppSettings[USE_DEFAULT_EDITOR].Setting := chkUseDefaultEditor.Checked;
    gAppSettings[EXT_EDITOR_FILENAME].Setting := edtExternalEditorFilename.Text;
    gAppSettings[EXT_EDITOR_PARAMS].Setting := edtExternalEditorParams.Text;
    btnApply.Enabled := false;
  end;
}
end;

procedure TfrmPreferences.btnSelExternalEditorFilenameClick(Sender: TObject);
var
  lOpenDialog: TOpenDialog;
begin
  lOpenDialog := nil;
  try
  begin
    lOpenDialog := TOpenDialog.Create(self);
    lOpenDialog.Title := 'Select Application';
    lOpenDialog.DefaultExt := 'exe';
    lOpenDialog.Filter := 'Application File (*.exe)|*.EXE|All files (*.*)|*.*';
    lOpenDialog.Options := [ofHideReadOnly,ofNoNetworkButton,ofEnableSizing];
    if lOpenDialog.Execute then
    begin
      edtExternalEditorFilename.Text := lOpenDialog.FileName;
    end;
  end
  finally
    lOpenDialog.free;
  end;
end;

function TfrmPreferences.VerifyInputData(): boolean;
begin
  if not chkUseDefaultEditor.Checked and
     not (edtExternalEditorFilename.GetTextLen > 0) then begin
     DisplayMsg (ERR_EDITOR_MISSING, '');
     result := false;
  end else
    result := true;
end;

function EditPreferences(): integer;
var
  frmPreferences: TfrmPreferences;
begin
  frmPreferences := TfrmPreferences.Create(Application);
  try
    frmPreferences.ShowModal;
    if frmPreferences.ModalResult = mrOK then
    begin
      result := SUCCESS;
    end
    else
      result := FAILURE;
  finally
    frmPreferences.Free;
  end;
end;


procedure TfrmPreferences.chkUseDefaultEditorClick(Sender: TObject);
begin
  SetEditorEnabled (not chkUseDefaultEditor.Checked);
end;

procedure TfrmPreferences.edtApplTempDirExit(Sender: TObject);
var
  sText: string;
begin
  sText := edtApplTempDir.text;
  if not CheckDirectory(sText) then
    edtApplTempDir.SetFocus;

end;

procedure TfrmPreferences.btnApplTempDirClick(Sender: TObject);
var
  lBrowseInfo: TBrowseInfo;
  lDirectory: pChar;
begin
  lDirectory := StrAlloc(MAX_PATH);
  lBrowseInfo.hwndOwner := handle;
  lBrowseInfo.pidlRoot := nil;
  lBrowseInfo.pszDisplayName := lDirectory;
  lBrowseInfo.lpszTitle := 'Select Directory';
  lBrowseInfo.ulFlags := BIF_DONTGOBELOWDOMAIN or
                           BIF_RETURNONLYFSDIRS;
  lBrowseInfo.lpfn := nil;
  lBrowseInfo.lParam := 0;
  lBrowseInfo.iImage := 0;

   if SHGetPathFromIDList(SHBrowseForFolder(lBrowseInfo), lDirectory) then
   begin
     if lDirectory <> '' then
       edtApplTempDir.Text := string(lDirectory) + '\';
   end;
   StrDispose(lDirectory);
end;

procedure TfrmPreferences.edtDataChange(Sender: TObject);
begin
  btnApply.Enabled := true;
end;

procedure TfrmPreferences.FormShow(Sender: TObject);
begin
  btnApply.Enabled := false;
end;

procedure TfrmPreferences.edtExternalEditorFilenameExit(Sender: TObject);
var
  buffer: String;
  FileToFind: String;
  length: integer;

begin
  with Sender as TEdit do begin
    if GetTextLen > 0 then begin
      length := GetEnvironmentVariable ('PATH', nil, 0);
      SetLength (buffer, length);
      GetEnvironmentVariable ('PATH', PChar(buffer), length);

      FileToFind := FileSearch(Text, GetCurrentDir + ';' + buffer);
      if FileToFind = '' then begin
        DisplayMsg (ERR_INV_EDITOR, 'File: '+ Text + ' doesn''t exist.');
        SetFocus;
      end;
    end;
  end;
end;

procedure TfrmPreferences.SetEditorEnabled(const enable: boolean);
begin
  if not enable then begin
    edtExternalEditorFileName.Color := clBtnFace;
    edtExternalEditorParams.Color := clBtnFace;
  end else
  begin
    edtExternalEditorFileName.Color := clWindow;
    edtExternalEditorParams.Color := clWindow;
  end;

  edtExternalEditorFilename.Enabled := enable;
  lblEditorFileName.Enabled := enable;
  btnSelExternalEditorFilename.Enabled := enable;
  lblEditorParameters.Enabled := enable;
  edtExternalEditorParams.Enabled := enable;
  btnApply.Enabled := true;
end;

end.
