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

{ TODO : 
Check for bugs.  IBConsole has had problems in the past, but this could 
have been an old IBX bug.  The current version of IBX, 4.2, does not 
exihibit this behavior so the problem can be here. }

unit frmuDBBackup;

{$MODE Delphi}

interface

uses                       
  SysUtils, Forms, ExtCtrls, StdCtrls, Classes, Controls, Dialogs, zluibcClasses,
  Grids, Graphics, LCLIntf, LCLType, Comctrls, IB, frmuDlgClass, FileUtil, resstring;

type
  TfrmDBBackup = class(TDialog)
    gbDatabaseFile: TGroupBox;
    lblDatabaseServer: TLabel;
    lblDatabaseAlias: TLabel;
    stxDatabaseServer: TStaticText;
    cbDatabaseAlias: TComboBox;
    imgDownArrow: TImage;
    gbBackupFiles: TGroupBox;
    lblBackupServer: TLabel;
    lblBackupAlias: TLabel;
    sgBackupFiles: TStringGrid;
    cbBackupServer: TComboBox;
    cbBackupAlias: TComboBox;
    sgOptions: TStringGrid;
    pnlOptionName: TPanel;
    cbOptions: TComboBox;
    btnOK: TButton;
    btnCancel: TButton;
    lblOptions: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure cbBackupAliasChange(Sender: TObject);
    procedure cbBackupServerChange(Sender: TObject);
    procedure cbOptionsChange(Sender: TObject);
    procedure cbOptionsDblClick(Sender: TObject);
    procedure cbOptionsExit(Sender: TObject);
    procedure cbOptionsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure sgBackupFilesDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure sgBackupFilesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure sgOptionsDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure sgOptionsSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure cbDatabaseAliasChange(Sender: TObject);
    Procedure TranslateVisual;override;
  private
    { Private declarations }
    FVerboseFile: string;
    FSourceServerNode: TibcServerNode;
    FSourceDatabaseNode: TibcDatabaseNode;
    FBackupFiles: TStringList;
    function VerifyInputData(): boolean;
  public
    { Public declarations }
  end;

function DoDBBackup(var SourceDBAlias,BackupAlias: string;
                    var BackupFiles: TStringList;
                    const SourceServerNode: TibcServerNode;
                    const SourceDatabaseNode: TibcDatabaseNode): integer;

implementation

uses zluGlobal, IBServices, frmuMessage,
  frmuMain, zluUtility, IBErrorCodes;

{$R *.lfm}

const
  OPTION_NAME_COL = 0;
  OPTION_VALUE_COL = 1;
  FORMAT_ROW = 0;
  METADATA_ONLY_ROW = 1;
  GARBAGE_COLLECTION_ROW = 2;
  TRANSACTIONS_IN_LIMBO_ROW = 3;
  CHECKSUMS_ROW = 4;
  CONVERT_TO_TABLES_ROW = 5;
  VERBOSE_OUTPUT_ROW = 6;

procedure TfrmDBBackup.FormCreate(Sender: TObject);
begin
  inherited;
  sgOptions.DefaultRowHeight := cbOptions.Height;
  cbOptions.Visible := True;
  pnlOptionName.Visible := True;

  sgBackupFiles.Cells[0,0] := LZTDBBackupsgBackupFilesCells0;
  sgBackupFiles.Cells[1,0] := LZTDBBackupsgBackupFilesCells1;
  sgOptions.RowCount := 7;

  sgOptions.Cells[OPTION_NAME_COL,FORMAT_ROW] := LZTDBBackupsgOptionsCellsOPTNAMCOLFORM;
  sgOptions.Cells[OPTION_VALUE_COL,FORMAT_ROW] := LZTDBBackupcbOptionsItems1;

  sgOptions.Cells[OPTION_NAME_COL,METADATA_ONLY_ROW] := LZTDBBackupsgOptionsCellsOPTNAMCOLMETADATA;
  sgOptions.Cells[OPTION_VALUE_COL,METADATA_ONLY_ROW] := LZTDBBackupsgOptionsCellsOPTVALCOLMETADATA;

  sgOptions.Cells[OPTION_NAME_COL,GARBAGE_COLLECTION_ROW] := LZTDBBackupsgOptionsCellsOPTNAMCOLGARBCOL;
  sgOptions.Cells[OPTION_VALUE_COL,GARBAGE_COLLECTION_ROW] := LZTDBBackupTrue;

  sgOptions.Cells[OPTION_NAME_COL,TRANSACTIONS_IN_LIMBO_ROW] := LZTDBBackupsgOptionsCellsOPTNAMCOLTRANSLIMBO;
  sgOptions.Cells[OPTION_VALUE_COL,TRANSACTIONS_IN_LIMBO_ROW] := LZTDBBackupsgOptionsCellsOPTVALCOLTRANSLIMBO;

  sgOptions.Cells[OPTION_NAME_COL,CHECKSUMS_ROW] := LZTDBBackupsFalse;
  sgOptions.Cells[OPTION_VALUE_COL,CHECKSUMS_ROW] := LZTDBBackupProcess;

  sgOptions.Cells[OPTION_NAME_COL,CONVERT_TO_TABLES_ROW] := LZTDBBackupsgOptionsCellsOPTNAMCOLCONVTOTAB;
  sgOptions.Cells[OPTION_VALUE_COL,CONVERT_TO_TABLES_ROW] := LZTDBBackupFormat;

  sgOptions.Cells[OPTION_NAME_COL,VERBOSE_OUTPUT_ROW] := LZTDBBackupsgOptionsCellsOPTNAMCOLVERBOUT;
  sgOptions.Cells[OPTION_VALUE_COL,VERBOSE_OUTPUT_ROW] := LZTDBBackupToScreen;

  pnlOptionName.Caption := LZTDBBackupFormat;
  cbOptions.Items.Add(LZTDBBackupcbOptionsItems1);
  cbOptions.Items.Add(LZTDBBackupcbOptionsItems2);
  cbOptions.ItemIndex := 0;
end;

procedure TfrmDBBackup.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrmDBBackup.btnOKClick(Sender: TObject);
var
  j: integer;
  lBackupService: TIBBackupService;
  lOptions: TBackupOptions;
  lVerboseInfo: TStringList;

begin
  if VerifyInputData() then
  begin
    Screen.Cursor := crHourGlass;
    lVerboseInfo := TStringList.Create;
    lBackupService := TIBBackupService.Create(nil);
    try
      try
        lBackupService.LoginPrompt := false;
        lBackupService.ServerName := FSourceServerNode.Server.ServerName;
        lBackupService.Protocol := FSourceServerNode.Server.Protocol;
        lBackupService.Params.Assign(FSourceServerNode.Server.Params);
        lBackupService.Attach();
      except
        on E:EIBError do
        begin
          DisplayMsg(ERR_SERVER_LOGIN, E.Message);
          Screen.Cursor := crDefault;
          if (EIBInterBaseError(E).IBErrorCode = isc_lost_db_connection) or
             (EIBInterBaseError(E).IBErrorCode = isc_unavailable) or
             (EIBInterBaseError(E).IBErrorCode = isc_network_error) then
             frmMain.SetErrorState;
            SetErrorState;
          Exit;
        end;
      end;

      lOptions := [];
      if lBackupService.Active = true then
      begin
        if sgOptions.Cells[OPTION_VALUE_COL,FORMAT_ROW] = LZTDBBackupNonTransportable then
        begin
          Include(lOptions, NonTransportable);
        end;
        if sgOptions.Cells[OPTION_VALUE_COL,METADATA_ONLY_ROW] = LZTDBBackupTrue then
        begin
          Include(lOptions, MetadataOnly);
        end;
        if sgOptions.Cells[OPTION_VALUE_COL,GARBAGE_COLLECTION_ROW] = LZTDBBackupsFalse then
        begin
          Include(lOptions, NoGarbageCollection);
        end;
        if sgOptions.Cells[OPTION_VALUE_COL,TRANSACTIONS_IN_LIMBO_ROW] = LZTDBBackupIgnore then
        begin
          Include(lOptions, IgnoreLimbo);
        end;
        if sgOptions.Cells[OPTION_VALUE_COL,CHECKSUMS_ROW] = LZTDBBackupTrue then
        begin
          Include(lOptions, IgnoreChecksums);
        end;
        if sgOptions.Cells[OPTION_VALUE_COL,CONVERT_TO_TABLES_ROW] = LZTDBBackupTrue then
        begin
          Include(lOptions, ConvertExtTables);
        end;

        lBackupService.Options := lOptions;

        if (sgOptions.Cells[OPTION_VALUE_COL,VERBOSE_OUTPUT_ROW] = LZTDBBackupToScreen) or
          (sgOptions.Cells[OPTION_VALUE_COL,VERBOSE_OUTPUT_ROW] = LZTDBBackupToFile) then
        begin
          lBackupService.Verbose := true;
        end;

        if TibcDatabaseNode(cbDatabaseAlias.Items.Objects[cbDatabaseAlias.ItemIndex]).DatabaseFiles.Count > 0 then
          lBackupService.DatabaseName := TibcDatabaseNode(cbDatabaseAlias.Items.Objects[cbDatabaseAlias.ItemIndex]).DatabaseFiles.Strings[0];

        if cbBackupServer.ItemIndex > -1 then
        begin
          for j := 1 to sgBackupFiles.RowCount - 1 do
          begin
            if not (IsValidDBName(sgBackupFiles.Cells[0,j])) then
              DisplayMsg(WAR_REMOTE_FILENAME, Format(LZTDBBackupFile, [sgBackupFiles.Cells[0,j]]));

            if (sgBackupFiles.Cells[0,j] <> '') and (sgBackupFiles.Cells[1,j] <> '') then
            begin
              lBackupService.BackupFile.Add(Format('%s=%s',[sgBackupFiles.Cells[0,j],
                sgBackupFiles.Cells[1,j]]));
            end
            else if (sgBackupFiles.Cells[0,j] <> '') then
            begin
              lBackupService.BackupFile.Add(sgBackupFiles.Cells[0,j]);
            end;
          end;
        end;

        FBackupFiles.Text := lBackupService.BackupFile.Text;

        Screen.Cursor := crHourGlass;
        try
          lBackupService.ServiceStart;
          FSourceServerNode.OpenTextViewer (lBackupService, LZTDBBackupDatabaseBackup);
          while (lBackupService.IsServiceRunning) and (not gApplShutdown) do
          begin
            Application.ProcessMessages;
            Screen.Cursor := crHourGlass;
          end;

          if lBackupService.Active then
            lBackupService.Detach();
          ModalResult := mrOK;            
        except
          on E: EIBError do
          begin
            DisplayMsg(EIBInterBaseError(E).IBErrorCode, E.Message);
            if (EIBInterBaseError(E).IBErrorCode = isc_lost_db_connection) or
               (EIBInterBaseError(E).IBErrorCode = isc_unavailable) or
               (EIBInterBaseError(E).IBErrorCode = isc_network_error) then
              frmMain.SetErrorState;
            SetErrorState;
            exit;
          end;
        end;
      end;
    finally
      if lBackupService.Active then
        lBackupService.Detach();
      lBackupService.Free();
      lVerboseInfo.Free;
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TfrmDBBackup.cbBackupAliasChange(Sender: TObject);
var
  i: integer;
  lCurrBackupAliasNode: TTreeNode;
  lCurrLine: string;
begin
  sgBackupFiles.RowCount := 2;
  for i := 1 to sgBackupFiles.RowCount do
  begin
    sgBackupFiles.Cells[0,i] := '';
    sgBackupFiles.Cells[1,i] := '';
  end;

  if (cbBackupAlias.ItemIndex > -1) and (Assigned(cbBackupAlias.Items.Objects[cbBackupAlias.ItemIndex])) then
  begin
    lCurrBackupAliasNode := frmMain.tvMain.Items[TibcBackupAliasNode(cbBackupAlias.Items.Objects[cbBackupAlias.ItemIndex]).NodeID];
    for i := 1 to TibcBackupAliasNode(lCurrBackupAliasNode.Data).BackupFiles.Count do
    begin
      lCurrLine := TibcBackupAliasNode(lCurrBackupAliasNode.Data).BackupFiles.Strings[i-1];
      while Length(lCurrLine) > 0 do
      begin
        sgBackupFiles.Cells[0,i] := zluUtility.GetNextField(lCurrLine,'=');
        sgBackupFiles.Cells[1,i] := zluUtility.GetNextField(lCurrLine,'=');
      end;
      sgBackupFiles.RowCount := sgBackupFiles.RowCount + 1;
    end;
  end;
end;

procedure TfrmDBBackup.cbBackupServerChange(Sender: TObject);
var
  i: integer;
  lCurrBackupAliasesNode: TTreeNode;
begin
  cbBackupAlias.Items.Clear;
  cbBackupAlias.Text := '';

  if cbBackupServer.ItemIndex <> -1 then
  begin
    lCurrBackupAliasesNode := frmMain.tvMain.Items[TibcServerNode(cbBackupServer.Items.Objects[cbBackupServer.ItemIndex]).BackupFilesID];

    // check if there are any backup aliases before trying to add to combo box
    if Assigned(lCurrBackupAliasesNode) then
    begin
      if TibcServerNode(lCurrBackupAliasesNode.Data).ObjectList.Count <> 0 then
      begin
        for i := 1 to TibcServerNode(lCurrBackupAliasesNode.Data).ObjectList.Count - 1 do
        begin
          cbBackupAlias.Items.AddObject(TibcTreeNode(lCurrBackupAliasesNode.Data).ObjectList.Strings[i],
            TibcBackupAliasNode(TTreeNode(TibcTreeNode(lCurrBackupAliasesNode.Data).ObjectList.Objects[i]).Data));
        end;
      end;
    end;
  end;
end;

procedure TfrmDBBackup.cbOptionsChange(Sender: TObject);
var
  lSaveDialog: TSaveDialog;
begin
  if (cbOptions.Text = LZTDBBackupToFile) and (sgOptions.Row = VERBOSE_OUTPUT_ROW) then
  begin
    lSaveDialog := TSaveDialog.Create(Self);
    try
      lSaveDialog.Title := LZTDBBackupSelectVerboseFile;
      lSaveDialog.DefaultExt := 'txt';
      lSaveDialog.Filter := LZTDBBackupFileExtFilter;
      lSaveDialog.Options := [ofHideReadOnly,ofEnableSizing];
      if lSaveDialog.Execute then
      begin
        if FileExists(lSaveDialog.FileName) then
        begin
          if MessageDlg(Format(LZTDBBackupOverwrite, [lSaveDialog.FileName]),
              mtConfirmation, mbYesNoCancel, 0) <> idYes then
          begin
            cbOptions.ItemIndex := cbOptions.Items.IndexOf(LZTDBBackupToScreen);
            Exit;
          end;
        end;
        FVerboseFile := lSaveDialog.FileName;
      end
      else
        cbOptions.ItemIndex := cbOptions.Items.IndexOf(LZTDBBackupToScreen);
    finally
      lSaveDialog.free;
    end;
  end;

  {
  sgOptions.Cells[sgOptions.Col,sgOptions.Row] :=
    cbOptions.Items[cbOptions.ItemIndex];
  cbOptions.Visible := false;
  sgOptions.SetFocus;
  }
end;

procedure TfrmDBBackup.cbOptionsDblClick(Sender: TObject);
begin
  if (sgOptions.Col = OPTION_VALUE_COL) or (sgOptions.Col = OPTION_NAME_COL) then
  begin
    if cbOptions.ItemIndex = cbOptions.Items.Count - 1 then
      cbOptions.ItemIndex := 0
    else
      cbOptions.ItemIndex := cbOptions.ItemIndex + 1;

    if sgOptions.Col = OPTION_VALUE_COL then
      sgOptions.Cells[sgOptions.Col,sgOptions.Row] := cbOptions.Items[cbOptions.ItemIndex];

    // cbOptions.Visible := True;
    // sgOptions.SetFocus;
  end;
  cbOptionsChange(self);
end;

procedure TfrmDBBackup.cbOptionsExit(Sender: TObject);
var
  lR     : TRect;
  iIndex : Integer;
begin
  iIndex := cbOptions.Items.IndexOf(cbOptions.Text);

  if (iIndex = -1) then
  begin
    MessageDlg(LZTDBBackupInvalidOptionValue, mtError, [mbOK], 0);

    cbOptions.ItemIndex := 0;          // reset to first item in list
    //Size and position the combo box to fit the cell
    lR := sgOptions.CellRect(OPTION_VALUE_COL, sgOptions.Row);
    lR.Left := lR.Left + sgOptions.Left;
    lR.Right := lR.Right + sgOptions.Left;
    lR.Top := lR.Top + sgOptions.Top;
    lR.Bottom := lR.Bottom + sgOptions.Top;
    cbOptions.Left := lR.Left + 1;
    cbOptions.Top := lR.Top + 1;
    cbOptions.Width := (lR.Right + 1) - lR.Left;
    cbOptions.Height := (lR.Bottom + 1) - lR.Top;
    // cbOptions.Visible := True;
    cbOptions.SetFocus;
  end
  else if (sgOptions.Col <> OPTION_NAME_COL) then
  begin
    sgOptions.Cells[sgOptions.Col,sgOptions.Row] := cbOptions.Items[iIndex];
  end
  else
  begin
    sgOptions.Cells[OPTION_VALUE_COL,sgOptions.Row] := cbOptions.Items[iIndex];
  end;
end;

procedure TfrmDBBackup.cbOptionsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_DOWN) then
    cbOptions.DroppedDown := true;
end;

procedure TfrmDBBackup.sgBackupFilesDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
const
  INDENT = 2;
var
  lLeft: integer;
  lText: string;
begin
  with sgBackupFiles.canvas do
  begin
    if (ACol = 2) and (ARow <> 0) then
    begin
      font.color := clBlack;
      if brush.color = clHighlight then
        font.color := clWhite;
      lText := sgBackupFiles.Cells[ACol,ARow];
      lLeft := Rect.Left + INDENT;
      TextRect(Rect, lLeft, Rect.top + INDENT, lText);
    end;
  end;
end;

procedure TfrmDBBackup.sgBackupFilesKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  lKey : Word;
begin
  if (Key = VK_TAB) and (ssCtrl in Shift) then
  begin
    if sgBackupFiles.Col < sgBackupFiles.ColCount - 1 then
    begin
      sgBackupFiles.Col := sgBackupFiles.Col + 1;
    end
    else
    begin
      if sgBackupFiles.Row = sgBackupFiles.RowCount - 1 then
        sgBackupFiles.RowCount := sgBackupFiles.RowCount + 1;
      sgBackupFiles.Col := 0;
      sgBackupFiles.Row := sgBackupFiles.Row + 1;
    end;
  end;

  if (Key = VK_RETURN) and
    (sgBackupFiles.Cells[sgBackupFiles.Col,sgBackupFiles.Row] <> '') then
  begin
    lKey := VK_TAB;
    sgBackupFilesKeyDown(Self, lKey, [ssCtrl]);
  end;

end;

procedure TfrmDBBackup.sgOptionsDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
const
  INDENT = 2;
var
  // lWidth
  lLeft: integer;
  lText: string;
begin
  with sgOptions.canvas do
  begin
    if ACol = OPTION_VALUE_COL then
    begin
      font.color := clBlue;
      if brush.color = clHighlight then
        font.color := clWhite;
      lText := sgOptions.Cells[ACol,ARow];
      lLeft := Rect.Left + INDENT;
      TextRect(Rect, lLeft, Rect.top + INDENT, lText);
    end;
  end;
end;

procedure TfrmDBBackup.sgOptionsSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  lR, lName : TRect;
begin
  cbOptions.Items.Clear;
  case ARow of
    FORMAT_ROW:
    begin
      cbOptions.Items.Add(LZTDBBackupcbOptionsItems1);
      cbOptions.Items.Add(LZTDBBackupcbOptionsItems2);
    end;
    METADATA_ONLY_ROW:
    begin
      cbOptions.Items.Add(LZTDBBackupTrue);
      cbOptions.Items.Add(LZTDBBackupsFalse);
    end;
    GARBAGE_COLLECTION_ROW:
    begin
      cbOptions.Items.Add(LZTDBBackupTrue);
      cbOptions.Items.Add(LZTDBBackupsFalse);
    end;
    TRANSACTIONS_IN_LIMBO_ROW:
    begin
      cbOptions.Items.Add(LZTDBBackupProcess);
      cbOptions.Items.Add(LZTDBBackupIgnore);
    end;
    CHECKSUMS_ROW:
    begin
      cbOptions.Items.Add(LZTDBBackupProcess);
      cbOptions.Items.Add(LZTDBBackupIgnore);
    end;
    CONVERT_TO_TABLES_ROW:
    begin
      cbOptions.Items.Add(LZTDBBackupTrue);
      cbOptions.Items.Add(LZTDBBackupsFalse);
    end;
    VERBOSE_OUTPUT_ROW:
    begin
      cbOptions.Items.Add(LZTDBBackupNone);
      cbOptions.Items.Add(LZTDBBackupToScreen);
      cbOptions.Items.Add(LZTDBBackupToFile);
    end;
  end;

  pnlOptionName.Caption := sgOptions.Cells[OPTION_NAME_COL, ARow];

  if ACol = OPTION_NAME_COL then
    cbOptions.ItemIndex := cbOptions.Items.IndexOf(sgOptions.Cells[ACol+1,ARow])
  else if ACol = OPTION_VALUE_COL then
    cbOptions.ItemIndex := cbOptions.Items.IndexOf(sgOptions.Cells[ACol,ARow]);

  if ACol = OPTION_NAME_COL then
  begin
    lName := sgOptions.CellRect(ACol, ARow);
    lR := sgOptions.CellRect(ACol + 1, ARow);
  end
  else
  begin
    lName := sgOptions.CellRect(ACol - 1, ARow);
    lR := sgOptions.CellRect(ACol, ARow);
  end;

  // lName := sgOptions.CellRect(ACol, ARow);
  lName.Left := lName.Left + sgOptions.Left;
  lName.Right := lName.Right + sgOptions.Left;
  lName.Top := lName.Top + sgOptions.Top;
  lName.Bottom := lName.Bottom + sgOptions.Top;
  pnlOptionName.Left := lName.Left + 1;
  pnlOptionName.Top := lName.Top + 1;
  pnlOptionName.Width := (lName.Right + 1) - lName.Left;
  pnlOptionName.Height := (lName.Bottom + 1) - lName.Top;
  pnlOptionName.Visible := True;

  // lR := sgOptions.CellRect(ACol, ARow);
  lR.Left := lR.Left + sgOptions.Left;
  lR.Right := lR.Right + sgOptions.Left;
  lR.Top := lR.Top + sgOptions.Top;
  lR.Bottom := lR.Bottom + sgOptions.Top;
  cbOptions.Left := lR.Left + 1;
  cbOptions.Top := lR.Top + 1;
  cbOptions.Width := (lR.Right + 1) - lR.Left;
  cbOptions.Height := (lR.Bottom + 1) - lR.Top;
  cbOptions.Visible := True;
  cbOptions.SetFocus;
end;

function TfrmDBBackup.VerifyInputData(): boolean;
var
  lCnt, iRow : Integer;
  lGridRect : TGridRect;
  found: boolean;

begin
  result := true;
  found := false;
  
  // go through all rows and check for valid integer format in size column
  for iRow:=1 to sgBackupFiles.RowCount - 1 do
  begin
    // specify currently selected string field
    lGridRect.Left := OPTION_VALUE_COL;
    lGridRect.Top := iRow;
    lGridRect.Right := OPTION_VALUE_COL;
    lGridRect.Bottom := iRow;

    if (sgBackupFiles.Cells[OPTION_VALUE_COL, iRow] <> '') and (sgBackupFiles.Cells[OPTION_VALUE_COL, iRow] <> ' ') then
    begin
      try
        StrToInt(sgBackupFiles.Cells[OPTION_VALUE_COL, iRow])
      except on EConvertError do
        begin
          DisplayMsg(ERR_NUMERIC_VALUE,'');
          sgBackupFiles.SetFocus;      // give focus to string grid and select the erring field
          sgBackupFiles.Selection:=lGridRect;
          result := false;             // set result to false
          Exit;
        end;
      end;
    end;
  end;

  // check if combo box is empty or nothing selected
  if (cbDatabaseAlias.ItemIndex = -1) or (cbDatabaseAlias.Text = '') or (cbDatabaseAlias.Text = ' ') then
  begin
    DisplayMsg(ERR_DB_ALIAS,'');
    cbDatabaseAlias.SetFocus;
    result := false;
    Exit;
  end;

  // check if combo box is empty
  if (cbBackupAlias.Text = '') or (cbBackupAlias.Text = ' ') then
  begin
    DisplayMsg(ERR_DB_ALIAS,'');
    cbBackupAlias.SetFocus;
    result := false;
    Exit;
  end;

  // check if combo box is empty or nothing selected
  if (cbBackupServer.ItemIndex = -1) or (cbBackupServer.Text = '') or (cbBackupServer.Text = ' ') then
  begin
    DisplayMsg(ERR_SERVER_NAME,'');
    cbBackupServer.SetFocus;
    result := false;
    Exit;
  end;

  for lCnt := 1 to sgBackupFiles.RowCount - 1 do
  begin
    if (sgBackupFiles.Cells[0,lCnt] <> '') then
      found := true;
  end;

  if not found then
  begin
    DisplayMsg (ERR_NO_FILES,'');
    result := false;
    exit;
  end;

end;

function DoDBBackup(var SourceDBAlias,BackupAlias: string;
  var BackupFiles: TStringList; const SourceServerNode: TibcServerNode;
  const SourceDatabaseNode: TibcDatabaseNode): integer;
var
  i: integer;
  frmDBBackup: TfrmDBBackup;
  lCurrDatabasesNode: TTreeNode;
  AliasName: String;
begin
  frmDBBackup := nil;
  try
    frmDBBackup:= TfrmDBBackup.Create(Application.MainForm);
    frmDBBackup.FSourceServerNode := SourceServerNode;
    frmDBBackup.FSourceDatabaseNode := SourceDatabaseNode;
    frmDBBackup.FBackupFiles := BackupFiles;
    frmDBBackup.stxDatabaseServer.Caption := SourceServerNode.NodeName;
    lCurrDatabasesNode := frmMain.tvMain.Items[SourceServerNode.DatabasesID];

    for i := 1 to TibcTreeNode(lCurrDatabasesNode.Data).ObjectList.Count - 1 do
    begin
      AliasName := TibcDatabaseNode(lCurrDatabasesNode.Data).ObjectList.Strings[i];
      frmDBBackup.cbDatabaseAlias.Items.AddObject(GetNextField (AliasName, DEL),
        TibcDatabaseNode(TTreeNode(TibcTreeNode(lCurrDatabasesNode.Data).ObjectList.Objects[i]).Data));
    end;

    if Assigned(SourceDatabaseNode) then
    begin
      frmDBBackup.cbDatabaseAlias.ItemIndex := frmDBBackup.cbDatabaseAlias.Items.IndexOf(SourceDatabaseNode.NodeName);
      frmDBBackup.cbDatabaseAlias.Hint := frmDBBackup.cbDatabaseAlias.Text;
    end;

    for i := 1 to TibcTreeNode(frmMain.tvMain.Items[0].Data).ObjectList.Count - 1 do
    begin
      AliasName := TibcTreeNode(frmMain.tvMain.Items[0].Data).ObjectList.Strings[i];
      frmDBBackup.cbBackupServer.Items.AddObject(GetNextField(AliasName, DEL),
        TibcServerNode(TTreeNode(TibcTreeNode(frmMain.tvMain.Items[0].Data).ObjectList.Objects[i]).Data));
    end;

//    if Assigned(SourceBackupAliasNode) then
//    begin
      frmDBBackup.cbBackupServer.ItemIndex := frmDBBackup.cbBackupServer.Items.IndexOf(SourceServerNode.NodeName);
      frmDBBackup.cbBackupServerChange(frmDBBackup);

      frmDBBackup.cbBackupAlias.ItemIndex := frmDBBackup.cbBackupAlias.Items.IndexOf(BackupAlias);
      frmDBBackup.cbBackupAliasChange(frmDBBackup);

      frmDBBackup.cbDatabaseAlias.ItemIndex := frmDBBackup.cbDatabaseAlias.Items.IndexOf(SourceDBAlias);
      frmDBBackup.cbDatabaseAliasChange(frmDBBackup);
//    end;

    frmDBBackup.ShowModal;

    if (frmDBBackup.ModalResult = mrOK)  and (not frmDBBackup.GetErrorState) then
    begin
      BackupFiles.Text := frmDBBackup.FBackupFiles.Text;
      BackupAlias := frmDBBackup.cbBackupAlias.Text;
      SourceDBAlias := frmDBBackup.cbDatabaseAlias.Text;
      DisplayMsg(INF_BACKUP_DB_SUCCESS,'');
      result := SUCCESS;
    end
    else
      result := FAILURE;
  finally
    frmDBBackup.Free;
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmDBBackup.cbDatabaseAliasChange(Sender: TObject);
begin
  cbDatabaseAlias.Hint := cbDatabaseAlias.Text;
end;

Procedure TfrmDBBackup.TranslateVisual;
Begin
  lblOptions.Caption := LZTDBBackuplblOptions;
  gbDatabaseFile.Caption := LZTDBBackupgbDatabaseFile;
  lblDatabaseServer.Caption := LZTDBBackuplblDatabaseServer;
  lblDatabaseAlias.Caption := LZTDBBackuplblDatabaseAlias;
  gbBackupFiles.Caption := LZTDBBackupgbBackupFiles;
  lblBackupServer.Caption := LZTDBBackuplblBackupServer;
  lblBackupAlias.Caption := LZTDBBackuplblBackupAlias;
  sgBackupFiles.Caption := LZTDBBackupsgBackupFiles;
  btnOK.Caption := LZTDBBackupbtnOK;
  btnCancel.Caption := LZTDBBackupbtnCancel;
  Self.Caption := LZTDBBackupFormTitle;
End;

end.
