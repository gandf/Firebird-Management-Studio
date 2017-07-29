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

unit frmuDBProperties;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, Forms, ExtCtrls, StdCtrls, Classes, Controls, zluibcClasses, ComCtrls,
  SysUtils, Dialogs, Grids, Graphics, Registry, IBDatabaseInfo,
  IBServices, frmuMessage, IB, IBDatabase, Db, IBCustomDataSet, resstring,
  IBQuery, frmuDlgClass;

type
  TfrmDBProperties = class(TDialog)
    TabAlias: TTabSheet;
    TabGeneral: TTabSheet;
    cbOptions: TComboBox;
    edtAliasName: TEdit;
    edtFilename: TEdit;
    gbSummaryInfo: TGroupBox;
    lblAliasName: TLabel;
    lblDBOwner: TLabel;
    lblDBPages: TLabel;
    lblFilename: TLabel;
    lblOptions: TLabel;
    lblPageSize: TLabel;
    lvSecondaryFiles: TListView;
    pgcMain: TPageControl;
    sgOptions: TStringGrid;
    stxDBOwner: TStaticText;
    stxDBPages: TStaticText;
    stxPageSize: TStaticText;
    btnSelFilename: TButton;
    pnlOptionName: TPanel;
    lblServerName: TLabel;
    stxServerName: TStaticText;
    btnApply: TButton;
    btnCancel: TButton;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure cbOptionsChange(Sender: TObject);
    procedure cbOptionsDblClick(Sender: TObject);
    procedure cbOptionsExit(Sender: TObject);
    procedure cbOptionsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure edtAliasNameChange(Sender: TObject);
    procedure sgOptionsDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure sgOptionsSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure btnSelFilenameClick(Sender: TObject);
    procedure edtFilenameChange(Sender: TObject);
    procedure SetDefaults (const readOnly, sweep, synch, dialect: String);
    procedure edtFilenameExit(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    Procedure TranslateVisual;override;
  private
    FApplyChanges: boolean;
    FOriginalAlias: string;
    FOriginalReadOnly: string;
    FOriginalSweepInterval: string;
    FOriginalSynchMode: string;
    FOriginalSQLDialect: String;
    FAliaschanged: boolean;
    function  VerifyInputData(): boolean;
  public
    sOriginalForcedWrites: string;
    sOriginalReadOnly: string;
    sOriginalSweepInterval: string;
    sOriginalSQLDialect: string;
    bOriginalConnectStatus: boolean;
    CurrSelDatabase: TibcDatabaseNode;
    CurrSelServer: TibcServerNode;
end;

function EditDBProperties(const CurrSelServer: TibcServerNode; var CurrSelDatabase: TibcDatabaseNode): integer;

implementation

uses
  zluGlobal, zluUtility,frmuMain, IBErrorCodes;

{$R *.lfm}

const
  OPTION_NAME_COL = 0;
  OPTION_VALUE_COL = 1;
  FORCED_WRITES_ROW = 0;
  SWEEP_INTERVAL_ROW = 1;
  READ_ONLY_ROW = 3;
  SQL_DIALECT_ROW = 2;
  SWEEP_INTERVAL_MIN = 0;
  SWEEP_INTERVAL_MAX = 200000;
  SQL_DIALECT1 = '1';
  SQL_DIALECT2 = '2';
  SQL_DIALECT3 = '3';

procedure TfrmDBProperties.FormCreate(Sender: TObject);
begin
  inherited;
  FApplyChanges := false;
  FAliasChanged := false;
  sgOptions.DefaultRowHeight := cbOptions.Height;
  sgOptions.RowCount := 4;
  sgOptions.Cells[OPTION_NAME_COL,FORCED_WRITES_ROW] := LZTDBPropForcedWrites;
  sgOptions.Cells[OPTION_NAME_COL,SWEEP_INTERVAL_ROW] := LZTDBPropSweepInterval;
  sgOptions.Cells[OPTION_NAME_COL,SQL_DIALECT_ROW] := LZTDBPropDatabaseDialect;
  sgOptions.Cells[OPTION_NAME_COL,READ_ONLY_ROW] := LZTDBPropReadOnly;

  cbOptions.Visible := True;
  pnlOptionName.Visible := True;
  btnApply.Enabled := false;
end;

procedure TfrmDBProperties.FormShow(Sender: TObject);
begin
  FOriginalAlias := edtAliasName.Text;
  pnlOptionName.Caption := LZTDBProppnlOptionNameCaption;
  cbOptions.Style := csDropDown;
  cbOptions.Items.Add(LZTDBPropFORCED_WRITES_TRUE);
  cbOptions.Items.Add(LZTDBPropFORCED_WRITES_FALSE);
  cbOptions.ItemIndex := cbOptions.Items.IndexOf(FOriginalSynchMode);
  cbOptions.Tag := FORCED_WRITES_ROW;
  btnApply.Enabled := false;
  FAliasChanged := false;
end;

procedure TfrmDBProperties.btnApplyClick(Sender: TObject);
var
  lRegistry: TRegistry;
  i: integer;
  lConfigService: TIBConfigService;

begin
  if VerifyInputData() then
  begin
    Screen.Cursor := crHourGlass;
    // save alias and database file information
    lRegistry := TRegistry.Create;
    lConfigService := TIBConfigService.Create(self);

    CurrSelDatabase.DatabaseFiles.Clear;
    CurrSelDatabase.DatabaseFiles.Add(edtFilename.Text);

    for i := 0 to lvSecondaryFiles.Items.Count-1 do
      CurrSelDatabase.DatabaseFiles.Add(lvSecondaryFiles.Items[i].Caption);

    if lRegistry.OpenKey(Format('%s%s\Databases\%s',[gRegServersKey,CurrSelServer.NodeName,CurrSelDatabase.NodeName]),false) then
    begin
      lRegistry.WriteString('DatabaseFiles',CurrSelDatabase.DatabaseFiles.Text);
      lRegistry.CloseKey();
      lRegistry.MoveKey(Format('%s%s\Databases\%s',[gRegServersKey,CurrSelServer.NodeName,CurrSelDatabase.NodeName]),
        Format('%s%s\Databases\%s',[gRegServersKey,CurrSelServer.NodeName, edtAliasName.Text]), true);
    end;

    CurrSelDatabase.NodeName := edtAliasName.Text;
    frmMain.RenameTreeNode(CurrSelDatabase, edtAliasName.Text);

    // Set properties if general tab was shown

    if TabGeneral.TabVisible then
    begin
      try  // try to connect to configuration service
        lConfigService.DatabaseName := CurrSelDatabase.Database.DatabaseName;
        lConfigService.LoginPrompt := false;
        lConfigService.ServerName := CurrSelServer.Servername;
        lConfigService.Protocol := CurrSelServer.Server.Protocol;
        lConfigService.Params.Add(Format('isc_spb_user_name=%s', [CurrSelDatabase.UserName]));
        lConfigService.Params.Add(Format('isc_spb_password=%s', [CurrSelDatabase.Password]));
        lConfigService.Attach();
      except
        on E:EIBError do
        begin
          DisplayMsg(ERR_SERVER_LOGIN, E.Message);
          if (EIBInterBaseError(E).IBErrorCode = isc_lost_db_connection) or
             (EIBInterBaseError(E).IBErrorCode = isc_unavailable) or
             (EIBInterBaseError(E).IBErrorCode = isc_network_error) then
            frmMain.SetErrorState;
          SetErrorState;
          Screen.Cursor := crDefault;
          Exit;
        end;
      end;

      if lConfigService.Active then // if attached successfully
      begin
        try
          // Toggle Read-Only first if changing from Read_Only
          if ((sgOptions.Cells[OPTION_VALUE_COL,READ_ONLY_ROW] <> sOriginalReadOnly) and
           (sOriginalReadOnly = LZTDBPropREAD_ONLY_TRUE))   then
          begin
            CurrSelDatabase.Database.Connected := False;  // need to disconnect from database
            if not lConfigService.Active then
              lConfigService.Attach();

            if lConfigService.Active then  // if attached successfully
            begin
              lConfigService.SetReadOnly(False);  // toggle original value
              CurrSelDatabase.Database.Connected := bOriginalConnectStatus;
            end;
          end; // end if read-only changed

          // Set sweep interval if changed
          if sgOptions.Cells[OPTION_VALUE_COL,SWEEP_INTERVAL_ROW] <> sOriginalSweepInterval then
          begin
            lConfigService.SetSweepInterval(StrToInt(sgOptions.Cells[OPTION_VALUE_COL,SWEEP_INTERVAL_ROW]));
            while (lConfigService.IsServiceRunning) and (not gApplShutdown) do
              Application.ProcessMessages;
          end;

          // Set SQL Dialect if changed
          if sgOptions.Cells[OPTION_VALUE_COL,SQL_DIALECT_ROW] <> sOriginalSQLDialect then
          begin
            try
              lConfigService.SetDBSqlDialect (StrToInt(sgOptions.Cells[OPTION_VALUE_COL,SQL_DIALECT_ROW]));
              while (lConfigService.IsServiceRunning) and (not gApplShutdown) do
                Application.ProcessMessages;
            except
              on E : EIBError do
              begin
                DisplayMsg(ERR_SERVICE, E.Message);
                if (EIBInterBaseError(E).IBErrorCode = isc_lost_db_connection) or
                   (EIBInterBaseError(E).IBErrorCode = isc_unavailable) or
                   (EIBInterBaseError(E).IBErrorCode = isc_network_error) then
                  frmMain.SetErrorState;
                SetErrorState;
                Screen.Cursor := crDefault;
                exit;
              end;
            end;
          end;

          // Set forced writes if changed
          if sgOptions.Cells[OPTION_VALUE_COL,FORCED_WRITES_ROW] <> sOriginalForcedWrites then
          begin
            lConfigService.SetAsyncMode(sOriginalForcedWrites = LZTDBPropFORCED_WRITES_TRUE);  // toggle original value
            while (lConfigService.IsServiceRunning) and (not gApplShutdown) do
              Application.ProcessMessages;
          end;

          // Toggle read only if changed
          if ((sgOptions.Cells[OPTION_VALUE_COL,READ_ONLY_ROW] <> sOriginalReadOnly) and
           (sOriginalReadOnly = LZTDBPropREAD_ONLY_FALSE)) then
          begin
            CurrSelDatabase.Database.Connected := False;  // need to disconnect from database
            try
              if not lConfigService.Active then
                lConfigService.Attach();
            except
              on E : EIBError do
              begin
                DisplayMsg(ERR_SERVER_LOGIN, E.Message);
                if (EIBInterBaseError(E).IBErrorCode = isc_lost_db_connection) or
                   (EIBInterBaseError(E).IBErrorCode = isc_unavailable) or
                   (EIBInterBaseError(E).IBErrorCode = isc_network_error) then
                begin
                  frmMain.SetErrorState;
                  SetErrorState;
                end
                else
                  CurrSelDatabase.Database.Connected := True;
                Screen.Cursor := crDefault;
                Exit;
              end;
            end;
            try
              if lConfigService.Active then               // if attached successfully
              begin
                lConfigService.SetReadOnly(True);         // toggle original value
                CurrSelDatabase.Database.Connected := bOriginalConnectStatus;
              end;
            except
              on E : EIBError do
              begin
                DisplayMsg(ERR_SERVER_LOGIN, E.Message);
                if (EIBInterBaseError(E).IBErrorCode = isc_lost_db_connection) or
                   (EIBInterBaseError(E).IBErrorCode = isc_unavailable) or
                   (EIBInterBaseError(E).IBErrorCode = isc_network_error) then
                begin
                  frmMain.SetErrorState;
                  SetErrorState;
                end
                else
                  // reconnect to database if an exception occurs
                  CurrSelDatabase.Database.Connected := True;
                Screen.Cursor := crDefault;
                Exit;
              end;
            end;
          end; // end if read-only changed
        except
          on E : EIBError do
          begin
            DisplayMsg(ERR_MODIFY_DB_PROPERTIES, E.Message);
            Screen.Cursor := crDefault;
            Exit;
          end;
        end;

      end; // end successful service start
      if lConfigService.Active then
        lConfigService.Detach();  // finally detach
      Screen.Cursor := crDefault;
    end;  // end if connected
  end;  // end if VerifyData
end;

procedure TfrmDBProperties.btnCancelClick(Sender: TObject);
begin
  Cursor := crHourGlass;
  btnApply.Click;
  Cursor := crHourGlass;
  ModalResult := mrOK;
end;

procedure TfrmDBProperties.cbOptionsChange(Sender: TObject);
begin
  FApplyChanges := True;
  btnApply.Enabled := True;
end;

procedure TfrmDBProperties.cbOptionsDblClick(Sender: TObject);
begin
  if (sgOptions.Col = OPTION_VALUE_COL) or (sgOptions.Col = OPTION_NAME_COL) then
  begin
    if cbOptions.ItemIndex = cbOptions.Items.Count - 1 then
      cbOptions.ItemIndex := 0
    else
      cbOptions.ItemIndex := cbOptions.ItemIndex + 1;

    if sgOptions.Col = OPTION_VALUE_COL then
      sgOptions.Cells[sgOptions.Col,sgOptions.Row] := cbOptions.Items[cbOptions.ItemIndex];
  end;
  FApplyChanges := True;
  btnApply.Enabled := True;
end;

procedure TfrmDBProperties.cbOptionsExit(Sender: TObject);
var
  lR     : Trect;
  iIndex : Integer;
begin
  iIndex := cbOptions.Items.IndexOf(cbOptions.Text);

  if (iIndex = -1) and (sgOptions.Row <> SWEEP_INTERVAL_ROW) then
  begin
    MessageDlg(LZTDBPropInvalidOptionValue, mtError, [mbOK], 0);

    cbOptions.ItemIndex := 0;
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
    cbOptions.Visible := True;
    cbOptions.SetFocus;
  end
  else if (sgOptions.Row = SWEEP_INTERVAL_ROW) then
  begin
    sgOptions.Cells[OPTION_VALUE_COL,sgOptions.Row] := cbOptions.Text;
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

procedure TfrmDBProperties.cbOptionsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_DOWN) then
    cbOptions.DroppedDown := true;
end;

procedure TfrmDBProperties.edtAliasNameChange(Sender: TObject);
begin
  FAliasChanged := true;
  FApplyChanges := True;
  btnApply.Enabled := True;
  edtAliasName.Hint := edtAliasName.Text;
end;

procedure TfrmDBProperties.sgOptionsDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
const
  INDENT = 2;
var
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

procedure TfrmDBProperties.sgOptionsSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  lR, lName : TRect;
begin
  cbOptionsExit(Sender);
  cbOptions.Items.Clear;

  case ARow of
    FORCED_WRITES_ROW:
    begin
      cbOptions.Style := csDropDown;
      cbOptions.Items.Add(LZTDBPropFORCED_WRITES_TRUE);
      cbOptions.Items.Add(LZTDBPropFORCED_WRITES_FALSE);
      cbOptions.Tag := FORCED_WRITES_ROW;
    end;
    SWEEP_INTERVAL_ROW:
    begin
      cbOptions.Style := csSimple;
      cbOptions.Text := sgOptions.Cells[OPTION_VALUE_COL,SWEEP_INTERVAL_ROW];
      cbOptions.Tag := SWEEP_INTERVAL_ROW;
    end;
    READ_ONLY_ROW:
    begin
      cbOptions.Style := csDropDown;
      cbOptions.Items.Add(LZTDBPropREAD_ONLY_TRUE);
      cbOptions.Items.Add(LZTDBPropREAD_ONLY_FALSE);
      cbOptions.Tag := READ_ONLY_ROW;
    end;
    SQL_DIALECT_ROW:
    begin
      cbOptions.Style := csDropDown;
      cbOptions.Items.Add(SQL_DIALECT1);
      cbOptions.Items.Add(SQL_DIALECT2);
      cbOptions.Items.Add(SQL_DIALECT3);
      cbOptions.ItemIndex := StrToInt(FOriginalSQLDialect)-1;
      cbOptions.Tag := SQL_DIALECT_ROW;
    end;
  end;

  pnlOptionName.Caption := sgOptions.Cells[OPTION_NAME_COL, ARow];

  if ACol = OPTION_NAME_COL then
    cbOptions.ItemIndex := cbOptions.Items.IndexOf(sgOptions.Cells[ACOL+1,ARow])
  else if ACol = OPTION_VALUE_COL then
  begin
    cbOptions.ItemIndex := cbOptions.Items.IndexOf(sgOptions.Cells[ACol,ARow]);
    if (cbOptions.ItemIndex = -1) or (ARow = SWEEP_INTERVAL_ROW) then
      cbOptions.Text := sgOptions.Cells[ACol,ARow];
  end;

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

function TfrmDBProperties.VerifyInputData(): boolean;
begin
  result := true;  // only if no exceptions raised

  if FAliasChanged and frmMain.AliasExists (edtAliasName.Text) then
  begin
    DisplayMsg(ERR_ALIAS_EXISTS, '');
    edtAliasName.text := FOriginalAlias;
    result := false;
    FAliasChanged := false;
  end;
  
  if TabGeneral.Visible then
  try
    if (StrToInt(sgOptions.Cells[OPTION_VALUE_COL,SWEEP_INTERVAL_ROW]) < SWEEP_INTERVAL_MIN) or
       (StrToInt(sgOptions.Cells[OPTION_VALUE_COL,SWEEP_INTERVAL_ROW]) > SWEEP_INTERVAL_MAX) then
      raise ERangeError.Create(LZTDBPropSweepIntervalValue + IntToStr(SWEEP_INTERVAL_MIN) +
                 ' to ' + IntToStr(SWEEP_INTERVAL_MAX) + '. ' + LZTDBPropPleaseEnterValidSweep);
    if ((FOriginalReadOnly = LZTDBPropREAD_ONLY_TRUE) and
       (sgOptions.Cells[OPTION_VALUE_COL,READ_ONLY_ROW] = LZTDBPropREAD_ONLY_TRUE) and
       ((sgOptions.Cells[OPTION_VALUE_COL,SWEEP_INTERVAL_ROW] <> FOriginalSweepInterval) or
       (sgOptions.Cells[OPTION_VALUE_COL,READ_ONLY_ROW] = FOriginalReadOnly))) then
      raise EPropReadOnly.Create(LZTDBPropDatabasePropCannotBeChanged);
    Exit;
  except
    on E:EConvertError do
    begin
      DisplayMsg(ERR_INVALID_PROPERTY_VALUE, LZTDBPropSweepInterval2 + E.Message );
      result := false;
    end;
    on E:ERangeError do
    begin
      DisplayMsg(ERR_NUMERIC_VALUE, E.Message );
      result := false;
    end;
    on E:EPropReadOnly do
    begin
      DisplayMsg(ERR_INVALID_PROPERTY_VALUE,E.Message);
      result := false;
    end;
  end;
end;

function EditDBProperties(const CurrSelServer: TibcServerNode; var CurrSelDatabase: TibcDatabaseNode): integer;
var
  frmDBProperties: TfrmDBProperties;
  lRegistry: TRegistry;
  lSubKeys: TStringList;
  lIBDBInfo: TIBDatabaseInfo;
  lConfigService: TIBConfigService;
  lListItem: TListItem;
  qryDBProperties: TIBQuery;
  sOriginalForcedWrites: string;
  sOriginalReadOnly: string;
  sOriginalSweepInterval: string;
  sOriginalSQLDialect: string;
  bOriginalConnectStatus: boolean;
begin
  frmDBProperties := TfrmDBProperties.Create(Application.MainForm);
  lRegistry := TRegistry.Create();
  lSubKeys := TStringList.Create();
  lIBDBInfo := TIBDatabaseInfo.Create(frmDBProperties);
  lConfigService := TIBConfigService.Create(frmDBProperties);
  qryDBProperties := TIBQuery.Create(frmDBProperties);
  try
    frmDBProperties.edtAliasName.Text := CurrSelDatabase.NodeName;
    frmDBProperties.edtFilename.Text := CurrSelDatabase.DatabaseFiles.Strings[0];
    frmDBProperties.stxServerName.Caption := CurrSelServer.NodeName;

    if CurrSelServer.Server.Protocol <> Local then
      frmDBProperties.btnSelFilename.Enabled := false;

    bOriginalConnectStatus := CurrSelDatabase.Database.Connected;
    if not CurrSelDatabase.Database.Connected then
      frmDBProperties.TabGeneral.TabVisible := false
    else
    begin  // retrieve database properties
      frmDBProperties.edtFileName.Enabled := false;
      frmDBProperties.btnSelFilename.Enabled := false;
      lIBDBInfo.Database := CurrSelDatabase.Database;                       // assign selected database to db info object
      frmDBProperties.stxPageSize.Caption := IntToStr(lIBDBInfo.PageSize);  // get page size from ib info object
      frmDBProperties.stxDBPages.Caption := IntToStr(lIBDBInfo.Allocation); // get number of pages allocated
      sOriginalSweepInterval := IntToStr(lIBDBInfo.SweepInterval);
      frmDBProperties.sgOptions.Cells[OPTION_VALUE_COL,SWEEP_INTERVAL_ROW] := sOriginalSweepInterval;

      if lIBDBInfo.ForcedWrites <> 0 then    // True
        sOriginalForcedWrites := LZTDBPropFORCED_WRITES_TRUE
      else                                   // False
        sOriginalForcedWrites := LZTDBPropFORCED_WRITES_FALSE;

      frmDBProperties.sgOptions.Cells[OPTION_VALUE_COL,FORCED_WRITES_ROW] := sOriginalForcedWrites;

      if lIBDBInfo.ReadOnly <> 0 then        // True
        sOriginalReadOnly := LZTDBPropREAD_ONLY_TRUE
      else                                   // False
        sOriginalReadOnly := LZTDBPropREAD_ONLY_FALSE;

      frmDBProperties.sgOptions.Cells[OPTION_VALUE_COL,READ_ONLY_ROW] := sOriginalReadOnly;

      sOriginalSQLDialect := IntToStr(lIBDBInfo.DBSQLDialect);
      frmDBProperties.sgOptions.Cells[OPTION_VALUE_COL,SQL_DIALECT_ROW] := sOriginalSQLDialect;

      if not CurrSelDatabase.Database.DefaultTransaction.InTransaction then
        CurrSelDatabase.Database.DefaultTransaction.StartTransaction;

      // Set the defaults for the database properties
      frmDBProperties.SetDefaults (sOriginalReadOnly, sOriginalSweepInterval, sOriginalForcedWrites, sOriginalSQLDialect);
      with qryDBProperties do
      begin
        Close;
        Database := CurrSelDatabase.Database;
        Transaction := CurrSelDatabase.Database.DefaultTransaction;
        SQL.Clear;
        SQL.Add('SELECT RDB$FILE_NAME, RDB$FILE_START FROM RDB$FILES ' +
                'WHERE RDB$SHADOW_NUMBER IS NULL OR RDB$SHADOW_NUMBER < 1 ' +
                'ORDER BY RDB$FILE_SEQUENCE ASC');
        try
          Open;
          First;
          while not eof do
          begin
            lListItem := frmDBProperties.lvSecondaryFiles.Items.Add;
            lListItem.Caption := qryDBProperties.Fields[0].AsString;
            lListItem.SubItems.Add(qryDBProperties.Fields[1].AsString);
            Next;
          end;
        except
          on e:EIBError do
          begin
            lListItem := frmDBProperties.lvSecondaryFiles.Items.Add;
            lListItem.Caption := LZTDBPropNotAvailable;
            lListItem.SubItems.Add(LZTDBPropNotAvailable);
            DisplayMsg(ERR_GET_TABLE_DATA,E.Message + LZTDBPropSecondaryFilesUnavailable);
          end;
        end;
        Close;
        if not CurrSelDatabase.Database.DefaultTransaction.InTransaction then
          CurrSelDatabase.Database.DefaultTransaction.StartTransaction;
        Transaction := CurrSelDatabase.Database.DefaultTransaction;
        SQL.Clear;
        SQL.Add('SELECT RDB$OWNER_NAME FROM RDB$RELATIONS ' +
                'WHERE RDB$RELATION_NAME = ''RDB$DATABASE'' ');
        try
          Open;
          First;
          frmDBProperties.stxDBOwner.Caption := Fields[0].AsString;
        except
          on E:EIBError do
          begin
            frmDBProperties.stxDBOwner.Caption := LZTDBPropNotAvailable;
            DisplayMsg(ERR_GET_TABLE_DATA,E.Message + LZTDBPropDatabaseOwnerUnavailable);
          end;
        end;
        Close;
      end; // with qryDBProperties
    end; // retrieve database properties
    frmDBProperties.CurrSelDatabase := CurrSelDatabase;
    frmDBProperties.CurrSelServer := CurrSelServer;
    frmDBProperties.sOriginalForcedWrites := sOriginalForcedWrites;
    frmDBProperties.sOriginalReadOnly := sOriginalReadOnly;
    frmDBProperties.sOriginalSweepInterval := sOriginalSweepInterval;
    frmDBProperties.sOriginalSQLDialect := sOriginalSQLDialect;
    frmDBProperties.bOriginalConnectStatus := bOriginalConnectStatus;

    frmDBProperties.ShowModal;
    Application.ProcessMessages;
    result := SUCCESS;
    
  finally
    Screen.Cursor := crDefault;
    qryDBProperties.Free;
    lConfigService.Free;
    lIBDBInfo.Free;
    frmDBProperties.Free;
    lSubKeys.Free;
    lRegistry.Free;
  end;
end;

procedure TfrmDBProperties.btnSelFilenameClick(Sender: TObject);
var
  lOpenDialog: TOpenDialog;
begin
  lOpenDialog := nil;
  try
  begin
    lOpenDialog := TOpenDialog.Create(self);
    // setup Open Dialog title, extension, filters and options
    lOpenDialog.Title := LZTDBPropSelectDatabase;
    lOpenDialog.DefaultExt := 'fdb';
    lOpenDialog.Filter := LZTDBPropDatabaseFile;
    lOpenDialog.Options := [ofHideReadOnly,ofNoNetworkButton, ofEnableSizing];
    if lOpenDialog.Execute then
    begin
      // get filename
      edtFilename.Text := lOpenDialog.FileName;
      // if no dbalias is specified then make it the name of the file
      if (edtAliasName.Text = '') or (edtAliasName.Text = ' ') then
      begin
        edtAliasName.Text := ExtractFileName(edtFilename.Text);
        if (edtAliasName.Text = '') or (edtAliasName.Text = ' ') then
        begin
          edtAliasName.Text := ExtractFileName(edtFilename.Text);
        end;
      end;
    end;
  end
  finally
    lOpenDialog.free;
  end;
end;

procedure TfrmDBProperties.edtFilenameChange(Sender: TObject);
begin
  FApplyChanges := True;
  btnApply.Enabled := True;
  edtFilename.Hint := edtFilename.Text;
end;

procedure TfrmDBProperties.SetDefaults(const readOnly, sweep, synch,
  dialect: String);
begin

  FOriginalReadOnly := readOnly;
  FOriginalSweepInterval := Sweep;
  FOriginalSynchMode := synch;
  FOriginalSQLDialect := dialect;

end;

procedure TfrmDBProperties.edtFilenameExit(Sender: TObject);
begin
  inherited;
  if not (IsValidDBName(edtFilename.text)) then
     DisplayMsg(WAR_REMOTE_FILENAME, Format(LZTDBPropFile, [edtFileName.text]));
end;

procedure TfrmDBProperties.Button1Click(Sender: TObject);
begin
  inherited;
  ModalResult := mrCancel;
end;

Procedure TfrmDBProperties.TranslateVisual;
Begin
  btnSelFilename.Hint := LZTDBPropbtnSelFilenameHint;
  lblServerName.Caption := LZTDBProplblServerName;
  TabAlias.Caption := LZTDBPropTabAlias;
  TabGeneral.Caption := LZTDBPropTabGeneral;
  lblAliasName.Caption := LZTDBProplblAliasName;
  lblFilename.Caption := LZTDBProplblFilename;
  btnCancel.Caption := LZTDBPropbtnCancel;
  Button1.Caption := LZTDBPropButton1;
  btnApply.Caption := LZTDBPropbtnApply;
  gbSummaryInfo.Caption := LZTDBPropgbSummaryInfo;
  lblDBOwner.Caption := LZTDBProplblDBOwner;
  lblOptions.Caption := LZTDBProplblOptions;
  lblDBPages.Caption := LZTDBProplblDBPages;
  lblPageSize.Caption := LZTDBProplblPageSize;
  lvSecondaryFiles.Columns[0].Caption := LZTDBProplvSecondaryFilesCol0;
  lvSecondaryFiles.Columns[1].Caption := LZTDBProplvSecondaryFilesCol1;
  Self.Caption := LZTDBPropFormTitle;
End;

end.
