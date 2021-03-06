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

unit frmuDBValidationReport;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, StdCtrls, ExtCtrls, IB, IBServices, IBDatabase, zluibcClasses, frmuDlgClass, resstring;

type
  TfrmDBValidationReport = class(TDialog)
    lblDatabaseName: TLabel;
    stxDatabaseName: TStaticText;
    bvlLine1: TBevel;
    memReport: TMemo;
    lblOptions: TLabel;
    pnlOptionName: TPanel;
    sgOptions: TStringGrid;
    cbOptions: TComboBox;
    btnRepair: TButton;
    btnCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnRepairClick(Sender: TObject);
    procedure cbOptionsChange(Sender: TObject);
    procedure cbOptionsDblClick(Sender: TObject);
    procedure cbOptionsExit(Sender: TObject);
    procedure cbOptionsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure sgOptionsDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure sgOptionsSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure FormShow(Sender: TObject);
    Procedure TranslateVisual;override;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function ShowReport(const str : String; const SourceServerNode: TibcServerNode;
  const CurrSelDatabase: TibcDatabaseNode; const Errors: boolean): integer;

implementation

uses zluGlobal, frmuMessage, frmuMain, IBErrorCodes;

{$R *.lfm}

const
  OPTION_NAME_COL = 0;
  OPTION_VALUE_COL = 1;
  VALIDATE_RECORD_FRAGMENTS_ROW = 0;
  // READ_ONLY_VALIDATION_ROW = 1;
  IGNORE_CHECKSUM_ERRORS_ROW = 1;

function ShowReport(const str : String; const SourceServerNode: TibcServerNode;
  const CurrSelDatabase: TibcDatabaseNode; const Errors: Boolean): integer;
var
  frmReport        : TfrmDBValidationReport;
  lValidation      : TIBValidationService;
  lValidateOptions : TValidateOptions;
begin
  frmReport   := Nil;
  lValidation := Nil;
  try
    frmReport:=TfrmDBValidationReport.Create(Application.MainForm);
    frmReport.stxDatabaseName.Caption := CurrSelDatabase.NodeName;
    frmReport.stxDatabaseName.Hint := CurrSelDatabase.NodeName;

    if Errors then
    begin
      frmReport.memReport.SetTextBuf(PChar(str));
      if (StrPos(PChar(str), PChar(LZTDBValidReportPleaseRetry)) <> Nil) or
        (StrPos(PChar(str), PChar(LZTDBValidReportPlausibleOptions)) <> Nil) then
        frmReport.btnRepair.Enabled := False;
      frmReport.memReport.Lines.Append('');
      frmReport.memReport.Lines.Append(LZTDBValidReportCheckFirebirdLogFileAddInfo);
    end
    else
    begin
      frmReport.sgOptions.Enabled := false;
      frmReport.cbOptions.Enabled := false;
      frmReport.memReport.Lines.Add(LZTDBValidReportNoDatabaseValidError);
      frmReport.btnRepair.Enabled := False;
      frmReport.btnCancel.Caption := LZTDBValidReportOK;
      frmReport.btnCancel.Default := true;
    end;

    frmReport.ShowModal;

    if (frmReport.ModalResult = mrOK) and
      (not frmReport.GetErrorState) then
    begin
      try
        lValidation := TIBValidationService.Create(Nil);
        lValidateOptions := [];

        // specify the repair database option
        Include(lValidateOptions, MendDB);

        try
          // assign server details
          lValidation.LoginPrompt := false;
          lValidation.ServerName := SourceServerNode.Server.ServerName;
          lValidation.Protocol := SourceServerNode.Server.Protocol;
          lValidation.Params.Clear;
          lValidation.Params.Assign(SourceServerNode.Server.Params);
          lValidation.Attach();        // attach to server
        except                         // if an exception occurs then trap it
          on E:EIBError do             // and display error message
          begin
            DisplayMsg(ERR_SERVER_LOGIN, E.Message);
            result := FAILURE;
            if (EIBInterBaseError(E).IBErrorCode = isc_lost_db_connection) or
               (EIBInterBaseError(E).IBErrorCode = isc_unavailable) or
               (EIBInterBaseError(E).IBErrorCode = isc_network_error) then
              frmMain.SetErrorState;
            Exit;
          end;
        end;

        if lValidation.Active = true then
        begin
          // assign database details
          case SourceServerNode.Server.Protocol of
            TCP : lValidation.DatabaseName := Format('%s:%s',[SourceServerNode.ServerName,CurrSelDatabase.DatabaseFiles.Strings[0]]);
            NamedPipe : lValidation.DatabaseName := Format('\\%s\%s',[SourceServerNode.ServerName,CurrSelDatabase.DatabaseFiles.Strings[0]]);
            SPX : lValidation.DatabaseName := Format('%s@%s',[SourceServerNode.ServerName,CurrSelDatabase.DatabaseFiles.Strings[0]]);
            Local : lValidation.DatabaseName := CurrSelDatabase.DatabaseFiles.Strings[0];
          end;

          // determine which options have been selected
          if frmReport.sgOptions.Cells[1,VALIDATE_RECORD_FRAGMENTS_ROW] = LZTDBValidReportTrue then
            Include(lValidateOptions, ValidateFull)
          else
            Exclude(lValidateOptions, ValidateFull);

          if frmReport.sgOptions.Cells[1,IGNORE_CHECKSUM_ERRORS_ROW] = LZTDBValidReportTrue then
            Include(lValidateOptions, IgnoreChecksum)
          else
            Exclude(lValidateOptions, IgnoreChecksum);

          // assign validation options
          lValidation.Options := lValidateOptions;

          // start service
          try
            lValidation.ServiceStart;
            while (lValidation.IsServiceRunning) and (not gApplShutdown) do
              Application.ProcessMessages;

            if lValidation.Active then
              lValidation.Detach();

            ShowMessage (LZTDBValidReportDatabaseValidComplete);
          except
            on E: EIBError do
            begin
              DisplayMsg(EIBInterBaseError(E).IBErrorCode, E.Message);
              if (EIBInterBaseError(E).IBErrorCode = isc_lost_db_connection) or
                 (EIBInterBaseError(E).IBErrorCode = isc_unavailable) or
                 (EIBInterBaseError(E).IBErrorCode = isc_network_error) then
                frmMain.SetErrorState;
            end;
          end;
        end
        else
        begin
          result := FAILURE;
          Exit;
        end;
      finally
        lValidation.Free;
      end;
      Result:=SUCCESS;
    end
    else
    begin
      Result:=FAILURE;
    end;
  finally
    frmReport.Free;
  end;
end;

procedure TfrmDBValidationReport.FormCreate(Sender: TObject);
begin
  inherited;
  sgOptions.DefaultRowHeight := cbOptions.Height;
  cbOptions.Visible := True;
  pnlOptionName.Visible := True;

  sgOptions.RowCount := 2;

  sgOptions.Cells[OPTION_NAME_COL,VALIDATE_RECORD_FRAGMENTS_ROW] := LZTDBValidReportValidateRecordFrag;
  sgOptions.Cells[OPTION_VALUE_COL,VALIDATE_RECORD_FRAGMENTS_ROW] := LZTDBValidReportFalse;

  // sgOptions.Cells[OPTION_NAME_COL,READ_ONLY_VALIDATION_ROW] := LZTDBValidReportReadOnlyValid;
  // sgOptions.Cells[OPTION_VALUE_COL,READ_ONLY_VALIDATION_ROW] := LZTDBValidReportFalse;

  sgOptions.Cells[OPTION_NAME_COL,IGNORE_CHECKSUM_ERRORS_ROW] := LZTDBValidReportIgnoreChecksumError;
  sgOptions.Cells[OPTION_VALUE_COL,IGNORE_CHECKSUM_ERRORS_ROW] := LZTDBValidReportFalse;

  pnlOptionName.Caption := LZTDBValidReportValidateRecordFrag;
  cbOptions.Items.Add(LZTDBValidReportTrue);
  cbOptions.Items.Add(LZTDBValidReportFalse);
  cbOptions.ItemIndex := 1;
end;

procedure TfrmDBValidationReport.cbOptionsChange(Sender: TObject);
begin
  {
  sgOptions.Cells[sgOptions.Col,sgOptions.Row] :=
    cbOptions.Items[cbOptions.ItemIndex];
  cbOptions.Visible := false;
  sgOptions.SetFocus;
  }
end;

procedure TfrmDBValidationReport.cbOptionsExit(Sender: TObject);
var
  lR     : TRect;
  iIndex : Integer;
begin
  iIndex := cbOptions.Items.IndexOf(cbOptions.Text);

  if (iIndex = -1) then
  begin
    MessageDlg(LZTDBValidReportInvalidOptionValue, mtError, [mbOK],0);

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
  else if (sgOptions.Col <> OPTION_NAME_COL) then
  begin
    sgOptions.Cells[sgOptions.Col,sgOptions.Row] := cbOptions.Items[iIndex];
  end
  else
  begin
    sgOptions.Cells[OPTION_VALUE_COL,sgOptions.Row] := cbOptions.Items[iIndex];
  end;
end;

procedure TfrmDBValidationReport.sgOptionsDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
const
  INDENT = 2;
var
  lLeft: integer;
  lText: string;
begin
  with sgOptions.canvas do
  begin
    if (ACol = OPTION_VALUE_COL) then
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

procedure TfrmDBValidationReport.sgOptionsSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
var
  lR, lName : TRect;
begin
  cbOptions.Items.Clear;
  case ARow of
    VALIDATE_RECORD_FRAGMENTS_ROW:
    begin
      cbOptions.Items.Add(LZTDBValidReportTrue);
      cbOptions.Items.Add(LZTDBValidReportFalse);
    end;
    {
    READ_ONLY_VALIDATION_ROW:
    begin
      cbOptions.Items.Add(LZTDBValidReportTrue);
      cbOptions.Items.Add(LZTDBValidReportFalse);
    end;
    }
    IGNORE_CHECKSUM_ERRORS_ROW:
    begin
      cbOptions.Items.Add(LZTDBValidReportTrue);
      cbOptions.Items.Add(LZTDBValidReportFalse);
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

procedure TfrmDBValidationReport.cbOptionsDblClick(Sender: TObject);
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
end;

procedure TfrmDBValidationReport.cbOptionsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_DOWN) then
    cbOptions.DroppedDown := true;
end;

procedure TfrmDBValidationReport.btnRepairClick(Sender: TObject);
begin
  ModalResult:=mrOK;
end;

procedure TfrmDBValidationReport.btnCancelClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TfrmDBValidationReport.FormShow(Sender: TObject);
begin
  inherited;
  if btnCancel.Default then
    btnCancel.SetFocus;
end;

Procedure TfrmDBValidationReport.TranslateVisual;
Begin
  lblDatabaseName.Caption := LZTDBValidReportlblDatabaseName;
  lblOptions.Caption := LZTDBValidReportlblOptions;
  btnRepair.Caption := LZTDBValidReportbtnRepair;
  btnCancel.Caption := LZTDBValidReportbtnCancel;
  Self.Caption := LZTDBValidReportFormTitle;
End;

end.
