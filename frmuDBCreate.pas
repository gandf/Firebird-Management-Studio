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
 * Contributor(s): Krzysztof Golko.
}

unit frmuDBCreate;

{$MODE Delphi}

interface

uses
  SysUtils, Forms, ExtCtrls, StdCtrls, Classes, Controls, Dialogs,
  zluibcClasses, Grids, LCLIntf, LCLType, Graphics, IB, IBDatabase,
  frmuDlgClass, resstring;

type
  TfrmDBCreate = class(TDialog)
    lblServer: TLabel;
    lblDBAlias: TLabel;
    stxServer: TStaticText;
    edtDBAlias: TEdit;
    lblDatabaseFiles: TLabel;
    sgDatabaseFiles: TStringGrid;
    lblOptions: TLabel;
    sgOptions: TStringGrid;
    btnOK: TButton;
    btnCancel: TButton;
    cbOptions: TComboBox;
    pnlOptionName: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure cbOptionsChange(Sender: TObject);
    procedure cbOptionsExit(Sender: TObject);
    procedure sgDatabaseFilesDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure sgDatabaseFilesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure sgDatabaseFilesSelectCell(Sender: TObject; ACol,ARow: Integer; var CanSelect: Boolean);
    procedure sgOptionsDblClick(Sender: TObject);
    procedure sgOptionsDrawCell(Sender: TObject; ACol, ARow: Integer;Rect: TRect; State: TGridDrawState);
    procedure sgOptionsSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure cbOptionsDblClick(Sender: TObject);
    procedure edtDBAliasChange(Sender: TObject);
    procedure cbOptionsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    Procedure TranslateVisual;override;
  private
    { Private declarations }
    function VerifyInputData(): boolean;
  public
    { Public declarations }
    FCurrSelServer: TIbcServerNode;
  end;

function CreateDB(var DBAlias: string; var DatabaseFiles: TStringList; const SelServerNode: TibcServerNode): integer;

implementation

uses zluGlobal,frmuMessage, zluUtility, zluPersistent;

{$R *.lfm}

const
  OPTION_NAME_COL = 0;            // option name column position
  OPTION_VALUE_COL = 1;           // option value column position
  PAGE_SIZE_ROW = 0;              // page size row position
  DEFAULT_CHARSET_ROW = 1;        // char set row position
  SQL_DIALECT_ROW = 2;
  MIN_PRIMARY_FILE_SIZE = 230;    // min page size for primary file
  MIN_SECONDARY_FILE_SIZE = 2;    // min page size for secondary files

function CreateDB(var DBAlias: string; var DatabaseFiles: TStringList; const SelServerNode: TibcServerNode): integer;
var
  frmDBCreate: TfrmDBCreate;          // create DB form
  lDatabase: TIBDatabase;             // temp database object
  iRow: Integer;                      // row counter for database string grid
  lStr: String;                       // string value for starting page
begin
  frmDBCreate := TfrmDBCreate.Create(Application.MainForm);
  lDatabase := TIBDatabase.Create(nil); // create database object
  try
                                          // set server name in form
    frmDBCreate.stxServer.Caption := SelServerNode.Servername;
    frmDBCreate.FCurrSelServer := SelServerNode;
    frmDBCreate.ShowModal;                // show form as modal dialog
    if frmDBCreate.ModalResult = mrOK then
    begin
      // of OK button is pressed and all data has been validated
      // start setting create database parameters
      // set the first database filename
      case SelServerNode.Server.Protocol of
        TCP: lDatabase.DatabaseName := Format('%s:%s',[SelServerNode.ServerName, frmDBCreate.sgDatabaseFiles.Cells[0,1]]);
        NamedPipe: lDatabase.DatabaseName := Format('\\%s\%s',[SelServerNode.ServerName, frmDBCreate.sgDatabaseFiles.Cells[0,1]]);
        SPX: lDatabase.DatabaseName := Format('%s@%s',[SelServerNode.ServerName, frmDBCreate.sgDatabaseFiles.Cells[0,1]]);
        Local: lDatabase.DatabaseName := frmDBCreate.sgDatabaseFiles.Cells[0,1];
      end;
      DatabaseFiles.Add(frmDBCreate.sgDatabaseFiles.Cells[0,1]);

      // supply login info for the current server
      lDatabase.Params.Add(Format(LZTDBCreateUser,[SelServerNode.UserName]));
      lDatabase.Params.Add(Format(LZTDBCreatePassword,[SelServerNode.Password]));

      // set page size
      if frmDBCreate.sgOptions.Cells[1,0] <> '' then
        lDatabase.Params.Add(Format(LZTDBCreatePageSize,[frmDBCreate.sgOptions.Cells[1,0]]));

      // set default character set
      if frmDBCreate.sgOptions.Cells[1,DEFAULT_CHARSET_ROW] <> LZTDBCreateNone then
        lDatabase.Params.Add(Format(LZTDBCreateDefaultCharacterSet,[frmDBCreate.sgOptions.Cells[1,1]]));

      // if more than 1 filename has been supplied then this is a
      // multifile database
      if frmDBCreate.sgDatabaseFiles.Cells[0,2] <> '' then
      begin
        // set length of first database file (in pages)
        if frmDBCreate.sgDatabaseFiles.Cells[1,1] <> '' then
          lDatabase.Params.Add(Format(LZTDBCreateLength, [frmDBCreate.sgDatabaseFiles.Cells[1,1]]));

        iRow:=2;                       // begin looping through rows starting from third row
        while (iRow < frmDBCreate.sgDatabaseFiles.RowCount) and
          (frmDBCreate.sgDatabaseFiles.Cells[0,iRow] <> '') do
        begin
          // set secondary filename
          lDatabase.Params.Add(Format(LZTDBCreateFile, [frmDBCreate.sgDatabaseFIles.Cells[0,iRow]]));
          DatabaseFiles.Add(frmDBCreate.sgDatabaseFiles.Cells[0,iRow]);

          // set length of file (in pages)
          if frmDBCreate.sgDatabaseFIles.Cells[1,iRow] <> '' then
            lDatabase.Params.Add(Format(LZTDBCreateLength, [frmDBCreate.sgDatabaseFiles.Cells[1,iRow]]));

          // set starting page (length of last file + 1)
          lStr:=IntToStr(StrToInt(frmDBCreate.sgDatabaseFiles.Cells[1,iRow - 1]) + 1);
          lDatabase.Params.Add(Format(LZTDBCreateStarting, [lStr]));

          Inc(iRow);                   // increment row count
        end;  // of loop through rows
      end;  // of multifile check

      lDatabase.SQLDialect := StrToInt(frmDBCreate.sgOptions.Cells[1,SQL_DIALECT_ROW]);

      lDatabase.CreateDatabase;        // create database
      lDatabase.Connected:=False;      // disconnect from database
      DBAlias := frmDBCreate.edtDBAlias.text;
      result := SUCCESS;               // set result as success
    end
    else                               // if OK button is pressed and data is bad or Cancel
      result := FAILURE;               // button is pressed then set result as failure
  finally
    // deallocate memory
    frmDBCreate.Free;
    lDatabase.Free;
  end;
end;

procedure TfrmDBCreate.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

function TfrmDBCreate.VerifyInputData(): boolean;
var
  x, y      : Integer;                 // row and column counters
  iMax      : Integer;                 // max row
  iPageSize : Integer;                 // integer page size
  lGridRect : TGridRect;               // current gridregion being validated
begin
  result := true;                      // assume all data is valid

  // check if a database alias was specified
  if (edtDBAlias.Text = '') or (edtDBAlias.Text = ' ') then
  begin
    DisplayMsg(ERR_DB_ALIAS,'');
    edtDBAlias.SetFocus;
    result := false;
    Exit;
  end;

  if PersistentInfo.DatabaseAliasExists(FCurrSelServer.NodeName, edtDBAlias.Text) then
  begin                                // show error message
    DisplayMsg(ERR_DB_ALIAS,LZTDBCreateDatabaseAlreadyExist);
    edtDBAlias.SetFocus;               // give focus to control
    result := false;
    Exit;
  end;

  // determine the maximum number of rows that have been used
  iMax:=1;
  while (iMax < sgDatabaseFiles.RowCount - 1) and
    (sgDatabaseFiles.Cells[0,iMax] <> '') and
    (sgDatabaseFiles.Cells[1,iMax] <> '') do
  begin
    if not (IsValidDBName(sgDatabaseFiles.Cells[0,iMax])) then
       DisplayMsg(WAR_REMOTE_FILENAME, Format(LZTDBCreateFile2, [sgDatabaseFiles.Cells[0,iMax]]));
    Inc(iMax);
  end;

  // loop through every field in grid until the maximum row has been reached
  for y:=1 to iMax - 1 do              // loop through rows
  begin                                // loop through columns
    for x:=0 to sgDatabaseFiles.ColCount - 1 do
    begin
      lGridRect.Left:=x;               // set current grid region
      lGridRect.Top:=y;
      lGridRect.Right:=x;
      lGridRect.Bottom:=y;

      // if the current field is empty and it is not the last line
      if ((sgDatabaseFiles.Cells[x,y] = '') or (sgDatabaseFiles.Cells[x,y] = ' ')) and
        ((sgDatabaseFiles.Cells[0,y+1] <> '') and (sgDatabaseFiles.Cells[1,y+1] <> '')) then
      begin
        case x of
          0 : DisplayMsg(ERR_DB_FILE,'');   // show the appropriate error message
          1 : DisplayMsg(ERR_DB_SIZE,'');   // corresponding to the field that is blank
        end;
        sgDatabaseFiles.SetFocus;      // give focus to string grid and select the erring field
        sgDatabaseFiles.Selection:=lGridRect;
        result := false;               // set result to false
        exit;
      end
      else   // if this is the last line check for a valid filename -
      begin  // last file does not need a value for the filesize
        if (sgDatabaseFiles.Cells[x,y] = '') or (sgDatabaseFiles.Cells[x,y] = ' ') then
        begin
          case x of
            0 : DisplayMsg(ERR_DB_FILE,'');   // show the appropriate error message
            1 : DisplayMsg(ERR_DB_SIZE,'');   // corresponding to the field that is blank
          end;
          sgDatabaseFiles.SetFocus;      // give focus to string grid and select the erring field
          sgDatabaseFiles.Selection:=lGridRect;
          result := false;               // set result to false
          exit;
        end;
      end;

      // must check to see if all sizes are numeric and exceed minimum values
      // if the current column is size column and this is not a blank line
      if (x = 1) and (sgDatabaseFiles.Cells[0,y] <> '') and (sgDatabaseFIles.Cells[1,y] <> '') then
      begin
        try                            // then convert string to integer
          iPageSize:=StrToInt(sgDatabaseFiles.Cells[x,y]);

          // check file size of primary file
          if (y = 1) and (iPageSize < MIN_PRIMARY_FILE_SIZE) then
          begin
            DisplayMsg(ERR_DB_SIZE,LZTDBCreateMinPageSize230);
            sgDatabaseFiles.SetFocus;  // give focus to string grid and select the erring field
            sgDatabaseFiles.Selection:=lGridRect;
            result := false;           // set result to false
            exit;
          end;

          // check file size of secondary files
          if (y <> 1) and (iPageSize < MIN_SECONDARY_FILE_SIZE) then
          begin
            DisplayMsg(ERR_DB_SIZE,LZTDBCreateMinPageSizeSecond2);
            sgDatabaseFiles.SetFocus;  // give focus to string grid and select the erring field
            sgDatabaseFiles.Selection:=lGridRect;
            result := false;           // set result to false
            exit;
          end;
        except on EConvertError do     // if an error occurs ten
          begin                        // display error message
            DisplayMsg(ERR_NUMERIC_VALUE,'');
            sgDatabaseFiles.SetFocus;  // give focus to string grid and select the erring field
            sgDatabaseFiles.Selection:=lGridRect;
            result := false;           // set result to false
            exit;
          end;  // of exception
        end;  // of try except block
      end;  // of column and blank line check
    end;  // of column loop
  end;  // of row loop
end;

procedure TfrmDBCreate.btnOKClick(Sender: TObject);
begin
  if VerifyInputData() then
    ModalResult := mrOK;
end;

procedure TfrmDBCreate.FormCreate(Sender: TObject);
begin
  inherited;
  sgOptions.DefaultRowHeight := cbOptions.Height;
  cbOptions.Visible := True;
  pnlOptionName.Visible := True;

  sgDatabaseFiles.Cells[0,0] := LZTDBCreateFilename;
  sgDatabaseFiles.Cells[1,0] := LZTDBCreateSizePages;

  sgOptions.Cells[OPTION_NAME_COL,PAGE_SIZE_ROW] := LZTDBCreatePageSize2;
  sgOptions.Cells[OPTION_VALUE_COL,PAGE_SIZE_ROW] := '4096';

  sgOptions.Cells[OPTION_NAME_COL,DEFAULT_CHARSET_ROW] := LZTDBCreateDefaultCharracterSet;
  sgOptions.Cells[OPTION_VALUE_COL,DEFAULT_CHARSET_ROW] := gAppSettings[CHARACTER_SET].Setting;

  sgOptions.Cells[OPTION_NAME_COL,SQL_DIALECT_ROW] := LZTDBCreateSQLDialect;
  sgOptions.Cells[OPTION_VALUE_COL,SQL_DIALECT_ROW] := gAppSettings[DEFAULT_DIALECT].Setting;

  pnlOptionName.Caption := LZTDBCreatePageSize2;
  cbOptions.Items.Add('1024');
  cbOptions.Items.Add('2048');
  cbOptions.Items.Add('4096');
  cbOptions.Items.Add('8192');
  cbOptions.ItemIndex := 2;
end;

procedure TfrmDBCreate.cbOptionsChange(Sender: TObject);
begin
  {
  if cbOptions.Style=csDropDownList then
  begin
    sgOptions.Cells[sgOptions.Col,sgOptions.Row] :=
      cbOptions.Items[cbOptions.ItemIndex];
    cbOptions.Visible := false;
    sgOptions.SetFocus;
  end
  else
  begin
    sgOptions.Cells[sgOptions.Col,sgOptions.Row] :=
      cbOptions.Text;
  end;
  }
end;

procedure TfrmDBCreate.cbOptionsExit(Sender: TObject);
var
  lR     : TRect;
  iIndex : Integer;
begin
  iIndex := cbOptions.Items.IndexOf(cbOptions.Text);

  if (iIndex = -1) then
  begin
    MessageDlg(LZTDBCreateInvalidOptionValue, mtError, [mbOK], 0);

    cbOptions.ItemIndex := 0;
    // Size and position the combo box to fit the cell
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

procedure TfrmDBCreate.sgOptionsSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  lR,lName : TRect;
begin
  cbOptions.Items.Clear;               // clear items in combo box
  case ARow of                         // determine which row is being selected
    PAGE_SIZE_ROW:                     // if the page size is selected
    begin                              // populate the combo with
      cbOptions.Items.Add('1024');
      cbOptions.Items.Add('2048');
      cbOptions.Items.Add('4096');
      cbOptions.Items.Add('8192');
    end;
    DEFAULT_CHARSET_ROW:               // if the default charset row is being selected
    begin                              // populate the combo with
      with cbOptions.Items do
      begin
        Add('ASCII');
        Add('BIG_5');
        Add('CP943C');
        Add('CYRL');
        Add('DOS437');
        Add('DOS737');
        Add('DOS850');
        Add('DOS852');
        Add('DOS857');
        Add('DOS858');
        Add('DOS860');
        Add('DOS861');
        Add('DOS862');
        Add('DOS863');
        Add('DOS864');
        Add('DOS865');
        Add('DOS866');
        Add('DOS869');
        Add('EUCJ_0208');
        Add('GB_2312');
        Add('ISO8859_1');
        Add('ISO8859_2');
        Add('ISO8859_3');
        Add('ISO8859_4');
        Add('ISO8859_5');
        Add('ISO8859_6');
        Add('ISO8859_7');
        Add('ISO8859_8');
        Add('ISO8859_9');
        Add('ISO8859_13');
        Add('KOI8R');
        Add('KOI8U');
        Add('KSC_5601');
        Add('NEXT');
        Add('None');
        Add('OCTETS');
        Add('SJIS_0208');
        Add('TIS620');
        Add('UNICODE_FSS');
        Add('UTF8');
        Add('WIN1250');
        Add('WIN1251');
        Add('WIN1252');
        Add('WIN1253');
        Add('WIN1254');
        Add('WIN1255');
        Add('WIN1256');
        Add('WIN1257');
        Add('WIN1258');
      end;
    end;
    SQL_DIALECT_ROW:
    begin
      cbOptions.Items.Add('1');
      cbOptions.Items.Add('3');
    end;
  end;

  pnlOptionName.Caption := sgOptions.Cells[OPTION_NAME_COL, ARow];

  if ACol = OPTION_NAME_COL then       // copy selected combo item to proper grid location
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

procedure TfrmDBCreate.sgOptionsDblClick(Sender: TObject);
begin
  {
  // if the option value column is selected
  if sgOptions.Col = OPTION_VALUE_COL then
  begin
    // cycle through items
    if cbOptions.ItemIndex = cbOptions.Items.Count - 1 then
      cbOptions.ItemIndex := 0
    else
      cbOptions.ItemIndex := cbOptions.ItemIndex + 1;

    sgOptions.Cells[sgOptions.Col,sgOptions.Row] :=
      cbOptions.Items[cbOptions.ItemIndex];
    cbOptions.Visible := false;
    sgOptions.SetFocus;
  end;
  // if the option name column is selected
  else  if (sgOptions.Col = OPTION_NAME_COL) and
    (sgOptions.Row >= PAGE_SIZE_ROW) and (sgOptions.Row <= DEFAULT_CHARSET_ROW) then
  begin
    // cycle through items
    if cbOptions.ItemIndex = cbOptions.Items.Count - 1 then
      cbOptions.ItemIndex := 0
    else
      cbOptions.ItemIndex := cbOptions.ItemIndex + 1;

    sgOptions.Cells[sgOptions.Col + 1,sgOptions.Row] :=
      cbOptions.Items[cbOptions.ItemIndex];
    cbOptions.Visible := false;
    sgOptions.SetFocus;
  end;
  }
end;

procedure TfrmDBCreate.sgOptionsDrawCell(Sender: TObject; ACol,
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

procedure TfrmDBCreate.sgDatabaseFilesSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  // the first cell in the grid (top left) is always selectable
  if (ACol = 0) and (ARow = 1) then
  begin
    CanSelect:=True;
  end

  // cell is selectable if previous column is populated
  else if (ACol - 1 >= 0) and (sgDatabaseFiles.Cells[ACol-1,ARow] <> '') then
  begin
    CanSelect := True;
  end

  // cell is selectable if last column in previous row is populated
  else if (sgDatabaseFiles.Cells[1,ARow-1] <> '') and (ARow - 1 <> 0) and (ACol = 0) then
  begin
    CanSelect := True;
  end

  // cell is not selectable if all other checks fail and current cell is empty
  else if (sgDatabaseFiles.Cells[ACol,ARow] = '') then
  begin
    CanSelect := False;
  end;
end;

procedure TfrmDBCreate.sgDatabaseFilesDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
const
  INDENT = 2;
var
  lLeft: integer;
  lText: string;
begin
  with sgDatabaseFiles.canvas do
  begin
    if (ACol = 2) and (ARow <> 0) then
    begin
      font.color := clBlack;
      if brush.color = clHighlight then
        font.color := clWhite;
      lText := sgDatabaseFiles.Cells[ACol,ARow];
      lLeft := Rect.Left + INDENT;
      TextRect(Rect, lLeft, Rect.top + INDENT, lText);
    end;
  end;
end;

procedure TfrmDBCreate.sgDatabaseFilesKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  lKey : Word;
begin
  if (Key = VK_TAB) and (ssCtrl in Shift) then
  begin
    if sgDatabaseFiles.Col < sgDatabaseFiles.ColCount - 1 then
    begin
      sgDatabaseFiles.Col := sgDatabaseFiles.Col + 1;
    end
    else
    begin
      if sgDatabaseFiles.Row = sgDatabaseFiles.RowCount - 1 then
        sgDatabaseFiles.RowCount := sgDatabaseFiles.RowCount + 1;
      sgDatabaseFiles.Col := 0;
      sgDatabaseFiles.Row := sgDatabaseFiles.Row + 1;
    end;
  end;

  if (Key = VK_RETURN) and
    (sgDatabaseFiles.Cells[sgDatabaseFiles.Col,sgDatabaseFiles.Row] <> '') then
  begin
    lKey := VK_TAB;
    sgDatabaseFilesKeyDown(Self, lKey, [ssCtrl]);
  end;

end;

procedure TfrmDBCreate.cbOptionsDblClick(Sender: TObject);
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

procedure TfrmDBCreate.edtDBAliasChange(Sender: TObject);
begin
  edtDBAlias.Hint := edtDBAlias.Text;
end;

procedure TfrmDBCreate.cbOptionsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_DOWN) then
    cbOptions.DroppedDown := True;
end;

Procedure TfrmDBCreate.TranslateVisual;
Begin
  lblServer.Caption := LZTDBCreateServerCaption;
  lblDBAlias.Caption := LZTDBCreateAliasCaption;
  lblDatabaseFiles.Caption := LZTDBCreateFileCaption;
  lblOptions.Caption := LZTDBCreateOptionsCaption;
  btnOK.Caption := LZTDBCreateOKCaption;
  btnCancel.Caption := LZTDBCreateCancelCaption;
  Self.Caption := LZTDBCreateFormTitle;
End;

end.
