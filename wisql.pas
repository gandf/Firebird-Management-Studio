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
 * Contributor(s): Jeff Overcash, Krzysztof Golko.
}

unit wisql;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Interfaces, Forms, Dialogs,
  Menus, ComCtrls, ToolWin, ExtCtrls, StdCtrls, Grids, DBGrids,
  Db, ImgList, StdActns, ActnList, zluibcClasses, IB, IBDatabase, IBCustomDataSet,
  zluSQL, MemoLists, FileUtil,RichBox;

type

  TCreateDBEvent = procedure (var Database: TIBDatabase) of Object;
  TConnectDBEvent = procedure (const Server: String; const Database: TIBDatabase) of Object;
  TServerConnectEvent = procedure (const ServerName: String) of Object;
  TDisconnectDBEvent = procedure (const Database: TIBDatabase) of Object;
  TUpdateObjectEvent = procedure (const Database: TIBDatabase;
                                  const ObjectType: integer) of Object;
  TDropDBEvent = procedure of Object;

  TdlgWisql = class(TForm)
    pgcOutput: TPageControl;
    TabData: TTabSheet;
    dbgSQLResults: TDBGrid;
    TabResults: TTabSheet;
    reSqlOutput: TlzRichEdit;
    splISQLHorizontal: TSplitter;
    GridSource: TDataSource;
    pmClientDialect: TPopupMenu;
    Dialect1: TMenuItem;
    Dialect2: TMenuItem;
    Dialect3: TMenuItem;
    MainMenu1: TMainMenu;
    Transactions1: TMenuItem;
    File1: TMenuItem;
    Edit1: TMenuItem;
    QueryLoadScript1: TMenuItem;
    QueryNext1: TMenuItem;
    QueryPrevious1: TMenuItem;
    QueryPrevious2: TMenuItem;
    QuerySaveScript1: TMenuItem;
    SaveOutput1: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    Commit1: TMenuItem;
    Rollback1: TMenuItem;
    Database1: TMenuItem;
    Disconnect1: TMenuItem;
    Create1: TMenuItem;
    Drop1: TMenuItem;
    N5: TMenuItem;
    Connect1: TMenuItem;
    pnlEnterSQL: TPanel;
    reSqlInput: TlzRichEdit;
    stbISQL: TStatusBar;
    Print1: TMenuItem;
    Close1: TMenuItem;
    TransactionCommit: TAction;
    TransactionRollback: TAction;
    DialectAction1: TAction;
    DialectAction2: TAction;
    DialectAction3: TAction;
    QueryPrevious: TAction;
    QueryNext: TAction;
    QueryExecute: TAction;
    QueryLoadScript: TAction;
    QuerySaveScript: TAction;
    QueryOptions: TAction;
    QuerySaveOutput: TAction;
    pmLastFiles: TPopupMenu;
    FileOptions: TAction;
    FileClose: TAction;
    QueryPrepare: TAction;
    EditFind: TAction;
    sbData: TStatusBar;
    EditFont: TAction;
    Help1: TMenuItem;
    SQLReference1: TMenuItem;
    N7: TMenuItem;
    About1: TMenuItem;
    N6: TMenuItem;
    Options1: TMenuItem;
    N8: TMenuItem;
    mnuEdit1: TMenuItem;
    Undo2: TMenuItem;
    N9: TMenuItem;
    mnuEdCopy1: TMenuItem;
    Cut2: TMenuItem;
    Paste2: TMenuItem;
    SelectAll2: TMenuItem;
    mnuEdN1: TMenuItem;
    mnuEdFind1: TMenuItem;
    Font2: TMenuItem;
    EditCopy1: TEditCopy;
    EditCut1: TEditCut;
    EditPaste1: TEditPaste;
    EditSelectAll1: TEditSelectAll;
    EditUndo1: TEditUndo;
    lblFileName: TLabel;
    Windows1: TMenuItem;
    TabStats: TTabSheet;
    lvStats: TListView;
    Prepare1: TMenuItem;
    DatabaseConnectAs: TAction;
    DatabaseDisconnect: TAction;
    DatabaseCreate: TAction;
    DatabaseDrop: TAction;
    ToolBar3: TToolBar;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton5: TToolButton;
    ToolButton10: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton20: TToolButton;
    pmRichEdit: TPopupMenu;
    Paste1: TMenuItem;
    Copy1: TMenuItem;
    Cut1: TMenuItem;
    Undo1: TMenuItem;
    Selectall1: TMenuItem;
    N3: TMenuItem;
    FontDialog1: TFontDialog;
    FindDialog1: TFindDialog;
    actWisql: TActionList;
    mnuPrev: TPopupMenu;
    mnuNext: TPopupMenu;
    Newconnection1: TMenuItem;
    procedure QueryExecuteExecute(Sender: TObject);
    procedure QueryLoadScriptExecute(Sender: TObject);
    procedure QuerySaveScriptExecute(Sender: TObject);
    procedure QueryPreviousExecute(Sender: TObject);
    procedure QueryNextExecute(Sender: TObject);
    procedure QuerySaveOutputExecute(Sender: TObject);
    procedure DialectChange(Sender: TObject);
    procedure DialectUpdate(Sender: TObject);
    procedure UpdateCursor(Sender: TObject);
    procedure reSqlInputKeyPress(Sender: TObject; var Key: Char);
    procedure TransactionExecute(Sender: TObject);
    procedure cbServersChange(Sender: TObject);
    procedure FileOptionsExecute(Sender: TObject);
    procedure EditFindExecute(Sender: TObject);
    procedure EditFindUpdate(Sender: TObject);
    procedure QueryUpdate(Sender: TObject);
    procedure QueryPrepareExecute(Sender: TObject);
    procedure dbgSQLResultsCellClick(Column: TColumn);
    procedure dbgSQLResultsDrawColumnCell(Sender: TObject;
      const Rect: TRect; DataCol: Integer; Column: TColumn;
      State: TGridDrawState);
    procedure dbgSQLResultsEditButtonClick(Sender: TObject);
    procedure EditFontExecute(Sender: TObject);
    procedure SQLReference1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FileCloseExecute(Sender: TObject);
    procedure PrintTStrings(Lst : TStrings);
    procedure Print1Click(Sender: TObject);
    procedure Drop1Click(Sender: TObject);
    procedure Disconnect1Click(Sender: TObject);
    procedure Connect1Click(Sender: TObject);
    procedure Create1Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Windows1Click(Sender: TObject);
    procedure QueryPreviousUpdate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure QuerySaveOutputUpdate(Sender: TObject);
    procedure DatabaseDisconnectUpdate(Sender: TObject);
    procedure DatabaseConnectAsUpdate(Sender: TObject);
    procedure FindDialog1Find(Sender: TObject);
    procedure mnuPrevPopup(Sender: TObject);
    procedure NavMenuClick(Sender: TObject);
    procedure mnuNextPopup(Sender: TObject);
    procedure Newconnection1Click(Sender: TObject);
  private
    { Private declarations }
    FDatabase: TIBDatabase;
    FDefaultTransaction: TIBTransaction;
    FDDLTransaction: TIBTransaction;
    FQryDataSet: TIBDataset;

    FOnCreateDB: TCreateDBEvent;
    FOnConnectDB: TConnectDBEvent;
    FOnServerConnect: TServerConnectEvent;
    FOnCreateObject: TUpdateObjectEvent;
    FOnDropDatabase: TDropDBEvent;
    FOnDropObject: TUpdateObjectEvent;
    FServerList: TStringList;
    FDefaultTransIdx,
    FDDLTransIDX,
    FServerIndex,
    FCurrSQLDialect: integer;
    FConnected: boolean;
    FAutoDDL: boolean;
    FQueryBuffer: TMemoList;

    procedure UpdateConnectStatus(const Connected: boolean);
    procedure UpdateTransactionStatus (const active: boolean);
    procedure UpdateOutputWindow (const Data: String);
    procedure ProcessISQLEvent (const ISQLEvent: TSQLEvent; const SubEvent: TSQLSubEvent;
                                const Data: Variant; const Database: TIBDatabase);
    procedure SetAutoDDL(const Value: boolean);
    procedure SetClientDialect(const Value: integer);
    procedure ShowStatistics(const Stats: TStringList);
    procedure CheckDisconnect(Sender: TObject);
    procedure SaveOutput;

  public
    { Public declarations }
    New_connection:boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ShowDialog;
    function CheckTransactionStatus(const Closing: boolean): boolean;
    function ConnectAsDatabase(Sender: Tobject): boolean;
    function CreateDatabase(Sender: TObject): boolean;

  published
    property OnCreateDatabase: TCreateDBEvent read FOnCreateDB write FOnCreateDB;
    property OnConnectDatabase: TConnectDBEvent read FOnConnectDB write FOnConnectDB;
    property OnCreateObject: TUpdateObjectEvent read FOnCreateObject write FOnCreateObject;
    property OnDropDatabase: TDropDBEvent read FOnDropDatabase write FOnDropDatabase;
    property OnDropObject: TUpdateObjectEvent read FOnDropObject write FOnDropObject;
    property OnServerConnect: TServerConnectEvent read FOnServerConnect write FOnServerConnect;
    property ServerList: TStringList read FServerList write FServerList;
    property Database: TIBDatabase read FDatabase write FDatabase;
    property ServerIndex: integer read FServerIndex write FServerIndex;
    property AutoDDL: boolean read FAutoDDL write SetAutoDDL;
    property Dialect: integer read FCurrSQLDialect write SetClientDialect;
  end;

implementation

uses frmuMessage, zluGlobal, frmuSQLOptions, frmuDisplayBlob,
     frmuDispMemo, zluContextHelp, Printers, fileCtrl, zluUtility,
     frmuMain, IBSQL, RichEdit, Math;

const
  OBJECTNAME = '\ISQL';
{$R *.lfm}

procedure TdlgWisql.UpdateTransactionStatus(const active: boolean);
begin
  if active then
  begin
    stbISQL.Panels[3].Text := 'Transaction is ACTIVE.';
    TransactionCommit.Enabled := true;
    TransactionRollback.Enabled := true;
  end
  else begin
    stbISQL.Panels[3].Text := 'No active transaction.';
    TransactionCommit.Enabled := false;
    TransactionRollback.Enabled := false;
  end
end;

procedure TdlgWisql.QuerySaveOutputExecute(Sender: TObject);
begin
  SaveOutput;
end;

procedure TdlgWisql.QueryNextExecute(Sender: TObject);
var
  iQry: TStrings;
begin
  // New QryList
  iQry := FQueryBuffer.GetNext;
  if Assigned(iQry) then
    reSQLInput.Lines := iQry
  else
    reSQLInput.Clear;
  reSQLInput.Modified := FALSE;
end;

procedure TdlgWisql.QueryPreviousExecute(Sender: TObject);
var
  iQry: TStrings;
begin
  iQry := FQueryBuffer.GetPrev;
  if Assigned(iQry) then
    reSQLInput.Lines := iQry
  else
    reSQLInput.Clear;
  reSQLInput.Modified := FALSE;
end;

procedure TdlgWisql.QuerySaveScriptExecute(Sender: TObject);
var
  lSaveDialog: TSaveDialog;
begin
  lSaveDialog := nil;
  try
  begin
    lSaveDialog := TSaveDialog.Create(Self);
    lSaveDialog.DefaultExt := 'sql';
    lSaveDialog.Filter := 'SQL Files (*.sql)|*.SQL|Text files (*.txt)|*.TXT|All files (*.*)|*.*';
    if lSaveDialog.Execute then
    begin
      if FileExistsUTF8(lSaveDialog.FileName) { *Converted from FileExists* } then
        if MessageDlg(Format('OK to overwrite %s', [lSaveDialog.FileName]),
          mtConfirmation, mbYesNoCancel, 0) <> idYes then Exit;
      reSQLInput.PlainText := true;
      reSQLInput.Lines.SaveToFile(lSaveDialog.FileName);
      reSQLInput.Modified := false;
      reSQLInput.PlainText := false;
    end;
  end
  finally
    lSaveDialog.free;
  end;
end;

procedure TdlgWisql.QueryLoadScriptExecute(Sender: TObject);
var
  lOpenDialog: TOpenDialog;
begin
  lOpenDialog := nil;
  try
  begin
    lOpenDialog := TOpenDialog.Create(self);
    lOpenDialog.DefaultExt := 'sql';
    lOpenDialog.Filter := 'SQL Files (*.sql)|*.SQL|Text files (*.txt)|*.TXT|All files (*.*)|*.*';
    if lOpenDialog.Execute then
    begin
      try
        Screen.Cursor := crHourGlass;
        try
          reSQLInput.Lines.LoadFromFile(lOpenDialog.FileName);
        except
          on E:Exception do
          begin
            MessageDlg(E.Message + #10#13+
            Format('Could not open file "%s".',[lOpenDialog.FileName]), mtError, [mbOK], 0);
            Exit;
          end;
        end;
        reSQLInput.SetFocus;
      finally
        Screen.Cursor := crDefault;
      end;
    end;
  end
  finally
    lOpenDialog.free;
  end;
end;

procedure TdlgWisql.QueryExecuteExecute(Sender: TObject);
var
  ISQLObj: TIBSQLObj;
begin
  if not Assigned(FDatabase) then
    FDatabase := TIBDatabase.Create(self);

  if Assigned (FQryDataSet) then
  begin
    GridSource.DataSet := nil;
    FQryDataSet.Free;
  end;

  FQryDataSet := TIBDataSet.Create(self);

  if Assigned (FDefaultTransaction) then
    FQryDataset.Transaction := FDefaultTransaction
  else
  begin
    FDefaultTransaction := TIBTransaction.Create(self);
    FDDLTransaction := TIBTransaction.Create(self);
    FDDLTransIdx := FDatabase.AddTransaction (FDDLTransaction);
    FDefaultTransIdx := FDatabase.AddTransaction (FDefaultTransaction);
    FDefaultTransaction.DefaultDatabase := Database;
    FDDLTransaction.DefaultDatabase := Database;
    FQryDataset.Transaction := FDefaultTransaction;
  end;

  reSQLOutput.Clear;
  ISQLObj := nil;
  try
    lvStats.Items.BeginUpdate;
    lvStats.Items.Clear;
    lvStats.Items.EndUpdate;

    ISQLObj := TIBSqlObj.Create (Self);
    try
      with ISQLObj do
      begin
        DefaultTransIDX := FDefaultTransIDX;
        DDLTransIDX := FDDLTransIDX;
        AutoDDL := FAutoDDL;
        Query := reSQLInput.Lines;
        Database := FDatabase;
        DataSet := FQryDataSet;
        OnDataOutput := UpdateOutputWindow;
        OnISQLEvent := ProcessISQLEvent;
        pgcOutput.ActivePage := TabData;
        Statistics := true;
        Cursor := crSQLWait;
        lvStats.Items.BeginUpdate;
        lvStats.Items.Clear;
        lvStats.Items.EndUpdate;
        DoIsql;
        Cursor := crDefault;
        if reSQLInput.Modified then
        begin
          FQueryBuffer.Add(reSqlInput.Lines);
          reSQLInput.Modified := false;
        end;
        if gAppSettings[CLEAR_INPUT].Setting then
        begin
          reSQLInput.Clear;
          FQueryBuffer.MovePast;
        end;
      end;
    except on
      E: EIsqlException do
      begin
        Cursor := crDefault;
        case E.ExceptionCode of
          eeInvDialect:
            DisplayMsg (E.ErrorCode, Format('%s'#13#10'Invalid client dialect %s',
              [E.Message, E.ExceptionData]));
          eeInitialization:
            DisplayMsg (E.ErrorCode, E.Message);
          eeFOpen:
            DisplayMsg (E.ErrorCode, Format('%s'#13#10'Unable to open file %s',
              [E.Message, E.ExceptionData]));
          eeParse:
            DisplayMsg (E.ErrorCode, E.Message);
          eeCreate,
          eeConnect:
            DisplayMsg (E.ErrorCode, Format('%s'#13#10'Database: %s', [E.Message, E.ExceptionData]));
          eeStatement,
          eeCommit,
          eeRollback,
          eeDDL,
          eeDML,
          eeQuery:
            DisplayMsg (E.ErrorCode, Format('%s'#13#10'Statement: %s', [E.Message, E.ExceptionData]));
          eeFree:
            DisplayMsg (E.ErrorCode, E.Message);
        end;
      end;
    end;
  finally
//    FDefaultTransaction := Database.Transactions[FDefaultTransIDX];
//    FDDLTransaction := Database.Transactions[FDDLTransIDX];
    GridSource.DataSet := FQryDataset;
    FDefaultTransaction := FQryDataset.Transaction;
    UpdateTransactionStatus ((FDefaultTransaction.InTransaction) or (FDDLTransaction.InTransaction));
    ShowStatistics (ISQLObj.StatisticsList);
    ISQLObj.Free;
  end;
end;

procedure TdlgWisql.UpdateOutputWindow(const Data: String);
begin
  reSqLOutput.Lines.Add (Data);
end;

procedure TdlgWisql.DialectChange(Sender: TObject);
var
  tmpdialect: integer;

begin
  if Assigned (FDatabase) then
  begin
    tmpdialect := TAction(Sender).Tag;
    with FDatabase do begin
      try
        if tmpdialect <> DBSQLDialect then
          DisplayMsg (WAR_DIALECT_MISMATCH, Format(
              'Database dialect (%d) does not match client dialect (%d).',
              [DBSQLDialect, tmpdialect]));
        SQLDialect := TAction(Sender).tag;
      except on E: Exception do
         DisplayMsg (ERR_INV_DIALECT, Format('%s'#13#10'Unable to set the client dialect to %d',
                                            [E.Message, tmpdialect]));
      end;
    end;
  end;
  Dialect := TAction(Sender).Tag;
end;

procedure TdlgWisql.DialectUpdate(Sender: TObject);
begin
   with Sender as TAction do
     Checked := (FCurrSQLDialect = Tag)
end;

procedure TdlgWisql.ShowDialog;
begin
  reSQLInput.Lines.Clear;
  reSQLOutput.Lines.Clear;

  frmMain.UpdateWindowList(Caption, TObject(Self), true);
  if Assigned (FDatabase) then
  begin
    FDatabase.BeforeDisconnect := CheckDisconnect;
    if not FDatabase.TestConnected then
    begin
      FDatabase.Connected := true;
    end;
    FConnected := false;
    UpdateConnectStatus(true);
    Dialect := FDatabase.SQLDialect;
  end
  else
    UpdateConnectStatus(false);
{
  if Assigned (FServerList) then
  begin
    cbServers.Items.Clear;
    cbServers.Items.Text := ServerList.Text;
    cbServers.ItemIndex := ServerIndex;
  end;
}
  if Assigned (FDefaultTransaction) and Assigned(FDDLTransaction) then
    UpdateTransactionStatus ((FDefaultTransaction.InTransaction) or (FDDLTransaction.InTransaction))
  else
    UpdateTransactionStatus (false);

  FQueryBuffer.Clear;
  Show;
  frmMain.UpdateWindowList(Caption, TObject(Self));
end;

procedure TdlgWisql.UpdateCursor(Sender: TObject);
const
  SColRowInfo = '%3d : %3d';
var
  CharPos: TPoint;
begin
  CharPos.Y := SendMessage(reSqlInput.Handle, EM_EXLINEFROMCHAR, 0,
    reSqlInput.SelStart);
  CharPos.X := (reSqlInput.SelStart -
    SendMessage(reSqlInput.Handle, EM_LINEINDEX, CharPos.Y, 0));
  Inc(CharPos.Y);
  Inc(CharPos.X);
  stbISQL.Panels[0].Text := Format(SColRowInfo, [CharPos.Y, CharPos.X]);
end;

procedure TdlgWisql.reSqlInputKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #10) and (GetKeyState(VK_CONTROL) < 0) then
  begin
    Key := #0;
    QueryExecuteExecute(Sender);
  end
  else
    UpdateCursor(Sender);
end;

procedure TdlgWisql.ProcessISQLEvent(const ISQLEvent: TSQLEvent;
  const SubEvent: TSQLSubEvent; const Data: Variant; const Database: TIBDatabase);
var
  objType: integer;

begin
  case ISQLEvent of
    evntISQL:
    begin
      case SUbEvent of
        seAutoDDL:
          AutoDDL := Data;
      end;
    end;
    evntDialect:
    begin
      case SubEvent of
        seDialect1:
          DialectChange(DialectAction1);
        seDialect2:
          DialectChange(DialectAction2);
        seDialect3:
          DialectChange(DialectAction3);
      end;
    end;
    evntConnect:
    begin
      if Assigned (OnConnectDatabase) and
        gAppSettings[UPDATE_ON_CONNECT].Setting then
      begin
        { force a path before the database name if this is a local connection }
        if ExtractFilePath(FDatabase.DatabaseName) = '' then
          FDatabase.DatabaseName := ExtractFilePath(Application.ExeName)+FDatabase.Databasename;

//        FDatabase.BeforeDisconnect := CheckDisconnect;
      end;
      UpdateConnectStatus(true);
      FCurrSQLDialect := FDatabase.SQLDialect;
      FConnected := true;
    end;

    evntCreate:
    begin
      if SubEvent = seDatabase then
      begin
        UpdateConnectStatus(true);
        if Assigned (OnCreateDatabase) and
           gAppSettings[UPDATE_ON_CREATE].Setting then
        OnCreateDatabase (FDatabase);

        FCurrSQLDialect := FDatabase.SQLDialect;
        FConnected := true;
        UpdateConnectStatus(true);
      end
      else
      begin
        case SubEvent of
          seDomain: objType := NODE_DOMAIN;
          seTable: objType := NODE_TABLE;
          seView: objType := NODE_VIEW;
          seProcedure: objType := NODE_PROCEDURE;
          seFunction: objType := NODE_FUNCTION;
          seGenerator: objType := NODE_GENERATOR;
          seException: objType := NODE_EXCEPTION;
          seFilter: objType := NODE_BLOB_FILTER;
          seRole: objType := NODE_ROLE;
          else
            objType := NODE_UNK;
        end;
        if Assigned (OnCreateObject) then
          OnCreateObject (FDatabase, ObjType);
      end;
    end;

    evntAlter:
      if Assigned (OnCreateObject) then
        OnCreateObject (Database, NODE_UNK);

    evntDrop:
    begin
      if SubEvent = seDatabase then
      begin
       if Assigned(OnDropDatabase) then
         OnDropDatabase;
       UpdateConnectStatus(false);
      end
      else
      begin
        case SubEvent of
          seDomain: objType := NODE_DOMAIN;
          seTable: objType := NODE_TABLE;
          seView: objType := NODE_VIEW;
          seProcedure: objType := NODE_PROCEDURE;
          seFunction: objType := NODE_FUNCTION;
          seGenerator: objType := NODE_GENERATOR;
          seException: objType := NODE_EXCEPTION;
          seFilter: objType := NODE_BLOB_FILTER;
          seRole: objType := NODE_ROLE;
          else
            objType := NODE_UNK;
        end;
          if Assigned (OnDropObject) then
            OnDropObject (Database, ObjType);
      end;
    end;

    evntTransaction:
    begin
      UpdateTransactionStatus (Data);
      if Assigned (OnCreateObject) then
        OnCreateObject (Database, NODE_UNK);
    end;
//    else
//      ShowMessage ('Unknown event');
  end;
end;

procedure TdlgWisql.TransactionExecute(Sender: TObject);
begin
  with (Sender as TAction) do
  begin
    if Tag = 0 then
    begin
      if MessageDlg('Are you sure that you want to rollback work to previous commit point?',
          mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      begin
        if FDefaultTransaction.InTransaction then
          FDefaultTransaction.Rollback;

        if FDDLTransaction.InTransaction then
          FDDLTransaction.Rollback;

        UpdateTransactionStatus ((FDefaultTransaction.InTransaction) or (FDDLTransaction.InTransaction));
      end;
    end
    else
    begin
      if FDefaultTransaction.InTransaction then
        FDefaultTransaction.Commit;
      if FDDLTransaction.InTransaction then
        FDDLTransaction.Commit;
      UpdateTransactionStatus ((FDefaultTransaction.InTransaction) or (FDDLTransaction.InTransaction));
    end;
  end;
  if Assigned (OnCreateObject) then
    OnCreateObject (Database, NODE_UNK);
end;

constructor TdlgWisql.Create(AOwner: TComponent);
begin
  inherited;
  FServerList := TStringList.Create;
  FConnected := false;
  Dialect := gAppSettings[DEFAULT_DIALECT].Setting;
  AutoDDL := gAppSettings[AUTO_COMMIT_DDL].Setting;

  { On create, the input window is always 1/2 of the window }
  reSQLInput.Height := Self.Height div 2;

  FQueryBuffer := TMemoList.Create;
end;

destructor TdlgWisql.Destroy;
begin
  FServerList.Free;
  FQueryBuffer.Free;
  inherited;
end;

procedure TdlgWisql.cbServersChange(Sender: TObject);

begin
  Disconnect1Click(Sender);

  if not Assigned(FDatabase.Handle) then
    if Assigned (OnServerConnect) then
      OnServerConnect ((Sender as TComboBox).Text);
end;

procedure TdlgWisql.FileOptionsExecute(Sender: TObject);
var
  origDialect: integer;
  origDDL : boolean;
  optsDlg: TfrmSQLOptions;
begin
  optsDlg := TfrmSQLOptions.Create (self);

  origDialect := gAppSettings[DEFAULT_DIALECT].Setting;
  origDDL := gAppSettings[AUTO_COMMIT_DDL].Setting;
  OptsDlg.ShowModal;
  OptsDlg.Free;
  if OrigDDL <> gAppSettings[AUTO_COMMIT_DDL].Setting then
    AutoDDL := gAppSettings[AUTO_COMMIT_DDL].Setting;

  if OrigDialect <> gAppSettings[DEFAULT_DIALECT].Setting then
    Dialect := gAppSettings[DEFAULT_DIALECT].Setting;
end;

procedure TdlgWisql.EditFindExecute(Sender: TObject);
begin
  FindDialog1.Execute;
end;

procedure TdlgWisql.EditFindUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (ActiveControl is TlzRichEdit);
end;

procedure TdlgWisql.QueryUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (reSQlInput.Lines.Count > 0);
  if reSQlInput.Modified then
    stbISQL.Panels[1].Text := 'Modified'
  else
    stbISQL.Panels[1].text := '';
end;

procedure TdlgWisql.QueryPrepareExecute(Sender: TObject);
var
  ISQLObj: TIBSQLObj;
begin
  try
    reSQLOutput.Clear;
    ISQLObj := TIBSqlObj.Create (Self);
    with ISQLObj do
    begin
      DefaultTransIDX := FDefaultTransIDX;
      DDLTransIDX := FDDLTransIDX;
      Query := reSQLInput.Lines;
      Database := FDatabase;
      DataSet := FQryDataSet;
      OnDataOutput := UpdateOutputWindow;
      pgcOutput.ActivePage := TabResults;
      Cursor := crSQLWait;
      DoPrepare;
      Cursor := crDefault;
      Free;
    end;
  except on
    E: EIsqlException do
    begin
      Cursor := crDefault;
      case E.ExceptionCode of
        eeStatement:
          DisplayMsg (E.ErrorCode, Format('%s'#13#10'Statement: %s', [E.Message, E.ExceptionData]));
        else
          DisplayMsg (ERR_ISQL_ERROR, E.Message);
      end;
    end;
  end;
end;

procedure TdlgWisql.dbgSQLResultsCellClick(Column: TColumn);
begin
  if Assigned(Column.Field) and (Column.Field.DataType in [ftMemo, ftBlob]) then
    Column.ButtonStyle := cbsEllipsis;
end;

procedure TdlgWisql.dbgSQLResultsDrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn;
  State: TGridDrawState);
var
  DisplayStr:  String;

begin
  with Sender as TDBGrid do begin
    if Column.Field = nil then begin
      with Canvas do begin
        font.color := clBlue;
        TextRect(Rect, Rect.Left, Rect.top, NULL_STR);
      end
    end
    else begin
      if Column.Field.IsNull then begin
        with Canvas do begin
          font.color := clBlue;
          TextRect(Rect, Rect.Left, Rect.top, NULL_STR);
        end;
      end
      else
      begin
        if Column.Field.DataType in [ftDateTime, ftTime] then
        begin
          { make sure that the time portion is always displayed! }
          if Column.Field.DataType = ftDateTime then
          begin
            if FDatabase.SQLDialect = 3 then
              DisplayStr := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Column.Field.AsDateTime)
            else
              DisplayStr := FormatDateTime('c', Column.Field.AsDateTime)
          end
          else
            DisplayStr := FormatDateTime('hh:nn:ss.zzz', Column.Field.AsDateTime);
          Canvas.TextRect(Rect, Rect.Left, Rect.Top, DisplayStr);
        end
      end;
    end;
  end;
end;

procedure TdlgWisql.dbgSQLResultsEditButtonClick(Sender: TObject);
var
  FieldObj: TField;

begin
  with Sender as TDBGrid do begin
    FieldObj := SelectedField;
    if FieldObj = nil then
      ShowMessage ('Unable to display Array Information')
    else begin
      case FieldObj.DataType of
        ftBlob:
           DisplayBlob (self, FieldObj, FQryDataSet);
        ftMemo:
          DisplayMemo (self, FieldObj, FQryDataSet);
        else
          ShowMessage (FieldObj.DisplayName+' is unknown');
      end;
    end;
  end;
end;

procedure TdlgWisql.EditFontExecute(Sender: TObject);
begin
  FontDialog1.Font.Assign(reSqlInput.SelAttributes);
  if FontDialog1.Execute then
    if reSqlInput.SelLength > 0 then
      reSqlInput.SelAttributes.Assign(FontDialog1.Font)
    else
      reSqlInput.Font.Assign(FontDialog1.Font);
  UpdateCursor(Self);
  reSqlInput.SetFocus;
end;

procedure TdlgWisql.SetAutoDDL(const Value: boolean);
begin
  FAutoDDL := Value;
  if FAutoDDL then
    stbISQL.Panels[4].Text := 'AutoDDL: ON'
  else
    stbISQL.Panels[4].Text := 'AutoDDL: OFF';
end;

procedure TdlgWisql.SQLReference1Click(Sender: TObject);
var
  hlpPath: String;
begin
  inherited;
  hlpPath := Format('%s%s',[ExtractFilePath(Application.ExeName), SQL_REFERENCE]);
  //WinHelp(WindowHandle, PChar(hlpPath),HELP_FINDER,0);
end;

procedure TdlgWisql.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if CheckTransactionStatus (true) then
  begin
    frmMain.UpdateWindowList(Self.Caption, TObject(Self), true);
    Action := caFree;
  end
  else
    Action := caNone;
end;

procedure TdlgWisql.FileCloseExecute(Sender: TObject);
begin
  Self.Close;
end;

procedure TdlgWisql.PrintTStrings(Lst : TStrings);
var
  I,
  Line : Integer;
begin
  I := 0;
  Line := 0 ;
  Printer.BeginDoc ;
  for I := 0 to Lst.Count - 1 do begin
    Printer.Canvas.TextOut(0, Line, Lst.Strings[I]);
    {Font.Height is calculated as -Font.Size * 72 / Font.PixelsPerInch which returns
     a negative number. So Abs() is applied to the Height to make it a non-negative
     value}
    Line := Line + Abs(Printer.Canvas.TextHeight('I'));
    if (Line >= Printer.PageHeight) then
      Printer.NewPage;
  end;
  Printer.EndDoc;
end;

procedure TdlgWisql.Print1Click(Sender: TObject);
var
  lPrintDialog: TCustomPrintDialog;
  lLine: integer;
  lPrintText: TextFile;
  lTexte: TStrings;
begin
  {
  lPrintDialog := nil;
  if ActiveControl is TlzRichEdit then
  begin
    try
      lPrintDialog := TCustomPrintDialog.Create(Self);
      try
        if lPrintDialog.Execute then
        begin
          lTexte.LoadFromFile(lPrintText);
          PrintTStrings(lTexte);
          Rewrite(lPrintText);
          Printer.Canvas.Font := (ActiveControl as TlzRichEdit).Font;
          for lLine := 0 to (ActiveControl as TlzRichEdit).Lines.Count - 1 do
            Writeln(lPrintText, (ActiveControl as TlzRichEdit).Lines[lLine]);
          CloseFile(lPrintText);
        end;
      except on E: Exception do
        DisplayMsg (ERR_PRINT, E.Message);
      end;
    finally
      lPrintDialog.free;
    end;
  end;   }
end;

procedure TdlgWisql.Drop1Click(Sender: TObject);
begin
  if CheckTransactionStatus (false) then
  begin
   if not FConnected then
      frmMain.DatabaseDrop.OnExecute (sender)
    else begin
      if MessageDlg('Are you sure that you want to drop the selected database?',
          mtConfirmation, mbOkCancel, 0) = mrOK then
        FDatabase.DropDatabase;
    end;

    if not Assigned(FDatabase.Handle) then
    begin
      frmMain.UpdateWindowList(Caption, TObject(Self), true);
      UpdateConnectStatus(false);
      frmMain.UpdateWindowList(Caption, TObject(Self));
    end;
  end;
end;

procedure TdlgWisql.Disconnect1Click(Sender: TObject);
begin
  if new_connection then
    FDatabase.Connected:=False;
  if not new_connection then
    frmMain.DatabaseDisconnectExecute(Self);
  Newconnection1.Enabled:=True;
  Caption := 'Interactive SQL';
end;

procedure TdlgWisql.Connect1Click(Sender: TObject);
begin
  if CheckTransactionStatus (false) then
  begin
    new_connection:=false;
    if frmMain.ConnectAsDatabase(Self) then
    begin
      frmMain.UpdateWindowList(Caption, TObject(Self), true);
      UpdateConnectStatus(Assigned(FDatabase.Handle));
      frmMain.UpdateWindowList(Caption, TObject(Self));
      FConnected := false;
    end;
    resqlInput.Clear;
    reSQLOutput.Clear;
    Newconnection1.Enabled:=False;
  end;
end;

procedure TdlgWisql.Create1Click(Sender: TObject);
begin
  New_Connection:=false;
  if CheckTransactionStatus(false) then
  begin
    if CreateDatabase (Sender) then
    begin
      frmMain.UpdateWindowList(Caption, TObject(Self), true);
      UpdateConnectStatus(true);
      frmMain.UpdateWindowList(Caption, TObject(Self));
      FConnected := true;
    end;
    resqlInput.Clear;
    reSQLOutput.Clear;
  end;
end;

function TdlgWisql.CheckTransactionStatus (const Closing: boolean): boolean;
var
  retval: integer;

begin
  { If there are any outstanding transactions, ask for a commit.  If no
    commit is issued, then do not allow the form to close}
  result := true;
  if Assigned (FDefaultTransaction) and Assigned (FDDLTransaction) then
  begin
    if gAppSettings[COMMIT_ON_EXIT].Setting and Closing then
    begin
      if FDefaultTransaction.InTransaction then
        FDefaultTransaction.Commit;
      if FDDLTransaction.InTransaction then
        FDDLTransaction.Commit;
      result := true;
    end
    else
    begin
      if FDefaultTransaction.InTransaction or
         FDDLTransaction.InTransaction then
      begin
        retval := MessageDlg ('Transactions are active.'#13#10+
                              'Would you like to commit the transactions?'#13#10+
                              #13#10+
                              'Choosing NO will rollback the active transactions.', mtInformation,
                              mbYesNoCancel, 0);
        case retval of
          mrYes:
          begin
            if FDefaultTransaction.InTransaction then
              FDefaultTransaction.Commit;
            if FDDLTransaction.InTransaction then
              FDDLTransaction.Commit;
            result := true;
          end;
          mrNo:
          begin
            if FDefaultTransaction.InTransaction then
              FDefaultTransaction.Rollback;

            if FDDLTransaction.InTransaction then
              FDDLTransaction.Rollback;

            result := true;
          end;
          mrCancel:
            result := false;
        end;
      end;
    end
  end;

  if Result and Closing and Assigned (FDatabase) then
  begin
    with FDatabase do
    begin
      BeforeDisconnect := nil;
      if TransactionCount > 0 then
      begin
        retval := FindTransaction(FDefaultTransaction);
        RemoveTransaction(retval);
        retval := FindTransaction(FDDLTransaction);
        RemoveTransaction(retval);
      end;

      { If a connection was made to a database (or a new database was created),
        disconnect from it now }
      if FConnected then
      begin
        FDatabase.Connected := false;
        FDatabase.Free;
        FDatabase := nil;
        FConnected := false;
      end;
    end;
    FDefaultTransaction.Free;
    FDDLTransaction.Free;
    FDefaultTransaction := nil;
    FDDLTransaction := nil;
  end;
end;

procedure TdlgWisql.FormResize(Sender: TObject);
begin
  reSQLInput.Refresh;
end;

procedure TdlgWisql.UpdateConnectStatus(const Connected: boolean);
var
  dbString: String;
begin
  if Assigned (sbData) then
  begin
    if Connected then
    begin
      lblFileName.Caption := FDatabase.DatabaseName;
      dbString := MinimizeName (lblFileName.Caption, lblFileName.Canvas,
        sbData.Panels[0].Width + 10);
      sbData.Panels[0].Text := dbString;
    end
    else
    begin
      sbData.Panels[0].Text := 'Not Connected';
      reSqlInput.Clear;
      reSQLOutput.Clear;
      FQueryBuffer.Clear;
      FConnected := false;
    end;
  end;
end;

procedure TdlgWisql.Windows1Click(Sender: TObject);
begin
  frmMain.ShowWindows;
end;

procedure TdlgWisql.SetClientDialect(const Value: integer);
begin
  FCurrSQLDialect := value;
  stbISQL.Panels[2].Text := Format ('Client dialect %d',[FCurrSQLDialect]);
end;

procedure TdlgWisql.QueryPreviousUpdate(Sender: TObject);
begin
  if FQueryBuffer.BOC then
    if FQueryBuffer.Count <> 0 then
      (Sender as TAction).Enabled := reSqlInput.Modified
    else
      (Sender as TAction).Enabled := false
  else
    (Sender as TAction).Enabled := true;
end;

procedure TdlgWisql.ShowStatistics(const Stats: TStringList);
var
  Line,
  Option,
  Value: String;
  lCnt : integer;
  lvItem: TListItem;
begin

  lvStats.Items.BeginUpdate;
  lvStats.Items.Clear;
  for lCnt := 0 to Stats.Count - 1 do
  begin
    Line := Stats[lCnt];
    Option := GetNextField(Line, DEL);
    Value := GetNextField(Line, DEL);
    if Pos('PLAN', Value) = 1 then
      reSQLOutput.Lines.Append(Value);
    lvItem := lvStats.Items.Add;
    lvItem.Caption := Option;
    lvItem.SubItems.Add (Value);
  end;
  lvStats.Items.EndUpdate;
end;

procedure TdlgWisql.FormShow(Sender: TObject);
begin
  pgcOutput.ActivePageIndex := 0;
end;

procedure TdlgWisql.CheckDisconnect(Sender: TObject);
begin
  if not CheckTransactionStatus(true) then
    exit;
  UpdateConnectStatus(false);
end;

procedure TdlgWisql.SaveOutput;
var
  SaveDialog: TSaveDialog;
  lColDel,
  lStr,
  SaveFileName: String;
  colWidth,
  i, lCnt: integer;
  BlobList,
  tmpList,
  Data: TStringList;
  blbStream: TStream;
  lBookMark: TBookMark;

begin
  SaveDialog := TSaveDialog.Create(self);
  Data := TStringList.Create;
  with SaveDialog do
  begin
    Options := [ofPathMustExist, ofHideReadOnly, ofOverwritePrompt];
    DefaultExt := 'TXT';
    Filter := 'Text files (*.txt)|*.TXT';
    FilterIndex := 1;
    Title := 'Save Query Output';
    if Execute then
      SaveFileName :=  FileName
    else
    begin
      Free;
      Exit;
    end;
    Free;
  end;

  dbgSQLResults.Enabled := false;
  lBookMark := FQryDataset.GetBookmark;
  FQryDataset.First;
  with FQryDataset do
  begin
    DisableControls;
    while not EOF do
    begin
      { If this is the first record, write out the column headings }
      if BOF then
      begin
        for lCnt := 0 to FieldCount-1 do
        begin
          ColWidth := Max(Fields[lCnt].DataSize, Length(Fields[lCnt].FieldName));
          lStr := Format('%s%-*s',[lStr, ColWidth, Fields[lCnt].FieldName]);
          lStr := lStr + ' '+ ' '+ ' '+ ' ';
          for i := 0 to ColWidth-1 do
          begin
            lColDel := lColDel + '=';
          end;
          lColDel := lColDel + ' '+ ' '+ ' '+ ' ';
        end;
        Data.Add(lStr);
        Data.Add(lColDel);
      end;

      { Write the actual data }
      lStr := '';
      BlobList := nil;     
      for lCnt := 0 to FieldCount - 1 do
      begin
        ColWidth := Max(Fields[lCnt].DataSize, Length(Fields[lCnt].FieldName));
        if Fields[lCnt].IsNull then
          lStr := Format('%s%-*s',[lStr, ColWidth, NULL_STR])
        else
        begin
          case Fields[lCnt].Datatype of
            ftString:
              lStr := Format('%s%-*s',[lStr, ColWidth, Fields[lCnt].AsString]);
            ftSmallint:
              lStr := Format('%s%*d',[lStr, ColWidth, Fields[lCnt].AsInteger]);
            ftInteger:
              lStr := Format('%s%*d',[lStr, ColWidth, Fields[lCnt].AsInteger]);
            ftWord:
              lStr := Format('%s%*d',[lStr, ColWidth, Fields[lCnt].AsInteger]);
            ftFloat:
              lStr := Format('%s%*d',[lStr, ColWidth, Fields[lCnt].AsInteger]);
            ftBoolean:
              lStr := Format('%s%*s',[lStr, ColWidth, Fields[lCnt].AsString]);
            ftCurrency:
              lStr := Format('%s%*s',[lStr, ColWidth, Fields[lCnt].AsCurrency]);
            ftBCD:
              lStr := Format('%s%*s',[lStr, ColWidth, Fields[lCnt].AsString]);
            ftDate:
              lStr := Format('%s%*s',[lStr, ColWidth, DateToStr(Fields[lCnt].AsDateTime)]);
            ftTime:
              lStr := Format('%s%*s',[lStr, ColWidth, TimeToStr(Fields[lCnt].AsDateTime)]);
            ftDateTime:
              lStr := Format('%s%*s',[lStr, ColWidth, DateTimeToStr(Fields[lCnt].AsDateTime)]);
            ftMemo:
            begin
              if gAppSettings[BLOB_SUBTYPE].Setting = 'Text' then
              begin
                tmpList := TStringList.Create;
                BlobList := TStringList.Create;
                blbStream := CreateBlobStream(Fields[lCnt], bmRead);
                tmpList.LoadFromStream(blbStream);
                tmpList.Insert(0, '====================================');
                tmpList.Insert(1, Fields[lCnt].FieldName);
                tmpList.Append('====================================');
                BlobList.AddStrings(tmpList);
                tmpList.Free;
                Continue;
              end;
              lStr := Format('%s%*s',[lStr, ColWidth, BLOB_STR]);
            end;
          end; { case }
        end; { else }
        lStr := lStr + ' '+ ' '+ ' '+ ' ';
      end;
      Data.Add(lStr);
      if Assigned (BlobList) then
      begin
        Data.AddStrings(BlobList);
        BlobList.Free;
      end;
      Next;
    end;
    Data.SaveToFile (SaveFileName);
    Data.Free;
    FQryDataset.GotoBookmark(lBookMark);
    FQryDataset.FreeBookmark(lBookMark);
    EnableControls;
    dbgSQLResults.Enabled := true;
    MessageDlg ('Data saved to file: ' + SaveFileName, mtInformation, [mbOk], 0);
  end;
end;

procedure TdlgWisql.QuerySaveOutputUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (Assigned(FQryDataset) and
                                 (FQryDataset.StatementType = SQLSelect));
end;

procedure TdlgWisql.DatabaseDisconnectUpdate(Sender: TObject);
begin
  if Assigned (FDatabase) then
    (Sender as TAction).Enabled := FDatabase.Connected
  else
    (Sender as TAction).Enabled := false;
end;

procedure TdlgWisql.DatabaseConnectAsUpdate(Sender: TObject);
begin
  if Assigned (FDatabase) then
    (Sender as TAction).Enabled := not FDatabase.Connected
//  else
//    (Sender as TAction).Enabled := (cbServers.ItemIndex <> -1);
end;

procedure TdlgWisql.FindDialog1Find(Sender: TObject);
var
  FoundAt: LongInt;
  StartPos, ToEnd: Integer;
begin
  with ActiveControl as TlzRichEdit do
  begin
    if SelLength <> 0 then
      StartPos := SelStart + SelLength
    else
      StartPos := 0;

    { ToEnd is the length from StartPos to the end of the text in the rich edit control }

    ToEnd := Length(Text) - StartPos;

    FoundAt := FindText(FindDialog1.FindText, StartPos, ToEnd, [stMatchCase], false);
    if FoundAt <> -1 then
    begin
      SetFocus;
      SelStart := FoundAt;
      SelLength := Length(FindDialog1.FindText);
    end;
  end;
end;

procedure TdlgWisql.mnuPrevPopup(Sender: TObject);
var
  i, line : Integer;
  t : TMenuItem;
  sList : TStrings;
begin
  while mnuPrev.Items.Count > 0 do
    mnuPrev.Items[0].Free;
  if FQueryBuffer.JustPast or
     ((reSqlInput.Text = '') or (reSqlInput.Modified)) then
    i := FQueryBuffer.Current
  else
    i := FQueryBuffer.Current - 1;
  while (i >= 0) and (i >= FQueryBuffer.Current - 10) do
  begin
    t := TMenuItem.Create(mnuPrev);
    mnuPrev.Items.Add(t);
    {First non blank line}
    line := 0;
    sList := FQueryBuffer[i];
    while (line < sList.Count) and (Trim(sList[line]) = '') do
      Inc(Line);
    t.Caption := Trim(sList[line]);
    t.Tag := i;
    t.OnClick := NavMenuClick;
    Dec(i);
  end;
end;

procedure TdlgWisql.NavMenuClick(Sender: TObject);
var
  iQry: TStrings;
begin
  iQry := FQueryBuffer.Items[TMenuItem(Sender).Tag];
  FQueryBuffer.Current := TMenuItem(Sender).Tag;
  if Assigned(iQry) then
    reSQLInput.Lines := iQry
  else
    reSQLInput.Clear;
  reSQLInput.Modified := FALSE;
end;

procedure TdlgWisql.mnuNextPopup(Sender: TObject);
var
  i, line : Integer;
  t : TMenuItem;
  sList : TStrings;
begin
  while mnuNext.Items.Count > 0 do
    mnuNext.Items[0].Free;
  if FQueryBuffer.JustPast then
    i := FQueryBuffer.Current
  else
    i := FQueryBuffer.Current + 1;
  while (FQueryBuffer.Count > 0) and
        (i < FQueryBuffer.Count) and
        (i < FQueryBuffer.Current + 10) do
  begin
    t := TMenuItem.Create(mnuPrev);
    mnuNext.Items.Add(t);
    {First non blank line}
    line := 0;
    sList := FQueryBuffer[i];
    while (line < sList.Count) and (Trim(sList[line]) = '') do
      Inc(Line);
    t.Caption := Trim(sList[line]);
    t.Tag := i;
    t.OnClick := NavMenuClick;
    Inc(i);
  end;
end;

function TdlgWisql.ConnectAsDatabase(Sender: Tobject): boolean;
begin
  FDataBase:=TIBDataBase.Create(self);
  if frmMain.CurrSelDatabase.Database.Connected then begin
      FDataBase.DatabaseName:=frmMain.CurrSelDatabase.Database.DatabaseName;
      FDataBase.Params.Text:=frmMain.CurrSelDatabase.Database.Params.Text;
      FDataBase.SQLDialect:=frmMain.CurrSelDatabase.DataBase.SQLDialect;
      FDataBase.LoginPrompt:=False;
      FDataBase.Open;
      result := true;
  end else begin
    try
      result := true;
      if Assigned(frmMain.CurrSelServer) and Assigned(frmMain.CurrSelDatabase) then begin
        if not frmMain.CurrSelDatabase.Database.Connected then
          frmMain.DoDBConnect(frmMain.CurrSelServer,frmMain.CurrSelDatabase,false,false);
      end;
      FDataBase.DatabaseName := frmMain.CurrSelDatabase.Database.DatabaseName;
      FDataBase.Params.Text := frmMain.CurrSelDatabase.Database.Params.Text;
      FDataBase.SQLDialect := frmMain.CurrSelDatabase.DataBase.SQLDialect;
      FDataBase.LoginPrompt := False;
      frmMain.CurrSelDatabase.Database.Connected := False;
      FDataBase.Open;
    except
      result := false;
    end;
  end;
end;

function TdlgWisql.CreateDatabase(Sender: TObject): boolean;
begin
  try
    result := true;
    frmMain.DatabaseCreateExecute(Sender);
    if Assigned (frmMain.CurrSelDatabase) then begin
      FDataBase:=TIBDataBase.Create(self);
      FDataBase.DatabaseName:=frmMain.CurrSelDatabase.Database.DatabaseName;
      FDataBase.Params.Text:=frmMain.CurrSelDatabase.Database.Params.Text;
      FDataBase.SQLDialect:=frmMain.CurrSelDatabase.DataBase.SQLDialect;
      FDataBase.LoginPrompt:=False;
      FDataBase.Open;
    end;
  except
    result := false;
  end;
end;

procedure TdlgWisql.Newconnection1Click(Sender: TObject);
begin
  if CheckTransactionStatus (false) then begin
    new_connection:=true;
    if ConnectAsDatabase(Self) then begin
      frmMain.UpdateWindowList(Caption, TObject(Self), true);
      UpdateConnectStatus(Assigned(FDatabase.Handle));
      frmMain.UpdateWindowList(Caption, TObject(Self));
      FConnected := false;
    end;
    resqlInput.Clear;
    reSQLOutput.Clear;
    Newconnection1.Enabled:=False;
    Caption := 'Interactive SQL - ' + ExtractFileName(Database.DatabaseName);
  end;
end;

end.

