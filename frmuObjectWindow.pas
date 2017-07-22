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

unit frmuObjectWindow;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Interfaces, Forms, Dialogs,
  StdCtrls, ToolWin, ComCtrls, ImgList, Buttons, Grids, DBGrids,
  ActnList, Db, ExtCtrls, IBDatabase, IBCustomDataSet, dbctrls,
  IBTable, RichBox, CommCtrl;

type
  TTblData = class
  public
    Columns,
    Indexes,
    Triggers,
    CheckConst,
    UniqueConst,
    RefConst: TStringList;
    constructor Create;
    destructor Destroy; override;
  end;

  TProcedureData = class
  public
    Params: TStringList;
    Source: TStringList;
    constructor Create;
    destructor Destroy; override;
  end;

  TFunctionData = class
  public
    Params: TStringList;
    ReturnVal,
    EntryPoint,
    ModuleName: String;
    constructor Create;
    destructor Destroy; override;
  end;

  TTableData = array of TTblData;
  TProcData = array of TProcedureData;
  TFuncData = array of TFunctionData;

  TfrmObjectView = class(TForm)
    StatusBar: TStatusBar;
    ToolBar1: TToolBar;
    cbObjectList: TComboBox;
    pgcProperties: TPageControl;
    tabProperties: TTabSheet;
    tabMetadata: TTabSheet;
    tabPermissions: TTabSheet;
    tabData: TTabSheet;
    dbgData: TDBGrid;
    tabDependencies: TTabSheet;
    dbgDataSource: TDataSource;
    btnApply: TButton;
    objControl: TPageControl;
    tabDomains: TTabSheet;
    Label5: TLabel;
    reConstraint: TlzRichEdit;
    tabTables: TTabSheet;
    ToolBar2: TToolBar;
    tbCols: TToolButton;
    tbTriggers: TToolButton;
    tbChkConst: TToolButton;
    tbIndexes: TToolButton;
    tbUnique: TToolButton;
    TableActions: TActionList;
    ShowColumns: TAction;
    ShowTriggers: TAction;
    ShowCheckConstraints: TAction;
    ShowIndexes: TAction;
    ShowUniqueConstraints: TAction;
    ShowReferentialConstraints: TAction;
    tbRef: TToolButton;
    lvTableObjects: TListView;
    reTriggerSource: TlzRichEdit;
    tabProcedures: TTabSheet;
    tabFunctions: TTabSheet;
    tabExceptions: TTabSheet;
    tabGenerators: TTabSheet;
    Splitter2: TSplitter;
    reProcSource: TlzRichEdit;
    lvParams: TListView;
    lvFuncView: TListView;
    tabFilters: TTabSheet;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    edFilterModule: TEdit;
    edFilterEntry: TEdit;
    edFilterInput: TEdit;
    edFilterOutput: TEdit;
    Label3: TLabel;
    Label18: TLabel;
    edExceptionNumber: TEdit;
    Label19: TLabel;
    edMessage: TEdit;
    edGenID: TEdit;
    Label21: TLabel;
    edNextValue: TEdit;
    Panel5: TPanel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    edReturnVal: TEdit;
    edEntrypoint: TEdit;
    edModName: TEdit;
    reMetadata: TlzRichEdit;
    lvDomains: TListView;
    lblFileName: TLabel;
    SplitterWnd: TSplitter;
    Panel2: TPanel;
    rbDependent: TRadioButton;
    rbDependedOn: TRadioButton;
    pnlDependents: TPanel;
    tvDependents: TTreeView;
    pnlDependencies: TPanel;
    tvDependencies: TTreeView;
    lvPermissions: TListView;
    Panel1: TPanel;
    Image1: TImage;
    Label1: TLabel;
    IBTable: TIBTable;
    Panel3: TPanel;
    DBNavigator1: TDBNavigator;
    Panel4: TPanel;
    sbCommitRefresh: TSpeedButton;
    Panel6: TPanel;
    cbExtractData: TCheckBox;
    procedure cbObjectListChange(Sender: TObject);
    procedure pgcPropertiesChange(Sender: TObject);
    procedure dbgDataCellClick(Column: TColumn);
    procedure dbgDataDrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure dbgDataEditButtonClick(Sender: TObject);
    procedure cbGetIndex(Sender: TObject);
    procedure ObjectChange(Sender: TObject);
    procedure ShowColumnsExecute(Sender: TObject);
    procedure ShowTriggersExecute(Sender: TObject);
    procedure ShowObjectSource(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure ShowCheckConstraintsExecute(Sender: TObject);
    procedure ShowIndexesExecute(Sender: TObject);
    procedure ShowUniqueConstraintsExecute(Sender: TObject);
    procedure ShowReferentialConstraintsExecute(Sender: TObject);
    procedure ShowProcSource(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure FormShow(Sender: TObject);
    procedure rbDependentClick(Sender: TObject);
    procedure rbDependedOnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure sbCommitRefreshClick(Sender: TObject);
    procedure cbExtractDataClick(Sender: TObject);

  private
    { Private declarations }
    FObjectArray: TStringList;
    FObjNameList: TStringList;

    { For displaying Object Information }
    FTableData : TTableData;
    FProcedureData: TProcData;
    FFunctionData: TFuncData;

    FRoleData: TStringList;
    FDomainData: TStringList;
    FViewData: TStringList;
    FGenData: TStringList;
    FFilterData: TStringList;
    FExceptionData: TStringList;

    FMetadataRefreshList: array of boolean;
    FRefreshList: array of boolean;

    FIndex,
    FObjType: Integer;
    FShowSystem: boolean;
    FDatabase: TIBDatabase;
    FDataSet: TIBDataSet;
    FTransaction: TIBTransaction;
    FObjName: String;
    FIdx: Integer;

    procedure GetDependencies (const ObjName: String; const ObjType: Integer);
    procedure GetPermissions (const ObjName: String; const ObjType: Integer);
    procedure GetDomainProperties(const ObjName: String);
    procedure GetTableProperties;
    procedure GetProcedureProperties;
    procedure GetFunctionProperties;
    procedure GetFilterProperties;
    procedure GetRoleProperties;
    procedure GetViewProperties;
    procedure GetExceptionProperties;
    procedure GetGeneratorProperties;
    procedure FillList (var ListObject: TListView; const StringList: TStringList);
  public
    { Public declarations }
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InitDlg(ObjType: Integer; ObjList: TStringList; ObjName: String;
                      Database: TIBDatabase; ObjIcon: TIcon;
                      const Showsystem, Refetch: boolean);
    procedure Refetch;
  end;

var
  frmObjectView: TfrmObjectView;

implementation

uses
  frmuMessage, frmuDisplayBlob, frmuDispMemo, dmuMain,
  zluUtility, zluGlobal, IBSQL, IBExtract, frmuMain, filectrl, zluPersistent;

{$R *.lfm}

const
  TAB_DOMAIN = 0;
  TAB_TABLE = 1;
  TAB_PROCEDURE = 2;
  TAB_TRIGGER = 3;
  TAB_EXCEPTION = 4;
  TAB_FILTER = 5;
  TAB_FUNCTION = 6;
  TAB_GENERATOR = 7;
  TAB_ROLE = 8;
  TAB_VIEW = 9;

  USR_COL = 0;
  SEL_COL = 1;
  DEL_COL = 2;
  INS_COL = 3;
  UPD_COL = 4;
  REF_COL = 5;
  EXE_COL = 6;
  MEM_COL = 7;
  GRANT_COL = 8;

  DESCRIPTION_ROW = 0;

  COLUMN_LIST = 0;
  TRIGGER_LIST = 1;
  CHECK_CONST_LIST = 2;
  INDEX_LIST = 3;
  UNIQUE_CONST_LIST = 4;
  REF_CONST_LIST = 5;

{ TfrmObjectView }

destructor TfrmObjectView.Destroy;
var
  lCnt: integer;

begin
  if Assigned (FObjNameList) then
    FObjNameList.Free;

  if Assigned(FObjectArray) then
    FObjectArray.Free;

  for lCnt := Low(FTableData) to High(FTableData) do
    FreeAndNil(FTableData[lCnt]);

  for lCnt := Low(FProcedureData) to High(FProcedureData) do
    FreeAndNil(FProcedureData[lCnt]);

  for lCnt := Low(FFunctionData) to High(FFunctionData) do
    FreeAndNil(FFunctionData[lCnt]);

  if Assigned(FFilterData) then
    FFilterData.Free;

  if Assigned(FRoleData) then
    FRoleData.Free;
  if Assigned(FExceptionData) then
    FExceptionData.Free;
  if Assigned(FViewData) then
    FViewData.Free;
  if Assigned(FGenData) then
    FGenData.Free;
  if Assigned(FDomainData) then
    FDomainData.Free;

  if Assigned(FDataSet) then
    FDataSet.Free;

  if Assigned(FTransaction) then
  begin
    if FTransaction.InTransaction then
      FTransaction.Commit;
    FTransaction.Free;
  end;

  inherited Destroy;
end;

procedure TfrmObjectView.InitDlg(ObjType: Integer;
                                 ObjList: TStringList;
                                 ObjName: String;
                                 Database: TIBDatabase; ObjIcon: TIcon;
                                 const Showsystem, Refetch: boolean);
var
  lRefetch: boolean;
  lCnt: Integer;
  s: string;

begin
  FObjType := ObjType;
  if not (ShowSystem = FShowSystem) then
    { All lists need to be refetched }
    lRefetch := true
  else
    lRefetch := Refetch;
  FShowSystem := ShowSystem;
  FDatabase := Database;
  FObjName := Trim(ObjName);
  Caption := Format('Properties for: %s',[Trim(ObjName)]);
  Icon := ObjIcon;
  if not Assigned(FObjNameList) then
    FObjNameList := TStringList.Create
  else
    FObjNameList.Clear;
  if not Assigned(FObjectArray) then
    FObjectArray := TStringList.Create
  else
    FObjectArray.Clear;
    
  cbObjectList.Items.Clear;
  reMetadata.Clear;
  for lCnt := 1 to ObjList.Count -1 do
  begin
    s := ObjList.Strings[lCnt];
    s := Trim(GetNextField(s,DEL));
    FObjNameList.Append (s);
    cbObjectList.Items.Append (s);
  end;

  { Set up the array to hold object data }
  cbObjectList.ItemIndex := FObjNameList.IndexOf(FObjName);
  FIdx := cbObjectList.ItemIndex;
  lblFileName.Caption := Database.DatabaseName;
  StatusBar.Panels[0].Text := MinimizeName (lblFileName.Caption, lblFileName.Canvas,
    StatusBar.Panels[0].Width);
  StatusBar.Panels[1].Text := NODE_ARRAY[FObjType];

  tabData.tabVisible := false;
  tabPermissions.tabVisible := false;
  tabProperties.TabVisible := true;

  SetLength (FRefreshList, ObjList.Count-1);
  SetLength (FMetadataRefreshList,ObjList.Count-1);
  for lCnt := Low(FRefreshList) to High(FRefreshList) do
  begin
    FRefreshList[lCnt] := lRefetch;
    FMetadataRefreshList[lCnt] := lRefetch;
  end;

  for lCnt := 0 to objControl.PageCount-1 do
    ObjControl.Pages[lCnt].TabVisible := false;
    
  pgcProperties.ActivePageIndex := 0;
  case FObjType of
    NODE_VIEWS,
    NODE_TABLES:
    begin
      tabData.tabVisible := true;
      tabPermissions.TabVisible := true;
      objControl.ActivePage := tabTables;
      SetLength(FTableData, ObjList.Count-1);
    end;

    NODE_DOMAINS:
    begin
      tabPermissions.tabVisible := false;
      objControl.ActivePage := tabDomains;
    end;

    NODE_PROCEDURES:
     begin
     { Do not show the data tab for procedures }
//     tabData.tabVisible := true;
       tabPermissions.tabVisible := true;
       objControl.ActivePage := tabProcedures;
       SetLength(FProcedureData, ObjList.Count-1);
     end;

    NODE_FUNCTIONS:
    begin
      tabPermissions.tabVisible := false;
      objControl.ActivePage := tabFunctions;
      SetLength(FFunctionData, ObjList.Count-1);
    end;

    NODE_BLOB_FILTERS:
    begin
      tabPermissions.tabVisible := false;
      objControl.ActivePage := tabFilters;
    end;

    NODE_ROLES:
    begin
      tabProperties.TabVisible := false;
      tabPermissions.TabVisible := true;
      pgcProperties.ActivePage := tabPermissions;
    end;

    NODE_EXCEPTIONS:
    begin
      ObjControl.ActivePage := tabExceptions;
    end;

    NODE_GENERATORS:
    begin
      ObjControl.ActivePage := tabGenerators;
    end;
  end;

  pgcPropertiesChange(pgcProperties);
end;

procedure TfrmObjectView.cbObjectListChange(Sender: TObject);
begin
  frmMain.UpdateWindowList(Caption, Self, true);
  with Sender as TComboBox do
  begin
    Caption := Format('Properties for: %s',[Items[ItemIndex]]);;
    FObjName := Items[ItemIndex];
    FIdx := ItemIndex;
    FObjectArray.Append (IntToStr(FIndex));
  end;
  reMetadata.Clear;
  pgcPropertiesChange(pgcProperties);
  frmMain.UpdateWindowList(Caption, Self);
end;

constructor TfrmObjectView.Create(AOwner: TComponent);
begin
  inherited;
  Visible := false;

  FDataSet := TIBDataSet.Create(nil);
  with FDataSet do
  begin
    Database := FDatabase;
    Transaction := FTransaction;

//    ObjectView := true;
//    SparseArrays := true;
  end;
end;

procedure TfrmObjectView.pgcPropertiesChange(Sender: TObject);
var
  ObjName: String;
  IBExtract: TIBExtract;
  ObjectType : TExtractObjectTypes;
  ExtractTypes : TExtractTypes;
begin
  with Sender as TPageControl do
  begin
    ObjName := cbObjectList.Items[cbObjectList.ItemIndex];
    if ActivePage = tabMetadata then
    begin
      { Only refetch if there is nothing there or something caused
        a refresh }
      if (reMetadata.Lines.Count = 0) or FMetadataRefreshList[FIdx] = true then
      begin
        FMetadataRefreshList[FIdx] := false;
        IBExtract := TIBExtract.Create (self);
        Screen.Cursor := crHourGlass;
        ExtractTypes := [];
        try
          with IBExtract do
          begin
            Database := FDatabase;
            ShowSystem := FShowSystem;
            case FObjType of
              NODE_DOMAINS:
                ObjectType := eoDomain;
              NODE_TABLES:
              begin
                ObjectType := eoTable;
                ExtractTypes := [etTrigger, etDomain, etForeign];
              end;
              NODE_VIEWS:
                ObjectType := eoView;
              NODE_PROCEDURES:
                ObjectType := eoProcedure;
              NODE_FUNCTIONS:
                ObjectType := eoFunction;
              NODE_GENERATORS:
                ObjectType := eoGenerator;
              NODE_EXCEPTIONS:
                ObjectType := eoException;
              NODE_BLOB_FILTERS:
                ObjectType := eoBLOBFilter;
              NODE_ROLES:
                ObjectType := eoRole;
              else
                ObjectType := eoDatabase;
            end;
            ExtractObject(ObjectType, ObjName, ExtractTypes);
            reMetadata.Text := Items.Text;
            reMetadata.Perform( EM_SCROLLCARET, 0, 0 );
          end;
        finally
          IBExtract.Free;
          Screen.Cursor := crDefault;
        end;
      end;
    end;

    if ActivePage = tabData then
    begin
      IBTable.Active := false;
      IBTable.Database := FDatabase;
      IBTable.TableName := ObjName;
      IBTable.ReadOnly := false;
      if not IBTable.Database.DefaultTransaction.Active then
        IBTable.Database.DefaultTransaction.StartTransaction;
      IBTable.Active := true;
    end;

    if ActivePage = tabPermissions then
      GetPermissions (ObjName, FObjType);

    if ActivePage = tabDependencies then
      GetDependencies (ObjName, FObjType);

    if ActivePage = tabProperties then
    begin
      case FObjType of
        NODE_DOMAINS:
          GetDomainProperties (ObjName);
        NODE_TABLES:
          GetTableProperties;
        NODE_PROCEDURES:
          GetProcedureProperties;
        NODE_FUNCTIONS:
          GetFunctionProperties;
        NODE_BLOB_FILTERS:
          GetFilterProperties;
        NODE_ROLES:
          GetRoleProperties;
        NODE_VIEWS:
        begin
          GetViewProperties;
          GetTableProperties;
        end;
        NODE_GENERATORS:
          GetGeneratorProperties;
        NODE_EXCEPTIONS:
          GetExceptionProperties;
      end;
    end;
  end;
end;

procedure TfrmObjectView.dbgDataCellClick(Column: TColumn);
begin
  if Column.Field.DataType in [ftMemo, ftBlob] then
    Column.ButtonStyle := cbsEllipsis;
end;

procedure TfrmObjectView.dbgDataDrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn;
  State: TGridDrawState);
var
  DisplayStr: string;

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
            if FDatabase.SQLDialect = 3 then
              DisplayStr := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Column.Field.AsDateTime)
            else
              DisplayStr := FormatDateTime('c', Column.Field.AsDateTime)
          else
            DisplayStr := FormatDateTime('hh:nn:ss.zzz', Column.Field.AsDateTime);
          Canvas.TextRect(Rect, Rect.Left, Rect.Top, DisplayStr);
        end;
      end;
    end;
  end;
end;

procedure TfrmObjectView.dbgDataEditButtonClick(Sender: TObject);
var
  FieldObj: TField;

begin
  with Sender as TDBGrid do begin
    FieldObj := SelectedField;
    case FieldObj.DataType of
      ftBlob:
         DisplayBlob (frmObjectView, FieldObj, TIBDataSet(IBTable));
      ftMemo:
        DisplayMemo (frmObjectView, FieldObj, TIBDataSet(IBTable));
      else
          ShowMessage (FieldObj.DisplayName+' is unknown');
    end;
  end;
end;

procedure TfrmObjectView.GetDependencies(const ObjName: String;
  const ObjType: Integer);
var
  RelName, SqlStr: String;
  Query: TIBSql;
  Trans: TIBTransaction;
  ParentNode, ChildNode: TTreeNode;

begin
  ParentNode := nil;

  if ObjType in [NODE_TABLES, NODE_GENERATORS, NODE_EXCEPTIONS, NODE_FUNCTIONS] then
    rbDependent.Checked := true
  else
    rbDependedOn.Checked := true;

  case ObjType of
    NODE_DOMAINS:
    begin
      SQLStr := 'select rdb$relation_name, rdb$field_name';
      SQLStr := Format('%s from rdb$relation_fields where', [SqlStr]);
      SQLStr := Format('%s rdb$field_source = ''%s''', [SQLStr, ObjName]);
      SQLStr := Format('%s order by rdb$relation_name, rdb$field_name', [SQLStr]);
    end;
    else
    begin
      SQLStr := 'select rdb$dependent_name, rdb$depended_on_name,';
      SQLStr := Format('%s rdb$field_name, rdb$dependent_type, rdb$depended_on_type', [SQLStr]);
      SQLStr := Format('%s from rdb$dependencies where', [SQLStr]);
      SQLStr := Format('%s rdb$dependent_name= ''%s''', [SQLStr, ObjName]);
      SQLStr := Format('%s order by rdb$field_name, rdb$depended_on_name', [SQLStr]);
    end;
  end;

  Query := TIBSql.Create (self);
  Trans := TIBTransaction.Create (self);
  try
    try
      Trans.DefaultDatabase := FDatabase;
      Query.Transaction := Trans;
      Query.Database := FDatabase;

      with Query do
      begin
        SQL.Add (SQLStr);
        Transaction.StartTransaction;
        Prepare;
        ExecQuery;
        tvDependencies.Items.BeginUpdate;
        tvDependencies.Items.Clear;
        if ObjType = NODE_DOMAINS then
        begin
          while not EOF do
          begin
            if AnsiCompareText (RelName, Trim(Fields[0].AsString)) <> 0 then
            begin
              RelName := Trim(Fields[0].AsString);
              ParentNode := tvDependencies.Items.Add (nil, RelName);
              ParentNode.ImageIndex := NODE_TABLES_IMG;
              ParentNode.SelectedIndex := NODE_TABLES_IMG;
            end;
            ChildNode := tvDependencies.Items.AddChild(ParentNode, Trim(Fields[1].AsString));
            ChildNode.ImageIndex := NODE_COLUMNS_IMG;
            ChildNode.SelectedIndex := NODE_COLUMNS_IMG;
            ParentNode.Expand (true);
            Next;
          end;
        end
        else
        begin
          while not EOF do
          begin
            if AnsiCompareText (RelName, Trim(Fields[1].AsString)) <> 0 then
            begin
              RelName := Trim(Fields[1].AsString);
              if (not (Fields[2].IsNull)) or
                 (Fields[4].AsInteger <> 0) then
              begin
                ParentNode := tvDependencies.Items.Add (nil, RelName);
                ParentNode.ImageIndex := GetImageIndex (Fields[4].AsInteger);
                ParentNode.SelectedIndex := ParentNode.ImageIndex;
              end;
            end;
            if not (Fields[2].IsNull) then
            begin
              ChildNode := tvDependencies.Items.AddChild(ParentNode, Trim(Fields[2].AsString));
              ChildNode.ImageIndex := NODE_COLUMNS_IMG;
              ChildNode.SelectedIndex := NODE_COLUMNS_IMG;
              ParentNode.Expand (true);
            end;
            Next;
          end;
          Close;

          { Now get the objects which depend on the current object }
          SQLStr := 'select rdb$dependent_name, rdb$depended_on_name,';
          SQLStr := Format('%s rdb$field_name, rdb$dependent_type, rdb$depended_on_type', [SQLStr]);
          SQLStr := Format('%s from rdb$dependencies where', [SQLStr]);
          SQLStr := Format('%s rdb$depended_on_name= ''%s''', [SQLStr, ObjName]);
          SQLStr := Format('%s order by rdb$dependent_name, rdb$field_name, rdb$depended_on_name', [SQLStr]);
          Sql.Clear;
          Sql.Add(SQLStr);
          Prepare;
          ExecQuery;
          tvDependents.Items.BeginUpdate;
          tvDependents.Items.Clear;
          RelName := '';
          while not EOF do
          begin
            if AnsiCompareText (RelName, Trim(Fields[0].AsString)) <> 0 then
            begin
              RelName := Trim(Fields[0].AsString);
              if (not (Fields[2].IsNull)) or
                 (Fields[3].AsInteger <> 0) then
              begin
                ParentNode := tvDependents.Items.Add (nil, RelName);
                ParentNode.ImageIndex := GetImageIndex (Fields[3].AsInteger);
                ParentNode.SelectedIndex := ParentNode.ImageIndex;
              end;
            end;
            if not (Fields[2].IsNull) then
            begin
              ChildNode := tvDependents.Items.AddChild(ParentNode, Trim(Fields[2].AsString));
              ChildNode.ImageIndex := NODE_COLUMNS_IMG;
              ChildNode.SelectedIndex := NODE_COLUMNS_IMG;
              ParentNode.Expand (true);
            end;
            Next;
          end;
        end;
        tvDependents.Items.Endupdate;
        tvDependencies.Items.EndUpdate;
        Trans.Commit;
        Close;
      end;
    except on E: Exception do
      begin
        ShowMessage (E.Message);
        if Assigned (Query) then
          Query.Free;
        if Assigned (Trans) then
          Trans.Free;
      end;
    end;
  finally
    Trans.Free;
    Query.Free;
  end;
end;

procedure TfrmObjectView.GetPermissions(const ObjName: String;
  const ObjType: Integer);
var
  GrantPermission: boolean;
  RelName, SqlStr: String;
  Perm: PChar;
  Query: TIBSql;
  Trans: TIBTransaction;
  lCnt, img: integer;
  ListItem: TListItem;

begin
  Query := nil;
  Trans := nil;
  ListItem := nil;
  SQLStr := 'select rdb$user, rdb$privilege, rdb$grant_option,';
  SQLStr := Format('%s rdb$relation_name, rdb$user_type from rdb$user_privileges where', [SqlStr]);
  SQLStr := Format('%s rdb$relation_name = ''%s''', [SQLStr, ObjName]);
  SQLStr := Format('%s order by rdb$user, rdb$privilege', [SQLStr]);

  try
    Query := TIBSql.Create (self);
    Trans := TIBTransaction.Create (self);
    Trans.DefaultDatabase := FDatabase;
    Query.Transaction := Trans;
    Query.Database := FDatabase;
    lvPermissions.Items.Clear;
    with Query do
    begin
      SQL.Add (SQLStr);
      Transaction.StartTransaction;
      Prepare;
      ExecQuery;
      while not EOF do
      begin
        if AnsiCompareText (RelName, Trim(Fields[0].AsString)) <> 0 then
        begin
          RelName := Trim(Fields[0].AsString);
          ListItem := lvPermissions.Items.Add;
          ListItem.Caption := RelName;

          case Fields[4].AsInteger of
            obj_relation: img := NODE_TABLES_IMG;
            obj_view: img := NODE_VIEWS_IMG;
            obj_trigger: img := NODE_TRIGGERS_IMG;
            obj_computed: img := NODE_UNK_IMG;
            obj_validation: img := NODE_UNK_IMG;
            obj_procedure: img := NODE_PROCEDUREs_IMG;
            obj_expression_index: img := NODE_UNK;
            obj_exception: img := NODE_EXCEPTIONS_IMG;
            obj_user: img := NODE_USERS_IMG;
            obj_field: img := NODE_COLUMNS_IMG;
            obj_index: img := NODE_INDEXES_IMG;
            obj_user_group: img := NODE_USERS_IMG;
            obj_sql_role: img := NODE_ROLES_IMG
            else
              img := NODE_UNK_IMG;
          end;
          ListItem.ImageIndex := img;

          for lCnt := 1 to lvPermissions.Columns.Count-1 do
            ListItem.SubItems.Add('');
        end;

        GrantPermission := false;
        if Fields[2].AsInteger = 1 then
          GrantPermission := true;

        Perm := Fields[1].AsPointer;
        case Perm^ of
          'D', 'd':
          begin
            ListItem.SubItems[DEL_COL-1] := 'Y';
            if GrantPermission then
              ListItem.SubItemImages[DEL_COL-1] := IMG_GRANT_OPT
            else
              ListItem.SubItemImages[DEL_COL-1] := -1;
          end;
          'I', 'i':
          begin
            ListItem.SubItems[INS_COL-1] := 'Y';
            if GrantPermission then
              ListItem.SubItemImages[INS_COL-1] := IMG_GRANT_OPT
            else
              ListItem.SubItemImages[INS_COL-1] := -1;
          end;
          'M', 'm':
          begin
            ListItem.SubItems[MEM_COL-1] := 'Y';
            if GrantPermission then
              ListItem.SubItemImages[MEM_COL-1] := IMG_GRANT_OPT
            else
              ListItem.SubItemImages[MEM_COL-1] := -1;
          end;
          'R', 'r':
          begin
            ListItem.SubItems[REF_COL-1] := 'Y';
            if GrantPermission then
              ListItem.SubItemImages[REF_COL-1] := IMG_GRANT_OPT
            else
              ListItem.SubItemImages[REF_COL-1] := -1;
          end;
          'S', 's':
          begin
            ListItem.SubItems[SEL_COL-1] := 'Y';
            if GrantPermission then
              ListItem.SubItemImages[SEL_COL-1] := IMG_GRANT_OPT
            else
              ListItem.SubItemImages[SEL_COL-1] := -1;
          end;
          'U', 'u':
          begin
            ListItem.SubItems[UPD_COL-1] := 'Y';
            if GrantPermission then
              ListItem.SubItemImages[UPD_COL-1] := IMG_GRANT_OPT
            else
              ListItem.SubItemImages[UPD_COL-1] := -1;
          end;
          'X', 'x':
          begin
            ListItem.SubItems[EXE_COL-1] := 'Y';
            if GrantPermission then
              ListItem.SubItemImages[EXE_COL-1] := IMG_GRANT_OPT
            else
              ListItem.SubItemImages[EXE_COL-1] := -1;
          end;
        end;
        Next;
      end;
      Trans.Commit;
      Close;
      Free;
      Trans.Free;
    end;
  except on E: Exception do
    begin
      lvPermissions.Items.EndUpdate;    
      ShowMessage (E.Message);
      if Assigned (Query) then
        Query.Free;
      if Assigned (Trans) then
        Trans.Free;
    end;
  end;
end;

procedure TfrmObjectView.cbGetIndex(Sender: TObject);
begin
  FIndex := cbObjectList.ItemIndex;
end;

procedure TfrmObjectView.ObjectChange(Sender: TObject);
begin
  btnApply.Enabled := true;
  Tag := 1;
end;

procedure TfrmObjectView.GetDomainProperties(const ObjName: String);
var
  TmpList: TStringList;
  TmpString: String;
  cnt: integer;
begin
  if not Assigned (FDomainData) then
    FDomainData := TStringList.Create;

  if (FDomainData.Count = 0) or FRefreshList[FIdx] = true then
  begin
    FDomainData.Clear;
    FRefreshList[FIdx] := false;
    dmMain.GetDomainData (FDomainData, FDatabase, FShowSystem);
  end;
  TmpList := TStringList.create;
  try
    TmpList.Add(Format('Type%sCharacter Set%sCollation%sDefault Value%sAllow Nulls', [DEL,DEL,DEL,DEL]));
    TmpList.Add(FDomainData.Strings[FIdx]);
    FillList(lVDomains, TmpList);

    tmpString := FDomainData.Strings[FIdx];
    for cnt := 0 to lvDomains.Items[0].SubItems.Count - 1 do
      GetNextField(tmpString, DEL);

    reConstraint.Text := GetNextField(tmpString, DEL);
  finally
    TmpList.Free;
  end;
end;

procedure TfrmObjectView.ShowColumnsExecute(Sender: TObject);
begin
  tbCols.Down := true;
  if FObjType <> NODE_VIEWS then
  begin
    reTriggerSource.Visible := false;
    lvTableObjects.Align := alClient;
    lvTableObjects.OnChange := nil;
    SplitterWnd.Visible := false;
  end;
  
  if not Assigned(FTableData[Fidx]) then
    FTableData[Fidx] := TTblData.Create;

  if (FTableData[Fidx].Columns.Count <= 0) or FRefreshList[FIdx] = true then
  begin
    FRefreshList[FIdx] := false;
    FTableData[Fidx].Columns.Clear;
    dmMain.GetColumnList(FTableData[Fidx].Columns, FDatabase, FObjName);
  end;

  FillList(lvTableObjects, FTableData[Fidx].Columns);

  if FObjType = NODE_VIEWS then
    GetViewProperties;
end;

procedure TfrmObjectView.GetTableProperties;
begin
  if tbCols.Down then
    ShowColumnsExecute(nil);

  if tbTriggers.Down then
    ShowTriggersExecute(nil);

  if tbChkConst.Down then
    ShowCheckConstraintsExecute(nil);

  if tbIndexes.Down then
    ShowIndexesExecute(nil);

  if tbUnique.Down then
    ShowUniqueConstraintsExecute(nil);

  if tbRef.Down then
    ShowReferentialConstraintsExecute(nil);
end;

procedure TfrmObjectView.FillList(var ListObject: TListView; const StringList: TStringList);
var
  loListItem: TListItem;
  loListColumn: TListColumn;
  lsCurrLine: string;
  i: integer;
  WasVisible: boolean;

begin
  // to prevent delay
  WasVisible := ListObject.Visible;
  ListObject.Visible := FALSE;
  try

    ListObject.Items.BeginUpdate;
    ListObject.Items.Clear;

    ListObject.Columns.Clear;

    ListObject.AllocBy := StringList.Count;

    { The column Headers are stored in element 0 }
    lsCurrLine := StringList.Strings[0];
    while Length(lsCurrLine) > 0 do
    begin
      loListColumn := ListObject.Columns.Add;
      loListColumn.Caption := GetNextField(lsCurrLine, DEL);
      loListColumn.Width := LVSCW_AUTOSIZE_USEHEADER;
    end;

    for i := 1  to StringList.Count - 1 do
    begin
      lsCurrLine := StringList.Strings[i];
      loListItem := ListObject.Items.Add;
      loListItem.Caption := GetNextField(lsCurrLine, DEL);

      while Length(lsCurrLine) > 0 do
      begin
        loListItem.SubItems.Add(GetNextField(lsCurrLine, DEL));
      end;
    end;
    ListObject.Items.EndUpdate;
    if ListObject.Items.Count > 0 then
      ListObject.Items[0].Selected := true;
  finally
    ListObject.Visible := WasVisible;
  end;
end;

procedure TfrmObjectView.ShowTriggersExecute(Sender: TObject);
begin
  tbTriggers.Down := true;
  reTriggerSource.Clear;
  reTriggerSource.Visible := true;
  lvTableObjects.Align := alTop;
  lvTableObjects.Anchors := [akLeft, akRight, akTop, akBottom];
  lvTableObjects.Height := lvTableObjects.Parent.Height div 2;
  SplitterWnd.Visible := true;
  SplitterWnd.Top := lvTableObjects.Height + lvTableObjects.Top;
  if not Assigned(FTableData[Fidx]) then
    FTableData[Fidx] := TTblData.Create;

  if (FTableData[Fidx].Triggers.Count <= 0) or FRefreshList[FIdx] = true then
  begin
    FRefreshList[FIdx] := false;
    FTableData[Fidx].Triggers.Clear;
    dmMain.GetTriggerList(FTableData[Fidx].Triggers, FDatabase, FObjName);
  end;

  FillList (lvTableObjects, FTableData[Fidx].Triggers);
  reTriggerSource.Clear;
  lvTableObjects.OnChange := ShowObjectSource;
  lvTableObjects.Selected := lvTableObjects.TopItem;
end;

procedure TfrmObjectView.ShowObjectSource(Sender: TObject;
  Item: TListItem; Change: TItemChange);
var
  lLine: String;
begin
  if (lvTableObjects.Items.Count > 0) and
     (Item.SubItems.Count > 0) then
  begin
    reTriggerSource.Lines.BeginUpdate;
    reTriggerSource.Clear;
    lLine := Item.SubItems.Strings[Item.SubItems.Count-1];
    reTriggerSource.Lines.Text := lLine;
    reTriggerSource.Lines.EndUpdate;
  end;
end;

procedure TfrmObjectView.ShowCheckConstraintsExecute(Sender: TObject);
begin
  tbChkConst.Down := true;
  reTriggerSource.Visible := true;
  reTriggersource.Clear;
  lvTableObjects.Align := alTop;
  lvTableObjects.Anchors := [akLeft, akRight, akTop, akBottom];
  lvTableObjects.Height := lvTableObjects.Parent.Height div 2;
  SplitterWnd.Visible := true;
  SplitterWnd.Top := lvTableObjects.Height + lvTableObjects.Top;
  if not Assigned(FTableData[Fidx]) then
    FTableData[Fidx] := TTblData.Create;

  if (FTableData[Fidx].CheckConst.Count <= 0) or FRefreshList[FIdx] = true then
  begin
    FRefreshList[FIdx] := false;
    FTableData[Fidx].CheckConst.Clear;
    dmMain.GetCheckConstList(FTableData[Fidx].CheckConst, FDatabase, FObjName);
  end;

  FillList (lvTableObjects, FTableData[Fidx].CheckConst);
  reTriggerSource.Clear;
  lvTableObjects.OnChange := ShowObjectSource;
  lvTableObjects.Selected := lvTableObjects.TopItem;
end;

procedure TfrmObjectView.ShowIndexesExecute(Sender: TObject);
begin
  tbIndexes.Down := true;
  reTriggerSource.Visible := false;
  lvTableObjects.Align := alClient;
  lvTableObjects.OnChange := nil;
  SplitterWnd.Visible := false;
  if not Assigned(FTableData[Fidx]) then
    FTableData[Fidx] := TTblData.Create;

  if (FTableData[Fidx].Indexes.Count <= 0) or FRefreshList[FIdx] = true then
  begin
    FRefreshList[FIdx] := false;
    FTableData[Fidx].Indexes.Clear;    
    dmMain.GetIndexList(FTableData[Fidx].Indexes, FDatabase, FObjName);
  end;

  FillList (lvTableObjects, FTableData[Fidx].Indexes);
end;

procedure TfrmObjectView.ShowUniqueConstraintsExecute(Sender: TObject);
begin
  tbUnique.Down := true;
  reTriggerSource.Visible := false;
  lvTableObjects.Align := alClient;
  lvTableObjects.OnChange := nil;
  SplitterWnd.Visible := false;
  if not Assigned(FTableData[Fidx]) then
    FTableData[Fidx] := TTblData.Create;

  if (FTableData[Fidx].UniqueConst.Count <= 0) or FRefreshList[FIdx] = true then
  begin
    FRefreshList[FIdx] := false;
    FTableData[Fidx].UniqueConst.Clear;
    dmMain.GetUniqueConstList(FTableData[Fidx].UniqueConst, FDatabase, FObjName);
  end;

  FillList (lvTableObjects, FTableData[Fidx].UniqueConst);
end;


procedure TfrmObjectView.ShowReferentialConstraintsExecute(
  Sender: TObject);
begin
  tbRef.Down := true;
  reTriggerSource.Visible := false;
  lvTableObjects.Align := alClient;
  lvTableObjects.OnChange := nil;
  SplitterWnd.Visible := false;
  if not Assigned(FTableData[Fidx]) then
    FTableData[Fidx] := TTblData.Create;

  if (FTableData[Fidx].RefConst.Count <= 0) or FRefreshList[FIdx] = true then
  begin
    FRefreshList[FIdx] := false;
    FTableData[Fidx].RefConst.Clear;    
    dmMain.GetReferentialConstList(FTableData[Fidx].RefConst, FDatabase, FObjName);
  end;

  FillList (lvTableObjects, FTableData[Fidx].RefConst);
end;

procedure TfrmObjectView.GetProcedureProperties;
begin
  if not Assigned(FProcedureData[Fidx]) then
    FProcedureData[Fidx] := TProcedureData.Create;

  if (FProcedureData[Fidx].Source.Count = 0) or FRefreshList[FIdx] = true then
  begin
    FProcedureData[Fidx].Params.Clear;
    FProcedureData[Fidx].Source.Clear;
    FRefreshList[FIdx] := false;
    dmMain.GetProcedureInfo(FProcedureData[FIdx].Params, FProcedureData[FIdx].Source, FDatabase, FObjName);

    if FProcedureData[FIdx].Params.Count = 1 then
      { This procedure has no parameters, so get the source }
      dmMain.getProcedureSource (FProcedureData[FIdx].Source, FDatabase, FObjName);
  end;

  if FProcedureData[Fidx].Params.Count > 1 then
    FillList (lvParams, FProcedureData[Fidx].Params)
  else
  begin
    lvParams.Items.Clear;
    ShowProcSource(nil, nil, ctText);
  end;
end;

procedure TfrmObjectView.ShowProcSource(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  reProcSource.Lines.BeginUpdate;
  reProcSource.Lines.Clear;
  if Assigned(FProcedureData[Fidx]) then
    reProcSource.Lines.AddStrings(FProcedureData[Fidx].Source);
  reProcSource.Lines.EndUpdate
end;

procedure TfrmObjectView.GetFunctionProperties;
begin
  if not Assigned(FFunctionData[FIdx]) then
    FFunctionData[Fidx] := TFunctionData.Create;

  if (Length(FFunctionData[FIdx].ModuleName) = 0) or FRefreshList[FIdx] = true then
  begin
    FFunctionData[Fidx].Params.Clear;
    FRefreshList[FIdx] := false;    
    with FFunctionData[FIdx] do
      dmMain.GetFunctionData(Params, ModuleName, EntryPoint, ReturnVal, Fdatabase, FObjName);
  end;

  FillList (lvFuncview, FFunctionData[FIdx].Params);
  edModName.Text := FFunctionData[FIdx].ModuleName;
  edEntrypoint.Text := FFunctionData[FIdx].Entrypoint;
  edReturnVal.Text := FFunctionData[FIdx].ReturnVal;
end;

procedure TfrmObjectView.GetFilterProperties;
var
  lLine: String;
begin
  if not Assigned(FFilterData) then
    FFilterData := TStringList.Create;

  if (FFilterData.Count = 0) or FRefreshList[FIdx] = true then
  begin
    FFilterData.Clear;
    FRefreshList[FIdx] := false;    
    dmMain.GetFilterData(FFilterData, Fdatabase, FObjName);
  end;

  lLine := FFilterData.Strings[FIdx];
  edFilterModule.Text := GetNextField(lLine,DEL);
  edFilterEntry.Text := GetNextField(lLine,DEL);
  edFilterInput.Text := GetNextField(lLine,DEL);
  edFilterOutput.Text := GetNextField(lLine,DEL);
end;


procedure TfrmObjectView.GetRoleProperties;
begin
  if not Assigned(FRoleData) then
    FRoleData := TStringList.Create;

  if (FRoleData.Count = 0) or FRefreshList[FIdx] = true then
  begin
    FRoleData.Clear;
    FRefreshList[FIdx] := false;
    dmMain.GetRoleData(FRoleData, Fdatabase, FObjName);
  end;
end;

procedure TfrmObjectView.GetExceptionProperties;
var
  lLine: String;
begin
  if not Assigned (FExceptionData) then
    FExceptionData := TStringList.Create;

  if (FExceptionData.Count = 0) or FRefreshList[FIdx] = true then
  begin
    FExceptionData.Clear;
    FRefreshList[FIdx] := false;    
    dmMain.GetExceptionData (FExceptionData, FDatabase, FObjName);
  end;

  lLine := FExceptionData.Strings[FIdx];
  edExceptionNumber.Text := GetNextField(lLine, DEL);
  edMessage.Text := GetNextField(lLine, DEL);
end;

procedure TfrmObjectView.GetGeneratorProperties;
var
  lLine: String;
begin
  if not Assigned (FGenData) then
    FGenData := TStringList.Create;

  if (FGenData.Count = 0) or FRefreshList[FIdx] = true then
  begin
    FGenData.Clear;
    FRefreshList[FIdx] := false;    
    dmMain.GetGeneratorData (FGenData, FDatabase, FShowSystem);
  end;

  lLine := FGenData.Strings[FIdx];
  edGenID.Text := GetNextField(lLine, DEL);
  edNextValue.Text := GetNextField(lLine, DEL);
end;

procedure TfrmObjectView.GetViewProperties;
var
  lLine: String;
begin
  if not Assigned (FViewData) then
    FViewData := TStringList.Create;

  if (FViewData.Count = 0) or FRefreshList[FIdx] = true then
  begin
    FViewData.Clear;
    FRefreshList[FIdx] := false;
    dmMain.GetViewData (FViewData, FDatabase, FObjName);
  end;
  reTriggerSource.Clear;
  reTriggerSource.Visible := true;
  lvTableObjects.Align := alTop;
  lvTableObjects.Anchors := [akLeft, akRight, akTop, akBottom];
  lvTableObjects.Height := lvTableObjects.Parent.Height div 2;
  SplitterWnd.Visible := true;
  SplitterWnd.Top := lvTableObjects.Height + lvTableObjects.Top;
  lLine := FViewData.Strings[FIdx];
  GetNextField(lLine, DEL);
  reTriggerSource.Lines.Text := GetNextField(lLine, DEL);
end;

procedure TfrmObjectView.FormShow(Sender: TObject);
begin
  FIdx := cbObjectList.ItemIndex;
  frmMain.PersistentInfo.GetFormSettings(Self, 'ObjState');
end;

procedure TfrmObjectView.rbDependentClick(Sender: TObject);
begin
  pnlDependents.Visible := true;
  pnlDependencies.Visible := false;
end;

procedure TfrmObjectView.rbDependedOnClick(Sender: TObject);
begin
  pnlDependents.Visible := false;
  pnlDependencies.Visible := true;
end;

procedure TfrmObjectView.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  frmMain.PersistentInfo.StoreFormSettings(Self, 'ObjState');
  frmMain.UpdateWindowList(Caption, Self, True);
end;

procedure TfrmObjectView.Refetch;
var
  lCnt: integer;
begin
  for lCnt := Low(FRefreshList) to High(FRefreshList) do
    FRefreshList[lCnt] := true;
  pgcPropertiesChange(pgcProperties);
end;

{ TTblData }

constructor TTblData.Create;
begin
  Columns := TStringList.Create;
  Indexes := TStringList.Create;
  Triggers := TStringList.Create;
  CheckConst := TStringList.Create;
  UniqueConst := TStringList.Create;
  RefConst := TStringList.Create;
end;

destructor TTblData.Destroy;
begin
  Columns.Free;
  Indexes.Free;
  Triggers.Free;
  CheckConst.Free;
  UniqueConst.Free;
  RefConst.Free;
  inherited Destroy;
end;

{ TProcedureData }

constructor TProcedureData.Create;
begin
  Params := TStringList.Create;
  Source := TStringList.Create;
end;

destructor TProcedureData.Destroy;
begin
  Params.Free;
  Source.Free;
  inherited;
end;

{ TFunctionData }

constructor TFunctionData.Create;
begin
  Params := TStringList.Create;
end;

destructor TFunctionData.Destroy;
begin
  Params.Free;
  inherited;
end;

procedure TfrmObjectView.sbCommitRefreshClick(Sender: TObject);
begin
  if Not IBTable.Active then exit;
  IBTable.Active := false;
  try
    IBTable.Database.DefaultTransaction.Commit;
  except
    IBTable.Database.DefaultTransaction.Rollback;
  end;
  IBTable.Database.DefaultTransaction.StartTransaction;
  IBTable.Active := true;
end;

procedure TfrmObjectView.cbExtractDataClick(Sender: TObject);
var
  ObjName: String;
  IBExtract: TIBExtract;
  ObjectType : TExtractObjectTypes;
  ExtractTypes : TExtractTypes;
begin
  ObjName := cbObjectList.Items[cbObjectList.ItemIndex];
  FMetadataRefreshList[FIdx] := false;
  IBExtract := TIBExtract.Create (self);
  Screen.Cursor := crHourGlass;
  ExtractTypes := [];
  try
  with IBExtract do
     begin
     Database := FDatabase;
     ShowSystem := FShowSystem;
           case FObjType of
              NODE_DOMAINS:
                 ObjectType := eoDomain;
              NODE_TABLES:
                 begin
                 ObjectType := eoTable;
                 if cbExtractData.Checked then
                    ExtractTypes := [etTrigger, etDomain, etForeign, etData]
                 else
                    ExtractTypes := [etTrigger, etDomain, etForeign];
                 end;
              NODE_VIEWS:
                  ObjectType := eoView;
              NODE_PROCEDURES:
                  ObjectType := eoProcedure;
              NODE_FUNCTIONS:
                  ObjectType := eoFunction;
              NODE_GENERATORS:
                  ObjectType := eoGenerator;
              NODE_EXCEPTIONS:
                  ObjectType := eoException;
              NODE_BLOB_FILTERS:
                  ObjectType := eoBLOBFilter;
              NODE_ROLES:
                  ObjectType := eoRole;
              else
                  ObjectType := eoDatabase;
              end;
     ExtractObject(ObjectType, ObjName, ExtractTypes);
     reMetadata.Text := Items.Text;
     reMetadata.Perform( EM_SCROLLCARET, 0, 0 );
     end;
  finally
     IBExtract.Free;
  Screen.Cursor := crDefault;
end;

end;

end.
