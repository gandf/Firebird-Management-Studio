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
 * Contributor(s):  Krzysztof Golko.
}
unit frmuServerProperties;

{$MODE Delphi}

interface

uses
  Forms, ExtCtrls, StdCtrls, Classes, Controls, SysUtils, zluibcClasses,
  ComCtrls, Graphics, IBServices, frmuMessage, IB, LCLIntf, LCLType, LMessages,
  Messages, frmuDlgClass, resstring;

type
  TfrmServerProperties = class(TDialog)
    TabAlias: TTabSheet;
    TabGeneral: TTabSheet;
    bvlLine1: TBevel;
    cboProtocol: TComboBox;
    edtAliasName: TEdit;
    edtHostName: TEdit;
    lblAliasName: TLabel;
    lblAttachmentNo: TLabel;
    lblCapabilities: TLabel;
    lblDatabaseNo: TLabel;
    lblHostName: TLabel;
    lblProtocol: TLabel;
    lblVersion: TLabel;
    lvDatabases: TListView;
    memCapabilities: TMemo;
    pgcMain: TPageControl;
    stxAttachmentNo: TStaticText;
    stxDatabaseNo: TStaticText;
    stxVersion: TStaticText;
    btnApply: TButton;
    btnCancel: TButton;
    btnRefresh: TButton;
    Button1: TButton;
    Label1: TLabel;
    edtDescription: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure cboProtocolChange(Sender: TObject);
    procedure cboProtocolDblClick(Sender: TObject);
    procedure edtAliasNameChange(Sender: TObject);
    procedure lvDatabasesDblClick(Sender: TObject);
    procedure pgcMainChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    Procedure TranslateVisual;override;
  private
    { Private declarations }
    FAssignedServer : TibcServerNode;
    FLicenseDesc: TStringList;
    procedure NoteChanges();
    procedure Refresh();
    procedure ShowActivity();
    procedure LMLButtonDown( var Message: TLMLButtonDown ); message WM_NCLBUTTONDOWN ;
  public
    { Public declarations }
    procedure AssignServerNode(const ServerNode: TibcServerNode);
    function GetNewSettings: TibcServerNode;
    procedure DecodeMask(AssignedServer: TibcServerNode);
  end;

  ENameError = class(Exception);

function EditServerProperties(const CurrSelServer: TibcServerNode): integer;

implementation

uses
  zluGlobal, frmuMain, IBDatabase, frmuDBConnections, IBHeader, IBErrorCodes,
  zluPersistent;

{$R *.lfm}

const
  LIC_A_BIT=0;
  LIC_B_BIT=3;
  LIC_C_BIT=16;
  LIC_D_BIT=1;
  LIC_E_BIT=4;
  LIC_F_BIT=5;
  LIC_I_BIT=8;
  LIC_L_BIT=11;
  LIC_P_BIT=15;
  LIC_Q_BIT=2;
  LIC_R_BIT=17;
  LIC_S_BIT=18;
  LIC_W_BIT=22;
  LIC_2_BIT=27;
  LIC_3_BIT=28;

procedure TfrmServerProperties.btnApplyClick(Sender: TObject);
var
  ServerActive: boolean;
  sProps: TIbcServerProps;
begin
  try
    // save alias and database file information
    Screen.Cursor := crHourGlass;
    ServerActive := FAssignedServer.Server.Active;
    if FAssignedServer.Server.Active then
      FAssignedServer.Server.Active := false;
    PersistentInfo.GetServerProps(FAssignedServer.NodeName, sProps);
    case cboProtocol.ItemIndex of
      0: sProps.Protocol := TCP;
      1: sProps.Protocol := NamedPipe;
      2: sProps.Protocol := SPX;
    end;
    sProps.ServerName := edtHostName.Text;
    sProps.Description := edtDescription.Text;
    PersistentInfo.StoreServerProps(FAssignedServer.NodeName, sProps);

    if FAssignedServer.NodeName <> edtAliasName.Text then
      PersistentInfo.RenameServerAlias(FAssignedServer.NodeName, edtAliasName.Text);

    FAssignedServer.Server.Protocol := sProps.Protocol;
    FAssignedServer.ServerName := sProps.ServerName;
    FAssignedServer.Server.ServerName := sProps.ServerName;
    FAssignedServer.Description := sProps.Description;

  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmServerProperties.btnCancelClick(Sender: TObject);
begin
  btnApply.Click;
  ModalResult := mrOK;
end;

procedure TfrmServerProperties.btnRefreshClick(Sender: TObject);
begin
  Refresh();
end;

procedure TfrmServerProperties.cboProtocolChange(Sender: TObject);
begin
  NoteChanges;
end;

procedure TfrmServerProperties.cboProtocolDblClick(Sender: TObject);
begin
  if not cboProtocol.DroppedDown then
  begin
    if cboProtocol.ItemIndex = cboProtocol.Items.Count - 1 then
      cboProtocol.ItemIndex := 0
    else
      cboProtocol.ItemIndex := cboProtocol.ItemIndex + 1;
  end;
  NoteChanges;
end;

procedure TfrmServerProperties.edtAliasNameChange(Sender: TObject);
begin
  NoteChanges;
end;

procedure TfrmServerProperties.lvDatabasesDblClick(Sender: TObject);
var
  sCurrSelDB:string;
  i:integer;
begin
  sCurrSelDB := '';

  if lvDatabases.Selected <> nil then
  begin
    sCurrSelDB := lvDatabases.Selected.Caption;  // get selected item
    if sCurrSelDB <> '' then             // if there was a selected item before
    begin
      i := lvDatabases.Items.Count;      // get number of databases in list
      while i > 0 do                     // loop to see if it is still attached
      begin
        dec(i);                          // start at end of list
        if lvDatabases.Items.Item[i].Caption = sCurrSelDB then  // if found/attached
        begin
          lvDatabases.Items.Item[i].Selected := true;
          ShowActivity;                  // show users
          i := - 1;                      // lower value of i to indicate success
        end;
      end;  // end loop through attached databases

      if i = 0 then  // no items or original item not found
        DisplayMsg(ERR_GET_USERS, LZTServerPropAllUsersDisconntedDatabase +
          sCurrSelDB + '. ' + LZTServerPropNoLongerAttachedToServer);

    end;  // end if double clicked on item
    Refresh();                           // refresh unselects item
  end;
end;

procedure TfrmServerProperties.pgcMainChange(Sender: TObject);
begin
  if pgcMain.ActivePage = tabAlias then
    btnRefresh.Enabled := false
  else
    btnRefresh.Enabled := true;
end;

procedure TfrmServerProperties.NoteChanges();
begin
  btnApply.Enabled := False;  // assume no changes made, then start checking
  case FAssignedServer.Server.Protocol of
    TCP:
      if (cboProtocol.ItemIndex <> 0) and (cboProtocol.ItemIndex >= 0) then
        btnApply.Enabled := True;
    NamedPipe:
      if (cboProtocol.ItemIndex <> 1)  and (cboProtocol.ItemIndex >= 0) then
        btnApply.Enabled := True;
    SPX:
      if (cboProtocol.ItemIndex <> 2)  and (cboProtocol.ItemIndex >= 0) then
        btnApply.Enabled := True;
  end;
  if edtAliasName.Text <> FAssignedServer.NodeName then
    btnApply.Enabled := True;
  if edtHostName.Text <> FAssignedServer.ServerName then
    btnApply.Enabled := true;

  if edtDescription.Text <> FAssignedServer.Description then
    btnApply.Enabled := true;
end;

procedure TfrmServerProperties.Refresh();
var
  i:integer;
  lDatabaseName:TListItem;

begin
  try
    FLicenseDesc.Clear;
    memCapabilities.Clear;
    if FAssignedServer = nil then
      Exit
    else
    begin
      if not FAssignedServer.Server.Active then
        FAssignedServer.Server.Attach;
      try
        FAssignedServer.Server.FetchDatabaseInfo;
        FAssignedServer.Server.FetchVersionInfo;
      except
        on E:EIBError do
          if EIBInterBaseError(E).IBErrorCode = isc_insufficient_svc_privileges then
            FAssignedServer.Server.Active := true
          else
          begin
            DisplayMsg(ERR_SERVER_SERVICE,E.Message);
            if (EIBInterBaseError(E).IBErrorCode = isc_lost_db_connection) or
               (EIBInterBaseError(E).IBErrorCode = isc_unavailable) or
               (EIBInterBaseError(E).IBErrorCode = isc_network_error) then
              frmMain.SetErrorState;
            SetErrorState;
            exit;
          end;
      end;
      DecodeMask(FAssignedServer);
      memCapabilities.Lines.Add(LZTServerPropUnlimitedUsers);
    end;
  except
    on E:EIBError do
    begin
      DisplayMsg(ERR_SERVER_SERVICE,E.Message);
      if (EIBInterBaseError(E).IBErrorCode = isc_lost_db_connection) or
         (EIBInterBaseError(E).IBErrorCode = isc_unavailable) or
         (EIBInterBaseError(E).IBErrorCode = isc_network_error) then
        frmMain.SetErrorState;
      SetErrorState;
      exit;
    end;
  end;
  stxVersion.Caption := FAssignedServer.Server.VersionInfo.ServerVersion;
  stxDatabaseNo.Caption := IntToStr(FAssignedServer.Server.DatabaseInfo.NoOfDatabases);
  stxAttachmentNo.Caption := IntToStr(FAssignedServer.Server.DatabaseInfo.NoOfAttachments);

  lvDatabases.Items.Clear;
  for i:= 0 to FAssignedServer.Server.DatabaseInfo.NoOfDatabases - 1 do
  begin
    lDatabaseName := lvDatabases.Items.Add;
    lDatabaseName.Caption := FAssignedServer.Server.DatabaseInfo.DbName[i];
  end;
end;

procedure TfrmServerProperties.ShowActivity();
var
  lDatabase : TIBDatabase;
begin
  lDatabase := TIBDatabase.Create(Application.MainForm);
  try
    case FAssignedServer.Server.Protocol of
      TCP: lDatabase.DatabaseName := Format('%s:%s',[FAssignedServer.ServerName,lvDatabases.Selected.Caption]);
      NamedPipe: lDatabase.DatabaseName := Format('\\%s\%s',[FAssignedServer.ServerName,lvDatabases.Selected.Caption]);
      SPX: lDatabase.DatabaseName := Format('%s@%s',[FAssignedServer.ServerName,lvDatabases.Selected.Caption]);
      Local:  lDatabase.DatabaseName := lvDatabases.Selected.Caption;
    end;
    frmuDBConnections.ViewDBConnections(FAssignedServer,lDatabase);
  finally
    lDatabase.Free;
  end;
end;

procedure TfrmServerProperties.AssignServerNode(const ServerNode: TibcServerNode);
begin
  FAssignedServer := ServerNode;
  if FAssignedServer <> nil then
  begin
    edtAliasName.Text:= FAssignedServer.NodeName;
    edtHostName.Text:= FAssignedServer.ServerName;
    edtDescription.Text := FAssignedServer.Description;
    case FAssignedServer.Server.Protocol of
    Local:
      begin
        edtAliasName.Color := clSilver;
        edtHostName.Color := clSilver;
        cboProtocol.Color := clSilver;
        edtAliasName.Enabled := false;
        edtHostName.Enabled := false;
        cboProtocol.Enabled := false;
        edtHostName.Text:= FAssignedServer.NodeName;
      end;
    TCP:
      cboProtocol.ItemIndex := 0;
    NamedPipe:
      cboProtocol.ItemIndex := 1;
    SPX:
      cboProtocol.ItemIndex := 2;
    end;

    if not FAssignedServer.Server.Active then
    begin
      TabGeneral.TabVisible := false;
      cboProtocol.Enabled := true;
      edtHostName.Enabled := true;
    end
    else
      Refresh();
  end;
end;

function EditServerProperties(const CurrSelServer: TibcServerNode): integer;
var
  frmServerProperties : TfrmServerProperties;
  tmpServer: TibcServerNode;
  ServerActive: boolean;

begin
  result := FAILURE;
  frmServerProperties := TfrmServerProperties.Create(Application);
  try
    frmServerProperties.AssignServerNode(CurrSelServer);
    if not frmServerProperties.GetErrorState then
    begin
      frmServerProperties.ShowModal;
      tmpServer := frmServerProperties.GetNewSettings;
      with CurrSelServer do
      begin
        if Server.Protocol <> tmpServer.Server.Protocol then
        begin
          ServerActive := Server.Active;
          if Server.Active then
            Server.Active := false;
          Server.Protocol := tmpServer.Server.Protocol;

          if ServerActive then
          Server.Active := true;
        end;
        NodeName := tmpServer.NodeName;
      end;
      frmMain.RenameTreeNode(CurrSelServer,frmServerProperties.edtAliasName.Text);
      result := SUCCESS;
    end;
  finally
    frmServerProperties.Free;
  end;
end;

procedure TfrmServerProperties.DecodeMask(AssignedServer: TibcServerNode);
var
  lLicenseMask: integer;
begin
  lLicenseMask := AssignedServer.Server.LicenseMaskInfo.LicenseMask;
  if ((lLicenseMask shr LIC_A_BIT) and 1) = 1 then
    FLicenseDesc.Add(LZTServerPropLIC_A_TEXT);

  if ((lLicenseMask shr LIC_B_BIT) and 1) = 1 then
    FLicenseDesc.Add(LZTServerPropLIC_B_TEXT);

  if ((lLicenseMask shr LIC_C_BIT) and 1) = 1 then
    FLicenseDesc.Add(LZTServerPropLIC_C_TEXT);

  if ((lLicenseMask shr LIC_D_BIT) and 1) = 1 then
    FLicenseDesc.Add(LZTServerPropLIC_D_TEXT);

  if ((lLicenseMask shr LIC_E_BIT) and 1) = 1 then
    FLicenseDesc.Add(LZTServerPropLIC_E_TEXT);

  if ((lLicenseMask shr LIC_F_BIT) and 1) = 1 then
    FLicenseDesc.Add(LZTServerPropLIC_F_TEXT);

  if ((lLicenseMask shr LIC_I_BIT) and 1) = 1 then
    FLicenseDesc.Add(LZTServerPropLIC_I_TEXT);

  if ((lLicenseMask shr LIC_L_BIT) and 1) = 1 then
    FLicenseDesc.Add(LZTServerPropLIC_L_TEXT);

  if ((lLicenseMask shr LIC_P_BIT) and 1) = 1 then
    FLicenseDesc.Add(LZTServerPropLIC_P_TEXT);

  if ((lLicenseMask shr LIC_Q_BIT) and 1) = 1 then
    FLicenseDesc.Add(LZTServerPropLIC_Q_TEXT);

  if ((lLicenseMask shr LIC_R_BIT) and 1) = 1 then
    FLicenseDesc.Add(LZTServerPropLIC_R_TEXT);

  if ((lLicenseMask shr LIC_S_BIT) and 1) = 1 then
    FLicenseDesc.Add(LZTServerPropLIC_S_TEXT);

  if ((lLicenseMask shr LIC_W_BIT) and 1) = 1 then
    FLicenseDesc.Add(LZTServerPropLIC_W_TEXT)
  else
    FLicenseDesc.Add(LZTServerPropLimitedTo + IntToStr(FAssignedServer.Server.LicenseInfo.LicensedUsers) + LZTServerPropUsers);

  if ((lLicenseMask shr LIC_2_BIT) and 1) = 1 then
    FLicenseDesc.Add(LZTServerPropLIC_2_TEXT);

  if ((lLicenseMask shr LIC_3_BIT) and 1) = 1 then
    FLicenseDesc.Add(LZTServerPropLIC_3_TEXT);
end;

procedure TfrmServerProperties.FormCreate(Sender: TObject);
begin
  inherited;
  FLicenseDesc := TStringList.Create();
end;

procedure TfrmServerProperties.FormDestroy(Sender: TObject);
begin
  FLicenseDesc.Free;
end;

procedure TfrmServerProperties.LMLButtonDown( var Message: TLMLButtonDown );
var
  ScreenPt: TPoint;
  ClientPt: TPoint;
begin
  ScreenPt.X := Message.XPos;
  ScreenPt.Y := Message.YPos;
  ClientPt := ScreenToClient( ScreenPt );
  if( ClientPt.X > Width-45 )and (ClientPt.X < Width-29) then
   begin
    Message.Result := 0;
  end else
   inherited;
end;

procedure TfrmServerProperties.FormShow(Sender: TObject);
begin
  inherited;
  pgcMain.ActivePage := tabAlias;
  pgcMainChange(Sender);
end;

function TfrmServerProperties.GetNewSettings: TibcServerNode;
begin
  result := FAssignedServer;
end;

procedure TfrmServerProperties.Button1Click(Sender: TObject);
begin
  inherited;
  ModalResult := mrCancel;
end;

Procedure TfrmServerProperties.TranslateVisual;
Begin
  lblVersion.Caption := LZTServerProplblVersion;
  stxVersion.Caption := LZTServerPropstxVersion;
  lblCapabilities.Caption := LZTServerProplblCapabilities;
  lblDatabaseNo.Caption := LZTServerProplblDatabaseNo;
  lblAttachmentNo.Caption := LZTServerProplblAttachmentNo;
  btnApply.Caption := LZTServerPropbtnApply;
  Button1.Caption := LZTServerPropButton1;
  btnCancel.Caption := LZTServerPropbtnCancel;
  btnRefresh.Caption := LZTServerPropbtnRefresh;
  lvDatabases.Columns[0].Caption := LZTServerProplvDatabasesCol;
  TabAlias.Caption := LZTServerPropTabAlias;
  TabGeneral.Caption := LZTServerPropTabGeneral;
  lblAliasName.Caption := LZTServerProplblAliasName;
  lblHostName.Caption := LZTServerProplblHostName;
  Label1.Caption := LZTServerPropLabel1;
  lblProtocol.Caption := LZTServerProplblProtocol;
  cboProtocol.Items.Clear;
  cboProtocol.Items.Add(LZTServerPropTCPIP);
  cboProtocol.Items.Add(LZTServerPropNetBEUI);
  cboProtocol.Items.Add(LZTServerPropSPX);
  cboProtocol.Items.Add(LZTServerPropLocalServer);
  Self.Caption := LZTServerPropFormTitle;
End;

end.


