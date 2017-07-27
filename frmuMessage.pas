unit frmuMessage;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, frmuDlgClass, resstring;

type
  TfrmMessage = class(TDialog)
    imgWarning: TImage;
    imgInformation: TImage;
    imgError: TImage;
    stxSummaryMsg: TStaticText;
    lblDetailMsg: TLabel;
    btnOK: TButton;
    memDetailMsg: TMemo;
    procedure btnOKClick(Sender: TObject);
    Procedure TranslateVisual;override;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

Const
  {Global Errors - (0001-2999}
  ERR_SYSTEM_INIT = 0001;
  ERR_USERNAME = 0002;
  ERR_PASSWORD = 0003;
  ERR_PASSWORD_MISSMATCH = 0004;
  ERR_ADD_USER = 0005;
  ERR_MODIFY_USER = 0006;
  ERR_DELETE_USER = 0007;
  ERR_GET_USERS = 0008;
  ERR_GET_USER_INFO = 0009;
  ERR_SOURCE_DB = 0010;
  ERR_DESTINATION_DB = 0011;
  ERR_SAME_SOURCE_DESTINATION = 0012;
  ERR_DB_FILE = 0013;
  ERR_SERVER_NAME = 0014;
  ERR_PROTOCOL = 0015;
  ERR_BACKUP_DB = 0016;
  ERR_RESTORE_DB = 0017;
  ERR_GET_TABLES = 0018;
  ERR_GET_VIEWS = 0019;
  ERR_SERVICE = 0020;
  ERR_NUMERIC_VALUE = 0025;
  ERR_GET_TABLE_DATA = 0026;
  ERR_DB_ALIAS = 0027;
  ERR_GET_ROLES = 0028;
  ERR_SERVER_LOGIN = 0029;
  ERR_DB_CONNECT = 0030;
  ERR_DB_DISCONNECT = 0031;
  ERR_GET_PROCEDURES = 0032;
  ERR_GET_FUNCTIONS = 0033;
  ERR_GET_GENERATORS = 0034;
  ERR_GET_EXCEPTIONS = 0035;
  ERR_GET_BLOB_FILTERS = 0036;
  ERR_GET_COLUMNS = 0037;
  ERR_GET_INDICES = 0038;
  ERR_GET_REFERENTIAL_CONST = 0039;
  ERR_GET_UNIQUE_CONST = 0040;
  ERR_GET_CHECK_CONST = 0041;
  ERR_GET_TRIGGERS = 0042;
  ERR_GET_DDL = 0043;
  ERR_INVALID_PROPERTY_VALUE = 0044;
  ERR_GET_DEPENDENCIES = 0045;
  ERR_GET_DB_PROPERTIES = 0046;
  ERR_DB_SIZE = 0047;
  ERR_ISQL_ERROR = 0048;
  ERR_SERVER_SERVICE = 0049;
  ERR_EXTERNAL_EDITOR = 0050;
  ERR_SERVER_ALIAS = 0051;
  ERR_BACKUP_ALIAS = 0052;
  ERR_DB_SHUTDOWN = 0053;
  ERR_MODIFY_DB_PROPERTIES = 0054;
  ERR_DROP_DATABASE = 0055;
  ERR_FILE_OPEN = 0056;
  ERR_INV_EDITOR = 0057;
  ERR_EDITOR_MISSING = 0058;
  ERR_BAD_FORMAT = 0059;
  ERR_INV_DIALECT = 0060;
  ERR_FOPEN = 0061;
  ERR_TEXT_NOT_FOUND = 0062;
  ERR_PRINT = 0063;
  ERR_NO_PATH = 0064;
  ERR_NO_FILES = 0065;
  ERR_GET_DOMAINS = 0066;
  ERR_EXT_TOOL_ERROR = 0067;
  ERR_PROPERTIES = 0068;
  ERR_ALIAS_EXISTS = 0069;
  ERR_INVALID_CERTIFICATE = 0070;

  {Global Warnings - (3000-5999}
  WAR_DB_UNAVAILABLE = 3000;
  WAR_NO_PERMISSION = 3001;
  WAR_SERVER_REGISTERED = 3002;
  WAR_DUPLICATE_DB_ALIAS = 3003;
  WAR_BACKUP_FILE_REGISTERED = 3004;
  WAR_DIALECT_MISMATCH = 3005;
  WAR_REMOTE_FILENAME = 3006;

  {Global Messages - (6000-9999}
  INF_ADD_USER_SUCCESS = 6000;
  INF_BACKUP_DB_SUCCESS = 6001;
  INF_RESTORE_DB_SUCCESS = 6002;
  INF_NO_PENDING_TRANSACTIONS = 6003;
  INF_RESTART_SERVER = 6004;
  INF_DATABASE_SHUTDOWN = 6005;
  INF_DATABASE_RESTARTED = 6006;
  INF_SQL_SCRIPT = 6007;
  INF_DATABASE_SWEEP = 6008;

  function DisplayMsg(const MsgNo: integer; MsgText: string): boolean;

implementation

uses zluUtility, IBErrorCodes;

{$R *.lfm}

function DisplayMsg(const MsgNo: integer; MsgText: string): boolean;
var
  frmMessage: TfrmMessage;
begin
  frmMessage := TfrmMessage.Create(Application.MainForm);
  try
    MsgText := RemoveControlChars(MsgText);

    case MsgNo of
      1..2999:
      begin
        frmMessage.Caption := LZTMessageErrorCaption;
        frmMessage.imgError.Visible := true;
        frmMessage.imgWarning.Visible := false;
        frmMessage.imgInformation.Visible := false;
        frmMessage.memDetailMsg.Text :=  MsgText;        
      end;
      3000..5999:
      begin
        frmMessage.Caption := LZTMessageWarningCaption;
        frmMessage.imgError.Visible := false;
        frmMessage.imgWarning.Visible := true;
        frmMessage.imgInformation.Visible := false;
        frmMessage.memDetailMsg.Text :=  MsgText;
      end;
      6000..9999:
      begin
        frmMessage.Caption := LZTMessageInformationCaption;
        frmMessage.imgError.Visible := false;
        frmMessage.imgWarning.Visible := false;
        frmMessage.imgInformation.Visible := true;
        frmMessage.memDetailMsg.Text :=  MsgText;
      end;
      335544321..336920607:
      begin
        frmMessage.Caption := LZTMessageErrorCaption;
        frmMessage.imgError.Visible := true;
        frmMessage.imgWarning.Visible := false;
        frmMessage.imgInformation.Visible := false;
        frmMessage.memDetailMsg.Text :=  MsgText;        
      end;
    end;

    case MsgNo of
      //****** Errors ******
      ERR_SYSTEM_INIT:
        frmMessage.stxSummaryMsg.Caption := LZTMessage1Caption;
      ERR_USERNAME:
        frmMessage.stxSummaryMsg.Caption := LZTMessage2Caption;
      ERR_PASSWORD:
        frmMessage.stxSummaryMsg.Caption := LZTMessage3Caption;
      ERR_PASSWORD_MISSMATCH:
        frmMessage.stxSummaryMsg.Caption := LZTMessage4Caption;
      ERR_ADD_USER:
        frmMessage.stxSummaryMsg.Caption := LZTMessage5Caption;
      ERR_MODIFY_USER:
        frmMessage.stxSummaryMsg.Caption := LZTMessage6Caption;
      ERR_DELETE_USER:
        frmMessage.stxSummaryMsg.Caption := LZTMessage7Caption;
      ERR_GET_USERS:
        frmMessage.stxSummaryMsg.Caption := LZTMessage8Caption;
      ERR_GET_USER_INFO:
        frmMessage.stxSummaryMsg.Caption := LZTMessage9Caption;
      ERR_SOURCE_DB:
        frmMessage.stxSummaryMsg.Caption := LZTMessage10Caption;
      ERR_DESTINATION_DB:
        frmMessage.stxSummaryMsg.Caption := LZTMessage11Caption;
      ERR_SAME_SOURCE_DESTINATION:
        frmMessage.stxSummaryMsg.Caption := LZTMessage12Caption;
      ERR_DB_FILE:
        frmMessage.stxSummaryMsg.Caption := LZTMessage13Caption;
      ERR_SERVER_NAME:
        frmMessage.stxSummaryMsg.Caption := LZTMessage14Caption;
      ERR_PROTOCOL:
        frmMessage.stxSummaryMsg.Caption := LZTMessage15Caption;
      ERR_BACKUP_DB:
        frmMessage.stxSummaryMsg.Caption := LZTMessage16Caption;
      ERR_RESTORE_DB:
        frmMessage.stxSummaryMsg.Caption := LZTMessage17Caption;
      ERR_GET_TABLES:
        frmMessage.stxSummaryMsg.Caption := LZTMessage18Caption;
      ERR_GET_VIEWS:
        frmMessage.stxSummaryMsg.Caption := LZTMessage19Caption;
      ERR_SERVICE:
        frmMessage.stxSummaryMsg.Caption := LZTMessage20Caption;
      ERR_NUMERIC_VALUE:
        frmMessage.stxSummaryMsg.Caption := LZTMessage21Caption;
      ERR_GET_TABLE_DATA:
        frmMessage.stxSummaryMsg.Caption := LZTMessage22Caption;
      ERR_DB_ALIAS:
        frmMessage.stxSummaryMsg.Caption := LZTMessage23Caption;
      ERR_GET_ROLES:
        frmMessage.stxSummaryMsg.Caption := LZTMessage24Caption;
      ERR_SERVER_LOGIN:
        frmMessage.stxSummaryMsg.Caption := LZTMessage25Caption;
      ERR_DB_CONNECT:
        frmMessage.stxSummaryMsg.Caption := LZTMessage26Caption;
      ERR_DB_DISCONNECT:
        frmMessage.stxSummaryMsg.Caption := LZTMessage27Caption;
      ERR_GET_PROCEDURES:
        frmMessage.stxSummaryMsg.Caption := LZTMessage28Caption;
      ERR_GET_FUNCTIONS:
        frmMessage.stxSummaryMsg.Caption := LZTMessage29Caption;
      ERR_GET_GENERATORS:
        frmMessage.stxSummaryMsg.Caption := LZTMessage30Caption;
      ERR_GET_EXCEPTIONS:
        frmMessage.stxSummaryMsg.Caption := LZTMessage31Caption;
      ERR_GET_BLOB_FILTERS:
        frmMessage.stxSummaryMsg.Caption := LZTMessage32Caption;
      ERR_GET_COLUMNS:
        frmMessage.stxSummaryMsg.Caption := LZTMessage33Caption;
      ERR_GET_INDICES:
        frmMessage.stxSummaryMsg.Caption := LZTMessage34Caption;
      ERR_GET_REFERENTIAL_CONST:
        frmMessage.stxSummaryMsg.Caption := LZTMessage35Caption;
      ERR_GET_UNIQUE_CONST:
        frmMessage.stxSummaryMsg.Caption := LZTMessage36Caption;
      ERR_GET_CHECK_CONST:
        frmMessage.stxSummaryMsg.Caption := LZTMessage37Caption;
      ERR_GET_TRIGGERS:
        frmMessage.stxSummaryMsg.Caption := LZTMessage38Caption;
      ERR_GET_DDL:
        frmMessage.stxSummaryMsg.Caption := LZTMessage39Caption;
      ERR_INVALID_PROPERTY_VALUE:
        frmMessage.stxSummaryMsg.Caption := LZTMessage40Caption;
      ERR_GET_DEPENDENCIES:
        frmMessage.stxSummaryMsg.Caption := LZTMessage41Caption;
      ERR_GET_DB_PROPERTIES:
        frmMessage.stxSummaryMsg.Caption := LZTMessage42Caption;
      ERR_DB_SIZE:
        frmMessage.stxSummaryMsg.Caption := LZTMessage43Caption;
      ERR_ISQL_ERROR:
        frmMessage.stxSummaryMsg.Caption := LZTMessage44Caption;
      ERR_SERVER_SERVICE:
        frmMessage.stxSummaryMsg.Caption := LZTMessage45Caption;
      ERR_EXTERNAL_EDITOR:
        frmMessage.stxSummaryMsg.Caption := LZTMessage46Caption;
      ERR_SERVER_ALIAS:
        frmMessage.stxSummaryMsg.Caption := LZTMessage47Caption;
      ERR_BACKUP_ALIAS:
        frmMessage.stxSummaryMsg.Caption := LZTMessage48Caption;
      ERR_DB_SHUTDOWN:
        frmMessage.stxSummaryMsg.Caption := LZTMessage49Caption;
      ERR_MODIFY_DB_PROPERTIES:
        frmMessage.stxSummaryMsg.Caption := LZTMessage50Caption;
      ERR_DROP_DATABASE:
        frmMessage.stxSummaryMsg.Caption := LZTMessage51Caption;
      ERR_FILE_OPEN:
        frmMessage.stxSummaryMsg.Caption := LZTMessage52Caption;
      ERR_INV_EDITOR:
        frmMessage.stxSummaryMsg.Caption := LZTMessage53Caption;
      ERR_EDITOR_MISSING:
        frmMessage.stxSummaryMsg.Caption := LZTMessage54Caption;
      ERR_BAD_FORMAT:
        frmMessage.stxSummaryMsg.Caption := LZTMessage55Caption;
      ERR_INV_DIALECT:
        frmMessage.stxSummaryMsg.Caption := LZTMessage56Caption;
      ERR_FOPEN:
        frmMessage.stxSummaryMsg.Caption := LZTMessage57Caption;
      ERR_TEXT_NOT_FOUND:
        frmMessage.stxSummaryMsg.Caption := LZTMessage58Caption;
      ERR_PRINT:
        frmMessage.stxSummaryMsg.Caption := LZTMessage59Caption;
      ERR_NO_PATH:
        frmMessage.stxSummaryMsg.Caption := LZTMessage60Caption;
      ERR_NO_FILES:
        frmMessage.stxSummaryMsg.Caption := LZTMessage61Caption;
      ERR_GET_DOMAINS:
        frmMessage.stxSummaryMsg.Caption := LZTMessage62Caption;
      ERR_EXT_TOOL_ERROR:
        frmMessage.stxSummaryMsg.Caption := LZTMessage63Caption;
      ERR_PROPERTIES:
        frmMessage.stxSummaryMsg.Caption := LZTMessage64Caption;
      ERR_ALIAS_EXISTS:
        frmMessage.stxSummaryMsg.Caption := LZTMessage65Caption;

      //****** Warnings ******
      WAR_NO_PERMISSION:
        frmMessage.stxSummaryMsg.Caption := LZTMessage66Caption;
      WAR_SERVER_REGISTERED:
        frmMessage.stxSummaryMsg.Caption := LZTMessage67Caption;
      WAR_DUPLICATE_DB_ALIAS:
        frmMessage.stxSummaryMsg.Caption := LZTMessage68Caption;
      WAR_BACKUP_FILE_REGISTERED:
        frmMessage.stxSummaryMsg.Caption := LZTMessage69Caption;
      WAR_DIALECT_MISMATCH:
        frmMessage.stxSummaryMsg.Caption := LZTMessage70Caption;
      WAR_REMOTE_FILENAME:
        frmMessage.stxSummaryMsg.Caption := LZTMessage71Caption;

      {****** Information ******}
      INF_ADD_USER_SUCCESS:
        frmMessage.stxSummaryMsg.Caption := LZTMessage72Caption;
      INF_BACKUP_DB_SUCCESS:
        frmMessage.stxSummaryMsg.Caption := LZTMessage73Caption;
      INF_RESTORE_DB_SUCCESS:
        frmMessage.stxSummaryMsg.Caption := LZTMessage74Caption;
      INF_NO_PENDING_TRANSACTIONS:
        frmMessage.stxSummaryMsg.Caption := LZTMessage75Caption;
      INF_RESTART_SERVER:
        frmMessage.stxSummaryMsg.Caption := LZTMessage76Caption;
      INF_DATABASE_SHUTDOWN:
        frmMessage.stxSummaryMsg.Caption := LZTMessage77Caption;
      INF_DATABASE_RESTARTED:
        frmMessage.stxSummaryMsg.Caption := LZTMessage78Caption;
      INF_SQL_SCRIPT:
        frmMessage.stxSummaryMsg.Caption := LZTMessage79Caption;
      INF_DATABASE_SWEEP:
        frmMessage.stxSummaryMsg.Caption := LZTMessage80Caption;

      {****** InterBase Errors ******}        
      isc_gbak_db_exists:
         frmMessage.stxSummaryMsg.Caption := LZTMessage81Caption;
      isc_gfix_invalid_sw:
         frmMessage.stxSummaryMsg.Caption := LZTMessage82Caption;
      isc_gfix_incmp_sw:
         frmMessage.stxSummaryMsg.Caption := LZTMessage83Caption;
      isc_gfix_retry:
         frmMessage.stxSummaryMsg.Caption := LZTMessage84Caption;
      isc_gfix_retry_db:
         frmMessage.stxSummaryMsg.Caption := LZTMessage85Caption;
      isc_gbak_page_size_missing:
         frmMessage.stxSummaryMsg.Caption := LZTMessage86Caption;
      isc_gsec_cant_open_db:
         frmMessage.stxSummaryMsg.Caption := LZTMessage87Caption;
      isc_gsec_no_usr_name:
         frmMessage.stxSummaryMsg.Caption := LZTMessage88Caption;
      isc_gsec_err_add:
         frmMessage.stxSummaryMsg.Caption := LZTMessage89Caption;
      isc_gsec_err_modify:
        frmMessage.stxSummaryMsg.Caption := LZTMessage90Caption;
      isc_gsec_err_find_mod:
        frmMessage.stxSummaryMsg.Caption := LZTMessage91Caption;
      isc_gsec_err_rec_not_found:
        frmMessage.stxSummaryMsg.Caption := LZTMessage92Caption;
      isc_gsec_err_delete:
        frmMessage.stxSummaryMsg.Caption := LZTMessage93Caption;
      isc_gsec_err_find_del:
        frmMessage.stxSummaryMsg.Caption := LZTMessage94Caption;
      isc_gsec_err_find_disp:
        frmMessage.stxSummaryMsg.Caption := LZTMessage95Caption;
      isc_sys_request:
        frmMessage.stxSummaryMsg.Caption := LZTMessage96Caption;
      else
        if MsgText <> '' then
          frmMessage.stxSummaryMsg.Caption := MsgText
        else
          frmMessage.stxSummaryMsg.Caption := LZTMessage97Caption;
    end;


    frmMessage.Width := frmMessage.btnOK.Left + frmMessage.btnOK.Width + 21;
    if (MsgText = '') or
       (frmMessage.stxSummaryMsg.Caption = MsgText) then
    begin
      frmMessage.Height := frmMessage.btnOK.Top + frmMessage.btnOK.Height + 21;
      frmMessage.lblDetailMsg.Visible:=false;
      frmMessage.memDetailMsg.Visible:=false;
      frmMessage.Update;
      Application.ProcessMessages;
    end
    else
    begin
      frmMessage.Height := frmMessage.memDetailMsg.Top + frmMessage.memDetailMsg.Height + 21;
      frmMessage.lblDetailMsg.Visible:=true;
      frmMessage.memDetailMsg.Visible:=true;
      frmMessage.Update;
      Application.ProcessMessages;
    end;
    
    if ((frmMessage.ShowModal) = mrAbort) then
      result := true
    else
      result := false;
  finally
    frmMessage.Free;
  end;
end;

procedure TfrmMessage.btnOKClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

Procedure TfrmMessage.TranslateVisual;
Begin
  Self.Caption := LZTMessageFormTitle;
  btnOK.Caption := LZTMessagebtnOKCaption;
  lblDetailMsg.Caption := LZTMessagelblDetailMsgCaption;
End;

end.
