{****************************************************************
*
*  z l u G l o b a l
*
****************************************************************
*  Author: The Client Server Factory Inc.
*  Date:   March 1, 1999
*
*  Description:  This unit contains the declarations of global
*                variables/constants/objects
*
*****************************************************************
* Revisions:
*
*****************************************************************}

unit zluGlobal;

{$MODE Delphi}

interface

Uses
  LCLIntf, LCLType, LMessages, Graphics, classes;
type
  TAppSetting = record
    Name: String;
    Setting: variant;
  end;

{  TFontProps = record
    FontName: String;
    FontSize: Integer;
    FontColor: TColor;
    FontStyle: TFontStyles;
    FontHeight: Integer;
  end;

  TASCIIChars = set of 0..255; }

  TAppSettings = array[0..17] of TAppSetting;

var
  gExternalApps: TStringList;
  gApplShutdown: boolean;
  gWinTempPath: String;
  gApplExePath: string;
  gRegServersKey: string;
  gRegToolsKey: string;
  gAppSettings: TAppSettings;

const
  { Transactions }
  TRA_DDL = 1;  // ddl transaction
  TRA_DFLT = 2;  // default transaction

  { Number of nodes starting at 0 }
  NODES = 39;

  { Tree Nodes.  Must be in the same order as NODE_ARRAY below! }
  NODE_SERVERS = 000;
  NODE_SERVER = 001;
  NODE_DATABASES = 002;
  NODE_DATABASE = 003;
  NODE_BACKUP_ALIASES = 004;
  NODE_BACKUP_ALIAS = 005;
  NODE_USERS = 006;
  NODE_USER = 007;
  NODE_DOMAINS = 008;
  NODE_DOMAIN = 009;
  NODE_TABLES = 010;
  NODE_TABLE = 011;
  NODE_VIEWS = 012;
  NODE_VIEW = 013;
  NODE_PROCEDURES = 014;
  NODE_PROCEDURE = 015;
  NODE_FUNCTIONS = 016;
  NODE_FUNCTION = 017;
  NODE_GENERATORS = 018;
  NODE_GENERATOR = 019;
  NODE_EXCEPTIONS = 020;
  NODE_EXCEPTION = 021;
  NODE_BLOB_FILTERS = 022;
  NODE_BLOB_FILTER = 023;
  NODE_ROLES = 024;
  NODE_ROLE = 025;
  NODE_COLUMNS = 026;
  NODE_COLUMN = 027;
  NODE_INDEXES = 028;
  NODE_INDEX = 029;
  NODE_REFERENTIAL_CONSTRAINTS = 030;
  NODE_REFERENTIAL_CONSTRAINT = 031;
  NODE_UNIQUE_CONSTRAINTS = 032;
  NODE_UNIQUE_CONSTRAINT = 033;
  NODE_CHECK_CONSTRAINTS = 034;
  NODE_CHECK_CONSTRAINT = 035;
  NODE_TRIGGERS = 036;
  NODE_TRIGGER = 037;
  NODE_LOGS = 038;
  NODE_UNK = 999;

  { Image list indexes }
  NODE_SERVERS_INACTIVE_IMG = 1;
  NODE_DATABASES_DISCONNECTED_IMG = 2;
  NODE_BACKUP_ALIAS_IMG = 3;
  NODE_USER_IMG = 4;
  NODE_DOMAINS_IMG = 6;
  NODE_TABLES_IMG = 7;
  NODE_VIEWS_IMG = 8;
  NODE_PROCEDURES_IMG = 9;
  NODE_FUNCTIONS_IMG = 10;
  NODE_GENERATORS_IMG = 11;
  NODE_EXCEPTIONS_IMG = 12;
  NODE_BLOB_FILTERS_IMG = 13;
  NODE_ROLES_IMG = 14;
  NODE_COLUMNS_IMG = 15;
  NODE_INDEXES_IMG = 16;
  NODE_REFERENTIAL_CONSTRAINTS_IMG = 17;
  NODE_UNIQUE_CONSTRAINTS_IMG = 18;
  NODE_CHECK_CONSTRAINTS_IMG = 19;
  NODE_TRIGGERS_IMG = 20;
  NODE_UNK_IMG = 21;
  NODE_SERVERS_ACTIVE_IMG = 22;
  NODE_DATABASES_CONNECTED_IMG = 23;
  NODE_DATABASES_IMG = 24;
  NODE_BACKUP_ALIASES_IMG = 25;
  NODE_USERS_IMG = 26;
  NODE_LOGS_IMG = 29;
  IMG_GRANT_OPT = 46;

  { This array must be in the same order as the node constants above }
  NODE_ARRAY: array [0..NODES] of String = (
    'Servers',
    'Server',
    'Databases',
    'Database',
    'Backup',
    'Backup',
    'Users',
    'User',
    'Domains',
    'Domain',
    'Tables',
    'Table',
    'Views',
    'View',
    'Stored Procedures',
    'Stored Procedure',
    'External Functions',
    'External Function',
    'Generators',
    'Generator',
    'Exceptions',
    'Exception',
    'Blob Filters',
    'Blob Filter',
    'Roles',
    'Role',
    'Columns',
    'Column',
    'Indexes',
    'Index',
    'Referential Constraints',
    'Referential Constraint',
    'Unique Constraints',
    'Unique Constraint',
    'Check Constraints',
    'Check Constraint',
    'Triggers',
    'Trigger',
    'Server Log',
    'Unknown');

  DEL = '~|';
  SING_QUOTE = '''';

  APP_VERSION = 'Version 1.0';

  ENABLE = true;
  DISABLE = false;

  SUCCESS = 0;
  FAILURE = -1;
  EMPTY = -2;
  CANCELED = -3;
  RETRY = -4;
  REGISTER_SERVER = 0;
  SELECT_SERVER = 1;

  FROM_MEMORY = 'M';
  FROM_FILE = 'F';

  DEP_TABLE = 0;
  DEP_VIEW = 1;
  DEP_TRIGGER = 2;
  DEP_COMPUTED_FIELD = 3;
  DEP_VALIDATION = 4;
  DEP_PROCEDURE = 5;
  DEP_EXPRESSION_INDEX = 6;
  DEP_EXCEPTION = 7;
  DEP_USER = 8;
  DEP_FIELD = 9;
  DEP_INDEX = 10;

  NULL_STR = '<null>';
  NULL_BLOB = '(Blob)';
  BLOB_STR = '(BLOB)';

{ From jrd\obj.h ... object types }
  obj_relation =           0;
  obj_view     =           1;
  obj_trigger  =           2;
  obj_computed =           3;
  obj_validation =         4;
  obj_procedure  =         5;
  obj_expression_index =   6;
  obj_exception        =   7;
  obj_user             =   8;
  obj_field            =   9;
  obj_index            =   10;
  obj_count            =   11;
  obj_user_group       =   12;
  obj_sql_role         =   13;

  NUM_SETTINGS = 18;

{ This list must be in the same order as the
  constants below!}

  SETTINGS: array [0..NUM_SETTINGS-1] of String = (
{Boolean Options}
    'SystemData',
    'Dependencies',
    'UseDefaultEditor',
    'ShowQueryPlan',
    'AutoCommitDDL',
    'ShowStatistics',
    'ShowInListFormat',
    'SaveISQLOutput',
    'UpdateOnConnect',
    'UpdateOnCreate',
    'ClearInput',

{String Options}
    'CharacterSet',
    'BlobDisplay',
    'BlobSubtype',
    'ISQLTerminator',

{Integer Settings}
    'CommitOnExit',
    'ViewStyle',
    'DefaultDialect'
    );

{This list is grouped by datatypes.  Change it and
 things will break!}
{Boolean Settings}
  SYSTEM_DATA = 0;
  DEPENDENCIES = 1;
  USE_DEFAULT_EDITOR = 2;
  SHOW_QUERY_PLAN = 3;
  AUTO_COMMIT_DDL = 4;
  SHOW_STATS = 5;
  SHOW_LIST = 6;
  SAVE_ISQL_OUTPUT = 7;
  UPDATE_ON_CONNECT = 8;
  UPDATE_ON_CREATE = 9;
  CLEAR_INPUT = 10;

{String Settings}
  CHARACTER_SET = 11;
  BLOB_DISPLAY = 12;
  BLOB_SUBTYPE = 13;
  ISQL_TERMINATOR = 14;

{Integer Settings}
  COMMIT_ON_EXIT = 15;
  VIEW_STYLE = 16;
  DEFAULT_DIALECT = 17;

  TAB_ACTIONS = 0;
  TAB_DEFINITION = 1;
  TAB_SUMMARY = 2;
  TAB_METADATA = 3;
  TAB_DATA = 4;
  TAB_ISQL = 5;

implementation

// kris
// I propose to put it here, at elast it a better place than before (in frmMain.OnCreate 
// If TAppSettings become a class, this will be put in a constructor
procedure InitSettings;
var
  i: integer;
begin
    { Initialize the application setting defaults }
  for i := 0 to NUM_SETTINGS-1 do
  begin
    gAppSettings[i].Name := SETTINGS[i];
    case i of
     {Boolean Settings}
      SYSTEM_DATA:
        gAppSettings[i].Setting := false;
      DEPENDENCIES:
        gAppSettings[i].Setting := true;
      USE_DEFAULT_EDITOR:
        gAppSettings[i].Setting := true;
      SHOW_QUERY_PLAN:
        gAppSettings[i].Setting := true;
      AUTO_COMMIT_DDL:
        gAppSettings[i].Setting := true;
      SHOW_STATS:
        gAppSettings[i].Setting := true;
      SHOW_LIST:
        gAppSettings[i].Setting := false;
      SAVE_ISQL_OUTPUT:
        gAppSettings[i].Setting := false;
      UPDATE_ON_CONNECT:
        gAppSettings[i].Setting := false;
      UPDATE_ON_CREATE:
        gAppSettings[i].Setting := false;
      CLEAR_INPUT:
        gAppSettings[i].Setting := true;

     {String Settings}
      CHARACTER_SET:
        gAppSettings[i].Setting := 'None';
      BLOB_DISPLAY:
        gAppSettings[i].Setting := 'Restrict';
      BLOB_SUBTYPE:
        gAppSettings[i].Setting := 'Text';
      ISQL_TERMINATOR:
        gAppSettings[i].Setting := ';';

    {Integer Settings}
      COMMIT_ON_EXIT:
        gAppSettings[i].Setting := 0;
      VIEW_STYLE:
        gAppSettings[i].Setting := 3;
      DEFAULT_DIALECT:
        gAppSettings[i].Setting := 3;
    end;
  end;
end;

initialization

  InitSettings;

end.
