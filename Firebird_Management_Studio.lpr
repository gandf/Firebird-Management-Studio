program Firebird_Management_Studio;

{$mode objfpc}{$H+}
//{$DEFINE LCLWIN32}
uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  { you can add units after this }

  LCLIntf, LCLType, LMessages, interfacebase,

  frmuSplash, dmuMain, frmuMain,

  Windows, win32int, RichBox, WSRichBoxFactory, WSRichBox,
  Win32WSRichBox, Win32WSRichBoxFactory,

  frmuAbout in 'frmuAbout.pas' {frmAbout},
  zluGlobal in 'zluGlobal.pas',
  frmuServerLogin in 'frmuServerLogin.pas' {frmServerLogin},
  frmuUser in 'frmuUser.pas' {frmUserInfo},
  frmuServerRegister in 'frmuServerRegister.pas' {frmServerRegister},
  zluibcClasses in 'zluibcClasses.pas',
  frmuDBRegister in 'frmuDBRegister.pas' {frmDBRegister},
  frmuMessage in 'frmuMessage.pas' {frmMessage},
  zluUtility in 'zluUtility.pas',
  frmuDBConnect in 'frmuDBConnect.pas' {frmDBConnect},
  frmuDBBackup in 'frmuDBBackup.pas' {frmDBBackup},
  frmuServerProperties in 'frmuServerProperties.pas' {frmServerProperties},
  frmuDBProperties in 'frmuDBProperties.pas' {frmDBProperties},
  frmuBackupAliasProperties in 'frmuBackupAliasProperties.pas' {frmBackupAliasProperties},
  frmuDBCreate in 'frmuDBCreate.pas' {frmDBCreate},
  zluContextHelp in 'zluContextHelp.pas',
  zluCommDiag in 'zluCommDiag.pas',
  frmuCommDiag in 'frmuCommDiag.pas' {frmCommDiag},
  frmuDBConnections in 'frmuDBConnections.pas' {frmDBConnections},
  frmuDBShutdown in 'frmuDBShutdown.pas' {frmDBShutdown},
  frmuDBStatistics in 'frmuDBStatistics.pas' {frmDBStatistics},
  frmuDBValidationReport in 'frmuDBValidationReport.pas' {frmDBValidationReport},
  frmuDBTransactions in 'frmuDBTransactions.pas' {frmDBTransactions},
  frmuDBValidation in 'frmuDBValidation.pas' {frmDBValidation},
  frmuDBRestore in 'frmuDBRestore.pas' {frmDBRestore},
  frmuDispMemo in 'frmuDispMemo.pas' {frmDispMemo},
  frmuModifyServerAlias in 'frmuModifyServerAlias.pas' {frmModifyServerAlias},
  zluSQL in 'zluSQL.pas',
  frmuDisplayBlob in 'frmuDisplayBlob.pas' {frmDisplayBlob},
  frmuDlgClass in 'frmuDlgClass.pas' {Dialog},
  frmuObjectWindow in 'frmuObjectWindow.pas' {frmObjectView},
  wisql in 'wisql.pas' {dlgWisql},
  frmuTextViewer in 'frmuTextViewer.pas' {frmTextViewer},
  frmuSQLOptions in 'frmuSQLOptions.pas' {frmSQLOptions},
  frmuTools in 'frmuTools.pas' {frmTools},
  frmuAddTool in 'frmuAddTool.pas' {frmAddTools},
  frmuDescription in 'frmuDescription.pas' {frmDescription},
  frmuWindowList in 'frmuWindowList.pas' {dlgWindowList},
  MemoLists in 'MemoLists.pas',
  zluPersistent in 'zluPersistent.pas';
{$R *.res}

begin
    { Create a mutex to make sure only 1 instance is running }
  CreateMutex (nil, false, 'fb_man_studio_mtx');
  if GetLastError() = ERROR_ALREADY_EXISTS then
  begin
    { It's already running so Restore the other copy }
    SendMessage (HWND_BROADCAST,
                 RegisterWindowMessage('fb_man_studio_mtx'),
                 0,
                 0);
    Halt(0);
  end;
  RequireDerivedFormResource := True;
  Application.Initialize;
  frmSplash := TfrmSplash.Create(nil);
  frmSplash.Show;
  frmSplash.Update;
  Application.ProcessMessages;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TdmMain, dmMain);
  frmSplash.Free;
  Application.ShowMainForm := true;
  ShowWindow(TWin32WidgetSet(WidgetSet).AppHandle, SW_RESTORE);
  Application.Run;
end.

