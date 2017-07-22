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
 * Contributor(s): Krzysztof Golko
}

unit frmuServerLogin;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, SysUtils, Forms, ExtCtrls, StdCtrls, Classes, Controls,
  Messages, zluibcClasses, IB, frmuDlgClass;

type
  TfrmServerLogin = class(TDialog)
    lblServerName: TLabel;
    stxServerName: TStaticText;
    lblUsername: TLabel;
    edtUsername: TEdit;
    lblPassword: TLabel;
    edtPassword: TEdit;
    btnLogin: TButton;
    btnCancel: TButton;
    bvlLine1: TBevel;
    function FormHelp(Command: Word; Data: Integer;var CallHelp: Boolean): Boolean;
    procedure FormShow(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnLoginClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    function VerifyInputData(): boolean;
    class function DoLogin(var CurrSelServer: TibcServerNode): boolean;
//    procedure LMLButtonDown( var Message: TLMLButtonDown ); message WM_NCLBUTTONDOWN ;
  public
    { Public declarations }
  end;

function ServerLogin(var CurrSelServer: TibcServerNode; const SilentLogin: boolean): boolean;

implementation

uses zluGlobal, frmuMessage, zluContextHelp, IBErrorCodes;

{$R *.lfm}

function TfrmServerLogin.FormHelp(Command: Word; Data: Integer;
  var CallHelp: Boolean): Boolean;
begin
  CallHelp := False;
//  Result := WinHelp(WindowHandle,CONTEXT_HELP_FILE,HELP_CONTEXT,SERVER_LOGIN);
end;

class function TfrmServerLogin.DoLogin(var CurrSelServer: TibcServerNode): boolean;
begin
  // if a silent login is specified or the user has successfully entered the
  // proper login details
  try
    Application.ProcessMessages;

    Screen.Cursor := crHourGlass;      // change cursor to hourglass
    // submit server login parameters and attach to server
    CurrSelServer.Version := 6;
    CurrSelServer.Server.Params.Add(Format('isc_spb_user_name=%s',[CurrSelServer.UserName]));
    CurrSelServer.Server.Params.Add(Format('isc_spb_password=%s',[CurrSelServer.Password]));
    CurrSelServer.Server.Attach();
    Screen.Cursor := crDefault;        // change cursor to default
    // if the server successfully attached
    if CurrSelServer.Server.Active = true then
      result := true                   // set result to true
    else
      result := false;
  except                               // otherwise set result to false
    on E : EIBError do                 // if an exception occurs then trap it
    begin                              // and show error message
      Screen.Cursor := crDefault;      // change cursor to default
      case E.IBErrorCode of
        isc_svcnotdef:
          raise Exception.Create ('Firebird Management Studio can not be used to administer pre-InterBase 6.0 servers');
        else
          DisplayMsg (ERR_SERVER_LOGIN, E.Message);
      end;
      result := false;
    end;
  end;
end;

function ServerLogin(var CurrSelServer: TibcServerNode; const SilentLogin: boolean): boolean;
var
  frmServerLogin: TfrmServerLogin;
begin
  // check if a silent login was specified
  if not SilentLogin then
  begin
    // if no silent login was specified
    frmServerLogin:= TfrmServerLogin.Create(Application.MainForm);
    try
      // set server and user name
      frmServerLogin.stxServerName.Caption := CurrSelServer.NodeName;
      frmServerLogin.edtUsername.Text := CurrSelServer.UserName;
      // show form as modal dialog box
      Result := FALSE;
      while frmServerLogin.ShowModal = mrOk do
      begin
        CurrSelServer.UserName := frmServerLogin.edtUsername.Text;
        CurrSelServer.Password := frmServerLogin.edtPassword.Text;
        if TfrmServerLogin.DoLogin(CurrSelServer) then
        begin
          Result := TRUE;
          Break;
        end;
      end;
    finally
      // deallocate memory
      frmServerLogin.Release;
    end;
  end
  else
    Result := TfrmServerLogin.DoLogin(CurrSelServer);
end;

procedure TfrmServerLogin.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrmServerLogin.btnLoginClick(Sender: TObject);
begin
  if VerifyInputData() then
    ModalResult := mrOK;
end;

{ Performs some basic validation on data entered by the user }
function TfrmServerLogin.VerifyInputData(): boolean;
begin
  result := true;

  // if no username is supplied
  if (edtUsername.Text = '') or (edtUsername.Text = ' ') then
  begin
    DisplayMsg(ERR_USERNAME,'');       // display an error message
    edtUsername.SetFocus;              // give focus to control
    result := false;
    Exit;
  end;

  // if no password is supplied
  if (edtPassword.Text = '') or (edtPassword.Text = ' ') then
  begin
    DisplayMsg(ERR_PASSWORD,'');       // display an error message
    edtPassword.SetFocus;              // give focus to control
    result := false;
    Exit;
  end;
end;

procedure TfrmServerLogin.FormShow(Sender: TObject);
begin
  if edtUserName.Text = '' then
    edtUserName.SetFocus
  else {if edtPassword.Text = '' then }
  begin
    edtPassword.SelectAll;
    edtPassword.SetFocus;
  end;
end;

{procedure TfrmServerLogin.LMLButtonDown( var Message: TLMLButtonDown );
var
  ScreenPt: TPoint;
  ClientPt: TPoint;
begin
  ScreenPt.X := Message.XPos;
  ScreenPt.Y := Message.YPos;
  ClientPt := ScreenToClient( ScreenPt );
  if( ClientPt.X > Width-45 )and (ClientPt.X < Width-29) then
   begin
    //WinHelp(WindowHandle,CONTEXT_HELP_FILE,HELP_CONTEXT,SERVER_LOGIN);
    Message.Result := 0;
  end else
   inherited;
end;}

procedure TfrmServerLogin.FormDestroy(Sender: TObject);
begin
  inherited;
  edtPassword.Clear;
end;

end.
