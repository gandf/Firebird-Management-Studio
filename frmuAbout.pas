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

unit frmuAbout;

{$MODE Delphi}

interface

uses LCLIntf, LCLType, Classes, Forms, Controls, StdCtrls, Buttons,
  Windows,
  ExtCtrls, Graphics, frmuDlgClass, FileUtil, SynEdit,
  SYSUtils, fileinfo, winpeimagereader, elfreader, machoreader, resstring;

type

  { TfrmAbout }

  TfrmAbout = class(TDialog)
    Panel1: TPanel;
    Image1: TImage;
    stxAppVersion: TStaticText;
    stxCopyright: TStaticText;
    stxFirebirdVer: TStaticText;
    stxhttpLink: TStaticText;
    stxhttpLink1: TStaticText;
    stxWindowsVersion: TStaticText;
    Button1: TButton;
    stxFbMStudioVer: TStaticText;
    stxInterBase: TStaticText;
    procedure btnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure stxhttpLinkClick(Sender: TObject);
    procedure GetFileVersion(const Filename: String; out CompanyName, FileVersion,
      LegalCopyright,  ProductName, ProductVersion: String);
    Procedure TranslateVisual;override;
    { Private declarations }
  public
    { Public declarations }
  end;

procedure ShowAboutDialog();

implementation

uses zluGlobal;

{$R *.lfm}

const
  BUILDSTR = 'Build %d %s';
  PLATFORM_W9x = 'Windows 9x';
  PLATFORM_NT  = 'Windows NT';

procedure ShowAboutDialog();
var
  frmAbout: TfrmAbout;
  Build: String;
  CompanyName,
  FileVersion,
  LegalCopyright,
  ProductName,
  ProductVersion: String;
  tmpBuffer,
  FPath: string;
  VersionInfo: TOSVersionInfo;
begin
  frmAbout := TfrmAbout.Create(Application.MainForm);
  with frmAbout do
  begin
    { Get OS Version Information }
    VersionInfo.dwOSVersionInfoSize := SizeOf(VersionInfo);
    GetVersionEx(VersionInfo);

    with VersionInfo do
    begin
    if dwPlatformID = VER_PLATFORM_WIN32_NT then
      begin
        build := Format (BUILDSTR, [LoWord(dwBuildNumber), szCSDVersion]);
        stxWindowsVersion.Caption := Format('%s %d.%d (%s)', [PLATFORM_NT, dwMajorVersion, dwMinorVersion, Build]);
      end
      else
        stxWindowsVersion.Caption := Format('%s', [PLATFORM_W9X]);
    end;

    { Get the version information for Firebird Management Studio }
    FPath := Application.ExeName;
    GetFileVersion(FPath, CompanyName, FileVersion, LegalCopyright, ProductName, ProductVersion);
    Caption := LZTAboutFormTitle + ' ' + ProductName + ' ' + FileVersion;
    stxFbMStudioVer.Caption := LZTAboutFbMStudioVer + ' ' + FileVersion;
    stxCopyright.Caption := LegalCopyright;

    //Get file version information for fbclient.dll or GDS32.DLL
    // Get the gds32.dll path
    SetLength(tmpBuffer, MAX_PATH);
    GetSystemDirectory(PChar(tmpBuffer), MAX_PATH);
    tmpBuffer := tmpBuffer.Remove(tmpBuffer.IndexOf(Char(0)));
    FPath := tmpBuffer + '\fbclient.dll';

    // Check to see if it exists
    if FileExists(FPath) then
    begin
      GetFileVersion(FPath, CompanyName, FileVersion, LegalCopyright, ProductName, ProductVersion);
      stxFirebirdVer.Caption := LZTAboutFirebirdVer + ' ' + ProductVersion;
    end
    else
    begin
      FPath := tmpBuffer + '\gds32.dll';
      if FileExists(FPath) then
      begin
        GetFileVersion(FPath, CompanyName, FileVersion, LegalCopyright, ProductName, ProductVersion);
        stxFirebirdVer.Caption := LZTAboutInterbase + ' ' + ProductVersion;
      end;
    end;

    ShowModal;
    Free;
  end;
end;

procedure TfrmAbout.btnOKClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmAbout.FormShow(Sender: TObject);
begin
  inherited;
end;

procedure TfrmAbout.GetFileVersion(const Filename: String; out CompanyName,
  FileVersion, LegalCopyright, ProductName, ProductVersion: String);
  { Helper function to get the actual file version information }
var
  FileVerInfo: TFileVersionInfo;
begin
  FileVerInfo:=TFileVersionInfo.Create(nil);
  FileVerInfo.FileName := Filename;
  try
    FileVerInfo.ReadFileInfo;
    CompanyName := FileVerInfo.VersionStrings.Values['CompanyName'];
    FileVersion := FileVerInfo.VersionStrings.Values['FileVersion'];
    LegalCopyright := FileVerInfo.VersionStrings.Values['LegalCopyright'];
    ProductName := FileVerInfo.VersionStrings.Values['ProductName'];
    ProductVersion := FileVerInfo.VersionStrings.Values['ProductVersion'];
  finally
    FileVerInfo.Free;
  end;
end;

procedure TfrmAbout.stxhttpLinkClick(Sender: TObject);
begin
  inherited;
   OpenDocument(PChar((Sender as TStaticText).Caption));
end;

Procedure TfrmAbout.TranslateVisual;
Begin
  Button1.Caption := LZTAboutButtonOkFiles;
  Self.Caption := LZTToolPropertiesFormTitle;
End;

end.



