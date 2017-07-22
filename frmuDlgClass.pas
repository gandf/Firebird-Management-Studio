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

unit frmuDlgClass;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, gettext, Translations;

type
  TDialog = class(TForm)
    Procedure TranslateVisual;virtual;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FErrorState: boolean;
  public
    { Public declarations }
    function GetErrorState: boolean;
    procedure SetErrorState;
  end;

implementation

{$R *.lfm}

type
  TFontClass = class (TControl);  { needed to get at font property to scale }

Procedure TDialog.TranslateVisual;
Begin

End;

procedure TDialog.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    ModalResult := mrCancel;
end;

procedure TDialog.FormCreate(Sender: TObject);
Var
  lg, language : String;
begin
  FErrorState := false;
  GetLanguageIDs(lg,language);
  Translations.TranslateUnitResourceStrings('resstring', '.\Lang\'+ChangeFileExt(ExtractFileName(Application.ExeName),
  '')+'.%s.po', lg, language);
  TranslateVisual;
end;

function TDialog.GetErrorState: boolean;
begin
  result := FErrorState;
end;

procedure TDialog.SetErrorState;
begin
  FErrorState := true;
end;

end.
