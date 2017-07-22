{****************************************************************
*
*  f r m u T e x t V i e w e r
*
****************************************************************
*  Author: The Client Server Factory Inc.
*  Date:   March 1, 1999
*
*  Description:  This unit provides an interface for viewing
*                text files
*
*****************************************************************
* Revisions:
*
*****************************************************************}
unit frmuTextViewer;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Interfaces, Dialogs,
  ComCtrls, ToolWin, StdCtrls, Menus, ImgList, Printers, IBServices, frmuDlgClass,
  RichBox, StdActns, ActnList, FileUtil;

type
  TfrmTextViewer = class(TForm)
    ToolButton5: TToolButton;
    imgToolbarImages: TImageList;
    mnuEdCopy: TMenuItem;
    mnuEdFind: TMenuItem;
    mnuEdN1: TMenuItem;
    mnuEdit: TMenuItem;
    mnuFiExit: TMenuItem;
    mnuFiN1: TMenuItem;
    mnuFiPrint: TMenuItem;
    mnuFiSaveAs: TMenuItem;
    mnuFile: TMenuItem;
    mnuMain: TMainMenu;
    sbCopy: TToolButton;
    sbSaveAs: TToolButton;
    stbStatusBar: TStatusBar;
    tlbStandard: TToolBar;
    reEditor: TlzRichEdit;
    TextViewActions: TActionList;
    EditCopy1: TEditCopy;
    EditCut1: TEditCut;
    EditPaste1: TEditPaste;
    EditSelectAll1: TEditSelectAll;
    EditUndo1: TEditUndo;
    Cut1: TMenuItem;
    Paste1: TMenuItem;
    SelectAll1: TMenuItem;
    Undo1: TMenuItem;
    N1: TMenuItem;
    Font1: TMenuItem;
    EditFont: TAction;
    FontDialog1: TFontDialog;
    FindDialog1: TFindDialog;
    function FormHelp(Command: Word; Data: Integer; var CallHelp: Boolean): Boolean;
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mnuEdFindClick(Sender: TObject);
    procedure mnuFiExitClick(Sender: TObject);
    procedure mnuFiSaveAsClick(Sender: TObject);
    procedure EditFontExecute(Sender: TObject);
    procedure EditCut1Update(Sender: TObject);
    procedure mnuFiPrintClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure reEditorEnter(Sender: TObject);
    procedure reEditorKeyPress(Sender: TObject; var Key: Char);
    procedure EditUndo1Update(Sender: TObject);
    procedure FindDialog1Find(Sender: TObject);
  private
    { Private declarations }
    FFileName: string;

    procedure SetFileName(const sFileName: String);
  public
    { Public declarations }
  published
    function OpenTextViewer(const Service: TIBControlAndQueryService; const sFormCaption: string; const readonly: boolean=true): integer;
    function ShowText (const Data: TStringList; const Title: String; const readonly: boolean=true): integer;
  end;

implementation

uses
  zluGlobal, zluContextHelp, frmuMain, frmuMessage, IB, IBErrorCodes;

{$R *.lfm}

{****************************************************************
*
*  O p e n T e x t V i e w e r ( )
*
****************************************************************
*  Author: The Client Server Factory Inc.
*  Date:   March 1, 1999
*
*  Input:  sInputStr    - a string containing the text source
*                         or a filename
*          sInputSrc    - a char specifying whether the input
*                         source if from memory or from a file
*          sFormCaption - string specifying the name of the form
*
*  Return: Integer - specifies whether task was successful
*
*  Description: Creates an instance of the text viewer and
*               displays the contents of the input string
*               if the source is from memory or opens the file
*               specified by the input string if source
*               is from file.
*
*****************************************************************
* Revisions:
*
*****************************************************************}
function TfrmTextViewer.OpenTextViewer(const Service: TIBControlAndQueryService;
        const sFormCaption: string; const readonly:boolean=true): integer;
begin
  Caption := sFormCaption;           // set caption for form
  reEditor.Lines.Clear;
  reEditor.Readonly := readonly;
  Show;
  while not service.Eof do
  begin
    Application.ProcessMessages;
    reEditor.Lines.Add(service.GetNextLine);
    reEditor.Modified := false;
  end;
  reEditor.SelStart := 0;
  result := SUCCESS;
  frmMain.UpdateWindowList(sFormCaption, TObject(Self));
  reEditor.Modified := false;
  if ReadOnly then
    stbStatusBar.Panels[1].Text := 'Read-Only';
end;

function TfrmTextViewer.FormHelp(Command: Word; Data: Integer;
  var CallHelp: Boolean): Boolean;
begin
  CallHelp := False;
  // call WinHelp and show internal text viewer topic
  //Result := WinHelp(WindowHandle,CONTEXT_HELP_FILE,HELP_CONTEXT,FEATURES_INTERNAL_TEXT_VIEWER);
end;

procedure TfrmTextViewer.FormResize(Sender: TObject);
begin
  reEditorEnter(self);
end;

procedure TfrmTextViewer.FormShow(Sender: TObject);
begin
  reEditorEnter(self);
  reEditor.Modified := false;
end;

procedure TfrmTextViewer.mnuEdFindClick(Sender: TObject);
begin
  FindDialog1.Execute;
end;

procedure TfrmTextViewer.mnuFiExitClick(Sender: TObject);
begin
  Close;                               // close the form
end;

{****************************************************************
*
*  m n u F i S a v e A s C l i c k
*
****************************************************************
*  Author: The Client Server Factory Inc.
*  Date:   March 1, 1999
*
*  Input:  Sender - the object the initiated the event
*
*  Return: None
*
*  Description: Shows the Save Dialog box and allows the user
*               to save the contents of the richedit compoent
*               to a specified file.
*
*****************************************************************
* Revisions:
*
*****************************************************************}
procedure TfrmTextViewer.mnuFiSaveAsClick(Sender: TObject);
var
  loSaveDialog: TSaveDialog;
begin
  loSaveDialog := nil;
  try
  begin
     // create and show save dialog box
    loSaveDialog := TSaveDialog.Create(Self);
    loSaveDialog.Filter := 'Text files (*.txt)|*.TXT|SQL files (*.sql)|*.SQL|All files (*.*)|*.*';

    if loSaveDialog.Execute then
    begin
      // if the specified file already exists the show overwrite message
      // if the user does not wish to overwrite the file then exit
      if FileExistsUTF8(loSaveDialog.FileName) { *Converted from FileExists* } then
        if MessageDlg(Format('OK to overwrite %s', [loSaveDialog.FileName]),
          mtConfirmation, mbYesNoCancel, 0) <> idYes then Exit;

      // if the file doesn't exist of the user wishes to overwrite it then
      // save the contents of the richedit component to the specified file
      reEditor.PlainText := true;
      reEditor.Lines.SaveToFile(loSaveDialog.FileName);
      SetFileName(loSaveDialog.FileName);
      reEditor.PlainText := false;      
      reEditor.Modified := False;      // set modified flag to false
    end;
  end
  finally
    // deallocate memory
    loSaveDialog.free;
  end;
end;

procedure TfrmTextViewer.SetFileName(const sFileName: String);
begin
  FFileName := sFileName;              // set filename
end;

function TfrmTextViewer.ShowText(const Data: TStringList;
  const Title: String; const readonly: boolean = true): integer;
begin
  Caption := Title;
  reEditor.Lines.BeginUpdate;
  reEditor.Lines.Clear;
  reEditor.ReadOnly := readonly;
  reEditor.Lines.AddStrings (Data);
  reEditor.SelStart := 0;
  reEditor.Modified := false;
  reEditor.Lines.EndUpdate;  
  Show;
  frmMain.UpdateWindowList(Title, TObject(Self));
  result := SUCCESS;
  reEditor.Modified := false;
end;

procedure TfrmTextViewer.EditFontExecute(Sender: TObject);
begin
  FontDialog1.Font.Assign(reEditor.SelAttributes);
  if FontDialog1.Execute then
    if reEditor.SelLength > 0 then
      reEditor.SelAttributes.Assign(FontDialog1.Font)
    else
      reEditor.Font.Assign(FontDialog1.Font);
  reEditorEnter(Self);
  reEditor.SetFocus;
end;

procedure TfrmTextViewer.EditCut1Update(Sender: TObject);
begin
  (Sender as TAction).Enabled := (not reEditor.ReadOnly) and (reEditor.SelLength > 0);
end;

procedure TfrmTextViewer.mnuFiPrintClick(Sender: TObject);
var
  lPrintDialog: TCustomPrintDialog;
  lLine: integer;
  lPrintText: TextFile;
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
          AssignPrn(lPrintText);
          Rewrite(lPrintText);
          Printer.Canvas.Font := TlzRichEdit(ActiveControl).Font;
          for lLine := 0 to TlzRichEdit(ActiveControl).Lines.Count - 1 do
            Writeln(lPrintText, TlzRichEdit(ActiveControl).Lines[lLine]);
          CloseFile(lPrintText);
        end
        else
          DisplayMsg (ERR_PRINT,'');
      except on E: Exception do
        DisplayMsg (ERR_PRINT, E.Message);
      end;
    finally
      lPrintDialog.free;
    end;
  end;
  }
end;

procedure TfrmTextViewer.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  frmMain.UpdateWindowList(Self.Caption, TObject(Self), true);
end;

procedure TfrmTextViewer.reEditorEnter(Sender: TObject);
const
  SColRowInfo = '%3d : %3d';
var
  CharPos: TPoint;
begin
  CharPos.Y := SendMessage(reEditor.Handle, EM_EXLINEFROMCHAR, 0,
    reEditor.SelStart);
  CharPos.X := (reEditor.SelStart -
    SendMessage(reEditor.Handle, EM_LINEINDEX, CharPos.Y, 0));
  Inc(CharPos.Y);
  Inc(CharPos.X);
  stbStatusBar.Panels[0].Text := Format(SColRowInfo, [CharPos.Y, CharPos.X]);
end;

procedure TfrmTextViewer.reEditorKeyPress(Sender: TObject; var Key: Char);
begin
  reEditorEnter(Sender);
end;

procedure TfrmTextViewer.EditUndo1Update(Sender: TObject);
begin
  (Sender as TAction).Enabled := reEditor.Modified;
  if reEditor.Modified then
    stbStatusBar.Panels[1].Text := 'Modified';
end;

procedure TfrmTextViewer.FindDialog1Find(Sender: TObject);
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

end.
