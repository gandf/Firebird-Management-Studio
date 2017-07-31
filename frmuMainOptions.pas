unit frmuMainOptions;

{$mode objfpc}{$H+}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  frmuDlgClass, StdCtrls, Buttons, ActnList, ComCtrls, ExtCtrls, resstring;

type

  { TfrmMainOptions }

  TfrmMainOptions = class(TDialog)
    btOk: TButton;
    btCancel: TButton;
    btApply: TButton;
    btChangeFont: TButton;
    chkForceUpper: TCheckBox;
    ComboBox1: TComboBox;
    FontDialog1: TFontDialog;
    lblLang: TLabel;
    lblFont: TLabel;
    lblTestFont: TLabel;
    PageControl1: TPageControl;
    radgrpDDLTrans: TRadioGroup;
    tabsheetGeneral: TTabSheet;
    procedure btCancelClick(Sender: TObject);
    procedure btOkClick(Sender: TObject);
    Procedure TranslateVisual;override;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMainOptions: TfrmMainOptions;

implementation

{$R *.lfm}

Procedure TfrmMainOptions.TranslateVisual;
Begin
  lblLang.Caption := LZTMainOptionlblLang;
  tabsheetGeneral.Caption := LZTMainOptiontabsheetGeneral;
  radgrpDDLTrans.Caption := LZTMainOptionradgrpDDLTransCaption;
  radgrpDDLTrans.Items.Text := LZTMainOptionradgrpDDLTransItems1 + #13#10 + LZTMainOptionradgrpDDLTransItems2;
  chkForceUpper.Caption := LZTMainOptionchkForceUpper;
  btChangeFont.Caption := LZTMainOptionbtChangeFont;
  lblFont.Caption := LZTMainOptionlblFont;
  lblTestFont.Caption := LZTMainOptionlblTestFont;
  btOk.Caption := LZTMainOptionbtOk;
  btCancel.Caption := LZTMainOptionbtCancel;
  btApply.Caption := LZTMainOptionbtApply;
  Self.Caption := LZTMainOptionFormTitle;
End;

procedure TfrmMainOptions.btOkClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TfrmMainOptions.btCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.

