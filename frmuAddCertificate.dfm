object frmAddCertificate: TfrmAddCertificate
  Left = 166
  Top = 317
  HelpContext = 1
  BorderIcons = [biSystemMenu, biHelp]
  BorderStyle = bsSingle
  Caption = 'Add Certificate'
  ClientHeight = 202
  ClientWidth = 341
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  HelpFile = '1'
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnHelp = FormHelp
  PixelsPerInch = 96
  TextHeight = 13
  object lblCertificateKey: TLabel
    Left = 15
    Top = 126
    Width = 71
    Height = 13
    Caption = 'Certificate &Key:'
    FocusControl = edtCertificateKey
  end
  object lblCertificateID: TLabel
    Left = 15
    Top = 89
    Width = 64
    Height = 13
    Caption = 'Certificate &ID:'
    FocusControl = edtCertificateID
  end
  object edtCertificateID: TEdit
    Left = 111
    Top = 89
    Width = 215
    Height = 21
    TabOrder = 0
  end
  object edtCertificateKey: TEdit
    Left = 111
    Top = 126
    Width = 215
    Height = 21
    TabOrder = 1
  end
  object btnOK: TButton
    Left = 140
    Top = 162
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    TabOrder = 2
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 236
    Top = 162
    Width = 75
    Height = 25
    Caption = '&Cancel'
    TabOrder = 3
    OnClick = btnCancelClick
  end
  object memInstructions: TMemo
    Left = 15
    Top = 15
    Width = 311
    Height = 60
    TabStop = False
    BorderStyle = bsNone
    Color = clSilver
    Lines.Strings = (
      'Please type in the Certificate ID and the Certificate'
      'Key exactly as they appear on your license '
      'agreement.')
    ReadOnly = True
    TabOrder = 4
  end
end
