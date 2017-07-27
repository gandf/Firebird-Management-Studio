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

unit frmuAddCertificate;

interface

uses
  Windows, Forms, ExtCtrls, StdCtrls, Classes, Controls, Messages, frmuDlgClass,
  lmessages;

type
  TfrmAddCertificate = class(TDialog)
    lblCertificateKey: TLabel;
    lblCertificateID: TLabel;
    edtCertificateID: TEdit;
    edtCertificateKey: TEdit;
    btnOK: TButton;
    btnCancel: TButton;
    memInstructions: TMemo;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    { Private declarations }
    function VerifyInputData(): boolean;
  public
    { Public declarations }
  end;

function AddCertificate(var CertificateID,CertificateKey: string): boolean;

implementation

uses frmuMessage;

{$R *.DFM}

function AddCertificate(var CertificateID,CertificateKey: string): boolean;
var
  frmAddCertificate: TfrmAddCertificate;
begin
  frmAddCertificate := TfrmAddCertificate.Create(Application);
  try
    // display id and key if supplied
    frmAddCertificate.edtCertificateID.Text := CertificateID;
    frmAddCertificate.edtCertificateKey.Text := CertificateKey;
    frmAddCertificate.ShowModal;       // show form as modal dialog box
    if frmAddCertificate.ModalResult = mrOK then
    begin
      // set certificate id and key
      CertificateID := frmAddCertificate.edtCertificateID.Text;
      CertificateKey := frmAddCertificate.edtCertificateKey.Text;
      result := true;
    end
    else
      result := false;
  finally
    // deallocate memory
    frmAddCertificate.Free;
  end;
end;

procedure TfrmAddCertificate.btnCancelClick(Sender: TObject);
begin
  // clear fields and return ModalResult as mrCancel
  edtCertificateID.Text := '';
  edtCertificateKey.Text := '';
  ModalResult := mrCancel;
end;

procedure TfrmAddCertificate.btnOKClick(Sender: TObject);
begin
  // check if all fields have been populated and
  // return ModalResult as mrOK
  if VerifyInputData() then
    ModalResult := mrOK;
end;

function TfrmAddCertificate.VerifyInputData(): boolean;
begin
  result := true;

  // if no certificate id is specified
  if (edtCertificateID.Text = '') or (edtCertificateID.Text = ' ') then
  begin                                // show error message
    DisplayMsg(ERR_INVALID_CERTIFICATE,'');
    edtCertificateID.SetFocus;         // gice focus to this control
    result := false;
    Exit;
  end;

  // if no certificate key is specified
  if (edtCertificateKey.Text = '') or (edtCertificateKey.Text = ' ') then
  begin                                // show error messaage
    DisplayMsg(ERR_INVALID_CERTIFICATE,'');
    edtCertificateKey.SetFocus;        // give focus to this control
    result := false;
    Exit;
  end;
end;

end.
