Unit uPreview;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls;

Type
  TfrmPreview = Class(TForm)
    Image: TImage;
    Procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    Procedure ImageClick(Sender: TObject);
    Procedure ImageResize(Sender: TObject);
  Private
    { private declarations }
  Public
   Procedure Run(const Img: TPicture);
  End;

var
  frmPreview: TfrmPreview;
  Picture   : TPicture;

Implementation
Uses LCLType;

{$R *.lfm}

(* TfrmPreview.ImageResize *)
Procedure TfrmPreview.ImageResize(Sender: TObject);
Begin
 ClientHeight := Round(ClientWidth * Picture.Height/Picture.Width);
End;

(* TfrmPreview.FormKeyDown *)
Procedure TfrmPreview.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
Begin
 if (Key = VK_ESCAPE) Then
  Close;
End;

(* TfrmPreview.ImageClick *)
Procedure TfrmPreview.ImageClick(Sender: TObject);
Begin
 Close;
end;

(* TfrmPreview.Run *)
Procedure TfrmPreview.Run(const Img: TPicture);
Begin
 if (Img = nil) or (Img.Width = 0) or (Img.Height = 0) Then
  Exit;

 Picture := Img;

 Image.Picture.Assign(Img);

 ClientWidth  := Img.Width;
 ClientHeight := Img.Height;

 if (ClientWidth >= Screen.Width) Then
 Begin
  ClientWidth  := Screen.Width - 100;
  ClientHeight := Round(ClientWidth * Picture.Height/Picture.Width);
 End;

 if (ClientHeight >= Screen.Height) Then
 Begin
  ClientHeight := Screen.Height;
  ClientWidth  := Round(ClientHeight * Picture.Width/Picture.Height);
 End;

 Top  := 10;
 Left := (Screen.Width - Width) div 2;

 ShowModal;
End;
End.

