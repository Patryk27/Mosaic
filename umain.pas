(*
 Mosaic image generator.
 Copyright © by Patryk Wychowaniec, 2014
*)
Unit uMain;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Spin, Buttons, LCLType,
  MosaicGen;

Type
  TfrmMain = Class(TForm)
    Bevel: TBevel;
    btnGenerateMosaic: TBitBtn;
    btnLoadInput: TBitBtn;
    btnSaveOutput: TBitBtn;
    btnChooseSubimageDir: TBitBtn;
    cbCache: TCheckBox;
    cbReusingMode: TCheckBox;
    eSubimageDir: TEdit;
    gbInputImage: TGroupBox;
    gbOutputImage: TGroupBox;
    GroupBox1: TGroupBox;
    gbLog: TGroupBox;
    imgInput: TImage;
    imgOutput: TImage;
    lblAbout: TLabel;
    lblSubimageDir: TLabel;
    lblSubimageWidth: TLabel;
    lblSubimageHeight: TLabel;
    lblThreadCount: TLabel;
    DirDialog: TSelectDirectoryDialog;
    mLog: TMemo;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    seSubimageHeight: TSpinEdit;
    seThreadCount: TSpinEdit;
    seSubimageWidth: TSpinEdit;
    Procedure btnChooseSubimageDirClick(Sender: TObject);
    Procedure btnGenerateMosaicClick(Sender: TObject);
    Procedure btnLoadInputClick(Sender: TObject);
    Procedure btnSaveOutputClick(Sender: TObject);
    Procedure cbCacheChange(Sender: TObject);
    Procedure eSubimageDirChange(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure imgInputClick(Sender: TObject);
    Procedure imgOutputClick(Sender: TObject);
    Procedure seSubimageHeightChange(Sender: TObject);
    Procedure seSubimageWidthChange(Sender: TObject);
  Private
    { private declarations }
  Public
   Procedure SetControls(const State: Boolean);
  End;

Var frmMain  : TfrmMain;
    Generator: TMosaicGen;

    InputImage, OutputImage: TPicture;
    ForceReload            : Boolean = True;

Implementation
Uses uPreview;

{$R *.lfm}

(* Log *)
Procedure Log(const Msg: String);
Begin
 frmMain.mLog.Lines.Add(Msg);
End;

// -------------------------------------------------------------------------- //
(* TfrmMain.btnChooseSubimageDirClick *)
Procedure TfrmMain.btnChooseSubimageDirClick(Sender: TObject);
Begin
 if (DirDialog.Execute) Then
 Begin
  eSubimageDir.Text := DirDialog.FileName;
 End;
End;

(* TfrmMain.btnGenerateMosaicClick *)
Procedure TfrmMain.btnGenerateMosaicClick(Sender: TObject);
Var Rect: TRect;
Begin
 Log('-- begin: '+TimeToStr(Time)+' --');

 if (InputImage = nil) Then
 Begin
  Log('No input image loaded, aborting.');
  Exit;
 End;

 // disable controls
 SetControls(False);

 // create generator, if required
 if (Generator = nil) Then
 Begin
  Generator := TMosaicGen.Create;
  Generator.SetLogProcedure(@Log);
 End;

 // reload options
 Generator.SetThreadCount(seThreadCount.Value);
 Generator.SetSubImageSize(seSubimageWidth.Value, seSubimageHeight.Value);
 Generator.SetReusingMode(cbReusingMode.Checked);
 Generator.SetCachingMode(cbCache.Checked);

 if (ForceReload) Then
 Begin
  if (not Generator.GenerateImageIndex(eSubimageDir.Text)) Then
  Begin
   SetControls(True);
   Exit;
  End;

  ForceReload := False;
 End;

 // generate mosaic
 OutputImage := Generator.GenerateMosaic(InputImage.Bitmap);

 Rect.Left   := 0;
 Rect.Top    := 0;
 Rect.Right  := imgOutput.Width;
 Rect.Bottom := imgOutput.Height;

 imgOutput.Picture.Bitmap.Canvas.StretchDraw(Rect, OutputImage.Bitmap);

 // enable controls
 SetControls(True);

 Log('-- end: '+TimeToStr(Time)+' --');
 Log('');
End;

(* TfrmMain.btnLoadInputClick *)
Procedure TfrmMain.btnLoadInputClick(Sender: TObject);
Var Rect: TRect;
Begin
 if (OpenDialog.Execute) Then
 Begin
  Try
   InputImage := TPicture.Create;
   InputImage.LoadFromFile(OpenDialog.FileName);

   Rect.Left   := 0;
   Rect.Top    := 0;
   Rect.Right  := imgInput.Width;
   Rect.Bottom := imgInput.Height;

   imgInput.Picture.Bitmap.Canvas.StretchDraw(Rect, InputImage.Bitmap);
  Except
   InputImage.Free;

   Application.MessageBox('Couldn''t load selected file!', 'Error', MB_ICONERROR);
  End;
 End;
End;

(* TfrmMain.btnSaveOutputClick *)
Procedure TfrmMain.btnSaveOutputClick(Sender: TObject);
Begin
 SaveDialog.Filter   := OpenDialog.Filter;
 SaveDialog.FileName := ExtractFileName(OpenDialog.FileName);

 if (SaveDialog.Execute) Then
 Begin
  OutputImage.SaveToFile(SaveDialog.FileName);
 End;
End;

(* TfrmMain.cbCacheChange *)
Procedure TfrmMain.cbCacheChange(Sender: TObject);
Begin
 ForceReload := True;
End;

(* TfrmMain.eSubimageDirChange *)
Procedure TfrmMain.eSubimageDirChange(Sender: TObject);
Begin
 ForceReload := True;
End;

(* TfrmMain.FormCreate *)
Procedure TfrmMain.FormCreate(Sender: TObject);
Begin
 Application.Title := Caption;

 imgInput.Picture.Bitmap.SetSize(imgInput.Width, imgInput.Height);
 imgOutput.Picture.Bitmap.SetSize(imgOutput.Width, imgOutput.Height);
End;

(* TfrmMain.imgInputClick *)
Procedure TfrmMain.imgInputClick(Sender: TObject);
Begin
 frmPreview.Run(InputImage);
End;

(* TfrmMain.imgOutputClick *)
Procedure TfrmMain.imgOutputClick(Sender: TObject);
Begin
 frmPreview.Run(OutputImage);
End;

(* TfrmMain.seSubimageHeightChange *)
Procedure TfrmMain.seSubimageHeightChange(Sender: TObject);
Begin
 ForceReload := True;
End;

(* TfrmMain.seSubimageWidthChange *)
Procedure TfrmMain.seSubimageWidthChange(Sender: TObject);
Begin
 ForceReload := True;
End;

(* TfrmMain.SetControls *)
{
 Enables/disables controls.
}
Procedure TfrmMain.SetControls(const State: Boolean);
Var I: Integer;
Begin
 For I := 0 To ControlCount-1 Do
 Begin
  if (Controls[I] is TLabel) Then
   Continue;

  Controls[I].Enabled := State;
 End;
End;
End.

