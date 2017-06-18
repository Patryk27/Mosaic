(* TRemderThread.DrawCandidate *)
{
 Renders candidate Image on given Bitmap canvas.
}
Procedure TRenderThread.DrawCandidate(const Bitmap, Image: TBitmap; const rX, rY: int32);
Var BmpPixel, ImgPixel: Pointer;
    ImgInc, I         : uint8;

    X, Y: uint32;
Begin
 // check pixel format
 ImgInc := uint8(Image.PixelFormat = pf32bit);

 // small synchronization; I'm not quite sure why/how does this happen, but sometimes on more than 2~3 threads, the "ScanLine" returns 'nil' for first few tries.
 I := 0;

 While (Bitmap.ScanLine[0] = nil) Do
 Begin
  Inc(I);

  if (I > 100) Then
   raise EMosaicGenException.Create('Fatal: render thread exceeded time for ScanLine. Try re-running the application.');

  Sleep(1);
 End;

 // do magic!
 For Y := 0 To fSubImgHeight-1 Do
 Begin
  ImgPixel := Image.ScanLine[Y];
  BmpPixel := Pointer(uint32(Bitmap.ScanLine[rY+Y]) + 3*rX);

  For X := 0 To fSubImgWidth-1 Do
  Begin
   For I := 0 To 2 Do
   Begin
    Puint8(BmpPixel)^ := Puint8(ImgPixel)^;

    Inc(BmpPixel);
    Inc(ImgPixel);
   End;

   Inc(ImgPixel, ImgInc);
  End;
 End;
End;

(* TRenderThread.GetAverageColor *)
{
 Returns average RGB color of given chunk.
}
Function TRenderThread.GetAverageColor(const X, Y: int32; pX, pY: int32): TRGB;
Var AvR, AvG, AvB: uint64;
    ImgInc       : uint8;
    Pixel        : Puint8;
    I, J         : int32;
Begin
 AvR := 0;
 AvG := 0;
 AvB := 0;

 // check for out of bounds
 if (X >= fInputWidth) or (Y >= fInputHeight) Then
  Exit;

 if (pX >= fInputWidth) Then
  pX := fInputWidth-1;

 if (pY >= fInputHeight) Then
  pY := fInputHeight-1;

 // check pixel format
 if (fInput.PixelFormat = pf24bit) Then
  ImgInc := 0 Else
  ImgInc := 1;

 // do magic!
 For J := Y To pY Do
 Begin
  Pixel := fInput.ScanLine[J];
  Inc(Pixel, 3*X + ImgInc*X);

  For I := X To pX Do
  Begin
   AvB += Pixel^;
   Inc(Pixel);

   AvG += Pixel^;
   Inc(Pixel);

   AvR += Pixel^;
   Inc(Pixel);

   Inc(Pixel, ImgInc);
  End;
 End;

 I := (pX-X+1) * (pY-Y+1);

 Result.R := AvR div I;
 Result.G := AvG div I;
 Result.B := AvB div I;
End;

(* TRenderThread.Execute *)
Procedure TRenderThread.Execute;
Var Average  : TLab;
    Candidate: TBitmap;

    X, Y: int32;
Begin
 fProcessing := True;

 X := fRenderRect.X;

 While (X < fRenderRect.X+fRenderRect.W) Do
 Begin
  Y := fRenderRect.Y;

  While (Y < fRenderRect.Y+fRenderRect.H) Do
  Begin
   if (Terminated) Then
    Exit;

   // get average color
   Average := RGBtoLab(GetAverageColor(X, Y, X+fSubImgWidth-1, Y+fSubImgHeight-1));

   // choose the best candidate
   Candidate := fGenerator.ChooseBestCandidate(Average);

   // draw it
   DrawCandidate(fOutput, Candidate, X, Y);

   // proceed further
   Inc(Y, fSubImgHeight);
  End;

  Inc(X, fSubImgWidth);
 End;

 fProcessing := False;
End;

(* TRenderThread.Create *)
Constructor TRenderThread.Create(const Generator: TMosaicGen; const RenderRect: TRenderRect; const Input, Output: TBitmap; const CreateSuspended: Boolean);
Begin
 inherited Create(CreateSuspended);

 fGenerator := Generator;

 fRenderRect := RenderRect;
 fInput      := Input;
 fOutput     := Output;

 fInputWidth  := fInput.Width;
 fInputHeight := fInput.Height;

 fSubImgWidth  := Generator.fSubImgWidth;
 fSubImgHeight := Generator.fSubImgHeight;

 fProcessing := True;
End;
