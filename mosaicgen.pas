(*
 Mosaic image generator.
 Copyright © by Patryk Wychowaniec, 2014
*)
Unit MosaicGen;

 Interface
 Uses FGL, Classes, Graphics, GraphType, Interfaces, SysUtils, Math, MD5;

 Const Version = 0.3;

 { EMosaicGenException }
 Type EMosaicGenException = Class(Exception);

 { TRGB }
 Type TRGB =
      Record
       R, G, B: uint8;
      End;

 { TXYZ }
 Type TXYZ =
      Record
       X, Y, Z: Single;
      End;

 { TLab }
 Type TLab =
      Record
       L, a, b: Single;
      End;

 { TImageData }
 Type PImageData = ^TImageData;
      TImageData =
      Record
       Picture: TPicture;

       AverageRGB: TRGB;
       AverageLab: TLab;

       CanBeUsed: Boolean;

       Hash: String;
      End;

 { TImageList }
 Type TImageList = specialize TFPGList<PImageData>;

 { TFileList }
 Type TFileList = specialize TFPGList<String>;

 { TLogVerbosity }
 Type TLogVerbosity = (lvInfo, lvWarn, lvError);

 { TLogProcedure }
 Type TLogProcedure = Procedure(const Msg: String);

 { TMosaicGen }
 Type TMosaicGen =
      Class sealed
       Private
        fSubImgWidth, fSubImgHeight: uint8;

        fImageList: TImageList;

        // settings
        fThreadCount  : uint8;
        fEnableCaching: Boolean;
        fEnableReusing: Boolean;

        // logging
        fLogProcedure: TLogProcedure;

       Private
        Procedure Log(const Verbosity: TLogVerbosity; const Msg: String; const Args: Array of Const);
        Procedure Log(const Verbosity: TLogVerbosity; const Msg: String);

        Function FindImageByHash(const Hash: String): Boolean;

        Procedure GenerateColorList;
        Function ChooseBestCandidate(const Color: TLab; const Depth: uint8=1): TBitmap;

       Public
        Constructor Create;
        Destructor Destroy; override;

        Procedure SetSubImageSize(const Width, Height: uint8);
        Procedure SetThreadCount(const Value: uint8);
        Procedure SetCachingMode(const Value: Boolean);
        Procedure SetReusingMode(const Value: Boolean);
        Procedure SetLogProcedure(const Value: TLogProcedure);

        Function GenerateImageIndex(const Directory: String): Boolean;

        Procedure GenerateMosaic(const InputImage, OutputImage: String);
        Function GenerateMosaic(const Input: TBitmap): TPicture;

        Procedure FreeResources;
       End;

 Implementation

{ TRenderRect }
Type TRenderRect =
     Record
      X, Y, W, H: int32;
     End;

{$I threads_decl.inc}

// -------------------------------------------------------------------------- //
(* CreateRGB *)
Function CreateRGB(const Color: uint32): TRGB;
Begin
 Result.R := Red(Color);
 Result.G := Green(Color);
 Result.B := Blue(Color);
End;

(* CreateRGB *)
Function CreateRGB(const R, G, B: uint8): TRGB;
Begin
 Result.R := R;
 Result.G := G;
 Result.B := B;
End;

(* RGBtoXYZ *)
Function RGBtoXYZ(const RGB: TRGB): TXYZ;

  { Adjust }
  Procedure Adjust(var Value: Single);
  Begin
   if (Value > 0.04045) Then
   Begin
    Value := (Value + 0.055) / 1.055;
    Value := Power(Value, 2.4);
   End Else
   Begin
    Value := Value / 12.92;
   End;
  End;

Var R, G, B: Single;
Begin
 R := RGB.R / 255;
 G := RGB.G / 255;
 B := RGB.B / 255;

 Adjust(R);
 Adjust(G);
 Adjust(B);

 R *= 100;
 G *= 100;
 B *= 100;

 Result.X := R*0.4124 + G*0.3576 + B*0.1805;
 Result.Y := R*0.2126 + G*0.7152 + B*0.0722;
 Result.Z := R*0.0193 + G*0.1192 + B*0.9505;
End;

(* XYZtoLab *)
Function XYZtoLab(const XYZ: TXYZ): TLab;

  { Adjust }
  Procedure Adjust(var Value: Single);
  Begin
   if (Value > 0.008856) Then
   Begin
    Value := Power(Value, 1/3);
   End Else
   Begin
    Value := 7.787*Value + 16/116;
   End;
  End;

Var X, Y, Z: Single;
Begin
 X := XYZ.X / 95.047;
 Y := XYZ.Y / 100;
 Z := XYZ.Z / 108.883;

 Adjust(X);
 Adjust(Y);
 Adjust(Z);

 Result.L := 116*Y - 16;
 Result.A := 500*(X-Y);
 Result.B := 200*(Y-Z);
End;

(* RGBtoLab *)
Function RGBtoLab(const RGB: TRGB): TLab; inline;
Begin
 Result := XYZtoLab(RGBtoXYZ(RGB));
End;

(* GetColorDistance *)
Function GetColorDistance(const A, B: TLab): Single;
Begin
 Result := sqrt
           (
            sqr(A.L - B.L) +
            sqr(A.a - B.a) +
            sqr(A.b - B.b)
           );
End;

(* RoundUp *)
{
 Rounds given number up to a multiply of another number.
}
Function RoundUp(const Num, Multiple: uint32): uint32;
Var Rem: uint32;
Begin
 if (Multiple = 0) Then
  Exit(Num);

 Rem := Num mod Multiple;

 if (Rem = 0) Then
  Exit(Num);

 Result := Num + Multiple - Rem;
End;

// -------------------------------------------------------------------------- //
{$I loadthread.inc}
{$I renderthread.inc}

// -------------------------------------------------------------------------- //
(* TMosaicGen.Log *)
Procedure TMosaicGen.Log(const Verbosity: TLogVerbosity; const Msg: String; const Args: Array of const);
Const VerbosityString: Array[TLogVerbosity] of String = ('info', 'warning', 'error');
Begin
 if (fLogProcedure <> nil) Then
  fLogProcedure(Format('[%s] %s', [VerbosityString[Verbosity], Format(Msg, Args)]));
End;

(* TMosaicGen.Log *)
Procedure TMosaicGen.Log(const Verbosity: TLogVerbosity; const Msg: String);
Begin
 Log(Verbosity, Msg, []);
End;

(* TMosaicGen.FindImageByHash *)
{
 Returns 'true' if image with given hash is on the list.
}
Function TMosaicGen.FindImageByHash(const Hash: String): Boolean;
Var Thread: TLoadThread;
Begin
 For Thread in LoadThreadList Do
 Begin
  For Result := 0 To
 End;
End;

(* TMosaicGen.GenerateColorList *)
{
 Generates color list containing average colors of the mosaic compound images.
}
Procedure TMosaicGen.GenerateColorList;
Var Data: PImageData;
    Img : TPicture;
    Bmp : TBitmap;

    X, Y : uint32;
    Pixel: Puint8;

    is32bit: Boolean;

    AverageR, AverageG, AverageB: uint64;
    AvR, AvG, AvB               : uint64;
Begin
 Log(lvInfo, 'Generating color list...');

 For Data in fImageList Do
 Begin
  Img := Data^.Picture;
  Bmp := Img.Bitmap;

  AverageR := 0;
  AverageG := 0;
  AverageB := 0;

  is32bit := (Bmp.PixelFormat = pf32bit);

  For Y := 0 To fSubImgHeight-1 Do
  Begin
   Pixel := Bmp.ScanLine[Y];
   AvR   := 0;
   AvG   := 0;
   AvB   := 0;

   For X := 0 To fSubImgWidth-1 Do
   Begin
    AvB += Pixel^;
    Inc(Pixel);

    AvG += Pixel^;
    Inc(Pixel);

    AvR += Pixel^;
    Inc(Pixel);

    if (is32bit) Then
     Inc(Pixel);
   End;

   AverageR += AvR div fSubImgWidth;
   AverageG += AvG div fSubImgWidth;
   AverageB += AvB div fSubImgWidth;
  End;

  AverageR := AverageR div fSubImgHeight;
  AverageG := AverageG div fSubImgHeight;
  AverageB := AverageB div fSubImgHeight;

  // small check
  if (AverageR > 255) or (AverageG > 255) or (AverageB > 255) Then
   raise Exception.CreateFmt('Invalid average RGB image value has been computed! (%d, %d, %d)', [AverageR, AverageG, AverageB]);

  // update data
  Data^.AverageRGB := CreateRGB(AverageR, AverageG, AverageB);
  Data^.AverageLab := RGBtoLAB(Data^.AverageRGB);
 End;
End;

(* TMosaicGen.ChooseBestCandidate *)
{
 Choosee best candidate of all the mosaic images.
}
Function TMosaicGen.ChooseBestCandidate(const Color: TLab; const Depth: uint8): TBitmap;
Var CurrentDistance, PreviousDistance: Single;
    I, ResultID: int32;
Begin
 Result           := nil;
 ResultID         := 0;
 PreviousDistance := 100000000;

 For I := 0 To fImageList.Count-1 Do
 Begin
  if (not fImageList[I]^.CanBeUsed) Then
   Continue;

  CurrentDistance := GetColorDistance(fImageList[I]^.AverageLab, Color);

  if (CurrentDistance < PreviousDistance) and
     (CurrentDistance < 40+2*Depth) Then
  Begin
   PreviousDistance := CurrentDistance;
   Result           := fImageList[I]^.Picture.Bitmap;
   ResultID         := I;
  End;
 End;

 if (Result = nil) Then
 Begin
  For I := 0 To fImageList.Count-1 Do
   fImageList[I]^.CanBeUsed := True;

  Result := ChooseBestCandidate(Color, Depth+1);
 End Else
 Begin
  fImageList[ResultID]^.CanBeUsed := fEnableReusing;
 End;
End;

(* TMosaicGen.Create *)
Constructor TMosaicGen.Create;
Begin
 fImageList := TImageList.Create;

 SetSubImageSize(10, 10);
 SetThreadCount(4);
 SetCachingMode(True);
 SetReusingMode(True);
 SetLogProcedure(nil);
End;

(* TMosaicGen.Destroy *)
Destructor TMosaicGen.Destroy;
Begin
 Log(lvInfo, 'TMosaicGen.Destroy()');

 FreeResources;
 fImageList.Free;

 inherited Destroy;
End;

(* TMosaicGen.SetSubImageSize *)
{
 Changes sub-images (images of which the mosaic is generated) desired size.
}
Procedure TMosaicGen.SetSubImageSize(const Width, Height: uint8);
Begin
 if (Width = 0) or (Height = 0) Then
  raise EMosaicGenException.Create('Invalid SetSubImageSize() arguments!');

 fSubImgWidth  := Width;
 fSubImgHeight := Height;
End;

(* TMosaicGen.SetThreadCount *)
{
 Changes number of threads used by the mosaic generator.
}
Procedure TMosaicGen.SetThreadCount(const Value: uint8);
Begin
 if (Value = 0) Then
  raise EMosaicGenException.Create('At least one thread must be created!');

 fThreadCount := Value;
End;

(* TMosaicGen.SetCachingMode *)
{
 Disables/enables image caching.
}
Procedure TMosaicGen.SetCachingMode(const Value: Boolean);
Begin
 fEnableCaching := Value;
End;

(* TMosaicGen.SetReusingMode *)
{
 Enables/disables subimage reusing.
}
Procedure TMosaicGen.SetReusingMode(const Value: Boolean);
Begin
 fEnableReusing := Value;
End;

(* TMosaicGen.SetLogProcedure *)
Procedure TMosaicGen.SetLogProcedure(const Value: TLogProcedure);
Begin
 fLogProcedure := Value;
End;

(* TMosaicGen.GenerateImageIndex *)
{
 Creates image index (prepares the mosaic generator for generating an image)
}
Function TMosaicGen.GenerateImageIndex(const Directory: String): Boolean;
Var FileCount, FileProcessed, Iteration: uint32;

    FileList  : Array of TFileList;
    ThreadList: Array of TLoadThread;

    I, J, ListID: int32;

    Done: Boolean;

    M: TSearchRec;
Begin
 // check if resources doesn't have to be freed before (so that we don't have a memleak)
 if (fImageList.Count > 0) Then
 Begin
  FreeResources;
 End;

 // create cache directory, if required
 if (fEnableCaching) and (not DirectoryExists('cache')) Then
 Begin
  mkdir('cache');
 End;

 // allocate arrays
 SetLength(FileList, fThreadCount);
 SetLength(ThreadList, fThreadCount);

 // create lists and Threads
 For I := 0 To fThreadCount-1 Do
 Begin
  FileList[I]   := TFileList.Create;
  ThreadList[I] := TLoadThread.Create(self, FileList[I], True);
 End;

 // clear list
 fImageList.Clear;

 // log
 Log(lvInfo, 'Generating image index for directory: %s', [Directory]);

 // do actual searching
 FindFirst(Directory+DirectorySeparator+'*.*', faAnyFile, M);

 FileCount := 0;
 ListID    := 0;

 While (FindNext(M) = 0) Do
 Begin
  Case LowerCase(ExtractFileExt(M.Name)) of
   '.bmp', '.jpg', '.png':
   Begin
    FileList[ListID].Add(Directory+DirectorySeparator+M.Name);

    Inc(ListID);
    ListID := ListID mod fThreadCount;

    Inc(FileCount);
   End;
  End;
 End;

 FindClose(M);

 // log
 Log(lvInfo, 'Got to check %d files...', [FileCount]);

 // run load threads
 For I := 0 To fThreadCount-1 Do
  ThreadList[I].Resume;

 // wait for threads to finish
 Iteration := 0;

 Repeat
  Done := True;

  if (Iteration > 0) and (Iteration mod 100 = 0) Then
  Begin
   FileProcessed := 0;

   For I := 0 To fThreadCount-1 Do
    Inc(FileProcessed, ThreadList[I].getFileID);

   Log(lvInfo, 'Processed %d%% (total %d of %d files)', [100*FileProcessed div FileCount, FileProcessed, FileCount]);
  End;

  For I := 0 To fThreadCount-1 Do
   Done := Done and (not ThreadList[I].isProcessing);

  Sleep(10);
  Inc(Iteration);
 Until (Done);

 // concatenate lists
 For I := 0 To fThreadCount-1 Do
 Begin
  For J := 0 To ThreadList[I].getImageList.Count-1 Do
  Begin
   fImageList.Add(ThreadList[I].getImageList[J]);
  End;
 End;

 // clean
 For I := 0 To fThreadCount-1 Do
 Begin
  FileList[I].Free;
  ThreadList[I].Free;
 End;

 // log
 Log(lvInfo, 'Loaded %d images.', [fImageList.Count]);

 Result := fImageList.Count > 0;

 if (Result) Then
 Begin
  GenerateColorList;
 End;
End;

(* TMosaicGen.GenerateMosaic *)
Procedure TMosaicGen.GenerateMosaic(const InputImage, OutputImage: String);
Var Input, Output: TPicture;
Begin
 Log(lvInfo, 'Generating image from file %s...', [InputImage]);

 Try
  // load input file
  Input := TPicture.Create;
  Input.LoadFromFile(InputImage);

  // generate mosaic
  Output := GenerateMosaic(Input.Bitmap);

  // save generated image
  Log(lvInfo, 'Saving generated mosaic to: %s', [OutputImage]);
  Output.SaveToFile(OutputImage);
 Finally
  // release resources
  Input.Free;
  Output.Free;
 End;
End;

(* TMosaicGen.GenerateMosaic *)
Function TMosaicGen.GenerateMosaic(const Input: TBitmap): TPicture;
Var FatalException: TObject = nil;
    ExceptionMsg  : String = '';

    ThreadList: Array of TRenderThread;
    ThreadID  : uint8;

    RenderRect: TRenderRect;

    Done: Boolean;
Begin
 Log(lvInfo, 'Rendering mosaic...');

 // create output bitmap
 Result                    := TPicture.Create;
 Result.Bitmap.PixelFormat := pf24bit;

 // resize it
 Result.Bitmap.SetSize(RoundUp(Input.Width, fSubImgWidth), RoundUp(Input.Height, fSubImgHeight));
 Result.Bitmap.FreeImage;

 Try
  // ensure input image pixel format is either 24 or 32-bit
  if (not (Input.PixelFormat in [pf24bit, pf32bit])) Then
   raise EMosaicGenException.Create('Invalid input image pixel format!');

  // create worker threads
  RenderRect.X := 0;
  RenderRect.Y := 0;
  RenderRect.W := RoundUp(Input.Width div fThreadCount, fSubImgWidth);
  RenderRect.H := Input.Height;

  SetLength(ThreadList, fThreadCount);

  For ThreadID := 0 To High(ThreadList) Do
  Begin
   ThreadList[ThreadID] := TRenderThread.Create(self, RenderRect, Input, Result.Bitmap, True);

   Inc(RenderRect.X, RenderRect.W);
  End;

  // run threads
  For ThreadID := 0 To High(ThreadList) Do
   ThreadList[ThreadID].Resume;

  // wait for threads to finish
  Repeat
   Done := True;

   // iterate each thread
   For ThreadID := 0 To High(ThreadList) Do
   Begin
    Done := Done and (not ThreadList[ThreadID].isProcessing);

    if (ThreadList[ThreadID].FatalException <> nil) Then
     FatalException := ThreadList[ThreadID].FatalException;
   End;

   // if some thread raised an exception
   if (FatalException <> nil) Then
   Begin
    ExceptionMsg := Exception(FatalException).Message;

    Log(lvError, 'Some render thread reported an exception, aborting!');

    For ThreadID := 0 To High(ThreadList) Do
     ThreadList[ThreadID].Suspend;

    Done := True;
   End;

   // sleep
   Sleep(1);
  Until (Done);
 Finally
  For ThreadID := 0 To High(ThreadList) Do
  Begin
   ThreadList[ThreadID].Terminate;
   ThreadList[ThreadID].Free;
  End;
 End;

 // re-raise exception, if required
 if (Length(ExceptionMsg) > 0) Then
 Begin
  Result.Free;
  raise EMosaicGenException.Create(ExceptionMsg);
 End;
End;

(* TMosaicGen.FreeResources *)
Procedure TMosaicGen.FreeResources;
Var Data: PImageData;
Begin
 Log(lvInfo, 'Freeing resources...');

 For Data in fImageList Do
 Begin
  Data^.Picture.Free;
  FreeMem(Data);
 End;
End;
End.
