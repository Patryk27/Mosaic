(* TLoadThread.GetStreamHash *)
{
 Returns MD5 hash of given stream.
}
Class Function TLoadThread.GetStreamHash(const Stream: TStream): String;
Const BufferSize = 32 * 1024;
Var Context: TMDContext;
    Digest : TMDDigest;
    Buffer : PChar;
    Count  : uint32;
Begin
 Stream.Position := 0;
 MDInit(Context, MD_VERSION_5);

 Buffer := GetMem(BufferSize);

 While (True) Do
 Begin
  Count := Stream.Read(Buffer[0], BufferSize);

  if (Count > 0) Then
   MDUpdate(Context, Buffer[0], Count) Else
   Break;
 End;

 FreeMem(Buffer);

 MDFinal(Context, Digest);
 Result := MDPrint(Digest);
End;

(* TLoadThread.Execute *)
Procedure TLoadThread.Execute;

  { ScaleBitmap }
  Procedure ScaleBitmap(const Bmp: TBitmap);
  Var Rect: TRect;
      W, H: uint8;
      Tmp : TBitmap;
  Begin
   W := fGenerator.fSubImgWidth;
   H := fGenerator.fSubImgHeight;

   // create TRect
   Rect.Left := 0;
   Rect.Top  := 0;

   Rect.Right  := W;
   Rect.Bottom := H;

   // resize
   Tmp := TBitmap.Create;
   Tmp.Assign(Bmp);

   Bmp.SetSize(W, H);
   Bmp.Canvas.StretchDraw(Rect, Tmp);

   Tmp.Free;
  End;

Var FileName, FileHash: String;
    FileID            : int32;

    FileStream: TFileStream;

    Data: PImageData;
    Img : TPicture;
Begin
 fProcessing := True;

 fImageList.Clear;

 For FileID := 0 To fFileList.Count-1 Do
 Begin
  if (Terminated) Then
   Exit;

  // update status
  fFileID := FileID;

  // get file name
  FileName := fFileList[FileID];

  // try to load image
  Try
   Img := TPicture.Create;

   FileStream := TFileStream.Create(FileName, fmOpenRead);
   FileHash   := GetStreamHash(FileStream);

   // check for duplicates
   if (FindImageByHash(FileHash) = 0) Then
   Begin
    FileStream.Position := 0;
    Img.LoadFromStream(FileStream);
   End Else
   Begin
    FreeAndNil(Img);
   End;

   FileStream.Free;
  Except
   Img.Free;
   Continue;
  End;

  if (Img = nil) Then
   Continue;

  // create structure
  New(Data);
  Data^.Picture   := Img;
  Data^.CanBeUsed := True;
  Data^.Hash      := Hash;

  // scale it
  ScaleBitmap(Img.Bitmap);

  // save to cache
  if (Length(FileHash) > 0) Then
   Img.SaveToFile('cache/'+FileHash, 'bmp');

  // add to the list and proceed further
  fImageList.Add(Data);
 End;

 fProcessing := False;
End;

(* TLoadThread.Create *)
Constructor TLoadThread.Create(const Generator: TMosaicGen; const FileList: TFileList; const CreateSuspended: Boolean);
Begin
 inherited Create(CreateSuspended);

 fGenerator := Generator;
 fFileList  := FileList;
 fImageList := TImageList.Create;

 fProcessing := True;
End;
