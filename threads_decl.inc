{ TLoadThread }
Type TLoadThread =
     Class(TThread)
      Private
       fFileID: uint32;

       fGenerator: TMosaicGen;
       fFileList : TFileList;
       fImageList: TImageList;

       fProcessing: Boolean;

      Private
       Class Function GetStreamHash(const Stream: TStream): String;

      Protected
       Procedure Execute; override;

      Public
       Constructor Create(const Generator: TMosaicGen; const FileList: TFileList; const CreateSuspended: Boolean);

      Public
       Property getImageList: TImageList read fImageList;
       Property isProcessing: Boolean read fProcessing;
       Property getFileID: uint32 read fFileID;
      End;

{ TRenderThread }
Type TRenderThread =
     Class(TThread)
      Private
       fGenerator: TMosaicGen;

       fRenderRect    : TRenderRect;
       fInput, fOutput: TBitmap;

       fInputWidth, fInputHeight  : int32;
       fSubImgWidth, fSubImgHeight: uint8;

       fProcessing: Boolean;

      Private
       Procedure DrawCandidate(const Bitmap, Image: TBitmap; const rX, rY: int32);

       Function GetAverageColor(const X, Y: int32; pX, pY: int32): TRGB;

      Protected
       Procedure Execute; override;

      Public
       Constructor Create(const Generator: TMosaicGen; const RenderRect: TRenderRect; const Input, Output: TBitmap; const CreateSuspended: Boolean);

      Public
       Property isProcessing: Boolean read fProcessing;
      End;
