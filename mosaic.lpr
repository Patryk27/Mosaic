{
 Mosaic image generator.
 Copyright © by Patryk Wychowaniec, 2014

 Thanks to: http://www.emanueleferonato.com/2009/08/28/color-differences-algorithm/
}
Program Mosaic;
Uses Windows, SysUtils, FileUtil, MosaicGen;
Var FileList: TFileList;

    Wait  : Boolean;
    Stop  : Boolean;
    Output: String;

    Time: uint32;
    Gen : TMosaicGen;

(* ParseCommandLine *)
Procedure ParseCommandLine;
Var Str : String;
    W, H: uint8;

    I: int8;

  { GetBoolean }
  Function GetBoolean: Boolean;
  Begin
   Inc(I);

   Case ParamStr(I) of
    '1', 'true': Exit(True);
   End;

   Exit(False);
  End;

Begin
 I := 1;

 While (I <= ParamCount) Do
 Begin
  Case ParamStr(I) of
   // -output
   '-output':
   Begin
    Inc(I);

    Output := ParamStr(I);
   End;

   // -size
   '-size':
   Begin
    Inc(I);

    Str := ParamStr(I);

    W := StrToInt(Copy(Str, 1, Pos('x', Str)-1));
    H := StrToInt(Copy(Str, Pos('x', Str)+1, Length(Str)));

    Gen.SetSubImageSize(W, H);
   End;

   // -wait
   '-wait':
   Begin
    Wait := True;
   End;

   // -threads
   '-threads':
   Begin
    Inc(I);

    Gen.SetThreadCount(StrToInt(ParamStr(I)));
   End;

   // -cache
   '-cache':
   Begin
    Gen.SetCachingMode(GetBoolean);
   End;

   // -reuse
   '-reuse':
   Begin
    Gen.SetReusingMode(GetBoolean);
   End;

   // -purge-cache
   '-purge-cache':
   Begin
    DeleteDirectory('cache', False);
    RemoveDir('cache');
    Stop := True;
   End;

   // unknown
   Else
   Begin
    if (FileExists(ParamStr(I))) Then
    Begin
     FileList.Add(ParamStr(I));
    End Else
    Begin
     raise Exception.CreateFmt('Unknown command line argument: %s', [ParamStr(I)]);
    End;
   End;
  End;

  Inc(I);
 End;
End;

(* ChangeFileName *)
Function ChangeFileName(const FileName: String): String;
Var I: int32;
Begin
 if (Output <> '') Then
  Exit(Output);

 Result := ExtractFileName(FileName);

 For I := Length(Result) Downto 1 Do
 Begin
  if (Result[I] = '.') Then
  Begin
   Insert('_rendered', Result, I);
   Exit;
  End;
 End;
End;

(* Log *)
Procedure Log(const Msg: String);
Begin
 Writeln(Msg);
End;

Var FileName: String;
Begin
 DefaultFormatSettings.DecimalSeparator := '.';

 Writeln('Mosaic image generator v.', FloatToStr(Version), '; by Patryk Wychowaniec');

 Gen      := TMosaicGen.Create;
 FileList := TFileList.Create;

 Try
  Try
   ParseCommandLine;

   if (not Stop) Then
   Begin
    Gen.SetLogProcedure(@Log);

    Time := GetTickCount64;
    Gen.GenerateImageIndex('imgs');
    Time := GetTickCount64 - Time;
    Writeln('> Loading time: ', Time, ' ms');

    For FileName in FileList Do
    Begin
     Time := GetTickCount64;
     Gen.GenerateImage(FileName, ChangeFileName(FileName));
     Time := GetTickCount64 - Time;
     Writeln('> Generation time: ', Time, ' ms');
    End;
   End;
  Except
   On E: Exception Do
   Begin
    Writeln;
    Writeln('An exception has been raised:');
    Writeln('> ', E.Message);
    Writeln;
   End;
  End;
 Finally
  Gen.Free;
  FileList.Free;
 End;

 Writeln('[Info] -- done --');

 if (Wait) Then
 Begin
  Readln;
 End;
End.
