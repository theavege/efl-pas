//https://castle-engine.io/modern_pascal
//https://wiki.freepascal.org/ASCII
//instantfpc make.pas

program Make;
{$mode objfpc}{$H+}

uses
  Classes,
  SysUtils,
  StrUtils,
  FileUtil,
  LazFileUtils,
  Zipper,
  fpHTTPclient,
  RegExpr,
  OpenSSL,
  LazUTF8,
  OpenSSLsockets,
  EventLog,
  Process;

  function OutLog(const Knd: EventLog.TEventType; const Msg: string): string; cdecl;
  begin
    case Knd of
      etError: Result := #27'[31m%s'#27'[0m';
      etInfo:  Result := #27'[32m%s'#27'[0m';
      etDebug: Result := #27'[33m%s'#27'[0m';
    end;
    if Knd = etError then
      ExitCode += 1;
    Writeln(stderr, LazUTF8.UTF8ToConsole(Result.Format([Msg])));
  end;

  function SelectString(const Input, Reg: string): string; cdecl;
  var
    Line: string;
  begin
    Result := EmptyStr;
    with RegExpr.TRegExpr.Create do begin
      Expression := Reg;
      for Line in Input.Split(LineEnding) do
        if Exec(Line) then
          Result += Line + LineEnding;
      Free;
    end;
  end;

  function AddPackage(const Path: string): string; cdecl;
  begin
    AddPackage :=
      {$IFDEF MSWINDOWS}
        '(cocoa|x11|_template)'
      {$ELSE}
        '(cocoa|gdi|_template)'
      {$ENDIF};
    if SelectString(Path, AddPackage) = EmptyStr then
      if Process.RunCommand('lazbuild', ['--add-package-link', Path], Result, [poStderrToOutPut]) then
        OutLog(etDebug, 'Add package:'#9 + Path)
      else
        OutLog(etError, Result);
  end;

  function ConsoleTestRunner(const Path: String): string; cdecl;
  begin
    OutLog(etDebug, #9'Run test:'#9 + Path);
    if not Process.RunCommand(Path, ['--all', '--format=plain'], Result, [poStderrToOutPut]) then
      OutLog(etError, Result);
  end;

  function AddLibrary(const Path: String): string; cdecl;
  const
    LibPath: string = '/usr/lib/';
  begin
    OutLog(etDebug, #9'Add lib:'#9 + Path);
    if not FileExists(LibPath + ExtractFileName(Path)) and
       not Process.RunCommand('sudo', ['cp', Path, LibPath, ';',  'ldconfig'], Result, [poStderrToOutPut]) then
        OutLog(etError, Result);
  end;

  function BuildProject(const Path: string): string; cdecl;
  var
    Text: string;
  begin
    OutLog(etDebug, 'Build from:'#9 + Path);
    if Process.RunCommand('lazbuild',
      ['--build-all', '--recursive', '--no-write-project', Path], Result, [poStderrToOutPut, poWaitOnExit]) then
    begin
      Result := SelectString(Result, 'Linking').Split(' ')[2].Replace(LineEnding, EmptyStr);
      OutLog(etInfo, #9'to:'#9 + Result);
      Text := ReadFileToString(Path.Replace('.lpi', '.lpr'));
      if Text.Contains('program') and Text.Contains('consoletestrunner') then
        ConsoleTestRunner(Result)
      else if Text.Contains('library') and Text.Contains('exports') then
        AddLibrary(Result)
    end else
      OutLog(etError, SelectString(Result, '(Fatal|Error):'));
  end;

  function DownloadFile(const Uri: string): string; cdecl;
  var
    FileStream: TStream;
  begin
    openssl.InitSSLInterface;
    Result := GetTempFileName;
    FileStream := TFileStream.Create(Result, fmCreate or fmOpenWrite);
    with FPHttpClient.TFPHttpClient.Create(nil) do begin
      try
        AddHeader('User-Agent', 'Mozilla/5.0 (compatible; fpweb)');
        AllowRedirect := True;
        Get(Uri, FileStream);
        OutLog(etDebug, 'Download from'#9 + Uri + #9'to'#9 + Result);
      finally
        Free;
        FileStream.Free;
      end;
    end;
  end;

  procedure UnZip(const ZipFile, ZipPath: string); cdecl;
  begin
    with TUnZipper.Create do begin
      try
        FileName := ZipFile;
        OutputPath := ZipPath;
        Examine;
        UnZipAllFiles;
        OutLog(etDebug, 'Unzip from'#9 + ZipFile + #9'to'#9 + ZipPath);
        DeleteFile(ZipFile);
      finally
        Free;
      end;
    end;
  end;

  function InstallOPM(const Path: string): string; cdecl;
  begin
    Result :=
      {$IFDEF MSWINDOWS}
      GetEnvironmentVariable('APPDATA')
      {$ELSE}
      GetEnvironmentVariable('HOME')
      {$ENDIF}
      + '/.lazarus/onlinepackagemanager/packages/'.Replace('/', DirectorySeparator)
      + Path;
    if not DirectoryExists(Result) and ForceDirectories(Result) then
        Zipper.UnZip(DownloadFile('https://packages.lazarus-ide.org/' + Path + '.zip'), Result);
  end;

  function BuildAll(const Dependencies: array of string): string;
  var
    List: TStringList;
    DT: TDateTime;
  begin
    // INSTALL-ENVIRONMENTS
    DT := Time;
    if FileExists('.gitmodules') then
      if not Process.RunCommand('git', ['submodule', 'update', '--init', '--recursive',
        '--force', '--remote'], Result, [poStderrToOutPut]) then
        OutLog(etError, Result);
    // INSTALL-PACKAGES
    List := FindAllFiles(GetCurrentDir, '*.lpk');
    try
      for Result in Dependencies do
        List.AddStrings(FindAllFiles(InstallOPM(Result), '*.lpk'));
      List.Sort;
      for Result in List do
        AddPackage(Result);
      // BUILD-PROJECTS
      List := FindAllFiles(GetCurrentDir, '*.lpi');
      List.Sort;
      for Result in List do
        if not Result.Contains(DirectorySeparator + 'use' + DirectorySeparator) then
          BuildProject(Result);
    finally
      List.Free;
    end;
    OutLog(etDebug, 'Duration:'#9'%s'#10'Errors:'#9'%s'.Format(
      [FormatDateTime('hh:nn:ss', Time - DT), ExitCode.ToString]
    ));
  end;

{------------------------------------------------------------------------------}

begin
  try
    if ParamCount > 0 then
      case ParamStr(1) of
        'build': BuildAll([]);
        else OutLog(etError, ParamStr(1));
      end;
  except
    on E: Exception do
      OutLog(etError, E.ClassName + #9 + E.Message);
  end;
end.
