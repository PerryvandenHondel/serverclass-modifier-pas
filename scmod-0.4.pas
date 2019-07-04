program ServerClassModifier;

//
// Modify the Splunk serverclass.conf
//
// Version 0.4
//


{$mode objfpc}
{$H+}


uses
    Crt,
    SysUtils;



type
    TConfigRec = record
        lineCount: Integer;
        serverClass: AnsiString;
        listType: AnsiString;
        line: AnsiString;
        host: AnsiString;
    end;
    TConfigArray = array of TConfigRec;

var
    pathServerClassConf: AnsiString;
    configArray: TConfigArray;
    configLineCount: Integer;


function GetPathServerClassConf(): AnsiString;
//
// Get the path to the serverclass.conf file.
// Location is stored in scmod.conf
//
var
    path: AnsiString;
    r: AnsiString;
    tf: TextFile;
begin
    r := '';

    path := ParamStr(0) +'.conf';
    
    AssignFile(tf, path);
    Reset(tf);

    ReadLn(tf, r);

    Close(tf);

    GetPathServerClassConf := r;
end; // of function ReadLocationServerClass


procedure ConfigArrayAdd(lineCount: Integer; serverClass: AnsiString; listType: AnsiString; host: AnsiString; line: AnsiString);
var
    size: Integer;
begin
    // Get the current size of the hostArray.
    size := Length(configArray);

    // Increase the size of the hostArray with extra room for one.
    SetLength(configArray, size + 1);

    // Assign the host to the .host field.
    configArray[size].lineCount := lineCount;
    configArray[size].serverClass := serverClass;
    configArray[size].listType := listType;
    configArray[size].host := host;
    configArray[size].line := line;
end; // of procedure ConfigArrayAdd


procedure ConfigArrayShow();
var
    i: Integer;
begin
    WriteLn('ConfigArrayShow()');

    for i := 0 to High(configArray) do
    begin
        WriteLn('  ', i:4, ': ', configArray[i].lineCount:4, ' SC=', configArray[i].serverClass:24, ' LT=', configArray[i].listType:12, ' HOST=', configArray[i].host:24, ' LINE=', configArray[i].line);
    end; // of for
end; // of procedure ConfigArrayShow()


function OccurrencesOfChar(const S: string; const C: char): integer;
//
// Source: https://stackoverflow.com/questions/15294501/how-to-count-number-of-occurrences-of-a-certain-char-in-string
//
var
  i: Integer;
begin
  result := 0;
  for i := 1 to Length(S) do
    if S[i] = C then
      inc(result);
end;

procedure BuildConfigArray(p: AnsiString);
var
    tf: TextFile;
    l: Integer;
    buffer: AnsiString;
    listType: AnsiString;
    serverClass: AnsiString;
    host: AnsiString;
begin
    AssignFile(tf, p);
    Reset(tf);

    l := 0;
   
    while not eof(tf) do
    begin
        readln(tf, buffer);
        Inc(l);
        configLineCount := configLineCount + 10;

        if Pos('whitelist', buffer) > 0 then
        begin
            listType := 'whitelist';    
            host := Trim(RightStr(buffer, Length(buffer) - Pos('=', buffer)));
        end; // of if

        if Pos('blacklist', buffer) > 0 then
        begin
            listType := 'blacklist';
            host := Trim(RightStr(buffer, Length(buffer) - Pos('=', buffer)));
        end; // of if

        if Length(buffer) = 0 then
        begin
            // When a empty line is encounterd. The config of the serverClass is done
            // Clear the 
            //      listType
            //      serverClass.
            //      host
            listType := '';
            serverClass := '';
            host := '';
        end; // of if

        //if Pos('[serverClass:', buffer) > 0 then
        if (OccurrencesOfChar(buffer, ':') = 1) and (Pos('[serverClass:', buffer) > 0) then
        begin
            //
            // buffer = [serverClass:svc_oslindel_linux_p]
            //
            // Obtain the serverclass from the buffer
            // Only one : is present in the buffer
            //
            serverClass := Copy(buffer, 14, Length(buffer) - 14);
            WriteLn(ServerClass);
        end; // of if

        WriteLn(l, ': ', buffer);

        ConfigArrayAdd(configLineCount, serverClass, listType, host, buffer);

        listType := '';
    end; // of while
    Close(tf);
end; // of procedure BuildConfigArray()


begin
    pathServerClassConf := GetPathServerClassConf();
    WriteLn(pathServerClassConf);

    configLineCount := 0;
    BuildConfigArray(pathServerClassConf);

    ConfigArrayShow();
end. // of program ServerClassModifier