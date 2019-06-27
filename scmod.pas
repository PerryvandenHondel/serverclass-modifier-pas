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
    

var
    pathServerClassConf: AnsiString;


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


procedure BuildArray(p: AnsiString);
var
    tf: TextFile;
    l: Integer;
    buffer: AnsiString;
begin
    AssignFile(tf, p);
    Reset(tf);

    l := 0;
   
    while not eof(tf) do
    begin
        readln(tf, buffer);
        Inc(l);

        WriteLn(l, ': ', buffer);
    end; // of while
    Close(tf);
end; // of procedure BuildArray()

begin
    pathServerClassConf := GetPathServerClassConf();
    WriteLn(pathServerClassConf);

    BuildArray(pathServerClassConf);
end. // of program ServerClassModifier