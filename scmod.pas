program ServerClassModifier;

//
// Modify the Splunk serverclass.conf
//
// Version 0.5
//


{$mode objfpc}
{$H+}


uses
    Crt,
    SysUtils;


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

begin
    pathServerClassConf := GetPathServerClassConf();
    WriteLn(pathServerClassConf);
end. // of program ServerClassModifier