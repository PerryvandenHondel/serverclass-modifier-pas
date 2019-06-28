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
    paramServerClass: AnsiString;
    paramListType: AnsiString;
    paramAction: AnsiString;
    paramHost: AnsiString;


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
end; // of function GetPathServerClassConf





begin
    pathServerClassConf := GetPathServerClassConf();
    WriteLn(pathServerClassConf);

    paramServerClass := 'svc_oslindel_linux_p';
    paramListType := 'whitelist';
    paramAction := 'add';
    paramHost := 'lsrvnew01';


    WriteLn('Action on ', pathServerClassConf, ' in server class ', paramServerClass, ' for the ', paramListType,' to ', paramAction, ' ', paramHost);
end. // of program ServerClassModifier