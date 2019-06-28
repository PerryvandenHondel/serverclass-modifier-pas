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


procedure ProcessConfig(fn: AnsiString);
var
    tf: TextFile;
    l: Integer;
    buffer: AnsiString;
    inServerClass: Boolean;
begin
    AssignFile(tf, fn);
    Reset(tf);

    l := 0;
   
    while not eof(tf) do
    begin
        ReadLn(tf, buffer);
        Inc(l);
        

        if Pos('[serverClass:' + paramServerClass + ']', buffer) > 0 then
        begin
            //writeln('-- FOUND SERVERCLASS: ', sc);
            inServerClass := true;
        end; // of if 

        if (inServerClass = true) and (Length(buffer) = 0) then
        begin
            // You are in the serverclass and encounter a empty line
            // Stepped out of the serverclass
            inServerClass := false;
        end; // of if


        WriteLn(l:4, ': INSERVERCLASS=', inServerClass, ' ', buffer);
    end; // of while
end; // of procedure ProcessConfig


begin
    pathServerClassConf := GetPathServerClassConf();
    WriteLn(pathServerClassConf);

    paramServerClass := 'svc_oslindel_linux_p';
    paramListType := 'whitelist';
    paramAction := 'add';
    paramHost := 'lsrvnew01';

    WriteLn('Action on ', pathServerClassConf, ' in server class ', paramServerClass, ' for the ', paramListType,' to ', paramAction, ' ', paramHost);

    ProcessConfig(pathServerClassConf);
end. // of program ServerClassModifier