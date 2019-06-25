program ModifyServerClass;

//
// Modify Splunk serverclass.conf
//
// Version 0.3
//



{$mode objfpc}
{$H+}


uses
    Crt,
    SysUtils;


var
    tfOrg: TextFile;
    fnOrg: AnsiString;
    paramServerClass: AnsiString;
    paramList: AnsiString;
    paramAction: AnsiString;
    paramHost: AnsiString;


procedure AddRecord(sc: AnsiString; host: AnsiString);

begin
    writeln('AddRecord(): ', list, ' > ', host);
end; // of procedure AddRecord


procedure BuildTable(fn: Ansistring; sc: AnsiString; list: AnsiString);
var
    tf: TextFile;
    line: integer;
    buffer: AnsiString;
    inServerClass: Boolean;
    inList: Boolean;
    host: AnsiString;
begin
    AssignFile(tf, fn);
    Reset(tf);

    line := 0;
    inServerClass := false;
    inList := false;

    while not eof(tf) do
    begin
        readln(tf, buffer);
        Inc(line);
        //writeln(line, ': ', buffer);

        if Pos('[serverClass:' + sc + ']', buffer) > 0 then
        begin
            //writeln('-- FOUND SERVERCLASS: ', sc);
            inServerClass := true;
        end; // of if 

        if Pos(list, buffer) > 0 then
        begin
            //writeln('-- FOUND LIST: ', list);
            inList := true;
        end; // of if 

        if Length(buffer) = 0 then
            inServerClass := false;

        writeln(line, ' inServerClass=', inServerClass, '   inList=', inList, ' > ', buffer);

        if (inServerClass = true) and (inList = true) then
        begin
            host := Trim(RightStr(buffer, Length(buffer) - Pos('=', buffer)));
            writeln('ADD RECORD: ', list, ' > ', host);
        end; // of if

    end; // of while

    Close(tf);
end; // of procedure BuildTable


begin
    fnOrg := './serverclass.conf';
    paramServerClass := 'svc_testclass';
    paramList := 'whitelist';
    paramAction := 'add';
    paramHost := 'servertodo10';



    BuildTable(fnOrg, paramServerClass, paramList);

    writeln('Program completed succesfully.');
end. // of program ModifyServerClass