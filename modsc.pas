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


type
    THostRec = record
        host: AnsiString;
    end;
    THostArray = array of THostRec;


var
    //tfOrg: TextFile;
    fnOrg: AnsiString;
    paramServerClass: AnsiString;
    paramList: AnsiString;
    paramAction: AnsiString;
    paramHost: AnsiString;
    hostArray: THostArray;


function InHostArray(host: AnsiString): Boolean;
//
// Search for host in hostArray
// Returns:
//   true > found the name in the array
//   false > Did not found the host in the array
//
var
    r: Boolean;
    i: integer;
begin
    r := false;

    for i := 0 to High(hostArray) do
    begin
        if host = hostArray[i].host then
            r := true;
    end; // of for
    InHostArray := r;
end; // of function InHostArray();


procedure HostRecordAdd(host: AnsiString);
//
//  Add new record to table
//      host:       hostname   
//
var
    size: integer;
    
begin
    writeln('HostRecordAdd(): ', host);

    if InHostArray(host) = false then 
    begin
        // The host does not exist in the array; add it.

        // Get the current size of the hostArray.
        size := Length(hostArray);

        // Increase the size of the hostArray with extra room for one.
        SetLength(hostArray, size + 1);

        // Assign the host to the hostArray.host field.
        hostArray[size].host := host; 
    end;
end; // of procedure AddRecord


procedure HostRecordShow();
var
    i: integer;
begin
    writeln();
    writeln('HostRecordShow()');
    
    for i := 0 to High(hostArray) do
    begin
        writeln(i, ': ', hostArray[i].host);
    end; // of for
end; // of procedure HostRecordShow


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

        if (inServerClass = true) and (inList = true) and (Pos('=', buffer) > 0) then
        begin
            //
            // Only do this when these 3 rules apply:
            //   1) inServerClass = true
            //   2) inList = true
            //   3) We found a = char in the buffer (whitelist.x = hostname)
            //
            host := Trim(RightStr(buffer, Length(buffer) - Pos('=', buffer)));

            // Add this found host name to the array of hosts.
            HostRecordAdd(host);
        end; // of if

    end; // of while

    Close(tf);
end; // of procedure BuildTable


begin
    fnOrg := './serverclass.conf';
    //paramServerClass := 'svc_testclass';
    paramServerClass := 'sc_emptyone';
    paramList := 'whitelist';
    paramAction := 'add';
    paramHost := 'servertodo10';



    //BuildTable(fnOrg, paramServerClass, paramList);

    HostRecordAdd('totallynewserver');
    HostRecordAdd('totallynewserver1');
    HostRecordAdd('totallynewserver2');
    HostRecordAdd('serverfind');
    HostRecordAdd('totallynewserver4');
    HostRecordAdd('totallynewserver');
    HostRecordAdd('serverfind');
    

    HostRecordShow();

    writeln('Program completed succesfully.');
end. // of program ModifyServerClass