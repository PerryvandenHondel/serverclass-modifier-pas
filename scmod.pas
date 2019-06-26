program ServerClassModifier;

//
// Modify the Splunk serverclass.conf
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


function GetPathServerClass(): AnsiString;
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

    GetPathServerClass := r;
end; // of function ReadLocationServerClass



function HostRecordFind(host: AnsiString): Integer;
//
// Search for host in hostArray
// Returns:
//   -1: Not found
//   >=0: Position of found host
var
    r: Integer;
    i: Integer;
begin
    r := -1;

    for i := 0 to High(hostArray) do
    begin
        
        if host = hostArray[i].host then
        begin
            // WriteLn('HostRecordFind() FOUND ', i, ': ', host, ' = ', hostArray[i].host);
            r := i;
            // We know the number; Break this loop.
            Break;
        end; // of if
    end; // of for

    HostRecordFind := r;
end; // of function HostRecordFind();


procedure HostRecordAdd(host: AnsiString);
//
//  Add new record to table
//      host:       hostname   
//
var
    size: integer;
    
begin
    writeln('HostRecordAdd(): ', host);

    if HostRecordFind(host) = -1 then 
    begin
        // The host does not exist in the array; add it.

        // Get the current size of the hostArray.
        size := Length(hostArray);

        // Increase the size of the hostArray with extra room for one.
        SetLength(hostArray, size + 1);

        // Assign the host to the hostArray.host field.
        hostArray[size].host := host; 
    end;
end; // of procedure HostRecordAdd


procedure HostRecordDel(host: AnsiString);
//
// Delete a item from a dynamic array.
// Source: https://www.tweaking4all.com/forums/topic/pascal-how-to-remove-an-array-element/
//
var
    pos: Integer;
    ALength: Cardinal;
    i: Cardinal;
begin
    pos := HostRecordFind(host);
    if pos <> -1 then
    begin
        WriteLn('HostRecordDel(): Found ', host, ' at pos ', pos);

        ALength := Length(hostArray);
        Assert(ALength > 0);
        Assert(pos < ALength);
        for i := pos + 1 to ALength - 1 do
            hostArray[i - 1] := hostArray[i];
        SetLength(hostArray, ALength - 1);
    end;
end; // of procedure HostRecordDel()


procedure HostRecordShow();
var
    i: integer;
begin
    writeln('HostRecordShow()');
    for i := 0 to High(hostArray) do
    begin
        
        writeln('  ', i:4, ': ', hostArray[i].host);
    end; // of for
end; // of procedure HostRecordShow


procedure BuildHostArray(fn: Ansistring; sc: AnsiString; list: AnsiString);
//
//  Build the arrayHost with existing host entries.
//
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

    writeln('BuildHostArray()');
    writeln(' Server Class : ', sc);
    writeln(' List Type    : ', list);

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
        end // of if 
        else 
            inList := false;

        if Length(buffer) = 0 then
            // When an empty line is encounterd then the server class has no more entries.
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
end; // of procedure BuildHostArray


procedure WriteRecordToConfig(listType: AnsiString);
var
    i: Integer;
begin
    for i := 0 to High(hostArray) do
    begin
        writeln(listType, '.', i, ' = ', hostArray[i].host);
    end; // of for
end; // of procedure WriteRecordToConfig()


procedure ProgUsage();
begin
    writeln();
    writeln('scmod v0.3 - Splunk serverclass.conf modifier, add or delete hosts from a server class in whitelist and blacklist.');
    writeln();
    writeln('Usage: scmod <serverclass> <listtype> <action> <hostname>');
    writeln('  <serverclass>        Name of the server class to modify');
    writeln('  <listtype>           Select the "whitelist" or "blacklist"');
    writeln('  <action>             What action to perform on the serverclass, select "add" or "delete"');
    writeln('  <hostname>           Name of host to add or delete from the server class');
    writeln();
    Halt; // Stop the program.
end; // of procedure ProgUsage()


begin
    if ParamCount <> 4 then
        ProgUsage();

    fnOrg := GetPathServerClass();
    paramServerClass := paramStr(1);
    paramList := paramStr(2);
    paramAction := paramStr(3);
    paramHost := paramStr(4);

    WriteLn('Server Class Config file: ', fnOrg);
    writeln('            Server Class: ', paramServerClass);
    writeln('               List Type: ', paramList);
    writeln('                  Action: ', paramAction);
    writeln('                    Host: ', paramHost);

    // Build the aarayHost with the current entries of the server class
    BuildHostArray(fnOrg, paramServerClass, paramList);

    // 
    writeln('BEFORE ACTION');
    HostRecordShow();

    if paramAction = 'add' then
        HostRecordAdd(paramHost);

    if paramAction = 'del' then
        HostRecordDel(paramHost);

    writeln('AFTER ACTION');
    HostRecordShow();

    WriteRecordToConfig(paramList);

    writeln('Program completed succesfully.');
end. // of program ModifyServerClass