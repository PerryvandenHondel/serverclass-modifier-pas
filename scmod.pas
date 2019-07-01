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
    tfnew: TextFile;

    l: Integer;
    fnNew: AnsiString;
    bufferRead: AnsiString;
    bufferWrite: AnsiString;
    inServerClass: Boolean;
    inListType: Boolean;
    listHighest: Integer;
    listCurrent: Integer;
begin
    pathServerClassConfNew := fn + '.new';

    AssignFile(tf, fn);
    Reset(tf);

    AssignFile(tfnew, fnNew);
    ReWrite(tfNew);


    l := 0;
    inServerClass := false;
    inListType := false;
    listHighest := -1; // Higest found number of a list type; can be 0 for first entry. 'whitelist.0 = hostname'
    listCurrent := -1;
   
    while not eof(tf) do
    begin
        ReadLn(tf, bufferRead);
        Inc(l);

        if Pos('[serverClass:' + paramServerClass + ']', bufferRead) > 0 then
        begin
            // Found the server class
            inServerClass := true;
        end; // of if 

         if (inServerClass = true) and (Pos(paramListType, bufferRead) > 0) then
        begin
            // We have entries for the list type.
            inListType := true;
            
            listCurrent := StrToInt(Trim(Copy(bufferRead, Pos('.', bufferRead) + 1, (Pos(' =', bufferRead) - 2) - Pos('.', bufferRead) + 1)));
            if listCurrent > listHighest then 
                listHighest := listCurrent;
        end; // of if

        if (inServerClass = true) and (Length(bufferRead) = 0) then
        begin
            // You are in the serverclass and encounter a empty line
            // Stepping out of the serverclass part
            inServerClass := false;
            inListType := false;
            
            if UpperCase(paramAction) = 'ADD' then
            begin
                // 
                WriteLn(paramListType, '.', listHighest + 1, ' = ', paramHost);
            end; // of if

            listHighest := 0; // Number of list will be 0 for the first
        end; // of if 

        if (inServerClass = true) and (Pos(paramHost, bufferRead) > 0) and (UpperCase(paramAction) = 'DEL') then
        begin
            // When in the serverclass and the host is found and you need to delete it.
            //
            WriteLn(' Delete this host: ', paramHost);
        end; 
       

  
        WriteLn(l:4, ': INSC=', inServerClass:5, ' INLT=', inListType:5, ' LH=', listHighest:3, ' > ', bufferRead);
    end; // of while

    Close(tfNew);

    Close(tf);
end; // of procedure ProcessConfig


begin
    pathServerClassConf := GetPathServerClassConf();
    WriteLn(pathServerClassConf);

    paramServerClass := 'svc_oslindel_linux_p'; // ServerClass with whitelist entries
    //paramServerClass := 'svc_testclass';
    //paramServerClass := 'not_existsing_sc';
    //paramserverClass := 'svc_emptyclassservers'; // Empty ServerClass 
    paramListType := 'whitelist';
    paramAction := 'del';
    paramHost := 'lsrv0011';

    WriteLn('Action on ', pathServerClassConf, ' in server class ', paramServerClass, ' for the ', paramListType,' to ', paramAction, ' ', paramHost);

    ProcessConfig(pathServerClassConf);
end. // of program ServerClassModifier