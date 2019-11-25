program ServerClassModifier;
//
// Modify the Splunk serverclass.conf
//
// Version 0.6
//


{$mode objfpc}
{$H+}


uses
    Classes,
    Crt,
    Dos,
    DateUtils,
    UBuild,
    UTextFile,
    USupLib,
    SysUtils;


var
    pathServerClassConf: AnsiString;
    pathLog: AnsiString;
    paramServerClass: AnsiString;
    paramListType: AnsiString;
    paramAction: AnsiString;
    paramHost: AnsiString;
    log: CTextFile;
    pathModify: AnsiString;         // Path to the file with the attributes to modify.



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
    WriteLn('Config file is ', path);
    
    AssignFile(tf, path);
    Reset(tf);

    ReadLn(tf, r);

    Close(tf);

    GetPathServerClassConf := r;
end; // of function GetPathServerClassConf


procedure ProcessConfig(fnConf: AnsiString);
//
//  Process the original config file: 
//  1) Copy the original file to a backup.
//  2) Read the backup file and write modifictions to the orginal name.
//
var
    tfConf: TextFile;
    tfBackup: TextFile;

    l: Integer;
    fnBackup: AnsiString;
    bufferRead: AnsiString;
    bufferWrite: AnsiString;
    inServerClass: Boolean;
    inListType: Boolean;
    listHighest: Integer;
    listCurrent: Integer;
    skipLine: Boolean;
begin
    // Create a new file name as backup. serverclass.conf --> serverclass.conf.1234567890.scmod
    fnBackup := fnConf + '.' + IntToStr(DateTimeToUnix(Now())) + '.scmod';
    CopyTheFile(fnConf, fnBackup);

    // Open the back file and write the modifications into the originial .conf file.
    AssignFile(tfBackup, fnBackup);
    Reset(tfBackup);

    // Open the config file to write to.
    AssignFile(tfConf, fnConf);
    ReWrite(tfConf);
    bufferWrite := '';


    l := 0;
    inServerClass := false;
    inListType := false;
    skipLine := false;
    listHighest := -1; // Higest found number of a list type; can be 0 for first entry. 'whitelist.0 = hostname'
    listCurrent := -1;
   
    while not eof(tfBackup) do
    begin
        ReadLn(tfBackup, bufferRead);
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
            
            // Get the current number of the listtype: whitelist.x = hostname
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
                // Add a new system to the list type.
                bufferWrite := paramListType + '.' + IntToStr(listHighest + 1) + ' = ' + paramHost;
                WriteLn('Adding new line to ', fnConf, ': ', bufferWrite);
                WriteLn(tfConf, bufferWrite);

            end; // of if

            listHighest := 0; // Number of list will be 0 for the first
        end; // of if 

        if (inServerClass = true) and (inListType = true) and (Pos(paramHost, bufferRead) > 0) and (UpperCase(paramAction) = 'DEL') then
        begin
            // When in the serverclass and the host is found and you need to delete it.
            WriteLn('Delete this host: ', paramHost);
            skipLine := true;
        end;
        
        //WriteLn(l:4, ': INSC=', inServerClass:5, ' INLT=', inListType:5, ' LH=', listHighest:3, ' > ', bufferRead);
        bufferWrite := bufferRead;
        if skipLine = true then
        begin 
            
            skipLine := false; // and back to false for the next line.
        end
        else
            WriteLn(tfConf, bufferWrite);
        begin
        
        end;
    end; // of while

    CloseFile(tfConf);

    CloseFile(tfBackup);
end; // of procedure ProcessConfig



procedure LogWrite(t: AnsiString);
begin
    log.WriteToFile(GetCurrentDateTimeMicro + ' ' + t);
end; // of procedure LogWrite()


procedure LogOpen();
begin
    pathLog := ReadSettingKey(ParamStr(0) +'.conf','Settings', 'PathLog');
    WriteLn('pathLog=', pathLog);

    log := CTextFile.Create(pathLog);
	log.OpenFileWrite();
    LogWrite('Started');
end; // of procedure LogOpen()


procedure LogClose();
begin
    LogWrite('Ended');
    log.CloseFile();
end; // of procedure LogClose()


procedure ProgTitle();
begin
    writeln();
    writeln('scmod v', VERSION_MAJOR,'.', VERSION_MINOR, '.', BUILD, ' - Splunk serverclass.conf modifier, add or delete hosts from a server class in the whitelist or blacklist section.');
    writeln();
end; // of procedure ProgTitle()


{procedure ProgReadConfig()
var
    pathServerClass := 
begin

end; // of procedure ProgReadConfig
}

procedure MakeBackupServerClass();
//
// Read the backup directory location for the config file.
// Read the serverclass.conf location for the server class from the config file.
// Make a backup of the serverclass to the backup directory.
//
var
    directoryBackup: AnsiString;
    pathServerClass: AnsiString;
    pathServerClassBackup: AnsiString;
begin
    directoryBackup := ReadSettingKey(ParamStr(0) + '.conf', 'Settings', 'DirBackup');
    pathServerClass := ReadSettingKey(ParamStr(0) +'.conf','Settings', 'PathServerClass');

    WriteLn('directoryBackup=', directoryBackup);
    WriteLn('pathServerClass=', pathServerClass);

    pathServerClassBackup := directoryBackup + '/serverclass.conf.' + IntToStr(DateTimeToUnix(Now())) + '.scmod';

    Writeln('Create backup of the Server Class:', pathServerClass + '  >>  ', pathServerClassBackup);
    LogWrite('Create backup of ' + pathServerClass + ' to backup file ' + pathServerClassBackup);
end; // of procedure MakeBackupServerClass() 


procedure ProgUsage();
begin
    writeln('Usage: scmod <modifyfile>');
    writeln('  <modifyfile>     File with the modify information.');
    writeln();
    Halt; // Stop the program.
end; // of procedure ProgUsage()





procedure ProgInit();
begin
    ProgTitle();

    if ParamCount <> 1 then
        ProgUsage()
    else
        pathModify := ParamStr(1);

    WriteLn('Mod file: ', pathModify);

    LogOpen();

end; // of procedure ProgInit()


procedure ProgRun();
//
// pathModify:     Path to the Modify file.
//
var
    tfm: CTextFile;
    line: AnsiString;
    s: TStringArray;
    x : integer;
begin
    MakeBackupServerClass();


    tfm := CTextFile.Create(pathModify);
    tfm.OpenFileRead();
    Writeln('The status of ' + tfm.GetPath + ' is ' + BoolToStr(tfm.GetStatus, 'OPEN', 'CLOSED'));
    repeat
        line := tfm.ReadFromFile();
        WriteLn(IntToStr(tfm.GetCurrentLine()) + ': ' + line);
        
        SetLength(s, 0);
        s := SplitString(line, ';');
        for x := 0 to High(s) do 
        begin
            // For every part of the array print it
            WriteLn(#9#9, IntToStr(x) + ': ' + s[x]);
        end;
        SetLength(s, 0); // Set the array to 0 after use.
      
    until tfm.GetEof();
    tfm.CloseFile();

end; // of procedure ProgRun()



procedure ProgDone();
begin
    LogClose();
    
    WriteLn;
    Writeln('splunk reload deploy-server -class ' + paramServerClass);
    WriteLn;
end; // of procedure ProgDone()


procedure ProgRunOld();
begin
    Sleep(1001);

    if ParamCount <> 4 then
        ProgUsage();

    // Get the parameters from the command line.
    paramServerClass := ParamStr(1);
    paramListType := ParamStr(2);
    paramAction := ParamStr(3);
    paramHost := ParamStr(4);
{
    WriteLn('paramServerClass=[', paramServerClass, ']');
    WriteLn('paramListType=[', paramListType, ']');
    WriteLn('paramAction=[', paramAction, ']');
    WriteLn('paramHost=[', paramHost, ']');
    
    WriteLn(CompareText(paramListType, 'whitelist'));

    if (CompareText(paramListType, 'whitelist') <> 0) then
    begin
    or (CompareText(paramListType, 'blacklist') <> 0) then
    begin
        WriteLn('ERROR: Option listtype is not "whitelist" or "blacklist".');
        ProgUsage();
    end; // of if paramListType

    if (UpperCase(paramAction) <> 'ADD') or (UpperCase(paramAction) <> 'DEL') then
    begin
        WriteLn('ERROR: Option action is not "add" or "del".');
        ProgUsage();
    end; // of if paramListType
}


    //pathServerClassConf := GetPathServerClassConf();
    //WriteLn('Working on config file: ', pathServerClassConf);

    pathServerClassConf := ReadSettingKey(ParamStr(0) +'.conf','Settings', 'PathServerClass');

    pathLog := ReadSettingKey(ParamStr(0) +'.conf','Settings', 'PathLog');
    WriteLn('pathLog=', pathLog);

    log := CTextFile.Create(pathLog);
	log.OpenFileWrite();

    //WriteLn('GetCurrentDateTimeMicro()=', GetCurrentDateTimeMicro());

    //paramServerClass := 'svc_oslindel_linux_p'; // ServerClass with whitelist entries
    //paramServerClass := 'svc_testclass';
    //paramServerClass := 'not_existsing_sc';
    //paramserverClass := 'svc_emptyclassservers'; // Empty ServerClass 
    //paramListType := 'whitelist';
    //paramAction := 'add';
    //paramHost := 'lsrvbl001*';

    WriteLn('Action on ', pathServerClassConf, ' in server class ', paramServerClass, ' for the ', paramListType,' to ', paramAction, ' ', paramHost);
    log.WriteToFile(GetCurrentDateTimeMicro() + 
        ' USERNAME=' + GetCurrentUser() +
        ' SERVERCLASS=' + paramServerClass +
        ' LISTTYPE=' + paramListType +
        ' ACTION=' + paramAction +
        ' HOSTNAME=' + paramHost);

    ProcessConfig(pathServerClassConf);

    log.CloseFile();
end; // of procedure ProgRunOld()


begin
    ProgInit();
    ProgRun();
    ProgDone();
end. // of program ServerClassModifier