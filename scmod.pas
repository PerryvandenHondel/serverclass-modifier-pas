program ServerClassModifier;
{
    Server Class Modifier for Splunk
    
    Modify the Splunk serverclass.conf

    Version 0.6

    Reads the csv file with modify instructions.

    serverclass;listtype;action;hostname

    Example:

    sc_testmod;whitelist;add;lsrvtest01*
    sc_testmod;whitelist;add;lsrvtest02*
    sc_testmod;whitelist;add;lsrvtest03*
}



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



const
    TAB = #9;
    DEBUG_MODE_ON = 1;
    CONF_DEDUG_MODE_ON = 'DebugModeOn';
    CONF_PATH_SERVER_CLASS_CONF = 'PathServerClass';
    CONF_DIR_TEMP = 'ditTemp';


var
    pathServerClassConf: AnsiString;
    pathLog: AnsiString;
    log: CTextFile;
    pathModify: AnsiString;         // Path to the file with the attributes to modify.
    pathServerClass: AnsiString;
    reference: AnsiString;          // Reference; Under what reference the action is executed; ADO PBI (ADOP), ADO Task (ADOT), Change number
    dirTemp: AnsiString;            // Temp directory to store the temp work file.
    debugModeOn: Integer;           // When debug mode on = 1; debugmode off = 0



procedure DebugWriteLn(line: AnsiString);
begin
    if debugModeOn = 1 then
        WriteLn(line);
end;


function GetPathServerClassConf(): AnsiString;
{
    Get the path to the serverclass.conf file.
    Location is stored in scmod.conf
}
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


function FindHostInClass(pathServerClassConf: AnsiString; findInServerClass: AnsiString; findInType: AnsiString; hostName: AnsiString): Integer;
//
//  pathServerClassConf:    Path to the server class
//  findInServerClass:      Find the hostname in this server class
//
//
var
    isFound: Boolean;
    inServerClass: Boolean;
    inType: Boolean;
    tf: CTextFile;
    buffer: AnsiString;
begin
    Result := 0; 
    isFound := false;
    inServerClass := false;
    inType := false;

    //WriteLn('FindHostInClass(): ', findInServerClass, ' --> ', hostName);

    tf := CTextFile.Create(pathServerClassConf);
	tf.OpenFileRead();
    
    repeat
        buffer := tf.ReadFromFile();
        
        if Pos('[serverClass:' + findInServerClass + ']', buffer) > 0 then
        begin
            inServerClass := true;
        end; // of if Pos

        if Pos(findInType, buffer) > 0 then
        begin
            inType := true;
        end; // of if

        if (inServerClass = true) and (Length(buffer) = 0) then
        begin
            // You are in the server class and and empty line is found. Must be the end of the server class.
            inServerClass := false;
        end; // of if 

        if Pos(hostName, buffer) > 0 then
        begin
            isFound := true;
        end; // of if

        //WriteLn(IntToStr(tf.GetLineNumber()),':', TAB, 'SERVERCLASS=', inServerClass, TAB, 'TYPE=', inType, TAB, 'FOUND=', isFound, TAB, buffer);

        if (inServerClass = true) and (inType = true) and (isFound = true) then
        begin
            result := tf.GetLineNumber();
            // WriteLn('*** FOUND ***');
            break; // We found the hostname we searched for, break this loop.
        end; // of if
    until tf.GetEof();

    tf.CloseFile();
    FindHostInClass := result;
end; // of function FindHostInClass()



function FindServerClassInConf(pathServerClassConf: AnsiString; searchForServerClass: AnsiString): Integer;
{
    Find the serverClass in the pathServerClassConf

    pathServerClassConf
    searchForServerClass

    Returns the line number when found.
}
var
    tf: CTextFile;
    buffer: AnsiString;
begin
    Result := 0;
  
    tf := CTextFile.Create(pathServerClassConf);
	tf.OpenFileRead();
    
    repeat
        buffer := tf.ReadFromFile();
        
        //DebugWriteLn(IntToStr(tf.GetLineNumber) + ':' + TAB + buffer);

        if Pos('[serverClass:' + searchForServerClass + ']', buffer) > 0 then
        begin
            Result := tf.GetLineNumber();
        
            break; // We found the server class we searched for, break this loop. result = line number when found.
        end; { of if }
     until tf.GetEof();

    tf.CloseFile();
    FindServerClassInConf := Result;
end; { of function FindServerClassInConf() }



procedure ProcessServerClass(pathServerClass: AnsiString; serverClass: AnsiString; listType: AnsiString; action: AnsiString; hostName: AnsiString);
//
//  Process the original config file: 
//  1) Copy the original file to a work file, this is what we read.
//  2) Read the backup file and write modifictions to the orginal name.
//
var
    tfServerClass: TextFile;
    tfWork: TextFile;

    l: Integer;
    pathWork: AnsiString;
    bufferRead: AnsiString;
    bufferWrite: AnsiString;
    inServerClass: Boolean;
    inListType: Boolean;
    listHighest: Integer;
    listCurrent: Integer;
    skipLine: Boolean;
begin
    Writeln('ProcessServerClass()' + pathServerClass + '  ' + serverClass + '  ' + listType + '  ' + action + '  ' + hostName);

    LogWrite('USERNAME=' + GetCurrentUser() +
        ' SERVERCLASS=' + serverClass +
        ' LISTTYPE=' + listType +
        ' ACTION=' + action +
        ' HOSTNAME=' + hostName +
        ' REFERENCE=' + reference);

    // Create a new file name as backup. serverclass.conf --> serverclass.conf.$WORK
    pathWork := pathServerClass + '.$WORK';
    CopyTheFile(pathServerClass, pathWork);

    // Open the back file and write the modifications into the originial .conf file.
    AssignFile(tfWork, pathWork);
    Reset(tfWork);

    // Open the config file to write to.
    AssignFile(tfServerClass, pathServerClass);
    ReWrite(tfServerClass);
    bufferWrite := '';

    l := 0;
    inServerClass := false;
    inListType := false;
    skipLine := false;
    listHighest := -1; // Higest found number of a list type; can be 0 for first entry. 'whitelist.0 = hostname'
    listCurrent := -1;
   
    while not eof(tfWork) do
    begin
        ReadLn(tfWork, bufferRead);
        Inc(l);

        if Pos('[serverClass:' + serverClass + ']', bufferRead) > 0 then
        begin
            // Found the server class
            inServerClass := true;
        end; // of if 

         if (inServerClass = true) and (Pos(listType, bufferRead) > 0) then
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
            
            if UpperCase(action) = 'ADD' then
            begin
                // Add a new system to the list type.
                bufferWrite := listType + '.' + IntToStr(listHighest + 1) + ' = ' + hostName;
                WriteLn('Adding new line to ', pathServerClass, ': ', bufferWrite);
                WriteLn(tfServerClass, bufferWrite);

            end; // of if

            listHighest := 0; // Number of list will be 0 for the first
        end; // of if 

        if (inServerClass = true) and (inListType = true) and (Pos(hostName, bufferRead) > 0) and (UpperCase(action) = 'DEL') then
        begin
            // When in the serverclass and the host is found and you need to delete it.
            WriteLn('Delete this host: ', hostName);
            skipLine := true;
        end;
        
        //WriteLn(l:4, ': INSC=', inServerClass:5, ' INLT=', inListType:5, ' LH=', listHighest:3, ' > ', bufferRead);
        bufferWrite := bufferRead;
        if skipLine = true then
        begin 
            skipLine := false; // and back to false for the next line.
        end
        else
            WriteLn(tfServerClass, bufferWrite);
        begin
        
        end;
    end; // of while

    CloseFile(tfServerClass);

    CloseFile(tfWork);
end; // of procedure ProcessServerClass


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
    pathServerClassBackup: AnsiString;
begin
    directoryBackup := ReadSettingKey(ParamStr(0) + '.conf', 'Settings', 'DirBackup');

    WriteLn('directoryBackup=', directoryBackup);
    WriteLn('pathServerClass=', pathServerClass);

    if ForceDirectories(directoryBackup) = true then
        LogWrite('Created the directory for backups ' + directoryBackup);

    pathServerClassBackup := directoryBackup + '/serverclass.conf.' + IntToStr(DateTimeToUnix(Now())) + '.scmod';

    Writeln('Create backup of the Server Class:', pathServerClass + '  >>  ', pathServerClassBackup);
    LogWrite('Create backup of ' + pathServerClass + ' to backup file ' + pathServerClassBackup);
    
    CopyTheFile(pathServerClassConf, pathServerClassBackup);
end; // of procedure MakeBackupServerClass() 



function GetReferenceFromPath(path: AnsiString): AnsiString;
//
// Returns the reference from the path of the modify file.
//
//  path: /dir/dir/dir/<reference>.csv
//
// Returns: reference path of the file.
//
var
    r: AnsiString;
begin
    r := ExtractFileName(path); // From Sysutils
    r := LeftStr(r, Pos('.', r) - 1);

    Writeln('GetReferenceFromPath()=', r);
    GetReferenceFromPath := r;
end; // of function GetReferenceFromPath



procedure ProcessLineFromModifyFile(pathServerClass: AnsiString; serverClass: AnsiString; listType: AnsiString; action: AnsiString; hostName: AnsiString);
{
    Process a line from the modify file


    pathServerClass
    serverClass
    listType
    action
    hostName
}
begin
    DebugWriteLn('ProcessLineFromModifyFile() ' + pathServerClass + TAB + serverClass + TAB + listType + TAB + action + TAB + hostName);

    if FindServerClassInConf(pathServerClass, serverClass) > 0 then
    begin
        WriteLn('Found ', serverClass, ' in ', pathServerClass);
    end
    else
    begin
        WriteLn('NOT FOUND ', serverClass, ' in ', pathServerClass);
    end; { of if FindServerClassInConf }

    Writeln; { add an empty line to the screen between each processed lines. }
end; { of procedure ProcessLineFromModifyFile() }

procedure ProgUsage();
begin
    writeln('Usage: scmod <modifyfile>');
    writeln('  <modifyfile>     File with the modify information.');
    writeln();
    writeln('The modify file is a CSV formated file.');
    writeln('Format:');
    writeln('<serverclass>;<listtype>;<action>;<hostname>');
    writeln(TAB, 'serverclass = the name of the server class to modify');
    writeln(TAB, 'listtype = whitelist or blacklist');
    writeln(TAB, 'action = add or del');
    writeln(TAB, 'hostname = The name of the host, use wildcard at the end');
    Writeln();
    
    Halt; // Stop the program.
end; // of procedure ProgUsage()



function GetConfigPath(): AnsiString;
{
    Get the path to the config file.
}
begin
    GetConfigPath := ParamStr(0) + '.conf';
end; { of function GetConfigPath() }



procedure ProgInit();
begin
    ProgTitle();

    if ParamCount <> 1 then
        ProgUsage(); // Show program usage, close after showing usage.
        
    LogOpen();
    
    // We have a param string; must be the path to the Modigy file.
    pathModify := ParamStr(1);
    reference := GetReferenceFromPath(pathModify);

    debugModeOn := StrToInt(ReadSettingKey(GetConfigPath(),'Settings', CONF_DEDUG_MODE_ON));
    if debugModeOn = 1 then
        WriteLn('DebugModeOn = ON; set in scmod.conf (DebugMode=1)');
end; // of procedure ProgInit()



procedure ProgTest();
begin
    //WriteLn(FindHostInClass('serverclass.test', 'sc_testserverclass', 'whitelist', 'servertobefound*'));
    //WriteLn(FindServerClassInConf('serverclass.test', 'sc_testserverclass'));

    //WriteLn(FindServerClassInConf('serverclass.test', 'sc_nowheretobefound'));
 
    //WriteLn(FindHostInClass(pathServerClassConf, 'nottobefound*'));
    WriteLn(GetConfigPath());
end; // of procedure ProgTest()



procedure ProgRun();
//
// pathModify:     Path to the Modify file.
//
var
    tfm: CTextFile;
    line: AnsiString;
    s: TStringArray;
    serverClass: AnsiString;
    listType: AnsiString;
    action: AnsiString;
    hostName: AnsiString;
    foundHostAt: Integer;
begin
    //    MakeBackupServerClass();

    pathServerClassConf := ReadSettingKey(GetConfigPath(),'Settings', CONF_PATH_SERVER_CLASS_CONF);

    foundHostAt := 0;

    WriteLn(pathModify);
    tfm := CTextFile.Create(pathModify);
    tfm.OpenFileRead();
    Writeln('The status of ' + tfm.GetPath + ' is ' + BoolToStr(tfm.GetStatus, 'OPEN', 'CLOSED'));
    repeat
        line := tfm.ReadFromFile();
        //DebugWriteLn(IntToStr(tfm.GetLineNumber()) + ': ' + line);
        
        SetLength(s, 0);
        s := SplitString(line, ';');

        serverClass := s[0];
        listType := s[1];
        action := s[2];
        hostName := s[3];

        ProcessLineFromModifyFile(pathServerClassConf, serverClass, listType, action, hostName);

        SetLength(s, 0); // Set the array to 0 after use.
      
    until tfm.GetEof();
    tfm.CloseFile();
end; // of procedure ProgRun()



procedure ProgDone();
begin
    LogClose();
    
    WriteLn;
    Writeln('splunk reload deploy-server -class '); // + serverClass);
    WriteLn;
end; // of procedure ProgDone()



begin
    ProgInit();
    ProgRun();
    //ProgTest();
    ProgDone();
end. // of program ServerClassModifier