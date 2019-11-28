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


const
    TAB = #9;

var
    pathServerClassConf: AnsiString;
    pathLog: AnsiString;
    log: CTextFile;
    pathModify: AnsiString;         // Path to the file with the attributes to modify.
    pathServerClass: AnsiString;
    reference: AnsiString;          // Reference; Under what reference the action is executed; ADO PBI (ADOP), ADO Task (ADOT), Change number
    dirTemp: AnsiString;            // Temp directory to store the temp work file.


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


function FoundHostInClass(pathServerClassConf: AnsiString; findInServerClass: AnsiString; findInType: AnsiString; hostName: AnsiString): Boolean;
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
    isFound := false;
    inServerClass := false;
    inType := false;

    WriteLn('FoundHostInClass(): ', findInServerClass, ' --> ', hostName);

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

        

        WriteLn(IntToStr(tf.GetCurrentLine()),':', TAB, 'SERVERCLASS=', inServerClass, TAB, 'TYPE=', inType, TAB, 'FOUND=', isFound, TAB, buffer);

    until tf.GetEof();

    tf.CloseFile();
    FoundHostInClass := isFound;
end; 

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


procedure ProgUsage();
begin
    writeln('Usage: scmod <modifyfile>');
    writeln('  <modifyfile>     File with the modify information.');
    writeln();
    writeln('The modifyfile is a CSV formarted file.');
    writeln('Format:');
    writeln('<serverclass>;<listtype>;<action>;<hostname>');
    writeln(TAB, 'serverclass = the name of the server class to modify');
    writeln(TAB, 'listtype = whitelist or blacklist');
    writeln(TAB, 'action = add or del');
    writeln(TAB, 'hostname = The name of the host, use wildcard at the end');
    Writeln();
    
    Halt; // Stop the program.
end; // of procedure ProgUsage()


procedure ProgInit();
begin
    ProgTitle();

    if ParamCount <> 1 then
        ProgUsage(); // Show program usage, close after showing usage.
    
    // We have a param string; must be the path to the Modigy file.
    pathModify := ParamStr(1);

    reference := GetReferenceFromPath(pathModify);

    LogOpen();

    pathServerClassConf := ReadSettingKey(ParamStr(0) +'.conf','Settings', 'PathServerClass');
    dirTemp := ReadSettingKey(ParamStr(0) +'.conf','Settings', 'dirTemp');

    Writeln('Server class location: ', pathServerClassConf);
    Writeln('Temp directory: ', dirTemp);
    Writeln('Modify file: ', pathModify);

end; // of procedure ProgInit()


procedure ProgTest();
begin

    WriteLn(FoundHostInClass('serverclass.test', 'sc_testserverclass', 'whitelist', 'servertobefound*'));
    //WriteLn(FoundHostInClass(pathServerClassConf, 'nottobefound*'));
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
begin
    MakeBackupServerClass();


    WriteLn(pathModify);
    tfm := CTextFile.Create(pathModify);
    tfm.OpenFileRead();
    Writeln('The status of ' + tfm.GetPath + ' is ' + BoolToStr(tfm.GetStatus, 'OPEN', 'CLOSED'));
    repeat
        line := tfm.ReadFromFile();
        WriteLn(IntToStr(tfm.GetCurrentLine()) + ': ' + line);
        
        SetLength(s, 0);
        s := SplitString(line, ';');

        serverClass := s[0];
        listType := s[1];
        action := s[2];
        hostName := s[3];

        ProcessServerClass(pathServerClassConf, serverClass, listType, action, hostName);
        
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

{
procedure ProgRunOld();
begin
    Sleep(1001);

    if ParamCount <> 4 then
        ProgUsage();

    // Get the parameters from the command line.
    //paramServerClass := ParamStr(1);
    //paramListType := ParamStr(2);
    //paramAction := ParamStr(3);
    //sparamHost := ParamStr(4);

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

    //WriteLn('Action on ', pathServerClassConf, ' in server class ', paramServerClass, ' for the ', paramListType,' to ', paramAction, ' ', paramHost);
    log.WriteToFile(GetCurrentDateTimeMicro() + 
        ' USERNAME=' + GetCurrentUser() +
        ' SERVERCLASS=' + paramServerClass +
        ' LISTTYPE=' + paramListType +
        ' ACTION=' + paramAction +
        ' HOSTNAME=' + paramHost);

   //ProcessServerClass(pathServerClassConf);

    log.CloseFile();
end; // of procedure ProgRunOld()
}

begin
    ProgInit();
    //ProgRun();
    ProgTest();
    ProgDone();
end. // of program ServerClassModifier