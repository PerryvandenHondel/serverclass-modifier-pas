program ServerClassModifier;
{
    Server Class Modifier for Splunk
    
    Modify the Splunk serverclass.conf

    Version 3.0

    Reads the csv file with modify instructions.

    File: mods12345.csv (seperated by semi-colon chars (;))
    serverclass;listtype;action;hostname

    Example:

    add;sc_testmod;whitelist;lsrvtest01*
    add;sc_testmod;whitelist;lsrvtest02*
    del;sc_testmod;whitelist;lsrvtest03*


    Application Flow
    ================

    MAIN
        ProgInit
            ProgLogInit
            ProgTitle
        ProgRun
            ProgUsage
            ModifyServerClass
                ProcessServerClassLine
        ProgDone
            ProgLogDone

}



{$MODE DELPHI}



uses
    SysUtils,
    UBuild,
    USplunkLog,
    USupLib,
    UTextFile;



const
    CONF_SETTINGS = 'Settings'; { The default part of the .conf file. }
    CONF_PATH_SERVERCLASS = 'PathServerClass'; { The path to the server class in the .conf of the application. }
    CONF_PATH_LOG = 'PathLog'; { The path to the applications log file in the config file. }


var
    gPathConfig: Ansistring; { Global path of the applications path \dir\dir\file.conf. }
    gPathServerClass: Ansistring; { Global path of Splunk's serverclass.conf file. }
    gTextFileLog: CSplunkLog; { Class from USplunkLog. }
    gSessionId: Ansistring; { Use a unique Session ID for each run. }
    gPathModify: Ansistring; { File with the modifications for the serverclass.conf. }
    gReference: Ansistring; { Get the reference from the path of the modify file. }



function GetReferenceFromPath(path: AnsiString): AnsiString;
{
    Returns the reference from the path of the modify file.

    path: /dir/dir/dir/<reference>.csv

    Returns: <reference> path of the file.
}
var
    r: AnsiString;
begin
    r := ExtractFileName(path); // From Sysutils
    r := LeftStr(r, Pos('.', r) - 1);

    // Writeln('GetReferenceFromPath()=', r);
    GetReferenceFromPath := UpperCase(r);
end; // of function GetReferenceFromPath


function GetServerClass(buffer: Ansistring): Ansistring;
begin
    
end; { of function GetServerClass() }


procedure ProcessServerClassLine(buffer: Ansistring);
var
    currentServerClass: Ansistring;
begin
    WriteLn('ProcessServerClassLine(): buffer=[', buffer, ']');
    WriteLn(Occurs(buffer, ':'));
    WriteLn(Pos('[serverClass:', buffer));
    //if Occurs(':', buffer)

    if (Occurs(buffer, ':') = 1) and (Pos('[serverClass:', buffer) > 0) then
    begin
        { We find the line of the server class [serverClass:thisisaserverclass] }
        WriteLn('Found serverclass with hostnames!!')
    end; { of if }

    WriteLn();

end; {of procedure ProcessServerClassLine() }


procedure ModifyServerClass(pathServerClass: Ansistring);
var
    textFileServerClass: CTextFile;
    buffer: Ansistring;
begin
    WriteLn('ModifyServerClass() pathServerClass=', pathServerClass);

    textFileServerClass := CTextFile.CreateTheFile(pathServerClass);
	textFileServerClass.OpenFileForRead();
        
    repeat
        buffer := textFileServerClass.ReadFromFile();
        //WriteLn(textFileServerClass.GetLineNumber(), ': ', buffer);
        ProcessServerClassLine(buffer);

    until textFileServerClass.GetEof();

    textFileServerClass.CloseTheFile();

end; {of procedure ModifyServerClass() }


procedure ProgLogInit();
var
    pathLog: Ansistring;
begin
    pathLog := ReadSettingKey(gPathConfig, CONF_SETTINGS, CONF_PATH_LOG);
    WriteLn('pathLog=', pathLog);

    gTextFileLog := CSplunkLog.CreateTheFile(pathLog);
	gTextFileLog.OpenFileForWrite();

    gTextFileLog.SetDate();
	gTextFileLog.SetStatus(USPLUNKLOG_LOGLEVEL_INFO);
    gTextFileLog.AddKey(USPLUNKLOG_COMPONENT, 'scmod');
    gTextFileLog.AddKey(USPLUNKLOG_SESSION, gSessionId);
	gTextFileLog.AddKey(USPLUNKLOG_ACTION, 'started');
	gTextFileLog.WriteLineToFile();
end; { of procedure ProgLogInit() }



procedure ProgLogDone();
begin
    gTextFileLog.SetDate();
	gTextFileLog.SetStatus(USPLUNKLOG_LOGLEVEL_INFO);
    gTextFileLog.AddKey(USPLUNKLOG_COMPONENT, 'scmod');
    gTextFileLog.AddKey(USPLUNKLOG_SESSION, gSessionId);
	gTextFileLog.AddKey(USPLUNKLOG_ACTION, 'ended');
	gTextFileLog.WriteLineToFile();

    gTextFileLog.CloseTheFile();
end; { of procedure ProgLogDone() }



procedure ProgTitle();
begin
    writeln();
    writeln('scmod v', VERSION_MAJOR,'.', VERSION_MINOR, '.', BUILD, ' - Splunk serverclass.conf modifier, add or delete hosts from a server class in the whitelist or blacklist section.');
    writeln();
end; { of procedure ProgTitle() }



procedure ProgUsage();
begin
    writeln('Usage: scmod <modifyfile>');
    writeln('  <modifyfile>     File with the modify information.');
    writeln();
    writeln('The modify file is a CSV formated file.');
    writeln('Format:');
    writeln('<action>;<serverclass>;<listtype>;<hostname>');
    writeln(CHAR_TAB, 'action = add or del');
    writeln(CHAR_TAB, 'serverclass = the name of the server class to modify');
    writeln(CHAR_TAB, 'listtype = whitelist or blacklist');
    writeln(CHAR_TAB, 'hostname = The name of the host, use wildcard at the end');
    Writeln();
 end; { of procedure ProgUsage() }



procedure ProgInit();
begin
    gSessionId := RandomString(32); { Generate a unique Session ID for each run. }
    
    { Get the config file of this application. }
    gPathConfig := GetConfigPath();
    WriteLn(gPathConfig);

    ProgLogInit();

    ProgTitle();

end; { of procedure ProgInit() }



procedure ProgRun();
begin
     // We have a param string; must be the path to the Modigy file.
    if ParamCount <> 1 then
    begin
        ProgUsage();
    end; { of if }

    if ParamCount = 1 then
    begin
        gPathModify := ParamStr(1);
        gReference := GetReferenceFromPath(gPathModify);
        Writeln('gReference=', gReference);

        gPathServerClass := ReadSettingKey(gPathConfig, CONF_SETTINGS, CONF_PATH_SERVERCLASS);
        WriteLn('gPathServerClass=', gPathServerClass);

        ModifyServerClass(gPathServerClass); 

    end; { of if }
end; { of procedure ProgRun() }



procedure ProgDone();
begin
    ProgLogDone();
end; { of procedure ProgDone() }



{ Main }
begin
    ProgInit();
    ProgRun();
    ProgDone();
end. { of program ServerClassModifier }