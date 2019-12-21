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
}



{$MODE DELPHI}



uses
    USplunkLog,
    USupLib;



const
    CONF_SETTINGS = 'Settings'; { The default part of the .conf file. }
    CONF_PATH_SERVERCLASS = 'PathServerClass'; { The path to the server class in the .conf of the application. }
    CONF_PATH_LOG = 'PathLog'; { The path to the applications log file in the config file. }


var
    gPathConfig: Ansistring; { Global path of the applications path \dir\dir\file.conf }
    gPathServerClass: Ansistring; { Global path of Splunk's serverclass.conf file. }
    gTextFileLog: CSplunkLog; { Class from USplunkLog }
    gSessionId: Ansistring; { Use a unique Session ID for each run }



procedure ProgLogInit();
var
    pathLog: Ansistring;
begin
    pathLog := ReadSettingKey(GetConfigPath(), CONF_SETTINGS, CONF_PATH_LOG);
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



procedure ProgInit();
begin
    gSessionId := RandomString(32); { Generate a unique Session ID for each run. }
    
    { Get the config file of this application. }
    gPathConfig := GetConfigPath();
    WriteLn(gPathConfig);

    ProgLogInit();
end; { of procedure ProgInit() }



procedure ProgRun();
begin
    WriteLn(RandomString(32));
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