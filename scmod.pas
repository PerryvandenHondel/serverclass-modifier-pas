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



procedure ProgLogInit();
var
    pathLog: Ansistring;
begin
    pathLog := ReadSettingKey(GetConfigPath(), CONF_SETTINGS, CONF_PATH_LOG);
    WriteLn('pathLog=', pathLog);

    gTextFileLog := CSplunkLog.CreateTheFile(pathLog);
	gTextFileLog.OpenFileForWrite();
    
end; { of procedure ProgLogInit() }



procedure ProgLogDone();
begin
    gTextFileLog.CloseTheFile();
end; { of procedure ProgLogDone() }



procedure ProgInit();
begin
    { Get the config file of this application. }
    gPathConfig := GetConfigPath();
    WriteLn(gPathConfig);

    ProgLogInit();
    
    gPathServerClass := 

end; { of procedure ProgInit() }



procedure ProgRun();
begin
    
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