program ServerClassModifier;
{
    Server Class Modifier for Splunk
    
    Modify the Splunk serverclass.conf

    Version 3.0

    Reads the csv file with modify instructions.

    File: mods12345.csv (seperated by semi-colon chars (;))
    serverclass;listtype;action;hostname

    Example:

    sc_testmod;whitelist;add;lsrvtest01*
    sc_testmod;whitelist;add;lsrvtest02*
    sc_testmod;whitelist;add;lsrvtest03*
}



{$MODE DELPHI}



uses
      USupLib;


var
    gPathConfig: Ansistring;



procedure ProgInit();
begin
    gPathConfig := GetConfigPath();
    WriteLn(gPathConfig);
end; { of procedure ProgInit() }



procedure ProgRun();
begin
    
end; { of procedure ProgRun() }



procedure ProgDone();
begin
    
end; { of procedure ProgDone() }



{ Main }
begin
    ProgInit();
    ProgRun();
    ProgDone();
end. { of program ServerClassModifier }