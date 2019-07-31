
program ges;

uses
    SysUtils;

var
    i: integer;


function GetCurrentUser(): AnsiString;
//
// Get the current user on a Linux system.
//
// Reference: https://forum.lazarus.freepascal.org/index.php?topic=4474.0
//
var
    es: AnsiString;
    r: AnsiString;
    p: Integer;
    s: AnsiString;
begin
    s := 'USER=';

    for i := 0 to 80 do
    begin
        es := SysUtils.getEnvironmentString(i);
        p := Pos(s, es); 
        If p > 0 then
        begin
            r := RightStr(es, Length(es) - Length(s));
            Exit(r)
        end; // of if

    end; // end for
end; // of function GetCurrentUser()

begin
    WriteLn('GetCurrentUser()=[', GetCurrentUser(), ']');

end.