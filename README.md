
[serverClass:sc_nexttest2]
whitelist.0 = servert01
blacklist.0 = servering922

Type
// Event Record type definition
	TEventRecord = record
		eventId: integer;
		description: string;
		count: integer;
		osVersion: word;
	end; // of record
	TEventArray = array of TEventRecord;

var
    eventArray: TEventArray;


size := Length(eventArray);

procedure EventRecordAdd(newEventId: integer; newDescription: string);
//
//
//	Add a new record in the array of Event
//  
//	newEventId      integer		The event id to search for
//	newDescription  string		Description of the event
//									
var
	size: integer;
begin
	size := Length(eventArray);
	SetLength(eventArray, size + 1);
	eventArray[size].eventId := newEventId;
	eventArray[size].description := newDescription;
end; // of procedure EventRecordAdd



procedure EventAndEventDetailsShow();
//
//	Show the Events and Events Details.
//
var
	i: integer;
	j: integer;
	t: Ansistring;
begin
	WriteLn();
	WriteLn('EVENTS TO PROCESS');
	WriteLn('=================');
	for i := 0 to High(eventArray) do
	begin
		//Writeln(IntToStr(i) + Chr(9) + ' ' + IntToStr(EventArray[i].eventId) + Chr(9), EventArray[i].isActive, Chr(9) + IntToStr(EventArray[i].osVersion) + Chr(9) + EventArray[i].description);
		Writeln(AlignRight(i, 6) + AlignRight(eventArray[i].eventId, 6) + '  ' + eventArray[i].description);
		
		for j := 0 to High(eventDetailArray) do
		begin
			if eventDetailArray[j].eventId = eventArray[i].eventId then
			begin
				t := '      ';
				t := t + AlignRight(j, 6);                                                      // Number of line
				t := t + ' ' + AlignRight(eventDetailArray[j].eventId, 6);                      // Number of event id
				t := t + ' ' + AlignLeft(eventDetailArray[j].keyName, 10);                      // Splunk Key Name
				t := t + ' ' + AlignRight(eventDetailArray[j].position, 3);                     // Position in the export file
				t := t + ' ' + AlignLeft(BoolToStr(eventDetailArray[j].IsString), 5);           // Boolean of type (TRUE=String/FALSE=Number)
				t := t + ' ' + AlignLeft(eventDetailArray[j].description, 50);                  // Descripion of field.
				WriteLn(t);
			end;
		end;
		
	end;
end; // of procedure EventAndEventDetailsShow	