#include "fbJSON.bi"

? "**********************************************************************"
? "  Test #1 - Reading json and saving"
? "**********************************************************************"

dim test as fbJSON ptr = fbJSON_ImportFile( "test1.json" )

if test = NULL then
	? "Unable to load json file/string!"
	end 1
end if

print fbJSON_ExportString( test, 1 )
fbJSON_ExportFile( test, "test1_clone.json" )

fbJSON_Delete( test )

? "**********************************************************************"
? "  Test #2 - Opening generated json file"
? "**********************************************************************"
 dim test2 as fbJSON ptr = fbJSON_ImportFile( "test1_clone.json" )
 
 if test2 = NULL then
	? "Unable to load json file/string"
	end 2
end if

print fbJSON_ExportString( test2, 1 )

fbJSON_Delete( test2 )
