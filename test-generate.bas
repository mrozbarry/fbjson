#include "fbJSON.bi"

? "**********************************************************************"
? "  Test #1 - Generate a complete virtual json model"
? "**********************************************************************"

dim doc as fbJSON ptr = new fbJSON()

' To be a valid docutment, the root either needs to be ->asObject() or ->asArray
doc->asObject( )

' Add an array of names
doc = doc->appendChild( new fbJSON( "Name" ) )
doc->asArray()

' Create a list of 5 random names
dim nameList(1 to 5) as string = { "Ella", "Marly", "Chris", "John", "Melissa" }
' Loop through the names, adding each one as a new String
for n as integer = 1 to 5
	dim docName as fbJSON ptr = NULL
	docName = new fbJSON( ) ' Since we're in an array, names are ignored, so I used the default constructor
	docName->asString( nameList( n ) ) ' Set as a string variable with a name as it's text
	doc->appendChild( docName ) ' Add it to our virutal document
next n
' Return to parent node
doc = doc->parent

' Create a new boolean node called awesome
dim docAwesome as fbJSON ptr = new fbJSON( "Awesome" )
docAwesome->asBool(1) ' Could also be written as docAwesome->asTrue(); ->asBool(is_true) is just a convenience function
doc->appendChild( docAwesome )

fbJSON_ExportFile( doc, "generated.json" )

fbJSON_Delete( doc )

? "**********************************************************************"
? "  Test #2 - Opening generated json file"
? "**********************************************************************"
 dim test as fbJSON ptr = fbJSON_ImportFile( "generated.json" )
 
 if test = NULL then
	? "Unable to load json file/string"
	end 1
end if

print fbJSON_ExportString( test, 1 )
print "----"
print !"As a test, object->childByName('Awesome') should equal 'true'; =" & test->childByName("Awesome")->toBoolStr( )

fbJSON_Delete( test )
