#include "fbJSON.bi"

function determine_success( byref description as string, byref referenceString as string, byref testString as UTF8String ) as ubyte
	? description
	? " - Test case string"
	? "   `";
	color 3
	? referenceString ;
	color 7
	? "`"
	? "Output after parse"
	? "   `";
	color 3
	? testString.ascii() ;
	color 7
	? "`"
	return (referenceString = testString.ascii())
end function

color 15

? "**********************************************************************"
? "  Unit test -> utf-8"
? "**********************************************************************"

Dim As String text(1 To 3) = { _
	!"test string", _
	!"whoah, look at this greek letter \u038f", _
	!"abcdefghijklmnopqrstuvwxyz1234567890" _
}

For i As Integer = LBound(text) to UBound(text)
	Dim As UTF8String utf8 = type<UTF8String>(text(i))
	dim as ubyte success = determine_success( "Reference Output", text(i), utf8 )
	
	success And= determine_success( "Grabbing characters 3-5 (2 characters)", mid( text(i), 3, 2 ), utf8.mid(3, 2) )
	success And= determine_success( "Grabbing left(5)", left( text(i), 5 ), utf8.left( 5 ) )
	success And= determine_success( "Grabbing right(5)", right( text(i), 5 ), utf8.right( 5 ) )
	
	? "Test Status       : ";
	''success And= (text(i) = utf8.ascii())
	If  success Then
		color 10
		? "success"
	Else
		color 12
		? "fail"
	End If
	color 14
	? "----------------------------------------------------------------------"
	color 7
Next i


color 15

? "**********************************************************************"
? "  Unit test -> json"
? "**********************************************************************"

Dim As String testStrings(1 To 3) = { _
	!"{\"name\":[[[1,2,3,4],5,6],7,8],\"test\":[3,2,1],\"another test\":{\"5\":[1,1,0,1]}}", _
	!"[{\"hello\":1},{\"foo\":\"what_foo\"}]", _
	!"[[],[],{\"a\":\"b\"}]" _
}

For i As Integer = LBound(testStrings) to UBound(testStrings)
	Dim json As fbJSON ptr = fbJSON_ImportString( testStrings(i) )
	Dim jsonString As UTF8String = fbJSON_ExportString( json, 0 )
	dim success as ubyte = determine_success( "After parsing as JSON", testStrings(i), jsonString )
	? "Test Status       : ";
	If success Then
		color 10
		? "success"
	Else
		color 12
		? "fail"
	End If
	fbJSON_Delete( json )
	color 14
	? "----------------------------------------------------------------------"
	color 7
Next i

