#include "fbJSON.bi"

color 15

? "**********************************************************************"
? "  Unit test -> utf-8"
? "**********************************************************************"

Dim As UTF8String text(1 To 3) = { _
	!"test string", _
	!"whoah, look at this greek letter \u038f", _
	!"abcdefghijklmnopqrstuvwxyz1234567890" _
}

For i As Integer = LBound(text) to UBound(text)
	Dim As UTF8String utf8 = type<UTF8String>(text(i))
	? "Test case string"
	color 3
	? text(i).ascii
	color 7
	? "Output after parse"
	color 3
	? utf8.ascii()
	color 7
	
	dim as ubyte success = 1
	
	? "* Grabbing characters 3-5 (2 characters)"
	? "Test case string"
	color 3
	? mid( text(i), 3, 2 )
	color 7
	? "Output after parse"
	color 3
	? utf8.mid(3, 2)
	color 7
	
	success And= (mid( text(i), 3, 2 ) = utf8.mid(3, 2).ascii())
	
	? "* Grabbing LEFT() 5 characters"
	? "Test case string"
	color 3
	? left( text(i), 5 )
	color 7
	? "Output after parse"
	color 3
	? utf8.left( 5 )
	color 7
	
	success And= (left( text(i), 5 ) = utf8.left( 5 ).ascii())
	
	? "* Grabbing RIGHT() 5 characters"
	? "Test case string"
	color 3
	? right( text(i), 5 )
	color 7
	? "Output after parse"
	color 3
	? utf8.right( 5 )
	color 7
	
	success And= (right( text(i), 5 ) = utf8.right( 5 ).ascii())
	
	? "Test Status       : ";
	success And= (text(i) = utf8.ascii())
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
	? "Test case string"
	color 3
	? testStrings(i)
	Dim jsonString As String = fbJSON_ExportString( json, 0 )
	color 7
	? "Output after parse"
	color 3
	? jsonString
	color 7
	? "Test Status       : ";
	If testStrings(i) = jsonString Then
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

