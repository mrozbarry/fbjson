''''
''	FreeBASIC JSON Parser
''	"JSON is simple, so the interface should also be simple"
''
''	Copyright (c) 2011 Alex Barry
''
''	Permission is hereby granted, free of charge, to any person obtaining a copy
''	of this software and associated documentation files (the "Software"), to deal
''	in the Software without restriction, including without limitation the rights
''	to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
''	copies of the Software, and to permit persons to whom the Software is
''	furnished to do so, subject to the following conditions:
''
''	The above copyright notice and this permission notice shall be included in
''	all copies or substantial portions of the Software.
''
''	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
''	IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
''	FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
''	AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
''	LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
''	OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
''	THE SOFTWARE.
''

#ifndef __fbJSON
#define __fbJSON

#ifndef USE_DEBUG
'' Comment out to disable debug print output
#define USE_DEBUG	1
#endif

#ifndef NULL
#	define NULL	0
#endif

''
''	JSON Data Types
''
enum JSON_TYPES
	JSON_False = 0
	JSON_True
	JSON_Null
	JSON_Number
	JSON_String
	JSON_Array
	JSON_Object
end enum

''
''	fbJSON Type
''
''	fbJSON is a base-class for all data types available in
''	standard JSON.  It covers strings, numbers (as doubles),
''	arrays (with the ability to import FreeBASIC style arrays),
''	and JSON objects.  The methods should be fairly straight
''	forward, and anything that is unclear can be clarified in the
''	attached fbJSON.readme file.
''
''	This is essentially based around a linked-list implementation
''	which means there is a certain speed deficiency, but it makes
''	the code a little more elegant.
''

type fbJSON
public:
	' Default constructor is a blank, nameless node
	' Secondary constructor sets the name of the node
	' Destructor cleans up children
	declare constructor()
	declare constructor( byref newName as string )
	declare destructor()
	
	'' Build this node as a new JSON data type
	declare sub asNull()
	declare sub asTrue()
	declare sub asFalse()
	declare sub asBool( byval is_true as ubyte )
	declare sub asNumber( byval value as double )
	declare sub asString( byval strValue as string )
	declare sub asArray()
	declare sub asObject()
	
	'' Set or retrieve the node name
	declare property jName() as string
	declare property jName( byref newName as string )
	
	'' Get a child by its name
	declare function childByName( byref cName as string ) as fbJSON ptr
	'' Get a child by its index (for use with arrays)
	declare function childByIndex( byval childIndex as integer ) as fbJSON ptr
	
	'' Convert the value to a string (if datatype is not a string, returns [Error:<Datatype>]"
	declare function toString() as string
	'' Convert the value to a number/double (if datatype is not a number, returns 0)
	declare function toNumber() as double
	'' Get boolean value (0 = false, 1 = true); (if datatype is not a boolean, always returns 0)
	declare function toBool() as ubyte
	'' Get string representation of boolean value (0 = "false", 1 = "true"); (if datatype is not a boolean, always returns 0 ["false"])
	declare function toBoolStr() as string
	
	'' Check data types
	declare function getType() as JSON_TYPES
	
	'' Get the number of children; if check_nested = 1, then this returns all the children and subchildren
	declare function numChildren( byval check_nested as ubyte = 0 ) as integer
	
	'' Add newChild to this node, newChild is always placed at the back of the list
	declare function appendChild( byval newChild as fbJSON ptr ) as fbJSON ptr
	
	'' Remove a node from childNode list, but don't delete it
	declare function removeChild overload ( byval node as fbJSON ptr ) as fbJSON ptr
	declare function removeChild( byref strName as string ) as fbJSON ptr
	declare function removeChild( byval index as integer ) as fbJSON ptr
	'' Remove a node from childNode list, and delete it
	declare sub deleteChild overload ( byval node as fbJSON ptr )
	declare sub deleteChild( byref strName as string )
	declare sub deleteChild( byval index as integer )
	
	as fbJSON ptr		parent, firstChild, nextSibling
	
private:
	declare sub setData( byval text as string )
	declare sub unsetData()
	as JSON_TYPES		jType
	as zstring ptr		_jName, _jData
	as double			jNum
end type

''
''	Standard functions for importing and exporting JSON data
''
''	These functions include the ability to import and export
''	JSON data through files and strings (ascii and unicode).
''	

declare function fbJSON_ImportFile( byref path as string, byval utf8 as ubyte = 0 ) as fbJSON ptr
declare function fbJSON_ImportString( byref dstr as string ) as fbJSON ptr

declare sub fbJSON_ExportFile( byval root as fbJSON ptr, byref path as string, byval utf8 as ubyte = 0 )
declare function fbJSON_ExportString( byval root as fbJSON ptr, byval useFormatting as ubyte = 0 ) as string

declare sub fbJSON_Delete( byref root as fbJSON ptr )

#define whitespace				chr(9)&chr(10)&chr(13)&chr(32)
const as string jsonSyntax =	":{}[],"

#endif /' __fbJSON '/
