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
''	UTF8 String Handling
''

type UTF8Char
	declare constructor()
	declare constructor(byref character as string)
	declare constructor(byval rawUtf8 as ubyte ptr)
	declare destructor()
	
	''declare operator = ( byval rhs as UTF8Char ) as ubyte
	
	declare sub assign(byval rawUtf8 as ubyte ptr)
	declare function isAscii() as ubyte
	
	as ubyte ptr			bytes
	as uinteger			length
end type

declare operator=(byval lhs as UTF8Char, byval rhs as UTF8Char) as ubyte
declare operator<>(byval lhs as UTF8Char, byval rhs as UTF8Char) as ubyte

type UTF8String
public:
	declare constructor()
	declare constructor(byval chars as ubyte ptr, byval len_ as integer)
	declare constructor(byref copyFrom as string)
	declare constructor(byref copyFrom as zstring ptr, byval len_ as integer)
	declare destructor()
	
	declare operator cast() as string
	declare operator let( byref fbstr as string )
	
	declare function ascii(byref unsupportedCharsAs as string = "?") as string
	declare function utf8(byval exported as ubyte ptr) as uinteger
	
	declare function length() as uinteger
	
	declare function at(byval index as integer) as UTF8Char
	declare function mid(byval start as integer, byval len as integer = -1) as UTF8String
	declare function left(byval len_ as integer = 1) as UTF8String
	declare function right(byval len_ as integer = 1) as UTF8String
	
	declare function instr(byref search as UTF8String, byval start as integer = 0) as integer
	declare function instr_any(byref search as UTF8String, byval start as integer = 0) as integer
	
	declare sub insert(byref text as UTF8String, byval pos as uinteger)
	declare sub prepend(byref text as UTF8String)
	declare sub append(byref text as UTF8String)
	
	declare function ltrim( byref trimMe as UTF8String, byval trimIndividualCharacters as ubyte = 1 ) as UTF8String
	declare function rtrim( byref trimMe as UTF8String, byval trimIndividualCharacters as ubyte = 1 ) as UTF8String
	declare function trim( byref trimMe as UTF8String, byval trimIndividualCharacters as ubyte = 1 ) as UTF8String
	
	declare function equals( byval comparison as UTF8String ) as ubyte
	
private:
	declare function determinePosition( byval p as integer ) as uinteger
	
	as UTF8Char ptr ptr		m_string
	as uinteger				m_length
end type

declare operator=(byval lhs as UTF8String, byval rhs as UTF8String) as ubyte
declare operator<>(byval lhs as UTF8String, byval rhs as UTF8String) as ubyte


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
	declare constructor( byref stringVal as UTF8String )
	declare constructor( byval doubleVal as double )
	''declare constructor( byref newName as string ) '' Todo: remove this as a contructor to name the node, and create a set of constructors
	declare destructor()
	
	' Build this node as a new JSON data type
	' Code:
	' Dim json As fbJSON
	' json.asNumber( 100 ) ' Sets it as a number
	' ? json.toNumber() ' Prints 100
	' ? json.toString() ' Also prints 100
	' ? json.toBool() ' Prints 1 (true), any non-zero value is true.
	' ? json.toBoolStr() ' Prints true
	' json.asString( "Test" ) ' Changes to a string
	' json.asArray() ' Now the object is ready to accept array values
	' json.appendChild( new fbJSON() )->asString( "foo" )
	' json.appendChild( new fbJSON() )->asString( "bar" )
	' json.appendChild( new fbJSON() )->asNumber( 1234 )
	' ? fbJSON_ExportString( json, 0 ) ' should output ["foo","bar",1234]
	' json.asObject()
	' json.appendChild( new fbJSON("1") )->asString( "foo" )
	' json.appendChild( new fbJSON("test") )->asString( "bar" )
	' json.appendChild( new fbJSON("ooga booga") )->asNumber( 1234 )
	' ? fbJSON_ExportString( json, 0 ) ' should output {"1":"foo","test":"bar","ooga booga":1234}
	
	declare sub asNull()
	declare sub asTrue()
	declare sub asFalse()
	declare sub asBool( byval is_true as ubyte )
	declare sub asNumber( byval value as double )
	declare sub asString( byval strValue as UTF8String )
	declare sub asArray()
	declare sub asObject()
	
	'' Set or retrieve the node name
	declare property identifier() as UTF8String
	declare property identifier( byref newName as UTF8String )
	
	'' Get a child by its name
	declare function childByName( byref cName as UTF8String ) as fbJSON ptr
	declare function c( byref cName as UTF8String ) as fbJSON ptr
	'' Get a child by its index (for use with arrays)
	declare function childByIndex( byval childIndex as integer ) as fbJSON ptr
	
	'' Convert the value to a string (if datatype is not a string, returns [Error:<Datatype>]"
	declare function toString() as UTF8String
	'' Convert the value to a number/double (if datatype is not a number, returns 0)
	declare function toNumber() as double
	'' Get boolean value (0 = false, 1 = true); (if datatype is not a boolean, always returns 0)
	declare function toBool() as ubyte
	'' Get string representation of boolean value (0 = "false", 1 = "true"); (if datatype is not a boolean, always returns 0 ["false"])
	declare function toBoolStr() as UTF8String
	
	'' Check data types
	declare function getType() as JSON_TYPES
	declare function getTypeName() as UTF8String
	
	'' Get the number of children; if check_nested = 1, then this returns all the children and subchildren
	declare function numChildren( byval check_nested as ubyte = 0 ) as integer
	declare function arraySize( ) as integer
	
	'' Add newChild to this node, newChild is always placed at the back of the list
	declare function appendChild( byval newChild as fbJSON ptr ) as fbJSON ptr
	' ToDo: Add a series of appendChild overloads that take the various constructor parameters similar to the as<Type> calls.
	
	'' Remove a node from childNode list, but don't delete it
	declare function removeChild overload ( byval node as fbJSON ptr ) as fbJSON ptr
	declare function removeChild( byref strName as UTF8String ) as fbJSON ptr
	declare function removeChild( byval index as integer ) as fbJSON ptr
	'' Remove a node from childNode list, and delete it
	declare sub deleteChild overload ( byval node as fbJSON ptr )
	declare sub deleteChild( byref strName as UTF8String )
	declare sub deleteChild( byval index as integer )
	' Remove and delete all childnodes
	declare sub deleteChildren( )
	
	as fbJSON ptr		parent, firstChild, nextSibling
	
private:
	declare sub setData( byval text as string )
	declare sub unsetData()
	as JSON_TYPES		jType
	as UTF8String		_jIdentifier, _jData
	as double			jNum
end type

''
''	Standard functions for importing and exporting JSON data
''
''	These functions include the ability to import and export
''	JSON data through files and strings (ascii and unicode).
''	

declare function fbJSON_ImportFile( byref path as string, byval utf8 as ubyte = 0 ) as fbJSON ptr
declare function fbJSON_ImportString( byref dstr as UTF8String ) as fbJSON ptr

declare sub fbJSON_ExportFile( byval root as fbJSON ptr, byref path as string, byval utf8 as ubyte = 0 )
declare function fbJSON_ExportString( byval root as fbJSON ptr, byval useFormatting as ubyte = 0 ) as UTF8String

declare sub fbJSON_Delete( byref root as fbJSON ptr )

#define whitespace				chr(9)&chr(10)&chr(13)&chr(32)
const as string jsonSyntax =	":{}[],"

#endif /' __fbJSON '/
