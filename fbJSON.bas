''
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

#include "fbJSON.bi"

#ifdef USE_DEBUG
#	define DEBUG_OUT( msg )	? msg
#else
#	define DEBUG_OUT( msg )
#endif

''**********************************************************************
''	fbJSON Internal Data
''**********************************************************************
type fbJSONToken
public:
	declare constructor()
	declare destructor()
	declare property blockString() as string
	declare property blockString( byref assign as string )
	lineNo		as integer
	linePos		as integer
private:
	_blockString	as zstring ptr
end type

constructor fbJSONToken()
	this._blockString = NULL
	this.lineNo = 0
	this.linePos = 0
end constructor

destructor fbJSONToken()
	if this._blockString <> NULL then deallocate this._blockString
end destructor

property fbJSONToken.blockString() as string
	if this._blockString <> NULL then return str( *this._blockString ) else return ""
end property

property fbJSONToken.blockString( byref assign as string )
	if this._blockString <> NULL then deallocate this._blockString
	DEBUG_OUT( "Assigning string to blockString: " + assign )
	this._blockString = callocate( sizeof( zstring ) * ( len( assign ) + 1 ) )
	for i as integer = 0 to len( assign ) - 1
		this._blockString[ i ] = assign[i]
	next i
	this._blockString[ len( assign ) ] = NULL
end property

''**********************************************************************
''	fbJSON Implementation
''**********************************************************************
constructor fbJSON()
	this.parent = NULL
	this.firstChild = NULL
	this.nextSibling = NULL
	this.jType = JSON_False
	this._jName = NULL
	this._jData = NULL
	this.jNum = 0.0
end constructor

constructor fbJSON( byref newName as string )
	this.parent = NULL
	this.firstChild = NULL
	this.nextSibling = NULL
	this.jType = JSON_False
	this._jName = callocate( sizeof( zstring ptr ) * 1 )
	this.jName = newName
	this._jData = NULL
	this.jNum = 0.0
end constructor

destructor fbJSON()
	'print "*** Destruction fbJSON '" & this.toString() & "'0x" & hex( cint( @this ) ) & " ***"
	if this.firstChild <> NULL then
		'print " -> fbJSON UDT '" & this.toString() & "'0x" & hex( cint( @this ) ) & " has children"
		delete this.firstChild
	end if
	if this.nextSibling <> NULL then
		'print " -> fbJSON UDT '" & this.toString() & "'0x" & hex( cint( @this ) ) & " has next sibling"
		delete this.nextSibling
	end if
	if this._jName <> NULL then deallocate this._jName
	if this._jData <> NULL then deallocate this._jData
end destructor

sub fbJSON.asNull()
	this.jType = JSON_Null
	this.setData( "null" )
end sub

sub fbJSON.asTrue()
	this.jType = JSON_True
	this.setData( "true" )
end sub

sub fbJSON.asFalse()
	this.jType = JSON_False
	this.setData( "false" )
end sub

sub fbJSON.asBool( byval is_true as ubyte )
	if is_true then this.asTrue() else this.asFalse()
end sub

sub fbJSON.asNumber( byval value as double )
	this.jType = JSON_Number
	this.setData( "<Number>" )
	this.jNum = value
end sub

sub fbJSON.asString( byval strValue as string )
	this.jType = JSON_String
	this.setData( strValue )
end sub

sub fbJSON.asArray()
	this.jType = JSON_Array
	this.setData( "<Array>" )
end sub

sub fbJSON.asObject()
	this.jType = JSON_Object
	this.setData( "<Object>" )
end sub

property fbJSON.jName() as string
	if this._jName = NULL then return ""
	return str( *this._jName )
end property

property fbJSON.jName( byref newName as string )
	if this._jName <> NULL then deallocate this._jName
	if len( newName ) > 0 then
		this._jName = callocate( sizeof( zstring ) * ( len( newName ) + 1 ) )
		for i as integer = 0 to len( newName ) - 1
			this._jName[ i ] = wchr( newName[i] )
		next i
		this._jName[ len( newName ) ] = NULL
	else
		this._jName = NULL
	end if
end property

function fbJSON.childByName( byref cName as string ) as fbJSON ptr
	dim node as fbJSON ptr = this.firstChild
	while node <> NULL
		if node->jName = cName then return node
		node = node->nextSibling
	wend
	return NULL
end function

function fbJSON.childByIndex( byval childIndex as integer ) as fbJSON ptr
	dim node as fbJSON ptr = this.firstChild
	dim c as integer = childIndex
	while ( node <> NULL ) and ( c > 0 )
		c -= 1
		node = node->nextSibling
	wend
	return node
end function

function fbJSON.toString() as string
	select case this.jType
	case JSON_String, JSON_True, JSON_False
		return str( *this._jData )
	case JSON_Number
		return str( this.jNum )
	case else
		return "Warning:" & str( *this._jData )
	end select
end function

function fbJSON.toNumber() as double
	if this.jType = JSON_Number then return this.jNum
	if this.jType = JSON_String then return cdbl( str( *this._jData ) )
	return 0.0
end function

function fbJSON.toBool() as ubyte
	if this.jType = JSON_True then
		return 1
	elseif this.jType = JSON_False then
		return 0
	end if
	'' Report an error?
	return 0
end function

function fbJSON.toBoolStr() as string
	if this.jType = JSON_True then
		return "true"
	elseif this.jType = JSON_False then
		return "false"
	end if
	'' Report an error?
	return "InvalidType"
end function

function fbJSON.getType() as JSON_TYPES
	return this.jType
end function

function fbJSON.numChildren( byval check_nested as ubyte = 0 ) as integer
	dim count as integer
	dim node as fbJSON ptr = this.firstChild
	count = 0
	while node <> NULL
		count = count + 1
		if ( check_nested = 1 ) then count += node->numChildren( 1 )
		node = node->nextSibling
	wend
	return count
end function

function fbJSON.appendChild( byval newChild as fbJSON ptr ) as fbJSON ptr
	if newChild = NULL then return NULL
	newChild->parent = @this
	newChild->nextSibling = NULL
	dim node as fbJSON ptr = this.firstChild
	if node = NULL then
		this.firstChild = newChild
	else
		do
			if node->nextSibling = NULL then
				node->nextSibling = newChild
				exit do
			end if
			node = node->nextSibling
		loop
	end if
	return newChild
end function

function fbJSON.removeChild overload ( byval node as fbJSON ptr ) as fbJSON ptr
	if node = NULL then return NULL
	
	dim n as fbJSON ptr = this.firstChild
	while ( n <> NULL )
		if n->nextSibling = node then
			n->nextSibling = node->nextSibling
			node->nextSibling = NULL
			return node
		end if
		n = n->nextSibling
	wend
	
	return NULL
end function

function fbJSON.removeChild( byref strName as string ) as fbJSON ptr
	return this.removeChild( this.childByName( strName ) )
end function

function fbJSON.removeChild( byval index as integer ) as fbJSON ptr
	return this.removeChild( this.childByIndex( index ) )
end function

sub fbJSON.deleteChild overload ( byval node as fbJSON ptr )
	dim n as fbJSON ptr
	n = this.removeChild( node )
	delete n '' n = node
end sub

sub fbJSON.deleteChild( byref strName as string )
	this.deleteChild( this.childByName( strName ) )
end sub

sub fbJSON.deleteChild( byval index as integer )
	this.deleteChild( this.childByIndex( index ) )
end sub

sub fbJSON.setData( byval text as string )
	this.unsetData()
	this._jData = callocate( sizeof( zstring ) * ( len( text ) + 1 ) )
	for i as integer = 0 to len( text ) - 1
		this._jData[ i ] = text[i]
	next i
	this._jData[ len( text ) ] = NULL
end sub

sub fbJSON.unsetData()
	if this._jData <> NULL then
		deallocate this._jData
	end if
	this._jData = NULL
end sub

''**********************************************************************
''	Internal Functions
''**********************************************************************

function fbJSON_DecodeUnicodeEscapes( byref ustring as string ) as string
	dim as string decoder( 1 to 8 ) = { chr(34), "\", "/", "b", "f", "n", "r", "t" }
	dim as string replace( 1 to 8 ) = { chr(34), "\", !"\/", !"\b", !"\f", !"\n", !"\r", !"\t" }
	dim as string processed = ustring
	dim p as integer
	for c as integer = 1 to 8
		do
			p = instr( processed, "\"&decoder(c) )
			if p = 0 then exit do
			processed = mid( processed, 1, p-1 ) & replace(c) & mid( processed, p+2 )
		loop until p <= 0
	next c
	do
		p = instr( processed, "\u" )
		if p = 0 then exit do
		processed = mid( processed, 1, p-1 ) & chr( val( "&h" & mid( processed, p+2, 4 ) ) ) & mid( processed, p+6 )
	loop
	return processed
end function

function fbJSON_EncodeUnicodeEscapes( byref ustring as string ) as string
	dim as string processed = ustring
	dim p as integer
	dim s as integer = 1
	do
		p = instr( s, processed, chr(34) )
		if p = 0 then exit do
		if mid( processed, p-1, 1 ) <> "\" then
			processed = mid( processed, 1, p-1 ) & "\" & chr(34) & mid( processed, p+1 )
		else
			s = p + 1
		end if
	loop
	return processed
end function

function fbJSON_Tokenizer( byref jsonString as string, tokens() as fbJSONToken ) as uinteger
	redim tokens(0) as fbJSONToken
	dim as integer startToken, tokenLen, inString=0, blockCount=0, l=1, p=1, maxLen
	maxLen = len( jsonString )
	
	startToken = 1
	do
		var c = mid( jsonString, startToken, 1 )
		tokenLen = 1
		if ( instr( c, any chr(9)&chr(32) ) = 0 ) then
			dim index as integer
			index = startToken + 1
			if c = chr(34) then
				DEBUG_OUT( "Found beginning of quote, searching for end" )
				do
					index = instr( index, jsonString, chr(34) )
					DEBUG_OUT( " - Starting at " + str( startToken ) + " and going to " + str( index ) )
				loop until mid( jsonString, index-1,1 ) <> "\"
			elseif instr( c, any jsonSyntax ) then
				index = startToken
			else
				index = instr( startToken+1, jsonString, any whitespace & jsonSyntax )-1
			end if
			tokenLen = ( index - startToken )+1
			var tokenString = trim( mid( jsonString, startToken, tokenLen ), any whitespace )
			if len( tokenString ) then
				redim preserve tokens(1 to ubound(tokens)+1) as fbJSONToken
				with tokens(ubound(tokens))
					.lineNo = l
					.linePos = p
					.blockString = fbJSON_DecodeUnicodeEscapes( tokenString )
				end with
			end if
		elseif ( instr( c, chr(10) ) ) then
			l += 1 : p = 1
		end if
		startToken += tokenLen
		p += tokenLen
	loop until ( startToken > maxLen ) or ( tokenLen <= 0 )
	return ubound(tokens)
end function

function fbJSON_Print( byval n as fbJSON ptr, byval level as integer, byval useIndents as ubyte = 1, byval escaped as ubyte = 0 ) as string
	dim result as string = ""
	dim indent as string = ""
	dim newline as string = ""
	
	if ( useIndents <> 0 ) And ( level > 0 ) then indent = string( level, !"\t" )
	if ( useIndents <> 0 ) then newline = chr(10)
	
	select case n->getType()
	case JSON_Object
		if len( n->jName ) > 0 then result &= indent & chr(34) & n->jName & chr(34) & ":{" & newline else result &= indent & "{" & newline
		var num = n->numChildren
		if num > 0 then
			for i as integer = 0 to num-1
				var comma = ","
				if i = (num-1) then comma = ""
				result &= indent & fbJSON_Print( n->childByIndex( i ), level + 1, useIndents, escaped ) & comma & newline
			next i
		end if
		result &= indent & "}"

	case JSON_Array
		if len( n->jName ) > 0 then result &= indent & chr(34) & n->jName & chr(34) & ":[" else result &= indent & "["
		var num = n->numChildren
		if num > 0 then
			for i as integer = 0 to num-1
				var comma = ","
				if i = (num-1) then comma = ""
				result &= indent & fbJSON_Print( n->childByIndex( i ), level, 0, escaped ) & comma
			next i
		end if
		result &= indent & "]"

	case JSON_String
		dim as string useStr = n->toString()
		if ( escaped <> 0 ) then useStr = fbJSON_EncodeUnicodeEscapes( useStr )
		if n->parent->getType() = JSON_Array then
			result &= chr(34) & useStr & chr(34)
		else
			result &= indent & chr(34) & n->jName & chr(34) & ":" & chr(34) & useStr & chr(34)
		end if

	case JSON_True, JSON_False
		if n->parent->getType() = JSON_Array then
			result &= n->toBoolStr()
		else
			result &= indent & chr(34) & n->jName & chr(34) & ":" & n->toBoolStr()
		end if

	case JSON_Null
		if n->parent->getType() = JSON_Array then
			result &= "null"
		else
			result &= indent & chr(34) & n->jName & chr(34) & ":" & "null"
		end if

	case JSON_Number
		if n->parent->getType() = JSON_Array then
			result &= n->toNumber()
		else
			result &= indent & chr(34) & n->jName & chr(34) & ":" & n->toNumber()
		end if
	end select
	if (level > 0 ) then result &= newline
	return result
end function

''**********************************************************************
''	Standard function implementations
''**********************************************************************
function fbJSON_ImportFile( byref path as string, byval utf8 as ubyte ) as fbJSON ptr
	dim as string dline, dstr
	dim as integer fh=freefile()
	dim encodingType as string = "ascii"
	if utf8 <> 0 then encodingType = "utf-8"
	open path for input encoding encodingType as #fh
	do until eof( fh )
		line input #fh, dline
		dstr &= dline & chr(10)
	loop
	close #fh
	return fbJSON_ImportString( dstr )
end function

function fbJSON_ImportString( byref jsonString as string ) as fbJSON ptr
	if len( trim( jsonString, any whitespace ) ) = 0 then return NULL
	dim as fbJSON ptr current = NULL
	redim as fbJSONToken tokens(0)
	dim as integer arrayCount = 0, objectCount = 0
	
	var num = fbJSON_Tokenizer( jsonString, tokens() )
	if num < 1 then
		return NULL
	end if
	
	for i as integer = 1 to num
		var lower = lcase( tokens(i).blockString )
		select case lower
		case "{"
			objectCount += 1
			if ( current = NULL ) then
				current = new fbJSON()
				current->asObject()
			elseif ( i > 2 ) then
				if ( ( tokens(i-1).blockString = ":" ) And ( mid( tokens(i-2).blockString, 1, 1 ) = chr(34) ) ) Or ( (instr( tokens(i-1).blockString, any "[," )>0) And current->getType() = JSON_Array ) then
					if current->getType() = JSON_Array then
						current = current->appendChild( new fbJSON( ) )
					else
						current = current->appendChild( new fbJSON( trim( tokens(i-2).blockString, chr(34) ) ) )
					end if
					current->asObject()
				else
					' Report some sort of error?
				end if
			end if
		case "["
			arrayCount += 1
			if ( current = NULL ) then
				current = new fbJSON()
				current->asArray()
			elseif ( i > 2 ) then
				if ( ( tokens(i-1).blockString = ":" ) And ( mid( tokens(i-2).blockString, 1, 1 ) = chr(34) ) ) Or ( (instr( tokens(i-1).blockString, any "[," )>0) And current->getType() = JSON_Array ) then
					if current->getType() = JSON_Array then
						current = current->appendChild( new fbJSON( ) )
					else
						current = current->appendChild( new fbJSON( trim( tokens(i-2).blockString, chr(34) ) ) )
					end if
					current->asArray()
				else
					' Report some sort of error?
				end if
			end if
		case "]"
			arrayCount -= 1
			if current->parent <> NULL then current = current->parent
		case "}"
			objectCount -= 1
			if current->parent <> NULL then current = current->parent
		case else
			if ( i > 2 ) then
				dim node as fbJSON ptr = NULL
				' Need more robust IF!!!
				if ( current <> NULL ) And ( ( tokens(i-1).blockString = ":" ) And ( mid( tokens(i-2).blockString, 1, 1 ) = chr(34) ) Or ( ( current->getType() = JSON_Array ) And ( instr( tokens(i-1).blockString, any ",[" ) ) ) ) then
					if lower = "true" then
						node = current->appendChild( new fbJSON( trim( tokens(i-2).blockString, chr(34) ) ) )
						node->asTrue()
					elseif lower = "false" then
						node = current->appendChild( new fbJSON( trim( tokens(i-2).blockString, chr(34) ) ) )
						node->asFalse()
					elseif lower = "null" then
						node = current->appendChild( new fbJSON( trim( tokens(i-2).blockString, chr(34) ) ) )
						node->asNull()
					elseif mid( lower, 1, 1 ) = chr(34) then
						node = current->appendChild( new fbJSON( trim( tokens(i-2).blockString, chr(34) ) ) )
						node->asString( trim( tokens(i).blockString, chr(34) ) )
					elseif instr( mid( lower, 1, 1 ), any "0123456789-+" ) then
						if current->getType() = JSON_Array then
							node = current->appendChild( new fbJSON( ) )
						else
							node = current->appendChild( new fbJSON( trim( tokens(i-2).blockString, chr(34) ) ) )
						end if
						node->asNumber( cdbl( tokens(i).blockString ) )
					else
						' Error?  Likely not.
						print "== Not handled! =="
						print " `-> " & tokens(i).blockString
					end if
				else
					' Error?  Likely not.
				end if
			end if
		end select
	next i
	
	return current
end function

sub fbJSON_ExportFile( byval root as fbJSON ptr, byref path as string, byval utf8 as ubyte = 0 )
	dim as string dline, dstr
	dim as integer fh=freefile()
	dim encodingType as string = "ascii"
	if utf8 <> 0 then encodingType = "utf-8"
	open path for output encoding encodingType as #fh
	print #fh, fbJSON_Print( root, 0, 0, 0 )
	close #fh
end sub

function fbJSON_ExportString( byval root as fbJSON ptr, byval useFormatting as ubyte = 0 ) as string
	return fbJSON_Print( root, 0, useFormatting )
end function

sub fbJSON_Delete( byref root as fbJSON ptr )
	if root <> NULL then
		delete root
		root = NULL
	end if
end sub
