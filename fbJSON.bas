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

''
''	UTF8 String Handling
''

constructor UTF8Char()
	bytes = NULL
	length = 0
end constructor

constructor UTF8Char(byref character as string)
	assign( cptr( ubyte ptr, strptr( character ) ) )
end constructor

constructor UTF8Char(byval rawUtf8 as ubyte ptr)
	assign( rawUtf8 )
end constructor

destructor UTF8Char()
	if bytes <> NULL then deallocate bytes
	bytes = NULL
	length = 0
end destructor

sub UTF8Char.assign(byval rawUtf8 as ubyte ptr)
	
	'' Clean up any character in the buffer
	if bytes <> NULL then
		deallocate bytes
		bytes = NULL
		length = 0
	end if
	
	'' Make sure we don't have a NULL buffer
	if rawUtf8 = NULL then return
	
	Dim firstByte as ubyte = rawUtf8[0]
	length = 1
	if firstByte <= &h7f then
		length = 1
	elseif (firstByte And &b11000000) = &hc0 then
		length = 2
	elseif (firstByte And &b11100000) = &he0 then
		length = 3
	elseif (firstByte And &b11110000) = &hf0 then
		length = 4
	end if
	bytes = callocate(length, sizeof(ubyte))
	for i as uinteger = 0 to length-1
		bytes[i] = rawUtf8[i]
	next
end sub

function UTF8Char.isAscii() as ubyte
	return iif(length = 1, 1, 0)
end function

operator =(byval lhs as UTF8Char, byval rhs as UTF8Char) as ubyte
	if ( rhs.length = lhs.length ) then
		dim success as ubyte = 1
		for i as uinteger = 0 to lhs.length-1
			if lhs.bytes[i] <> rhs.bytes[i] then return 0
		next
		return 1
	end if
	return 0
end operator

operator <>(byval lhs as UTF8Char, byval rhs as UTF8Char) as ubyte
	return Not (lhs = rhs)
end operator

constructor UTF8String()
	m_string = NULL
	m_length = 0
end constructor

constructor UTF8String(byval chars as ubyte ptr, byval len_ as integer)
	m_length = len_
	m_string = callocate( m_length, sizeof(UTF8Char ptr) )
	dim i as uinteger = 0
	do
		m_string[i] = new UTF8Char( (chars + i) )
		i += m_string[i]->length
	loop while i <= (m_length-1)
end constructor

constructor UTF8String(byref copyFrom as string)
	m_length = Len( copyFrom )
	m_string = callocate( m_length, sizeof(UTF8Char ptr) )
	for i as uinteger = 0 to m_length-1
		m_string[i] = new UTF8Char( .mid( copyFrom, (i+1), 1 ) )
	next
end constructor

constructor UTF8String(byref copyFrom as zstring ptr, byval len_ as integer)
	m_length = len_
	m_string = callocate( m_length, sizeof(UTF8Char ptr) )
	for i as uinteger = 0 to m_length-1
		m_string[i] = new UTF8Char( cptr( ubyte ptr, copyFrom + i ) )
	next
end constructor

destructor UTF8String()
	if m_string then
		for i as uinteger = 0 to m_length-1
			delete m_string[i]
		next
		deallocate m_string
	end if
	m_string = NULL
	m_length = 0
end destructor

operator UTF8String.cast() as string
	return ascii()
end operator

operator UTF8String.let( byref fbstr as string )
	m_length = Len( fbstr )
	m_string = callocate( m_length, sizeof(UTF8Char ptr) )
	for i as uinteger = 0 to m_length-1
		m_string[i] = new UTF8Char( .mid( fbstr, (i+1), 1 ) )
	next
end operator

function UTF8String.ascii(byref unsupportedCharsAs as string = "?") as string
	dim exported as string = ""
	for i as uinteger = 0 to m_length-1
		if m_string[i]->isAscii() then
			exported += chr( m_string[i]->bytes[0] )
		else
			exported += unsupportedCharsAs
		end if
	next
	return exported
end function

function UTF8String.utf8(byval exported as ubyte ptr) as uinteger
	exported = NULL
	dim bytelength as integer = 0
	
	for i as uinteger = 0 to m_length-1
		bytelength += m_string[i]->length
		exported = reallocate( exported, sizeof(UTF8Char) * bytelength )
		dim byteref as byte ptr = exported + (bytelength - m_string[i]->length)
		for b as uinteger = 0 to m_string[i]->length
			byteref[b] = m_string[i]->bytes[b]
		next
	next
	return bytelength
end function

function UTF8String.length() as uinteger
	return m_length
end function

function UTF8String.at(byval index as integer) as UTF8Char
	return *m_string[ determinePosition(index) ]
end function

function UTF8String.mid(byval start as integer, byval len_ as integer) as UTF8String
	dim bytes as ubyte ptr = NULL
	dim bytelength as uinteger = 0
	
	dim as uinteger s, f
	s = determinePosition( start ) - 1
	f = s + len_ - 1
	if ( len_ < 0 ) then f = cuint( s + (m_length + len_) + 1 )
	
	for i as uinteger = s to f
		bytelength += m_string[i]->length
		bytes = reallocate( bytes, sizeof(ubyte) * bytelength )
		dim byteref as byte ptr = bytes + (bytelength - m_string[i]->length)
		for b as uinteger = 0 to m_string[i]->length
			byteref[b] = m_string[i]->bytes[b]
		next
	next
	
	dim returnMe as UTF8String = type<UTF8String>( bytes, cint(bytelength) )
	
	deallocate bytes
	
	return returnMe
end function

function UTF8String.left(byval len_ as integer) as UTF8String
	return this.mid( 1, determinePosition( len_ ) )
end function

function UTF8String.right(byval len_ as integer) as UTF8String
	dim as uinteger length2 = determinePosition( len_ )
	return this.mid( m_length - length2 + 1, length2 )
end function

function UTF8String.instr(byref search as UTF8String, byval start as integer = 0) as integer
	dim firstChar as UTF8Char = search.at(0)
	for i as uinteger = 0 to m_length
		if (firstChar = at(i)) And ((m_length - i) >= search.length()) then
			dim as ubyte success = 1
			for j as uinteger = 0 to search.length()
				if search.at(j) <> at(i + j) then
					success = 0
					exit for
				end if
			next
			if success then return i
		end if
	next
	return -1
end function

function UTF8String.instr_any(byref search as UTF8String, byval start as integer = 0) as integer
	dim found as uinteger = -1
	for i as uinteger = 0 to search.length()
		dim s as UTF8String = type<UTF8String>( search.at(i).bytes, 1 )
		dim p as uinteger = this.instr( s, start )
		if (found = -1) or (p > found) then found = p
	next
	return found
end function

sub UTF8String.insert(byref text as UTF8String, byval pos as uinteger)
	m_string = reallocate( m_string, sizeof(UTF8Char ptr) * (text.length() + m_length) )
	'' Shift forward any existing character data
	if pos < m_length then
		for i as uinteger = (m_length-1) to pos step -1
			m_string[i+text.length()-1] = m_string[i]
		next i
	end if
	for i as uinteger = 0 to text.length()-1
		m_string[i + pos]->assign( text.at(i).bytes )
	next
	m_length += text.length()
end sub

sub UTF8String.prepend(byref text as UTF8String)
	insert( text, 0 )
end sub

sub UTF8String.append(byref text as UTF8String)
	insert( text, m_length )
end sub

function UTF8String.ltrim( byref trimMe as UTF8String, byval trimIndividualCharacters as ubyte ) as UTF8String
	dim trimLength as uinteger = iif( trimIndividualCharacters = 0, trimMe.length(), 1 )
	dim offset as integer = 1
	do
		dim found as ubyte = 0
		dim piece as UTF8String = mid(offset, trimLength)
		if trimIndividualCharacters = 0 then
			if piece = trimMe then
				found = 1
			end if
		else
			for i as integer = 1 to trimMe.length()
				if trimMe.mid(i,1) = piece then
					found = 1
					exit for
				end if
			next
		end if
		if found = 0 then return this.mid( offset )
		offset += trimLength
	loop
	return this.mid( offset )
end function

function UTF8String.rtrim( byref trimMe as UTF8String, byval trimIndividualCharacters as ubyte ) as UTF8String
	dim trimLength as uinteger = iif( trimIndividualCharacters = 0, trimMe.length(), 1 )
	dim offset as integer = length()-trimLength
	do
		dim found as ubyte = 0
		dim piece as UTF8String = mid(offset, trimLength)
		if not trimIndividualCharacters then
			if piece = trimMe then found = 1
		else
			for i as integer = 1 to trimMe.length()
				if trimMe.mid(i,1) = piece then
					found = 1
					exit for
				end if
			next
		end if
		if found = 0 then return this.mid( offset )
		offset -= trimLength
	loop
	return this.mid( offset )
end function

function UTF8String.trim( byref trimMe as UTF8String, byval trimIndividualCharacters as ubyte ) as UTF8String
	return ltrim( trimMe, trimIndividualCharacters ).rtrim( trimMe, trimIndividualCharacters )
end function

function UTF8String.equals( byval comparison as UTF8String ) as ubyte
	if ( m_length <> comparison.length() ) then return 0
	for i as uinteger = 1 to (m_length-1)
		if at( i ) <> comparison.at( i ) then return 0
	next i
	return 1
end function

function UTF8String.determinePosition( byval p as integer ) as uinteger
	if p < 0 then return cast( uinteger, m_length + p )
	return cast( uinteger, p )
end function

operator =(byval lhs as UTF8String, byval rhs as UTF8String) as ubyte
	return lhs.equals( rhs )
end operator

operator <>(byval lhs as UTF8String, byval rhs as UTF8String) as ubyte
	return not lhs.equals( rhs )
end operator

''**********************************************************************
''	fbJSON Internal Data
''**********************************************************************
type fbJSONToken
public:
	declare constructor()
	declare destructor()
	declare property blockString() as UTF8String
	declare property blockString( byref assign as UTF8String )
	lineNo		as integer
	linePos		as integer
private:
	_blockString	as UTF8String ptr
end type

constructor fbJSONToken()
	this._blockString = new UTF8String()
	this.lineNo = 0
	this.linePos = 0
end constructor

destructor fbJSONToken()
	if this._blockString <> NULL then delete this._blockString
end destructor

property fbJSONToken.blockString() as UTF8String
	if this._blockString <> NULL then return *this._blockString else return type<UTF8String>()
end property

property fbJSONToken.blockString( byref assign as UTF8String )
	if this._blockString <> NULL then delete this._blockString
	this._blockString = new UTF8String( assign )
end property

''**********************************************************************
''	fbJSON Implementation
''**********************************************************************
constructor fbJSON()
	this.parent = NULL
	this.firstChild = NULL
	this.nextSibling = NULL
	this.jType = JSON_Null
	this._jIdentifier = type<UTF8String>()
	this._jData = type<UTF8String>()
	this.jNum = 0.0
end constructor

/'constructor fbJSON( byref newName as string )
	this.parent = NULL
	this.firstChild = NULL
	this.nextSibling = NULL
	this.jType = JSON_False
	this._jName = callocate( sizeof( zstring ptr ) * 1 )
	this.jName = newName
	this._jData = NULL
	this.jNum = 0.0
end constructor'/

constructor fbJSON( byref stringVal as UTF8String )
	this.parent = NULL
	this.firstChild = NULL
	this.nextSibling = NULL
	this.jType = JSON_Null
	this._jIdentifier = type<UTF8String>()
	this._jData = type<UTF8String>()
	this.jNum = 0.0
	this.asString( stringVal )
end constructor

constructor fbJSON( byval doubleVal as double )
	this.parent = NULL
	this.firstChild = NULL
	this.nextSibling = NULL
	this.jType = JSON_Null
	this._jIdentifier = type<UTF8String>()
	this._jData = type<UTF8String>()
	this.jNum = 0.0
	this.asNumber( doubleVal )
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

sub fbJSON.asString( byval strValue as UTF8String )
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

property fbJSON.identifier() as UTF8String
	return this._jIdentifier
end property

property fbJSON.identifier( byref newName as UTF8String )
	this._jIdentifier = newName
end property

function fbJSON.childByName( byref cName as UTF8String ) as fbJSON ptr
	dim node as fbJSON ptr = this.firstChild
	while node <> NULL
		if node->identifier = cName then return node
		node = node->nextSibling
	wend
	return NULL
end function

function fbJSON.c( byref cName as UTF8String ) as fbJSON ptr
	return this.childByName( cName )
end function

function fbJSON.childByIndex( byval childIndex as integer ) as fbJSON ptr
	dim node as fbJSON ptr = this.firstChild
	dim ci as integer = childIndex
	while ( node <> NULL ) and ( ci > 0 )
		ci -= 1
		node = node->nextSibling
	wend
	return node
end function

function fbJSON.toString() as UTF8String
	select case this.jType
	case JSON_String, JSON_True, JSON_False
		return this._jData
	case JSON_Number
		return str( this.jNum )
	case else
		return fbJSON_ExportString( @this, 0 )
	end select
end function

function fbJSON.toNumber() as double
	if this.jType = JSON_Number then return this.jNum
	if this.jType = JSON_String then return cdbl( this._jData.ascii )
	return 0.0
end function

function fbJSON.toBool() as ubyte
	if this.jType = JSON_True then return 1
	if this.jType = JSON_False then return 0
	if this.jType = JSON_String then return iif( this._jData.length() > 0, 1, 0 )
	if this.jType = JSON_Number then return iif( this.jNum = 0, 0, 1 )
	if (this.jType = JSON_Object) Or (this.jType = JSON_Array) then return iif( this.numChildren() = 0, 0, 1 )
	'' Report an error?
	return 0
end function

function fbJSON.toBoolStr() as UTF8String
	dim value as ubyte = this.toBool
	if value = 1 then return "true"
	return "false"
end function

function fbJSON.getType() as JSON_TYPES
	return this.jType
end function

function fbJSON.getTypeName() as UTF8String
	if ( this.jType = JSON_True ) Or ( this.jType = JSON_False ) then return "boolean"
	if ( this.jType = JSON_Null ) then return "null"
	if ( this.jType = JSON_Number ) then return "number"
	if ( this.jType = JSON_String ) then return "string"
	if ( this.jType = JSON_Array ) then return "array"
	if ( this.jType = JSON_Object ) then return "object"
	return "error"
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

function fbJSON.arraySize( ) as integer
	return this.numChildren( 0 )
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

function fbJSON.removeChild( byref strName as UTF8String ) as fbJSON ptr
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

sub fbJSON.deleteChild( byref strName as UTF8String )
	this.deleteChild( this.childByName( strName ) )
end sub

sub fbJSON.deleteChild( byval index as integer )
	this.deleteChild( this.childByIndex( index ) )
end sub

sub fbJSON.deleteChildren( )
	if this.firstChild <> NULL then
		delete this.firstChild
	end if
	this.firstChild = NULL
end sub

sub fbJSON.setData( byval text as string )
	this.unsetData()
	this._jData = text
end sub

sub fbJSON.unsetData()
	this._jData = ""
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

function fbJSON_Tokenizer( byref jsonString as UTF8String, tokens() as fbJSONToken ) as uinteger
	redim tokens(0) as fbJSONToken
	dim as integer startToken, tokenLen, inString=0, blockCount=0, l=1, p=1, maxLen
	maxLen = len( jsonString )
	
	startToken = 1
	do
		var c = jsonString.mid( startToken, 1 )
		tokenLen = 1
		if ( instr( c, any chr(9)&chr(32) ) = 0 ) then '' Space or Tab
			dim index as integer
			index = startToken + 1
			if c = chr(34) then '' Quote
				do
					index = instr( index, jsonString, chr(34) )
				loop until mid( jsonString, index-1,1 ) <> "\"
			elseif instr( c, any jsonSyntax ) then
				index = startToken
			else
				index = instr( startToken, jsonString, any whitespace & jsonSyntax )-1
			end if
			tokenLen = ( index - startToken )+1
			var tokenString = trim( mid( jsonString, startToken, tokenLen ), any whitespace )
			'? "Parser - Adding token `" & tokenString & "`(" & tokenLen & ")"
			if len( tokenString ) then
				redim preserve tokens(1 to ubound(tokens)+1) as fbJSONToken
				with tokens(ubound(tokens))
					.lineNo = l
					.linePos = p
					.blockString = tokenString 'fbJSON_DecodeUnicodeEscapes( tokenString )
				end with
			end if
		elseif ( instr( c, chr(10) ) ) then '' New Line
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
	
	if ( useIndents <> 0 ) And ( level > 0 ) then indent = string( level * 2, " " )
	if ( useIndents <> 0 ) then newline = chr(10)
	
	select case n->getType()
	case JSON_Object
		if n->identifier.length() > 0 then result &= indent & chr(34) & n->identifier & chr(34) & ":{" else result &= indent & "{"
		var num = n->numChildren
		if num > 0 then
			result &= newline & indent
			for i as integer = 0 to num-1
				var comma = ","
				if i = (num-1) then comma = ""
				result &= fbJSON_Print( n->childByIndex( i ), level + 1, useIndents, escaped ) & comma & newline
			next i
			'result &= newline & indent
		end if
		result &= indent & "}"

	case JSON_Array
		if n->identifier.length() > 0 then result &= indent & chr(34) & n->identifier & chr(34) & ":[" else result &= indent & "["
		var num = n->numChildren
		if num > 0 then
			result &= newline
			for i as integer = 0 to num-1
				var comma = ","
				if i = (num-1) then comma = ""
				result &= fbJSON_Print( n->childByIndex( i ), level + 1, useIndents, escaped ) & comma & newline
			next i
			'result &= newline & indent
		end if
		result &= indent & "]"

	case JSON_String
		dim as string useStr = n->toString()
		if ( escaped <> 0 ) then useStr = fbJSON_EncodeUnicodeEscapes( useStr )
		if n->parent->getType() = JSON_Array then
			result &= indent & chr(34) & useStr & chr(34)
		else
			result &= indent & chr(34) & n->identifier & chr(34) & ":" & chr(34) & useStr & chr(34)
		end if

	case JSON_True, JSON_False
		if n->parent->getType() = JSON_Array then
			result &= n->toBoolStr()
		else
			result &= indent & chr(34) & n->identifier & chr(34) & ":" & n->toBoolStr()
		end if

	case JSON_Null
		if n->parent->getType() = JSON_Array then
			result &= indent & "null"
		else
			result &= indent & chr(34) & n->identifier & chr(34) & ":" & "null"
		end if

	case JSON_Number
		if n->parent->getType() = JSON_Array then
			result &= n->toNumber()
		else
			result &= indent & chr(34) & n->identifier & chr(34) & ":" & n->toNumber()
		end if
	end select
	'if (level > 0 ) then result &= newline
	return result
end function

''**********************************************************************
''	Standard function implementations
''**********************************************************************
function fbJSON_ImportFile( byref path as string, byval utf8 as ubyte ) as fbJSON ptr
	dim as string dline, dstr
	dim as integer fh=freefile()
	if utf8 <> 0 then
		dim as ubyte byteOrderMark(0 To 1, 0 To 2) = { { &h3f, &hbb, &hbf }, {0,0,0} }
		open path for binary access read as #fh
		
		for j as integer = 0 to 2
			get #fh, , byteOrderMark(1, j)
		next j
		
		dim as ubyte success = 1
		for i as integer = 0 to 2
			if byteOrderMark(0,i) <> byteOrderMark(1,i) then
				success = 0
				close #fh
			end if
		next i
		if success = 0 then return NULL
		
		
		
	else
		open path for input as #fh
		do until eof( fh )
			line input #fh, dline
			dstr &= dline & chr(10)
		loop
	end if
	close #fh
	return fbJSON_ImportString( dstr )
end function

function fbJSON_ImportString( byref jsonString as UTF8String ) as fbJSON ptr
	if len( trim( jsonString, any whitespace ) ) = 0 then return NULL
	dim as fbJSON ptr current = NULL
	redim as fbJSONToken tokens(0)
	dim as integer arrayCount = 0, objectCount = 0
	
	var num = fbJSON_Tokenizer( jsonString, tokens() )
	if num < 1 then
		return NULL
	end if
	
	for i as integer = 1 to num
		var lower = type<UTF8String>( lcase( tokens(i).blockString ) ) '' TODO: Need a reliable way to lcase UTF8 characters
		'? "Current token=`" & tokens(i).blockString & "`"
		select case lower.ascii()
		case "{"
			objectCount += 1
			Dim node As fbJSON ptr = new fbJSON()
			node->asObject()
			if ( current = NULL ) then
				'? "(object) Current is NULL, creating new object"
				current = node
			else
				'? "(object) Current is set (probably an array)"
				If ( tokens(i-1).blockString = ":" ) And ( current->getType() = JSON_Object ) Then
					node->identifier = tokens(i-2).blockString
					If ( node->identifier.left(1) = chr(34) ) And ( node->identifier.right(1) = chr(34) ) Then node->identifier = node->identifier.mid( 2, node->identifier.length() - 2 )
					'? " ^ Note: current is an object"
				End If
				current = current->appendChild( node )
			end if
		case "["
			arrayCount += 1
			Dim node As fbJSON ptr = new fbJSON()
			node->asArray()
			if ( current = NULL ) then
				'? "(array) Current is NULL, creating new array"
				current = node
			else
				'? "(array) Current is set (probably an array)"
				If ( tokens(i-1).blockString = ":" ) And ( current->getType() = JSON_Object ) Then
					node->identifier = tokens(i-2).blockString
					' Strip quotes if they exist
					If ( node->identifier.left(1) = chr(34) ) And ( node->identifier.right(1) = chr(34) ) Then node->identifier = node->identifier.mid( 2, node->identifier.length() - 2 )
					'? " ^ Note: current is an object"
				End If
				current = current->appendChild( node )
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
				if ( current <> NULL ) And ( ( tokens(i-1).blockString = ":" ) And ( tokens(i-2).blockString.left(1) = chr(34) ) Or ( ( current->getType() = JSON_Array ) And ( tokens(i-1).blockString.instr_any( ",[" ) ) ) ) then
					if lower = "true" then
						node = current->appendChild( new fbJSON( ) )
						node->asTrue()
						node->identifier = tokens(i-2).blockString.trim( chr(34) )
					elseif lower = "false" then
						node = current->appendChild( new fbJSON( ) )
						node->asFalse()
						node->identifier = tokens(i-2).blockString.trim( chr(34) ) 
					elseif lower = "null" then
						node = current->appendChild( new fbJSON( ) )
						node->asNull()
						node->identifier = tokens(i-2).blockString.trim( chr(34) ) 
					elseif mid( lower, 1, 1 ) = chr(34) then
						node = current->appendChild( new fbJSON( tokens(i).blockString.trim( chr(34) ) ) )
						node->identifier = tokens(i-2).blockString.trim( chr(34) )
					elseif lower.left(1).instr_any( "0123456789-+" ) then
						node = current->appendChild( new fbJSON( cdbl( tokens(i).blockString.ascii() ) ) )
						if current->getType() <> JSON_Array then
							node->identifier = tokens(i-2).blockString.trim( chr(34) )
						end if
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

function fbJSON_ExportString( byval root as fbJSON ptr, byval useFormatting as ubyte = 0 ) as UTF8String
	return fbJSON_Print( root, 0, useFormatting )
end function

sub fbJSON_Delete( byref root as fbJSON ptr )
	if root <> NULL then
		delete root
		root = NULL
	end if
end sub
