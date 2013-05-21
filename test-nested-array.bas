
#include "fbJSON.bi"

? "**********************************************************************"
? "  Test #1 - AGS Test Cases"
? "**********************************************************************"

dim as string testData(1 to 2)
testData(1) = !"{\"name\":[[[1,2,3,4],5,6],7,8]}"
testData(2) = !"{\"\":\"\", \"test\":\"\"}"

for i as integer = lbound(testData) to ubound(testData)
	dim test as fbJSON ptr = fbJSON_ImportString( testData(i) )

	? "Raw data: " + testData(i)
	? fbJSON_ExportString( test, 0 )

	fbJSON_Delete( test )
	
	? "------------------------------"
next i
