FreeBASIC JSON Parser
======================
"JSON is simple, so the interface should also be simple"

Written by Oz (alex DOT barry AT gmail DOT com) - April 22, 2010, Updated May 21, 2013

Licensing
---------
This is MIT licensed.  Basically, you are free to submit patches to me/anyone who
wants to help develop, and you're free to use or share it, as long as the license
is still attached to it in one way or another.  See LICENSE, fbJSON.bi, or
fbJSON.bas for more information.

COMPILING
---------
To compile the test, use:

```bash
$ fbc test.bas fbJSON.bas -x unit-test.bas
```

This should create an executabled called test (test.exe in windows) - run that.

If you wanted to use fbJSON as a library, you could make it static easy by using:

````bash
$ fbc fbJSON.bas -lib
$ fbc yourProject.bas -lfbJSON
````

Todo
----
 * Polish the UTF8String and UTF8Char UDT/Classes.
 * Make the outputted formatting a little prettier
 * Give some meaningful output for errors
 
Notes
----
This all started with a physics game I was developing and figured the easiest
format for me to use for save states was json.  I searched the forums and found
to my surprise that another person recently requested the FB community for a
JSON interpreter, and KristopherWindsor posted [his code](http://www.freebasic.net/forum/viewtopic.php?t=17279).  I'm not one to try to
reinvent the wheel, but I needed some other features (like generating/saving
json), and didn't like how his code was all tied into an include (which isn't bad in itself, I just prefer separating out header and source code).

For more information, you can check out the [FreeBASIC forum thread](http://www.freebasic.net/forum/viewtopic.php?p=155994)

Enjoy!
