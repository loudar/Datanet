$EXEICON:'DN.ico'
$RESIZE:SMOOTH
CLS
CLEAR
CLOSE

startparameter$ = "graph"
DIM SHARED license$

'================================================================================================================ BASIC STUFF ======================================================================================

'base arrays
DIM SHARED nodelimit: nodelimit = 20000
maxcategories = 3
DIM SHARED cat$(maxcategories)
DIM SHARED maxnodes
DIM SHARED nodeID$(nodelimit)
DIM SHARED cutSID$
DIM SHARED cutCAT$
DIM SHARED cutSCAT$
DIM SHARED instDATA$(5000)
DIM SHARED attribute$(20)

'graph arrays
maxlinks = 50
DIM SHARED mainlink$(maxlinks)
DIM SHARED mainlinktext$(maxlinks)
DIM SHARED mainlinkname$(maxlinks)
DIM SHARED mainlinkx(maxlinks)
DIM SHARED mainlinky(maxlinks)
DIM SHARED linknamey(maxlinks)
DIM SHARED linknamex(maxlinks)
DIM SHARED ml
DIM SHARED maxml
DIM SHARED pcount
DIM SHARED p
DIM SHARED p(maxlinks)
DIM SHARED reruninst$
'overview
DIM SHARED linkCount(nodelimit)
DIM SHARED nodeNAME$(nodelimit)
DIM SHARED link$(nodelimit, 100)
DIM SHARED sortCount(nodelimit)
DIM SHARED sortNAME$(nodelimit)
DIM SHARED sortID$(nodelimit)
DIM SHARED sortlink$(nodelimit, 100)
DIM SHARED x(200)
DIM SHARED y(200)

'loads allowed characters (ac) from file
DIM SHARED ac$(100)
DIM SHARED alch
'font
DIM SHARED maxrows
DIM SHARED maxlines
DIM SHARED fontheight
DIM SHARED fontwidth
DIM SHARED firstline
DIM SHARED xcharacter
DIM SHARED xoffset(100)
DIM SHARED fr&
DIM SHARED fb&
DIM SHARED fj&

'menu (alphabetically sorted)
DIM SHARED maxmenuitems
maxmenuitems = 50
'text
'text arrays
DIM SHARED text$(maxmenuitems)
DIM SHARED char$(maxmenuitems, 500)
DIM SHARED editpos(maxmenuitems)
DIM SHARED c(maxmenuitems)
DIM SHARED gbf(maxmenuitems)
DIM SHARED UserInput$(maxmenuitems)
DIM SHARED backspace(maxmenuitems) 'determines whether backspace was pressed during an active input
DIM SHARED basex(maxmenuitems) 'represents the very left x value of a menu object
DIM SHARED basey(maxmenuitems) 'represents the very top y value of a menu object
DIM SHARED borderoffsetx(maxmenuitems)
DIM SHARED borderoffsety(maxmenuitems)
DIM SHARED bstyle(10)
DIM SHARED broken(50)
'DIM SHARED categories(maxmenuitems)   'is only used in dropdown at end of selection, what's it for???
DIM SHARED category$(maxmenuitems, 20)
DIM SHARED defaultstyle(maxmenuitems) 'determines the default style of a menu object
DIM SHARED destination$(maxmenuitems)
DIM SHARED endparameter$ 'used to get ending of menu
DIM SHARED endparameter2$ 'used to get ending of graph
DIM SHARED endnode$ 'used to get ending of nodefield
DIM SHARED endx(maxmenuitems) 'represents the very right x value of a menu object
DIM SHARED endy(maxmenuitems) 'represents the very bottom y value of a menu object
DIM SHARED expanded(maxmenuitems)
DIM SHARED file$(maxmenuitems)
DIM SHARED fileshort$(maxmenuitems)
DIM SHARED fill$(maxmenuitems)
DIM SHARED firstprint(maxmenuitems)
DIM SHARED kind$(maxmenuitems)
DIM SHARED i
DIM SHARED inter(maxmenuitems)
DIM SHARED linetbp(maxmenuitems)
DIM SHARED maxcatlength(maxmenuitems)
DIM SHARED maxg(maxmenuitems)
DIM SHARED maxii
DIM SHARED maxval(maxmenuitems)
DIM SHARED menu$(maxmenuitems, maxmenuitems)
DIM SHARED menufile$(maxmenuitems)
DIM SHARED minval(maxmenuitems)
DIM SHARED overlaptrigger(maxmenuitems)
DIM SHARED totalcat(maxmenuitems)
DIM SHARED trigger(maxmenuitems)
DIM SHARED type$(maxmenuitems)
DIM SHARED selected(maxmenuitems)
DIM SHARED status$(maxmenuitems)
DIM SHARED style(maxmenuitems)
DIM SHARED stylebf(maxmenuitems)
DIM SHARED subcategory$(maxmenuitems, 20, 20)
DIM SHARED subcount(maxmenuitems, 20)
DIM SHARED value(maxmenuitems)
DIM SHARED xvalue(maxmenuitems)
'dropdown positioning
DIM SHARED p1x(maxmenuitems)
DIM SHARED p1y(maxmenuitems)
DIM SHARED p2x(maxmenuitems)
DIM SHARED p2y(maxmenuitems)
DIM SHARED p3x(maxmenuitems)
DIM SHARED p3y(maxmenuitems)

'=========================================================================================================== BASIC / SPECIAL STUFF =================================================================================

'' Dialog flag constants (use + or OR to use more than 1 flag value)
'CONST OFN_ALLOWMULTISELECT = &H200& '  Allows the user to select more than one file, not recommended!
'CONST OFN_CREATEPROMPT = &H2000& '     Prompts if a file not found should be created(GetOpenFileName only).
'CONST OFN_EXTENSIONDIFFERENT = &H400& 'Allows user to specify file extension other than default extension.
'CONST OFN_FILEMUSTEXIST = &H1000& '    Chechs File name exists(GetOpenFileName only).
'CONST OFN_HIDEREADONLY = &H4& '        Hides read-only checkbox(GetOpenFileName only)
'CONST OFN_NOCHANGEDIR = &H8& '         Restores the current directory to original value if user changed
'CONST OFN_NODEREFERENCELINKS = &H100000& 'Returns path and file name of selected(i)shortcut(.LNK) file instead of file referenced.
'CONST OFN_NONETWORKNewButton= &H20000& ' Hides and disables the Network button.
'CONST OFN_NOREADONLYRETURN = &H8000& ' Prevents selection of read-only files, or files in read-only subdirectory.
'CONST OFN_NOVALIDATE = &H100& '        Allows invalid file name characters.
'CONST OFN_OVERWRITEPROMPT = &H2& '     Prompts if file already exists(GetSaveFileName only)
'CONST OFN_PATHMUSTEXIST = &H800& '     Checks Path name exists (set with OFN_FILEMUSTEXIST).
'CONST OFN_READONLY = &H1& '            Checks read-only checkbox. Returns if checkbox is checked
'CONST OFN_SHAREAWARE = &H4000& '       Ignores sharing violations in networking
'CONST OFN_SHOWHELP = &H10& '           Shows the help NewButton(useless!)

''DEFINT A-Z
'TYPE FILEDIALOGTYPE
'    lStructSize AS LONG '        For the DLL call
'    hwndOwner AS LONG '          Dialog will hide behind window when not set correctly
'    hInstance AS LONG '          Handle to a module that contains a dialog box template.
'    lpstrFilter AS _OFFSET '     Pointer of the string of file filters
'    lpstrCustFilter AS _OFFSET
'    nMaxCustFilter AS LONG
'    nFilterIndex AS LONG '       One based starting filter index to use when dialog is called
'    lpstrFile AS _OFFSET '       String full of 0's for the selected(i)file name
'    nMaxFile AS LONG '           Maximum length of the string stuffed with 0's minus 1
'    lpstrFileTitle AS _OFFSET '  Same as lpstrFile
'    nMaxFileTitle AS LONG '      Same as nMaxFile
'    lpstrInitialDir AS _OFFSET ' Starting directory
'    lpstrTitle AS _OFFSET '      Dialog title
'    flags AS LONG '              Dialog flags
'    nFileOffset AS INTEGER '     Zero-based offset from path beginning to file name string pointed to by lpstrFile
'    nFileExtension AS INTEGER '  Zero-based offset from path beginning to file extension string pointed to by lpstrFile.
'    lpstrDefExt AS _OFFSET '     Default/selected(i)file extension
'    lCustData AS LONG
'    lpfnHook AS LONG
'    lpTemplateName AS _OFFSET
'END TYPE

'DECLARE DYNAMIC LIBRARY "comdlg32" ' Library declarations using _OFFSET types
'    FUNCTION GetOpenFileNameA& (DIALOGPARAMS AS FILEDIALOGTYPE) ' The Open file dialog
'    FUNCTION GetSaveFileNameA& (DIALOGPARAMS AS FILEDIALOGTYPE) ' The Save file dialog
'END DECLARE

'DECLARE LIBRARY
'    FUNCTION FindWindow& (BYVAL ClassName AS _OFFSET, WindowName$) ' To get hWnd handle
'END DECLARE
'=============================================================================================================== SPECIAL STUFF =====================================================================================
restart:
ClearMenu
'setting variables
DIM SHARED windowscale
DIM SHARED tutorial
loadsettings
'variables
'screens
DIM SHARED c%
DIM SHARED v%
DIM SHARED n%
DIM SHARED e%
c% = _LOADIMAGE("data\bg\console.jpg", 32)
v% = _LOADIMAGE("data\bg\viewdata.jpg", 32)
n% = _LOADIMAGE("data\bg\newpoint.jpg", 32)
e% = _LOADIMAGE("data\bg\editpoint.jpg", 32)
DIM SHARED maxx
DIM SHARED maxy
PRINT windowscale
maxx = (_DESKTOPWIDTH / 2) * windowscale
maxy = maxx / 16 * 9
DIM SHARED logox
DIM SHARED logoy
logox = maxx / 3
logoy = (500 / 1920) * logox

DO: LOOP UNTIL _SCREENEXISTS

'Coded by Dav, JULY/2020
'I used API information found on this page....
'http://allapi.mentalis.org/apilist/apilist.php
DECLARE DYNAMIC LIBRARY "user32"
    'sets a created window region
    'http://allapi.mentalis.org/apilist/SetWindowRgn.shtml
    FUNCTION SetWindowRgn& (BYVAL hwnd&, BYVAL hrgn&, BYVAL bredraw%)
END DECLARE

DECLARE DYNAMIC LIBRARY "gdi32"
    'creates a rectangular region
    'http://allapi.mentalis.org/apilist/CreateRectRgn.shtml
    FUNCTION CreateRectRgn& (BYVAL x1&, BYVAL y1&, BYVAL x2&, BYVAL y2&)
    'creates an elliptical region
    'http://allapi.mentalis.org/apilist/CreateEllipticRgn.shtml
    FUNCTION CreateEllipticRgn& (BYVAL x1&, BYVAL y1&, BYVAL x2&, BYVAL y2&)
    'creates a rectangular region with rounded corners
    'http://allapi.mentalis.org/apilist/CreateRoundRectRgn.shtml
    FUNCTION CreateRoundRectRgn& (BYVAL x1&, BYVAL y1&, BYVAL x2&, BYVAL y2&, BYVAL x3&, BYVAL y3&)
END DECLARE

hwnd& = _WINDOWHANDLE 'need the windows handle to play with it
rounding = 30
rgn& = CreateRoundRectRgn(7, 30, maxx, maxy, rounding, rounding)
'Set the created region...
try& = SetWindowRgn(hwnd&, rgn&, 0)
'Returns zero if failed...
IF try& = 0 THEN
    PRINT "Failed...": END
END IF


SCREEN _NEWIMAGE(maxx, maxy, 32)
PAINT (maxx / 2, maxy / 2), colour&("white")
DO: LOOP UNTIL _SCREENEXISTS
_TITLE "DATANET"
DIM SHARED version$: version$ = "Version 0.3 - unstable"
_SCREENMOVE (_DESKTOPWIDTH / 2) - (maxx / 2), (_DESKTOPHEIGHT / 2) - (maxy / 2)

fontheight = 16
fontfilej$ = "data\bg\katakana.ttf"
fj& = _LOADFONT(fontfilej$, fontheight * 4, "MONOSPACE")
fontfiler$ = "C:\WINDOWS\FONTS\COUR.TTF"
fr& = _LOADFONT(fontfiler$, fontheight, "MONOSPACE")
fontfileb$ = "C:\WINDOWS\FONTS\COURBD.TTF"
fb& = _LOADFONT(fontfileb$, fontheight, "MONOSPACE")
_FONT fr&
fontwidth = _FONTWIDTH(fr&)
COLOR colour&("black"), colour&("white")

'tutorial variables
DIM SHARED t1%
DIM SHARED t2%
DIM SHARED twidth
DIM SHARED theight
maxrows = INT(maxx / fontwidth)
maxlines = INT(maxy / fontheight) - 4
firstline = INT(120 / fontheight)
xcharacter = INT((maxx / 2.7) / fontwidth)
twidth = maxx / 1.5
theight = twidth / 2

'Targon variables
DIM SHARED targondatapath$
targondatapath$ = "\..\..\Targon\data"
DIM SHARED lang$
lang$ = "eng"

loaddata '    does what it says ;)

'Tutorial
IF tutorial = 1 THEN t1% = _LOADIMAGE("data\bg\t1.jpg", 32): t2% = _LOADIMAGE("data\bg\t2.jpg", 32)
CLS
_PUTIMAGE (0, 120 - (40 / 3))-(logox, logoy), c%
IF tutorial = 1 THEN
    tutorial:
    ClearMenu
    NewSlider INT(maxlines / 4) - firstline, INT(maxrows / 2) - xcharacter - 30, 0, 100, "Window size"
    NewButton INT(maxlines / 4) - firstline + 2, INT(maxrows / 2) - xcharacter - 30, "Next", 0, "redirect", "end menu"
    RunMenu ("t")
    ClearMenu
    NewButton INT((maxlines / 2 + (theight / 2 / fontheight)) / 2), INT(maxrows / 2) - xcharacter - 1, "Next", 0, "redirect", "end menu"
    RunMenu ("t1")
    ClearMenu
    NewButton INT((maxlines / 2 + (theight / 2 / fontheight)) / 2) - 1, INT(maxrows / 2) - xcharacter - 2, "Next", 0, "redirect", "end menu"
    RunMenu ("t2")
    OPEN "data\settings.datnet" FOR OUTPUT AS #1
    WRITE #1, 0
    WRITE #1, security
    WRITE #1, windowscale
    WRITE #1, _DEFLATE$(license$)
    CLOSE #1
END IF

license: 'License activation
IF license$ = "" THEN
    ClearMenu
    NewText 3, 0, "This is the 35-characters-long code you've gotten via Gumroad.", 0
    InputSub 1, 0, "Your license key: ", ""
    RunMenu ("c")
    license$ = UserInput$(2)
    SELECT CASE checkLicense(UserInput$(2))
        CASE IS = 0
            ClearMenu
            NewText 1, 0, "[ License key could not be verified. Please be sure you have typed it correctly. ]", 2
            RunMenu ("c")
            license$ = ""
            _DELAY 2
            GOTO license
        CASE IS = 1
            ClearMenu
            NewText 1, 0, "[ License activation successful. ]", 1
            RunMenu ("c")
            _DELAY 2
            OPEN "data\settings.datnet" FOR OUTPUT AS #1
            WRITE #1, tutorial
            WRITE #1, security
            WRITE #1, windowscale
            WRITE #1, _DEFLATE$(license$)
            CLOSE #1
    END SELECT
END IF

console:
ClearMenu
InputSub 1, 0, "Command: ", ""

'new.nodeNAME$.nodeCAT.nodeSCAT (.file$)
'view.nodeNAME$ - initiates search for any node with nodeNAME$ OR view.nodeID$, views the point if only one exists
'edit.nodeNAME$ OR edit.nodeID$ - opens the search or the edit page if only one is found
'link.nodeID$1.linkNAME$.nodeID$2 - 1 is the sub-node
'new - opens the "create node" window with graphical elements

RunMenu ("c")
detP (UserInput$(1))
a = 0
IF LEN(UserInput$(1)) > 5 THEN
    p = 0
    DO
        p = p + 1
        attribute$(p) = MID$(UserInput$(1), p(p - 1) + 1, p(p) - p(p - 1) - 1)
    LOOP UNTIL p = pcount
    maxa = pcount
    IF maxa >= 2 THEN
        a = 0
        DO
            a = a + 1
            broken(a) = 0
            lp = 0
            DO
                lp = lp + 1
                IF ASC(MID$(attribute$(a), lp, 1)) > 47 AND ASC(MID$(attribute$(a), lp, 1)) < 58 THEN
                ELSE
                    broken(a) = 1
                END IF
            LOOP UNTIL lp = LEN(attribute$(a))
        LOOP UNTIL attribute$(a + 1) = ""
        continuebreak:
        a = 0
        DO
            a = a + 1
            IF broken(a) = 0 THEN
                IF broken(a + 1) = 0 THEN
                    attribute$(a) = attribute$(a) + "." + attribute$(a + 1) 'melts together all attributes with numbers into one
                    a = a + 1
                    b = a
                    DO
                        broken(a) = broken(a + 1)
                        attribute$(a) = attribute$(a + 1)
                        a = a + 1
                    LOOP UNTIL attribute$(a + 1) = ""
                    attribute$(a) = ""
                    a = b
                    GOTO continuebreak
                END IF
            END IF
        LOOP UNTIL a = maxa
        SELECT CASE attribute$(1)
            CASE IS = "new"
                CLOSE #1
                OPEN "data\infrs\cat1.datnet" FOR INPUT AS #1
                toaddcat = 0: catfound = 0
                DO
                    toaddcat = toaddcat + 1
                    INPUT #1, catinput$
                    IF catinput$ = attribute$(3) THEN catfound = 1
                LOOP UNTIL EOF(1) = -1 OR catfound = 1: CLOSE #1
                IF catfound = 0 THEN
                    ClearMenu
                    NewText 1, 0, "[ could not find category " + attribute$(3) + " ]", 2
                    RunMenu ("c")
                    _DELAY 2
                    GOTO console
                END IF

                OPEN "data\infrs\scat" + LTRIM$(STR$(toaddcat)) + ".datnet" FOR INPUT AS #1
                toaddscat = 0: scatfound = 0
                DO
                    toaddscat = toaddscat + 1
                    INPUT #1, scatinput$
                    IF scatinput$ = attribute$(4) THEN scatfound = 1
                LOOP UNTIL EOF(1) = -1 OR scatfound = 1: CLOSE #1
                IF scatfound = 0 THEN
                    ClearMenu
                    NewText 1, 0, "[ could not find subcategory " + attribute$(4) + " ]", 2
                    RunMenu ("c")
                    _DELAY 2
                    GOTO console
                END IF

                nodeID$ = LTRIM$(STR$(maxnodes + 1)) + "." + LTRIM$(STR$(toaddcat)) + "." + LTRIM$(STR$(toaddscat))
                OPEN "data\index.datnet" FOR APPEND AS #1
                WRITE #1, nodeID$
                CLOSE #1
                OPEN "data\net\" + LTRIM$(STR$(toaddcat)) + "\" + LTRIM$(STR$(toaddscat)) + "\" + nodeID$ + ".datinst" FOR OUTPUT AS #1
                WRITE #1, "time." + DATE$ + "." + TIME$
                WRITE #1, "name." + attribute$(2)
                IF a >= 4 THEN
                    IF attribute$(5) <> "" THEN WRITE #1, "file." + attribute$(5) 'writes the file attribute into the file only if it exists
                    CLOSE #1
                    ClearMenu
                    NewText 1, 0, "[ node " + nodeID$ + ":" + attribute$(2) + " added ]", 1
                    RunMenu ("c")
                    _DELAY 2
                    ClearMenu
                    loaddata
                    GOTO console
                ELSE
                    GOTO newpoint
                END IF
            CASE IS = "view"
                li = 0
                CLOSE #1
                OPEN "data\index.datnet" FOR INPUT AS #1
                IF EOF(1) = 0 THEN
                    node = 0
                    DO
                        node = node + 1
                        INPUT #1, nodeID$
                        cutID (nodeID$)
                        IF nodeID$ = attribute$(2) OR attribute$(2) = "all" OR attribute$(2) = "list" THEN
                            li = li + 1
                            listITEM$(li) = nodeID$
                            listNAME$(li) = nodeID$ + " : " + nodeNAME$
                        END IF
                        CLOSE #2
                        OPEN "data\net\" + cutCAT$ + "\" + cutSCAT$ + "\" + nodeID$ + ".datinst" FOR INPUT AS #2
                        d = 0
                        DO
                            d = d + 1
                            INPUT #2, instDATA$(d)
                            IF MID$(instDATA$(d), 1, 5) = "name." THEN
                                nodeNAME$ = MID$(instDATA$(d), 6, LEN(instDATA$(d)) - 5)
                            ELSE
                            END IF
                            IF attribute$(2) <> "all" THEN
                                p = 0
                                pl = 0
                                DO
                                    p = p + 1
                                    pl = LEN(attribute$(2))
                                    part$ = MID$(instDATA$(d), p, pl)
                                    IF part$ = attribute$(2) AND nodeID$ <> listITEM$(li) THEN
                                        li = li + 1
                                        listITEM$(li) = nodeID$
                                        listNAME$(li) = nodeID$ + " : " + nodeNAME$
                                    END IF
                                LOOP UNTIL p = LEN(instDATA$(d))
                            ELSE
                                listNAME$(li) = nodeID$ + " : " + nodeNAME$
                            END IF
                        LOOP UNTIL EOF(2) = -1
                        CLOSE #2
                    LOOP UNTIL EOF(1) = -1
                    CLOSE #1
                END IF
                selected = 1
                returnfromview:
                IF li > 1 THEN
                    maxli = li

                    printsearch:
                    ClearMenu
                    li = 0
                    DO
                        li = li + 1
                        IF li = selected THEN
                            NewText li, 0, listNAME$(li), 1
                        ELSE
                            NewText li, 0, listNAME$(li), 0
                        END IF
                    LOOP UNTIL li = maxli
                    RunMenu ("v")
                    change = 0
                    DO
                        Taste$ = INKEY$
                        SELECT CASE Taste$
                            CASE IS = CHR$(0) + CHR$(80)
                                IF selected = maxli THEN
                                    selected = 1
                                ELSE
                                    selected = selected + 1
                                END IF
                                change = 1
                            CASE IS = CHR$(0) + CHR$(72)
                                IF selected = 1 THEN
                                    selected = maxli
                                ELSE
                                    selected = selected - 1
                                END IF
                                change = 1
                            CASE IS = CHR$(13)
                                viewNode listITEM$(selected), flip
                                IF endparameter2$ <> "console" THEN
                                    PRINT endparameter2$, MID$(endparameter2$, 5, LEN(endparameter$) - 4)
                                    SLEEP
                                    viewNode MID$(endparameter2$, 5, LEN(endparameter$) - 4), flip
                                ELSE
                                    GOTO console
                                END IF
                            CASE IS = CHR$(27)
                                GOTO console
                        END SELECT
                    LOOP UNTIL change = 1
                    GOTO printsearch
                ELSEIF li = 1 THEN
                    viewNode listITEM$(li), flip
                    GOTO console
                ELSE
                    ClearMenu
                    NewText 1, 0, "[ no data matches your search. ]", 2
                    RunMenu ("c")
                    _DELAY 2
                    ClearMenu
                    GOTO console
                END IF
            CASE IS = "field"

            CASE IS = "link"
                noIDs = 1
                a = 0
                DO
                    a = a + 1
                    IF broken(a) = 0 THEN
                        noIDs = 0
                    END IF
                LOOP UNTIL attribute$(a + 1) = ""
                searchfor = 2
                IF noIDs = 0 THEN
                    retrylink:
                    CLOSE #1
                    OPEN "data\index.datnet" FOR INPUT AS #1
                    IF EOF(1) = 0 THEN
                        DO
                            INPUT #1, nodeID$
                            IF nodeID$ = attribute$(2) THEN
                                cutID (nodeID$)
                                searchfor = 5
                                CLOSE #1
                                OPEN "data\index.datnet" FOR INPUT AS #1
                                IF EOF(1) = 0 THEN
                                    DO
                                        INPUT #1, nodeID$
                                        IF nodeID$ = attribute$(5) AND attribute$(3) <> "" THEN
                                            CLOSE #2
                                            OPEN "data\net\" + cutCAT$ + "\" + cutSCAT$ + "\" + attribute$(2) + ".datinst" FOR APPEND AS #2
                                            WRITE #2, attribute$(1) + "." + attribute$(2) + "." + attribute$(3) + "." + attribute$(4) + "." + attribute$(5) + "." + attribute$(6)
                                            WRITE #2, "time." + DATE$ + "." + TIME$
                                            CLOSE #2
                                            ClearMenu
                                            NewText 1, 0, "[ link to " + attribute$(5) + " added to " + attribute$(2) + " ]", 1
                                            RunMenu ("c")
                                            _DELAY 2
                                            ClearMenu
                                            GOTO console
                                        END IF
                                    LOOP UNTIL EOF(1) = -1
                                END IF
                            END IF
                        LOOP UNTIL EOF(1) = -1
                    END IF
                END IF

                'only triggers if link couldn't be created on first try
                selected = 1
                li = 0
                CLOSE #1
                OPEN "data\index.datnet" FOR INPUT AS #1
                IF EOF(1) = 0 THEN
                    node = 0
                    DO
                        nextnode:
                        IF EOF(1) = -1 THEN GOTO returnfromview2
                        node = node + 1
                        INPUT #1, nodeID$

                        cutID (nodeID$)
                        CLOSE #2
                        OPEN "data\net\" + cutCAT$ + "\" + cutSCAT$ + "\" + nodeID$ + ".datinst" FOR INPUT AS #2

                        IF nodeID$ = attribute$(searchfor) THEN
                            nodeNAME$ = ""
                            DO
                                INPUT #2, instDATA$
                                IF MID$(instDATA$, 1, 5) = "name." THEN
                                    li = li + 1
                                    listITEM$(li) = nodeID$
                                    nodeNAME$ = MID$(instDATA$, 6, LEN(instDATA$) - 5)
                                    listNAME$(li) = nodeID$ + " :1 " + nodeNAME$
                                    GOTO nextnode
                                END IF
                            LOOP UNTIL EOF(2) = -1 OR nodeNAME$ <> ""
                        END IF
                        d = 0
                        DO
                            d = d + 1
                            INPUT #2, instDATA$(d)
                            IF MID$(instDATA$(d), 1, 5) = "name." THEN
                                nodeNAME$ = MID$(instDATA$(d), 6, LEN(instDATA$(d)) - 5)
                                IF noIDs = 1 THEN
                                    IF nodeNAME$ = attribute$(searchfor - 1) THEN
                                        li = li + 1
                                        listITEM$(li) = nodeID$
                                        listNAME$(li) = nodeID$ + " : " + nodeNAME$
                                        GOTO nextnode
                                    END IF
                                END IF
                                IF nodeNAME$ = attribute$(searchfor) THEN
                                    li = li + 1
                                    listITEM$(li) = nodeID$
                                    listNAME$(li) = nodeID$ + " : " + nodeNAME$
                                    GOTO nextnode
                                ELSEIF LEN(attribute$(searchfor)) > LEN(nodeNAME$) THEN
                                    IF MID$(attribute$(searchfor), LEN(attribute$(searchfor)) - LEN(nodeNAME$) + 1, LEN(nodeNAME$)) = nodeNAME$ THEN
                                        li = li + 1
                                        listITEM$(li) = nodeID$
                                        listNAME$(li) = nodeID$ + " : " + nodeNAME$
                                        GOTO nextnode
                                    END IF
                                END IF
                            ELSE
                            END IF
                            IF noIDs = 0 THEN
                                p = 0
                                pl = 0
                                DO
                                    p = p + 1
                                    pl = LEN(attribute$(searchfor))
                                    part$ = MID$(instDATA$(d), p, pl)
                                    IF part$ = attribute$(searchfor) THEN
                                        li = li + 1
                                        listITEM$(li) = nodeID$
                                        listNAME$(li) = nodeID$ + " : " + nodeNAME$
                                        GOTO nextnode
                                    END IF
                                LOOP UNTIL p = LEN(instDATA$(d))
                            END IF
                        LOOP UNTIL EOF(2) = -1
                        CLOSE #2
                    LOOP UNTIL EOF(1) = -1
                    CLOSE #1
                END IF
                returnfromview2:
                IF li > 1 THEN
                    maxli = li

                    printsearch2:
                    ClearMenu
                    IF searchfor = 2 THEN
                        NewText 1, 0, "[ please select an node for the subject ]", 2
                    ELSEIF searchfor = 5 THEN
                        NewText 1, 0, "[ please select an node for the object ]", 2
                    END IF
                    li = 0
                    DO
                        li = li + 1
                        IF li = selected THEN
                            NewText li + 2, 0, listNAME$(li), 1
                        ELSE
                            NewText li + 2, 0, listNAME$(li), 0
                        END IF
                    LOOP UNTIL li = maxli
                    RunMenu ("c")
                    change = 0
                    DO
                        Taste$ = INKEY$
                        SELECT CASE Taste$
                            CASE IS = CHR$(0) + CHR$(80)
                                IF selected = maxli THEN
                                    selected = 1
                                ELSE
                                    selected = selected + 1
                                END IF
                                change = 1
                            CASE IS = CHR$(0) + CHR$(72)
                                IF selected = 1 THEN
                                    selected = maxli
                                ELSE
                                    selected = selected - 1
                                END IF
                                change = 1
                            CASE IS = CHR$(13)
                                c = 6
                                DO
                                    attribute$(c) = attribute$(c - 1)
                                    c = c - 1
                                LOOP UNTIL c = searchfor
                                attribute$(searchfor) = listITEM$(selected)
                                GOTO retrylink
                            CASE IS = CHR$(27)
                                ClearMenu
                                NewText 1, 0, "[ could not create link. ]", 2
                                RunMenu ("c")
                                _DELAY 2
                                ClearMenu
                                GOTO console
                        END SELECT
                    LOOP UNTIL change = 1
                    GOTO printsearch2
                ELSEIF li = 1 THEN
                    c = 6
                    DO
                        attribute$(c) = attribute$(c - 1)
                        c = c - 1
                    LOOP UNTIL c = searchfor
                    attribute$(searchfor) = listITEM$(selected)
                    GOTO retrylink
                END IF
                ClearMenu
                NewText 1, 0, "[ could not create link. ]", 2
                RunMenu ("c")
                _DELAY 2
                ClearMenu
                GOTO console
            CASE IS = "edit"
        END SELECT
    END IF
    'IF a > 0 THEN GOTO keepreadingcomment 'prevents the in-line commands to be interrupted by a space character (e.g. new.Matt Mason.living.person)
    cmd$ = attribute$(1)
    node$ = attribute$(2)
    ClearMenu
    SELECT CASE cmd$
        CASE IS = "new"
            GOTO newpoint
        CASE IS = "edit"
            GOTO editpoint
        CASE IS = "view"
            IF node$ <> "" THEN
                viewNode node$, flip
                GOTO console
            END IF
        CASE IS = "security"
            ClearMenu
            IF node$ = "1" THEN
                IF security = 0 THEN security = 1
                NewText 1, 0, "[ data security is now on. ]", 3
            ELSEIF node$ = "0" THEN
                IF security = 1 THEN security = 0
                NewText 1, 0, "[ data security is now off. ]", 3
            END IF
            OPEN "data\settings.datnet" FOR OUTPUT AS #1
            WRITE #1, tutorial
            WRITE #1, security
            CLOSE #1
            RunMenu ("c")
            _DELAY 2
            GOTO console
        CASE IS = "tutorial"
            IF node$ = "1" THEN
                IF tutorial = 0 THEN tutorial = 1
                OPEN "data\settings.datnet" FOR OUTPUT AS #1
                WRITE #1, tutorial
                WRITE #1, security
                WRITE #1, _DEFLATE$(license$)
                CLOSE #1
                ClearMenu
                NewText 1, 0, "[ tutorial will be displayed on next start. ]", 3
                RunMenu ("c")
                _DELAY 2
                GOTO console
            END IF
        CASE IS = "reset"
            GOTO resetnet
    END SELECT
ELSE
    SELECT CASE UserInput$(1)
        CASE IS = "new"
            GOTO newpoint
        CASE IS = "exit"
            SYSTEM
        CASE IS = "close"
            SYSTEM
        CASE IS = "reset"
            resetnet:
            ClearMenu
            NewButton 1, 0, "DELETE ALL DATA", 1, "confirmation", "end menu"
            NewButton 2, 0, "Go Back", 0, "redirect", "console:"
            RunMenu ("c")
            IF endparameter$ = "true" THEN
                IF maxnodes > 0 THEN
                    i = 0
                    DO
                        i = i + 1
                        cutID (nodeID$(i))
                        IF _FILEEXISTS("data\net\" + cutCAT$ + "\" + cutSCAT$ + "\" + nodeID$(i) + ".datinst") THEN KILL "data\net\" + cutCAT$ + "\" + cutSCAT$ + "\" + nodeID$(i) + ".datinst"
                    LOOP UNTIL i = maxnodes
                END IF
                cat = 0
                DO
                    cat = cat + 1
                    IF cat = 1 OR cat = 3 THEN 'number of subcategories per category - this is a dumb way of doing it, maybe rewrite at some point
                        scatlimit = 3
                    ELSE
                        scatlimit = 1
                    END IF
                    scat = 0
                    IF scatlimit > 0 THEN
                        DO
                            scat = scat + 1
                            IF _DIREXISTS("data\net\" + LTRIM$(STR$(cat)) + "\" + LTRIM$(STR$(scat))) THEN RMDIR "data\net\" + LTRIM$(STR$(cat)) + "\" + LTRIM$(STR$(scat))
                        LOOP UNTIL scat = scatlimit
                    END IF
                    IF _DIREXISTS("data\net\" + LTRIM$(STR$(cat))) THEN RMDIR "data\net\" + LTRIM$(STR$(cat))
                LOOP UNTIL cat = 3
                RMDIR "data\net"
                KILL "data\index.datnet"
                KILL "data\settings.datnet"
                ClearMenu
                loaddata
                NewText 1, 0, "[ All data successfully erased. ]", 1
                RunMenu ("c")
                _DELAY 2
            END IF
            GOTO restart
    END SELECT
END IF
ClearMenu
NewText 1, 0, "[ command not recognized. ]", 2
RunMenu ("c")
SLEEP 3
ClearMenu
GOTO console

newpoint:
ClearMenu
InputSub 1, 0, "Data point name: ", node$
NewDropdown 2, 0, "Category: ", "data\infrs\cat1.datnet"
NewButton 3, 0, "Next", 1, "redirect", "end menu"
RunMenu ("n")
nodeID = maxnodes + 1
toaddcat = selected(2)
nodeID$ = LTRIM$(STR$(nodeID)) + "." + LTRIM$(STR$(selected(2))) 'adds the number of selected(2) category behind the actual ID to refer to proper folder
nodeNAME$ = UserInput$(1)
ClearMenu
NewText 1, 0, "new point -" + nodeNAME$ + "-", 0
NewDropdown 2, 0, "Subcategory: ", "data\infrs\scat" + LTRIM$(STR$(selected(2))) + ".datnet"
NewButton 3, 0, "Choose File", 0, "open file", "point"
NewButton 4, 0, "Create", 2, "redirect", ":console"
RunMenu ("n")
'#createpoint - using variables from the menu to build the basis of a new point
toaddscat = selected(2)
nodeID$ = nodeID$ + "." + LTRIM$(STR$(selected(2)))
OPEN "data\index.datnet" FOR APPEND AS #1
WRITE #1, nodeID$
CLOSE #1
OPEN "data\net\" + LTRIM$(STR$(toaddcat)) + "\" + LTRIM$(STR$(toaddscat)) + ".datinst" FOR OUTPUT AS #1
WRITE #1, "time." + DATE$ + "." + TIME$
WRITE #1, "name." + nodeNAME$
IF file$(3) <> "" THEN WRITE #1, "file." + file$(3) 'only writes the file attribute if a file was selected
CLOSE #1
IF MID$(destination$(maxm), 1, 1) = ":" THEN
    SELECT CASE LTRIM$(destination$(i))
        CASE IS = "console"
            ClearMenu
            GOTO console
    END SELECT
END IF
GOTO console

viewpoint:
InputSub 1, 0, "Instance to view: ", "ID or name"
RunMenu ("v")
IF UserInput$(1) <> "" THEN
    viewNode UserInput$(1), flip
END IF
GOTO console

editpoint:
CLS
_PUTIMAGE (0, 120 - (40 / 3))-(logox, logoy), e%
GOTO console

SUB viewNode (nodeID$, flip)
    ClearMenu
    cutID (nodeID$)
    IF _FILEEXISTS("data\net\" + cutCAT$ + "\" + cutSCAT$ + "\" + nodeID$ + ".datinst") = -1 THEN
        OPEN "data\net\" + cutCAT$ + "\" + cutSCAT$ + "\" + nodeID$ + ".datinst" FOR INPUT AS #1
        id = 0
        DO
            id = id + 1
            INPUT #1, instDATA$(id)
            IF MID$(instDATA$(id), 5, 1) = "." THEN
                instDATA$(id) = MID$(instDATA$(id), 1, 4) + ": " + MID$(instDATA$(id), 6, LEN(instDATA$(id)) - 5)
            END IF
            NewText id, 0, instDATA$(id), 0
        LOOP UNTIL EOF(1) = -1
        CLOSE #1
        NewText id + 2, 0, "[ press Enter to close ]", 1
        RunMenu ("v")
        RunGraph nodeID$, flip
    ELSE
        NewText 1, 0, "[ node does not exist ]", 1
        RunMenu ("v")
        RunGraph nodeID$, flip
    END IF
    IF MID$(endparameter2$, 1, 4) = "view" THEN
        IF endparameter2$ <> "console" THEN
            viewNode MID$(endparameter2$, 5, LEN(endparameter2$) - 4), flip
        ELSE
            EXIT SUB
        END IF
    END IF
END SUB

SUB editNode (nodeID$)
    ClearMenu
    cutID (nodeID$)
    IF _FILEEXISTS("data\net\" + cutCAT$ + "\" + cutSCAT$ + "\" + nodeID$ + ".datinst") = -1 THEN
        OPEN "data\net\" + cutCAT$ + "\" + cutSCAT$ + "\" + nodeID$ + ".datinst" FOR INPUT AS #1
        id = 0
        DO
            id = id + 1
            INPUT #1, instDATA$(id)
            IF MID$(instDATA$(id), 5, 1) = "." THEN
                InputSub id, 0, MID$(instDATA$(id), 1, 4) + ": ", MID$(instDATA$(id), 6, LEN(instDATA$(id)) - 5)
            END IF
            'NewText id, 0, instDATA$(id), 0 'rewrite into type based on data (links = remove/change, name = change, time = text)
        LOOP UNTIL EOF(1) = -1
        maxid = id
        CLOSE #1
        'linetbp, xoffset, text$, style, kind$, destination$
        NewButton id + 2, 0, "[ Abort (ESC) ]", 1, "redirect", "view:"
        NewButton id + 3, 0, "[ Save (ENTER) ]", 1, "redirect", "save:"
        RunMenu ("e")
        'RunGraph (nodeID$) 'run graph differently when editing??
        SELECT CASE endparameter$
            CASE IS = "view:"
                viewNode nodeID$, flip
            CASE IS = "aborted"
                viewNode nodeID$, flip
            CASE IS = "save:"
                OPEN "data\net\" + cutCAT$ + "\" + cutSCAT$ + "\" + nodeID$ + ".datinst" FOR OUTPUT AS #1
                id = 0
                DO
                    id = id + 1
                    WRITE #1, instDATA$(id)
                LOOP UNTIL id = maxid
                CLOSE #1
                ClearMenu
                NewText 1, 0, "[ node updates saved. ]", 0
                RunMenu ("e")
                _DELAY 2
                'viewNode (nodeID$)
        END SELECT
    ELSE
        NewText 1, 0, "[ node does not exist ]", 1
        RunMenu ("e")
        RunGraph nodeID$, flip
    END IF
END SUB

SUB nodeOverview (centerNode$)
    ClearMenu
    CLS
    _PUTIMAGE (0, 120 - (40 / 3))-(logox, 120 - (40 / 3) + logoy), v%
    cutID (centerNode$)

    'search the center node for outgoing links
    node = 1
    OPEN "data\net\" + cutCAT$ + "\" + cutSCAT$ + "\" + centerNode$ + ".datinst" FOR INPUT AS #1
    DO
        INPUT #1, line$
        detP line$
        IF MID$(line$, 1, 5) = "link." THEN
            linkCount(node) = linkCount(node) + 1
            link$(node, linkCount(node)) = line$
        END IF
        IF MID$(line$, 1, 5) = "name." THEN
            nodeNAME$(node) = MID$(line$, 6, LEN(line$) - 5)
        END IF
    LOOP UNTIL EOF(1) = -1
    CLOSE #1

    'search the index, then each individual file for incoming links to the relevant node
    runn = 0: DO: runn = runn + 1
        node = node + 1
        OPEN "data\index.datnet" FOR INPUT AS #1
        DO
            INPUT #1, indexID$
            IF runn = 1 THEN 'outgoing links
                IF indexID$ <> centerNode$ THEN
                    node = node + 1
                    nodeID$(node) = indexID$
                    cutID indexID$
                    OPEN "data\net\" + cutCAT$ + "\" + cutSCAT$ + "\" + indexID$ + ".datinst" FOR INPUT AS #2
                    DO
                        INPUT #2, line$
                        detP line$
                        IF MID$(line$, p(0) + 1, p(1) - p(0) - 1) = "link" THEN
                            linkCount(node) = linkCount(node) + 1
                            link$(node, linkCount(node)) = line$
                        END IF
                        IF MID$(line$, p(0) + 1, p(1) - p(0) - 1) = "name" THEN
                            nodeNAME$(node) = MID$(line$, 6, LEN(line$) - 5)
                        END IF
                    LOOP UNTIL EOF(2) = -1
                    CLOSE #2
                END IF
            ELSE 'incoming links
                node = node + 1
                cutID indexID$
                OPEN "data\net\" + cutCAT$ + "\" + cutSCAT$ + "\" + indexID$ + ".datinst" FOR INPUT AS #2
                DO
                    INPUT #2, line$
                    detP line$
                    IF MID$(line$, 1, 5) = "link." THEN
                        node2 = 0: DO: node2 = node2 + 1
                            IF nodeNAME$(node2) = MID$(line$, p(9) + 1, p(10) - p(9) - 1) AND nodeID$(node2) = MID$(line$, p(6) + 1, p(9) - p(6) - 1) THEN
                                linkCount(node2) = linkCount(node2) + 1
                                link$(node2, linkCount(node2)) = line$
                            END IF
                        LOOP UNTIL node2 = maxnodes
                    END IF
                LOOP UNTIL EOF(2) = -1
                CLOSE #2
            END IF
        LOOP UNTIL EOF(1) = -1
        CLOSE #1
        IF runn = 1 THEN maxnodes = node: node = 0
    LOOP UNTIL runn = 2

    'sort
    IF maxnodes > 3 THEN
        'clear sorting array
        sort = 0: DO: sort = sort + 1
            sortCount(sort) = 0
            sortNAME$(sort) = ""
            sortID$(sort) = ""
            IF linkCount(sort) > 0 THEN l = 0: DO: l = l + 1: sortlink$(sort, l) = "": LOOP UNTIL l = linkCount(sort)
        LOOP UNTIL sort = maxnodes
        node = 1
        sortCount(1) = linkCount(1)
        sortNAME$(1) = nodeNAME$(1)
        sortID$(1) = nodeID$(1)
        IF sortCount(1) > 0 THEN l = 0: DO: l = l + 1: sortlink$(1, l) = link$(1, l): LOOP UNTIL l = sortCount(1)
        DO: node = node + 1
            sort = 1
            DO: sort = sort + 1
                IF sortCount(sort) <= linkCount(node) AND sortCount(sort) <> 0 THEN
                    sortin = sort
                    sort = maxnodes + 1: DO: sort = sort - 1
                        sortCount(sort) = sortCount(sort - 1): sortNAME$(sort) = sortNAME$(sort - 1): sortID$(sort) = sortID$(sort - 1)
                        IF sortCount(sort - 1) > 0 THEN l = 0: DO: l = l + 1: sortlink$(sort, l) = sortlink$(sort - 1, l): LOOP UNTIL l = sortCount(sort - 1)
                    LOOP UNTIL sort = sortin + 1
                    sortCount(sortin) = linkCount(node): sortNAME$(sortin) = nodeNAME$(node): sortID$(sortin) = nodeID$(node)
                    IF linkCount(node) > 0 THEN l = 0: DO: l = l + 1: sortlink$(sortin, l) = link$(node, l): LOOP UNTIL l = linkCount(node)
                    sortset = 1
                END IF
            LOOP UNTIL sort = maxnodes - 1 OR sortNAME$(sort) = "" OR sortset = 1
            IF sortNAME$(sort) = "" AND sortset = 0 THEN
                sortCount(sort) = linkCount(node): sortNAME$(sort) = nodeNAME$(node): sortID$(sort) = nodeID$(node)
                IF linkCount(node) > 0 THEN l = 0: DO: l = l + 1: sortlink$(sort, l) = link$(node, l): LOOP UNTIL l = linkCount(node)
            END IF
            nodeNAME$(node) = ""
            nodeID$(node) = ""
            IF linkCount(sort) > 0 THEN l = 0: DO: l = l + 1: link$(node, l) = "": LOOP UNTIL l = linkCount(node)
            linkCount(node) = 0
            style(node) = 0
            sortset = 0
        LOOP UNTIL node = maxnodes
    END IF

    '--------------- showing graph ------------------

    printgraph:
    COLOR colour&("black"), colour&("white")
    CLS
    COLOR colour&("white"), colour&("black")

    'console-button
    IF bstyle(1) = 1 THEN COLOR colour&("fg"), colour&("bg"): ELSE COLOR colour&("bg"), colour&("fg")
    LOCATE 2, 2
    IF delconfirm = 0 THEN PRINT "* CONSOLE": ELSE PRINT "* SURE ? "
    IF bstyle(2) = 1 THEN COLOR colour&("fg"), colour&("bg"): ELSE COLOR colour&("bg"), colour&("fg")
    LOCATE 4, 2
    IF delconfirm = 0 THEN PRINT "* BACK TO NODE": ELSE PRINT "* SURE ?      "

    graphscale = 35
    spiralstrength = 0.9
    midmainx = (maxx / 2) - (LEN(sortNAME$(1)) / 2 * fontwidth)
    midmainy = maxy / 2

    IF maxnodes > 0 THEN
        node = 0: DO: node = node + 1
            y(node) = INT((midmainy + (SIN(0.5 * _PI + (((node MOD 6) / 6) * 2 * _PI)) * (graphscale * (node * spiralstrength)))) / fontheight)
            x(node) = INT((midmainx + (SIN(((node MOD 6) / 6) * 2 * _PI) * (graphscale * (node * spiralstrength)))) / fontwidth)
        LOOP UNTIL node = maxnodes
        node = 0: DO: node = node + 1
            IF sortCount(node) > 0 THEN
                l = 0: DO: l = l + 1
                    detP sortlink$(node, l)
                    node2 = 0: DO: node2 = node2 + 1
                        IF MID$(sortlink$(node, l), p(9) + 1, p(10) - p(9) - 1) = sortNAME$(node2) AND sortNAME$(node2) <> "" AND MID$(sortlink$(node, l), p(9) + 1, p(10) - p(9) - 1) <> "" THEN
                            transparency = ((sortCount(node) / sortCount(2)) * 122) + 133
                            LINE ((x(node) + (LEN(sortNAME$(node)) / 2) - 1) * fontwidth, (y(node) * fontheight) - (fontheight / 2))-((x(node2) + (LEN(sortNAME$(node2)) / 2) - 1) * fontwidth, (y(node2) * fontheight) - (fontheight / 2)), _RGBA(15, 15, 15, transparency)
                        END IF
                    LOOP UNTIL node2 = maxnodes
                LOOP UNTIL l = sortCount(node)
            END IF
        LOOP UNTIL node = maxnodes
        node = 0: DO: node = node + 1
            transparency = ((sortCount(node) / sortCount(2)) * 122) + 133
            IF style(node) = 0 THEN COLOR _RGBA(237, 237, 237, transparency), _RGBA(15, 15, 15, transparency) ELSE COLOR _RGBA(15, 15, 15, transparency), _RGBA(237, 237, 237, transparency)
            LOCATE y(node), x(node)
            IF node = 1 THEN COLOR colour&("white"), colour&("black")
            PRINT sortNAME$(node)
            stylebf(node) = style(node)
        LOOP UNTIL node = maxnodes
    END IF
    DO
        IF resized = 1 THEN GOTO printgraph
        'keys
        Taste$ = INKEY$
        SELECT CASE Taste$
            CASE IS = CHR$(27)
                endnode$ = "0.0.0"
                GOTO gonewnode
            CASE IS = CHR$(13)
                endnode$ = centerNode$
                GOTO gonewnode
        END SELECT
        'selection for links and nodes
        IF _MOUSEINPUT = -1 THEN

            IF _MOUSEY > fontheight AND _MOUSEY < 2 * fontheight THEN
                IF _MOUSEX > fontwidth AND _MOUSEX < 10 * fontwidth THEN 'delete button
                    IF bstyle(1) = 0 THEN bstyle(1) = 1: GOTO printgraph
                ELSE
                    IF bstyle(1) = 1 THEN bstyle(1) = 0: GOTO printgraph
                END IF
            ELSE
                IF bstyle(1) = 1 THEN bstyle(1) = 0: GOTO printgraph
            END IF
            IF _MOUSEY > 3 * fontheight AND _MOUSEY < 4 * fontheight THEN
                IF _MOUSEX > fontwidth AND _MOUSEX < 15 * fontwidth THEN 'delete button
                    IF bstyle(2) = 0 THEN bstyle(2) = 1: GOTO printgraph
                ELSE
                    IF bstyle(2) = 1 THEN bstyle(2) = 0: GOTO printgraph
                END IF
            ELSE
                IF bstyle(2) = 1 THEN bstyle(2) = 0: GOTO printgraph
            END IF

            IF maxnodes > 0 THEN
                node = 0
                DO
                    node = node + 1
                    IF _MOUSEY > (y(node) - 1) * fontheight - 2 AND _MOUSEY < y(node) * fontheight + 2 THEN
                        IF _MOUSEX > (x(node) - 1) * fontwidth AND _MOUSEX < (x(node) + LEN(sortNAME$(node)) + 1) * fontwidth THEN
                            IF style(node) = 1 AND _MOUSEBUTTON(1) = -1 THEN
                                endnode$ = sortID$(node)
                                GOTO gonewnode
                            END IF
                            IF style(node) = 0 THEN style(node) = 1: GOTO printgraph
                        ELSE
                            IF style(node) = 1 THEN style(node) = 0: GOTO printgraph
                        END IF
                    ELSE
                        IF style(node) = 1 THEN style(node) = 0: GOTO printgraph
                    END IF
                LOOP UNTIL node = maxnodes
            END IF

        END IF

        IF _MOUSEBUTTON(1) = -1 THEN
            IF maxml > 0 THEN
                ml = 0
                DO
                    ml = ml + 1
                    IF style(ml) = 1 THEN
                        detP (mainlink$(ml))
                        IF nodeTV$ = MID$(mainlink$(ml), p(1) + 1, 5) THEN
                            reruninst$ = MID$(mainlink$(ml), p(6) + 1, 5)
                        ELSE
                            reruninst$ = MID$(mainlink$(ml), p(1) + 1, 5)
                        END IF
                        'ERASE mainlink$, mainlinktext$, mainlinky, mainlinkx
                        IF flip = 0 THEN flip = 1: ELSE flip = 0
                        viewNode reruninst$, flip
                        endgraph = 1
                    END IF
                LOOP UNTIL ml = maxml
            END IF
            b = 0
            DO
                b = b + 1
                IF bstyle(b) = 1 THEN
                    SELECT CASE b
                        CASE IS = 1 'console
                            endnode$ = "console"
                            GOTO gonewnode
                        CASE IS = 2 'back
                            endnode$ = centerNode$
                            GOTO gonewnode
                    END SELECT
                END IF
            LOOP UNTIL b = 3
        END IF

    LOOP UNTIL INKEY$ <> ""
    gonewnode:
    node = 0: DO: node = node + 1
        x(node) = 0
        y(node) = 0
        sortNAME$(node) = ""
        l = 0: DO: l = l + 1: sortlink$(node, l) = "": LOOP UNTIL sortlink$(node, l) = ""
        sortCount(node) = 0
    LOOP UNTIL node = maxnodes
END SUB

SUB cutID (IDtbc$)
    cutSID$ = ""
    cutCAT$ = ""
    cutSCAT$ = ""
    g = 0
    a = 0
    l = 1
    DO
        g = g + 1
        IF MID$(IDtbc$, g, 1) = "." THEN
            a = a + 1
            SELECT CASE a
                CASE IS = 1
                    cutSID$ = MID$(IDtbc$, l, g - l)
                CASE IS = 2
                    cutCAT$ = MID$(IDtbc$, l, g - l)
                CASE IS = 3
                    cutCAT$ = MID$(IDtbc$, l, g - l)
            END SELECT
            l = g + 1
        END IF
    LOOP UNTIL g = LEN(IDtbc$)
    cutSCAT$ = MID$(IDtbc$, l, g - l + 1)
    'PRINT IDtbc$, cutCAT$, cutSCAT$
    'SLEEP
END SUB

SUB loadsettings
    IF _DIREXISTS("data") = 0 THEN MKDIR "data"
    PRINT "[ loading settings...]"
    IF _FILEEXISTS("data\settings.datnet") = 0 THEN
        writesettings: OPEN "data\settings.datnet" FOR OUTPUT AS #1
        tutorial = 1: WRITE #1, 1
        security = 1: WRITE #1, 1
        windowscale = 1.1: WRITE #1, 1.1
        license$ = "": WRITE #1, _DEFLATE$("")
    ELSE
        OPEN "data\settings.datnet" FOR INPUT AS #1
        IF EOF(1) = 0 THEN
            INPUT #1, tutorial
            INPUT #1, security
            INPUT #1, windowscale
            INPUT #1, license$: license$ = _INFLATE$(license$)
        ELSE CLOSE #1: GOTO writesettings
        END IF
    END IF
    CLOSE #1
END SUB

SUB loaddata 'not only loads, but also repairs the database
    'NewText 1, 0, "[ loading data... ]", 0
    'RunMenu
    CLS
    PRINT "[ loading data...]"
    'loads broad infrastructure
    IF _DIREXISTS("data\net") = 0 THEN MKDIR "data\net"
    IF _DIREXISTS("data\bg") = 0 THEN
        MKDIR "data\bg"
        'download graphics + katakana font
    END IF

    PRINT "[ checking license...]"
    IF license$ <> "" THEN
        IF checkLicense(license$) = 1 THEN
            PRINT "[ license active! ]"
        ELSE
            PRINT "[ license invalid. ]"
            license$ = ""
            OPEN "data\settings.datnet" FOR OUTPUT AS #1
            WRITE #1, tutorial
            WRITE #1, security
            WRITE #1, windowscale
            WRITE #1, _DEFLATE$(license$)
            CLOSE #1
        END IF
    ELSE
        PRINT "[ no license found. ]"
    END IF

    PRINT "[ loading allowed characters...]"
    'loads allowed characters
    IF _FILEEXISTS("data\AC.tcc") = 0 THEN
        OPEN "data\AC.tcc" FOR OUTPUT AS #1
        a = 31
        DO
            a = a + 1
            WRITE #1, CHR$(a)
        LOOP UNTIL a = 122
        CLOSE #1
    END IF
    OPEN "data\AC.tcc" FOR INPUT AS #1
    ac = 0
    DO
        ac = ac + 1
        INPUT #1, ac$(ac)
    LOOP UNTIL EOF(1) = -1
    alch = ac
    CLOSE #1

    PRINT "[ loading categories...]"
    'loads base categories
    IF _DIREXISTS("data\infrs") = 0 THEN MKDIR "data\infrs"
    IF _FILEEXISTS("data\infrs\cat1.datnet") = -1 THEN
        OPEN "data\infrs\cat1.datnet" FOR INPUT AS #1
    ELSE
        OPEN "data\infrs\cat1.datnet" FOR OUTPUT AS #1
        WRITE #1, "living"
        WRITE #1, "physi"
        WRITE #1, "concept"
        CLOSE #1
        OPEN "data\infrs\cat1.datnet" FOR INPUT AS #1
    END IF
    IF EOF(1) = 0 THEN
        c = 0
        DO
            c = c + 1
            INPUT #1, cat$(c)
            IF LEN(cat$(c)) > 2 THEN
                IF _DIREXISTS("data\net\" + LTRIM$(STR$(c))) = 0 THEN MKDIR "data\net\" + LTRIM$(STR$(c))
            END IF
        LOOP UNTIL EOF(1) = -1
    END IF
    maxcategories = c
    CLOSE #1

    PRINT "[ loading subcategories...]"
    'loads subcategories
    cat = 0
    DO
        cat = cat + 1
        IF _FILEEXISTS("data\infrs\scat" + LTRIM$(STR$(cat)) + ".datnet") = 0 THEN
            OPEN "data\infrs\scat" + LTRIM$(STR$(cat)) + ".datnet" FOR OUTPUT AS #1
            SELECT CASE cat
                CASE IS = 1
                    WRITE #1, "person"
                    WRITE #1, "animal"
                    WRITE #1, "plant"
                CASE IS = 2
                    WRITE #1, "cal"
                CASE IS = 3
                    WRITE #1, "group"
                    WRITE #1, "information"
                    WRITE #1, "term"
            END SELECT
            CLOSE #1
        END IF
        IF _DIREXISTS("data\net\" + LTRIM$(STR$(cat))) = 0 THEN MKDIR ("data\net\" + LTRIM$(STR$(cat)))
        IF cat = 1 OR cat = 3 THEN 'number of subcategories per category - this is a dumb way of doing it, maybe rewrite at some point
            scatlimit = 3
        ELSE
            scatlimit = 1
        END IF
        scat = 0
        IF scatlimit > 0 THEN
            DO
                scat = scat + 1
                IF _DIREXISTS("data\net\" + LTRIM$(STR$(cat)) + "\" + LTRIM$(STR$(scat))) = 0 THEN
                    PRINT "data\net\" + LTRIM$(STR$(cat)) + "\" + LTRIM$(STR$(scat))
                    MKDIR "data\net\" + LTRIM$(STR$(cat)) + "\" + LTRIM$(STR$(scat))
                END IF
            LOOP UNTIL scat = scatlimit
        END IF
    LOOP UNTIL cat = maxcategories

    PRINT "[ loading index...]"
    'loads index file for nodes
    IF _FILEEXISTS("data\index.datnet") = -1 THEN
        OPEN "data\index.datnet" FOR INPUT AS #1
    ELSE
        OPEN "data\index.datnet" FOR OUTPUT AS #1
        CLOSE #1
        OPEN "data\index.datnet" FOR INPUT AS #1
    END IF
    IF EOF(1) = 0 THEN
        node = 0
        DO
            node = node + 1
            INPUT #1, nodeID$(node)
        LOOP UNTIL EOF(1) = -1
        maxnodes = node
    ELSE
        maxnodes = 0
        nodeID$(0) = "00"
    END IF
    CLOSE #1
END SUB

SUB ClearMenu
    i = 0
    DO
        i = i + 1
        c(i) = 0
        gbf(i) = 0
        type$(i) = ""
        UserInput$(i) = ""
        status$(i) = ""
        kind$(i) = ""
    LOOP UNTIL i = maxmenuitems
    i = 0
    'ERASE c, gbf, char$, type$, UserInput$, status$, kind$, destination$, style, defaultstyle, basex, endx, basey, endy
    maxm = 0
    activei = 1
    maxii = 0
END SUB

SUB RunMenu (background$)
    reprintmenu:
    COLOR colour&("fg"), colour&("bg")
    CLS
    SELECT CASE background$
        CASE IS = "c"
            _PUTIMAGE (0, 120 - (40 / 3))-(logox, 120 - (40 / 3) + logoy), c%
        CASE IS = "e"
            _PUTIMAGE (0, 120 - (40 / 3))-(logox, 120 - (40 / 3) + logoy), e%
        CASE IS = "v"
            _PUTIMAGE (0, 120 - (40 / 3))-(logox, 120 - (40 / 3) + logoy), v%
        CASE IS = "n"
            _PUTIMAGE (0, 120 - (40 / 3))-(logox, 120 - (40 / 3) + logoy), n%
        CASE IS = "t1"
            _PUTIMAGE ((maxx / 2) - (twidth / 2), (maxy / 2) - (theight / 2))-((maxx / 2) + (twidth / 2), (maxy / 2) + (theight / 2)), t1%
        CASE IS = "t2"
            _PUTIMAGE ((maxx / 2) - (twidth / 2), (maxy / 2) - (theight / 2))-((maxx / 2) + (twidth / 2), (maxy / 2) + (theight / 2)), t2%
    END SELECT

    'zu = 0
    'DO
    '    zu = zu + 4
    '    LINE (0, zu)-(maxx, zu + 1), _RGBA(0, 0, 0, 5), BF
    'LOOP UNTIL zu >= maxy

    'katakana title
    _FONT fj&
    IF MID$(background$, 1, 1) <> "t" THEN
        LOCATE firstline / 4 + 3, xcharacter / 4 - 7
    ELSE
        LOCATE ((((maxy / 2) - (theight / 2)) / fontheight) / 4) + 1, ((maxrows / 2) / 4) - 1
    END IF
    'PRINT "DATANET"
    COLOR colour&("black"), colour&("transparent")
    COLOR colour&("yellow"): PRINT "7";
    COLOR colour&("orange"): PRINT "k";
    COLOR colour&("magenta"): PRINT "&";
    COLOR colour&("purple"): PRINT "T";
    COLOR colour&("blue"): PRINT "D";
    COLOR colour&("green"): PRINT CHR$(96)
    COLOR colour&("black")

    IF MID$(background$, 1, 1) <> "t" THEN
        _FONT fb&
        LOCATE firstline + 16, xcharacter - 13 - 15
        PRINT LTRIM$(STR$(maxnodes));
        _FONT fr&
        LOCATE firstline + 16, xcharacter - 13 - 5
        PRINT "NODES"
        SELECT CASE maxnodes / nodelimit
            CASE IS < 0.25
                LINE ((xcharacter - 37) * fontwidth, (firstline + 15) * fontheight + 2)-((xcharacter - 22) * fontwidth, (firstline + 16) * fontheight - 2), colour&("green"), B
                LINE ((xcharacter - 37) * fontwidth, (firstline + 15) * fontheight + 2)-((xcharacter - 37 + (15 * (maxnodes / nodelimit))) * fontwidth, (firstline + 16) * fontheight - 2), colour&("green"), BF
            CASE IS < 0.5
                LINE ((xcharacter - 37) * fontwidth, (firstline + 15) * fontheight + 2)-((xcharacter - 22) * fontwidth, (firstline + 16) * fontheight - 2), colour&("yellow"), B
                LINE ((xcharacter - 37) * fontwidth, (firstline + 15) * fontheight + 2)-((xcharacter - 37 + (15 * (maxnodes / nodelimit))) * fontwidth, (firstline + 16) * fontheight - 2), colour&("yellow"), BF
            CASE IS < 0.75
                LINE ((xcharacter - 37) * fontwidth, (firstline + 15) * fontheight + 2)-((xcharacter - 22) * fontwidth, (firstline + 16) * fontheight - 2), colour&("orange"), B
                LINE ((xcharacter - 37) * fontwidth, (firstline + 15) * fontheight + 2)-((xcharacter - 37 + (15 * (maxnodes / nodelimit))) * fontwidth, (firstline + 16) * fontheight - 2), colour&("orange"), BF
            CASE IS < 1
                LINE ((xcharacter - 37) * fontwidth, (firstline + 15) * fontheight + 2)-((xcharacter - 22) * fontwidth, (firstline + 16) * fontheight - 2), colour&("orange"), B
                LINE ((xcharacter - 37) * fontwidth, (firstline + 15) * fontheight + 2)-((xcharacter - 37 + (15 * (maxnodes / nodelimit))) * fontwidth, (firstline + 16) * fontheight - 2), colour&("orange"), BF
        END SELECT
        LOCATE firstline + 17, xcharacter - 13 - 23
        PRINT "ALL SYSTEMS OPERATIONAL"
    END IF

    _FONT fr&
    LOCATE 2, maxrows - LEN(version$) - 1
    PRINT version$
    LOCATE 2, 3
    PRINT "x"

    endparameter$ = ""
    maxm = i
    i = 0
    DO
        i = i + 1
        firstprint(i) = 0
    LOOP UNTIL type$(i) = ""
    activei = 1
    DO
        IF resized = 1 THEN GOTO reprintmenu
        Taste$ = INKEY$
        SELECT CASE Taste$
            CASE IS = CHR$(13)
                IF activei < maxii AND inter(activei) = 1 THEN
                    status$(activei) = "finished"
                    activei = activei + 1
                ELSEIF inter(activei) = 1 THEN
                    status$(activei) = "finished"
                END IF
            CASE IS = CHR$(0) + CHR$(80)
                IF activei < maxm THEN
                    activei = activei + 1
                ELSE
                    activei = 1
                END IF
            CASE IS = CHR$(0) + CHR$(72)
                IF activei > 1 THEN
                    activei = activei - 1
                ELSE
                    activei = maxm
                END IF
            CASE IS = CHR$(0) + CHR$(60)
                IF activei < maxm THEN
                    activei = activei + 1
                ELSE
                    activei = 1
                END IF
            CASE IS = CHR$(0) + CHR$(59)
                IF activei > 1 THEN
                    activei = activei - 1
                ELSE
                    activei = maxm
                END IF
        END SELECT
        mouseinput = _MOUSEINPUT
        mousebutton = _MOUSEBUTTON(1)
        mousex = _MOUSEX
        mousey = _MOUSEY
        IF mouseinput = -1 THEN
            IF mousebutton = -1 THEN
                IF mousey >= fontheight AND mousey <= 2 * fontheight THEN
                    IF mousex >= 2 * fontwidth AND mousex <= 3 * fontwidth THEN 'close button
                        SYSTEM
                    END IF
                    'IF mousex >= 4 * fontwidth AND mousex <= 5 * fontwidth THEN 'minimize button
                    '    _SCREENICON
                    '    mousex = 0: mousey = 0
                    'END IF
                END IF
                IF mousey < 3 * fontheight THEN
                    DO
                        mouseinput = _MOUSEINPUT
                        mousebutton = _MOUSEBUTTON(1)
                    LOOP UNTIL mousebutton <> -1
                    _SCREENMOVE _DESKTOPWIDTH / 2 - (_WIDTH / 2) + (_MOUSEX - mousex), _DESKTOPHEIGHT / 2 - (_HEIGHT / 2) + (_MOUSEY - mousey)
                END IF
            END IF
        END IF
        i = 0
        DO
            i = i + 1
            SELECT CASE type$(i)
                CASE IS = "text"
                    checki = 0
                    DO
                        checki = checki + 1
                        IF overlaptrigger(checki) = 1 AND checki < i THEN
                            overlaptrigger(checki) = 0
                            firstprint(i) = 0
                        END IF
                    LOOP UNTIL checki = maxm
                    IF firstprint(i) = 0 THEN
                        firstprint(i) = 1
                        LOCATE firstline + linetbp(i), xcharacter + xoffset(i)
                        SELECT CASE style(i)
                            CASE IS = 0
                                COLOR colour&("black"), colour&("white")
                            CASE IS = 1
                                COLOR colour&("green"), colour&("white") 'green
                            CASE IS = 2
                                COLOR colour&("red"), colour&("white") 'red
                        END SELECT
                        PRINT text$(i)
                    END IF
                CASE IS = "switch"
                    'TBD
                CASE IS = "button"
                    checki = 0
                    DO
                        checki = checki + 1
                        IF overlaptrigger(checki) = 1 AND checki < i THEN
                            overlaptrigger(checki) = 0
                            firstprint(i) = 0
                        END IF
                    LOOP UNTIL checki = maxm
                    IF firstprint(i) = 0 THEN
                        firstprint(i) = 1
                        SELECT CASE style(i)
                            CASE IS = 0 'default
                                LINE (basex(i), basey(i))-(endx(i), endy(i)), colour&("white"), BF
                                LINE (basex(i), basey(i))-(endx(i), endy(i)), colour&("black"), B
                                COLOR colour&("black"), colour&("white")
                            CASE IS = 1 'filled
                                LINE (basex(i), basey(i))-(endx(i), endy(i)), colour&("black"), BF
                                COLOR colour&("white"), colour&("black")
                            CASE IS = 2 'colored (green)
                                LINE (basex(i), basey(i))-(endx(i), endy(i)), colour&("green"), BF
                                COLOR colour&("white"), colour&("green")
                            CASE IS = 3 'color border (green)
                                LINE (basex(i), basey(i))-(endx(i), endy(i)), colour&("white"), B
                                LINE (basex(i), basey(i))-(endx(i), endy(i)), colour&("green"), B
                                COLOR colour&("green"), colour&("white")
                        END SELECT
                        LOCATE firstline + linetbp(i), xcharacter + xoffset(i)
                        PRINT text$(i)
                        COLOR colour&("black"), colour&("white")
                        LOCATE firstline + linetbp(i), xcharacter + xoffset(i) + LEN(text$(i)) + 2
                        PRINT fileshort$(i)
                    END IF
                    IF _MOUSEINPUT = -1 THEN
                        IF _MOUSEX > basex(i) AND _MOUSEX < endx(i) THEN
                            IF _MOUSEY > basey(i) AND _MOUSEY < endy(i) THEN
                                SELECT CASE defaultstyle(i)
                                    CASE IS = 0
                                        style(i) = 1
                                    CASE IS = 1
                                        style(i) = 0
                                    CASE IS = 2
                                        style(i) = 3
                                    CASE IS = 3
                                        style(i) = 2
                                END SELECT
                                IF firstprint(i) = 1 THEN firstprint(i) = 0
                                IF _MOUSEBUTTON(1) = -1 THEN
                                    SELECT CASE kind$(i)
                                        CASE IS = "confirmation"
                                            endparameter$ = "true"
                                            status$(maxii) = "finished"
                                        CASE IS = "redirect" 'used for going to another marker, menu or similar
                                            SELECT CASE destination$(i)
                                                CASE IS = "end menu"
                                                    status$(maxii) = "finished"
                                            END SELECT
                                            IF MID$(destination$(i), LEN(destination$(i)), 1) = ":" THEN
                                                status$(maxii) = "finished"
                                                endparameter$ = destination$(i)
                                            END IF
                                        CASE IS = "open file"
                                            'IF acceptinput(i) = 1 THEN
                                            'hWnd& = FindWindow(0, "DATANET" + CHR$(0)) 'get window handle using _TITLE string
                                            '' Do the Open File dialog call!
                                            'Filter$ = "Text files (*.txt)|*.TXT|mp3 files (*.mp3)|*.MP3|wav files (*.wav|*.WAV|DATANET files (*.net)|*.NET|All files (*.*)|*.*" + CHR$(0)
                                            'Flags& = OFN_FILEMUSTEXIST + OFN_NOCHANGEDIR + OFN_READONLY '    add flag constants here
                                            'file$(i) = GetOpenFileName$("Select File", ".\", Filter$, 1, Flags&, hWnd&)
                                            'acceptinput(i) = 0
                                            'LASTTIMER = TIMER
                                            'ELSE
                                            '    IF TIMER - LASTTIMER > 1 THEN
                                            '        acceptinput(i) = 1
                                            '    END IF
                                            '    END IF
                                        CASE IS = "save file"
                                            'IF acceptinput(i) = 1 THEN
                                            '    hWnd& = FindWindow(0, "DATANET" + CHR$(0)) 'get window handle using _TITLE string
                                            '    ' Do the Save File dialog call!
                                            '    Filter$ = "DATANET files (*.net)|*.NET" + CHR$(0)
                                            '    Flags& = OFN_FILEMUSTEXIST + OFN_NOCHANGEDIR + OFN_READONLY '    add flag constants here
                                            '    file$(i) = GetSaveFileName$("Save File", ".\", Filter$, 1, Flags&, hWnd&)
                                            '    acceptinput(i) = 0
                                            'ELSE
                                            '    IF TIMER - LASTTIMER > 1 THEN
                                            '        acceptinput(i) = 1
                                            '    END IF
                                            'END IF
                                    END SELECT
                                    COLOR colour&("black"), colour&("white")
                                    LOCATE firstline + linetbp(i), xcharacter + xoffset(i) + LEN(text$(i)) + 2
                                    IF file$(i) <> "" THEN
                                        p = 0
                                        DO
                                            p = p + 1
                                            IF MID$(file$(i), p, 1) = "\" THEN
                                                fileshort$(i) = MID$(file$(i), p + 1, LEN(file$(i)) - p)
                                            END IF
                                        LOOP UNTIL p = LEN(file$(i))
                                        PRINT fileshort$(i)
                                    ELSE
                                        fileshort$(i) = ""
                                    END IF
                                END IF
                            ELSE
                                IF defaultstyle(i) <> style(i) THEN style(i) = defaultstyle(i): firstprint(i) = 0
                            END IF
                        ELSE
                            IF defaultstyle(i) <> style(i) THEN style(i) = defaultstyle(i): firstprint(i) = 0
                        END IF
                    END IF
                CASE IS = "slider"
                    checki = 0
                    DO
                        checki = checki + 1
                        IF overlaptrigger(checki) = 1 AND checki < i THEN
                            overlaptrigger(checki) = 0
                            firstprint(i) = 0
                        END IF
                    LOOP UNTIL checki = maxm
                    IF firstprint(i) = 0 THEN
                        firstprint(i) = 1
                        LOCATE firstline + linetbp(i), xcharacter + xoffset(i)
                        COLOR colour&("black"), colour&("white")
                        PRINT text$(i)
                        'horizontal
                        LINE (basex(i), (firstline + linetbp(i) - 1) * fontheight + (fontheight / 2))-(basex(i) + (maxx / 4), (firstline + linetbp(i) - 1) * fontheight + (fontheight / 2) + 1), colour&("black"), BF
                        'vertical
                        LINE (basex(i) + (value(i) / maxval(i) * (maxx / 4)), (firstline + linetbp(i) - 1) * fontheight + 1)-(basex(i) + 4 + (value(i) / maxval(i) * (maxx / 4)), (firstline + linetbp(i)) * fontheight - 1), colour&("black"), BF
                        xvalue(i) = xcharacter + xoffset(i) + LEN(text$(i)) + ((maxx / 4) / fontwidth) + 2
                        LOCATE firstline + linetbp(i), xvalue(i)
                        PRINT value(i); "   "
                    END IF
                    IF _MOUSEINPUT = -1 THEN
                        IF _MOUSEBUTTON(1) = -1 THEN
                            IF _MOUSEY > (firstline + linetbp(i) - 1) * fontheight + 1 AND _MOUSEY < (firstline + linetbp(i)) * fontheight - 1 THEN
                                DO
                                    IF _MOUSEINPUT = -1 AND _MOUSEX > basex(i) AND _MOUSEX < endx(i) THEN
                                        value(i) = _MOUSEX - (basex(i))
                                        value(i) = INT(value(i) / (maxx / 4) * 100)
                                        printslider:
                                        'cleanup
                                        LINE (basex(i), (firstline + linetbp(i) - 1) * fontheight + 1)-(endx(i), (firstline + linetbp(i)) * fontheight - 1), colour&("white"), BF

                                        'horizontal
                                        LINE (basex(i), (firstline + linetbp(i) - 1) * fontheight + (fontheight / 2))-(endx(i), (firstline + linetbp(i) - 1) * fontheight + (fontheight / 2) + 1), colour&("black"), BF
                                        'vertical
                                        LINE (basex(i) + (value(i) / maxval(i) * (maxx / 4)), (firstline + linetbp(i) - 1) * fontheight + 1)-(basex(i) + 4 + (value(i) / maxval(i) * (maxx / 4)), (firstline + linetbp(i)) * fontheight - 1), colour&("black"), BF
                                        xvalue(i) = xcharacter + xoffset(i) + LEN(text$(i)) + ((maxx / 4) / fontwidth) + 2
                                        LOCATE firstline + linetbp(i), xvalue(i)
                                        PRINT value(i); "   "
                                    ELSEIF _MOUSEINPUT = -1 AND _MOUSEX < basex(i) OR _MOUSEX > basex(i) + 4 + (maxx / 4) THEN
                                        IF _MOUSEX < basex(i) THEN
                                            value(i) = minval(i)
                                        ELSE
                                            value(i) = maxval(i)
                                        END IF
                                        GOTO printslider
                                    END IF
                                LOOP UNTIL _MOUSEBUTTON(1) = 0
                                IF text$(i) = "Window size" THEN
                                    windowscale = (value(i) / maxval(i)) * 1 + 0.5
                                    OPEN "data\settings.datnet" FOR OUTPUT AS #1
                                    WRITE #1, tutorial: WRITE #1, security: WRITE #1, windowscale: WRITE #1, license$
                                    CLOSE #1
                                    resizeWindow
                                    GOTO reprintmenu
                                END IF
                            END IF
                        END IF
                    END IF
                CASE IS = "dropdown"
                    IF activei = i THEN 'only enables keys if dropdown menu is active
                        SELECT CASE Taste$
                            CASE IS = CHR$(9)
                                IF expanded(i) = 1 THEN
                                    expanded(i) = 0
                                    overlaptrigger(i) = 1
                                    GOTO reprint
                                ELSE
                                    expanded(i) = 1
                                    GOTO reprint
                                END IF
                            CASE IS = CHR$(0) + CHR$(72)
                                IF selected(i) > 1 THEN
                                    selected(i) = selected(i) - 1
                                ELSE
                                    selected(i) = totalcat(i)
                                END IF
                                GOTO reprint
                            CASE IS = CHR$(0) + CHR$(80)
                                IF selected(i) < totalcat(i) THEN
                                    selected(i) = selected(i) + 1
                                ELSE
                                    selected(i) = 1
                                END IF
                                GOTO reprint
                        END SELECT
                        IF Taste$ <> "" AND activei < maxm THEN
                            activei = activei + 1
                        ELSE
                            activei = 1
                        END IF
                    END IF
                    IF _MOUSEINPUT = -1 THEN
                        selline = INT(_MOUSEY / (fontheight)) + 1
                        IF selline > (firstline + linetbp(i) - 1) AND selline < (firstline + linetbp(i) + (totalcat(i) * 2) - 1) THEN
                            mouseselected = INT((selline - (firstline + linetbp(i) - 1)) / 2) + 1
                            IF mouseselected <> selected(i) AND expanded(i) = 1 THEN
                                selected(i) = mouseselected
                                GOTO reprint
                            END IF
                        END IF
                        IF _MOUSEBUTTON(1) = -1 THEN
                            IF _MOUSEX > basex(i) AND _MOUSEX < endx(i) THEN
                                IF expanded(i) = 1 THEN
                                    IF _MOUSEY > (firstline + linetbp(i) - 1) * fontheight - (fontheight / 3) + borderoffsety(i) AND _MOUSEY < (firstline + linetbp(i) + (totalcat(i) * 2) - 1) * fontheight + (fontheight / 2) + borderoffsety(i) THEN
                                        expanded(i) = 0
                                        overlaptrigger(i) = 1
                                        GOTO reprint
                                    END IF
                                ELSE
                                    IF _MOUSEY > (firstline + linetbp(i) - 1) * fontheight - (fontheight / 3) + borderoffsety(i) AND _MOUSEY < (firstline + linetbp(i)) * fontheight + (fontheight / 2) + borderoffsety(i) THEN
                                        expanded(i) = 1
                                        GOTO reprint
                                    END IF
                                END IF
                            END IF
                        END IF
                    END IF
                    IF firstprint(i) = 0 THEN 'only prints menu once and sets variables, loads menu etc..
                        firstprint(i) = 1
                        CLOSE #1
                        OPEN menufile$(i) FOR INPUT AS #1
                        IF EOF(1) = -1 THEN
                            PRINT "menu file empty, can't display dropdown menu."
                            SLEEP 3
                            SYSTEM
                        END IF
                        selected(i) = 1
                        maxcatlength(i) = 0
                        c = 0
                        m = 0
                        DO
                            c = c + 1
                            INPUT #1, category$(i, c), subcount(i, c)
                            m = m + 1
                            menu$(i, m) = category$(i, c)
                            IF LEN(category$(i, c)) > maxcatlength(i) THEN
                                maxcatlength(i) = LEN(category$(i, c)) + 1
                            END IF
                            IF subcount(i, c) > 0 THEN
                                sc = 0
                                DO
                                    sc = sc + 1
                                    INPUT #1, subcategory$(i, c, sc)
                                    m = m + 1
                                    menu$(i, m) = "  " + subcategory$(i, c, sc)
                                    IF LEN(subcategory$(i, c, sc)) > maxcatlength(i) THEN
                                        maxcatlength(i) = LEN(subcategory$(i, c, sc)) + 4
                                    END IF
                                LOOP UNTIL sc = subcount(i, c)
                            END IF
                        LOOP UNTIL EOF(1) = -1
                        'categories(i) = c
                        totalcat(i) = m
                        endx(i) = (xcharacter + xoffset(i) + LEN(text$(i)) + maxcatlength(i)) * fontwidth + (fontwidth / 2) + borderoffsetx(i) 'right end of dropdown box
                        CLOSE #1

                        'prints text in front of dropdown
                        LOCATE firstline + linetbp(i), xcharacter + xoffset(i)
                        COLOR colour&("black"), colour&("white")
                        PRINT text$(i)

                        reprint: 'triggered when a change occurs
                        activei = i
                        IF expanded(i) = 0 THEN
                            LINE (basex(i), (firstline + linetbp(i) - 1) * fontheight - (fontheight / 3) + borderoffsety(i))-(endx(i), (firstline + linetbp(i) + (totalcat(i) * 2) - 1) * fontheight + (fontheight / 2) + borderoffsety(i)), colour&("white"), BF
                            LINE (basex(i), (firstline + linetbp(i) - 1) * fontheight - (fontheight / 3) + borderoffsety(i))-(endx(i), (firstline + linetbp(i)) * fontheight + (fontheight / 2) + borderoffsety(i)), colour&("black"), B
                            COLOR colour&("black"), colour&("white")
                            LOCATE firstline + linetbp(i), xcharacter + xoffset(i) + LEN(text$(i)) + 1
                            PRINT menu$(i, selected(i))
                        ELSE
                            LINE (basex(i), (firstline + linetbp(i) - 1) * fontheight - (fontheight / 3) + borderoffsety(i))-(endx(i), (firstline + linetbp(i) + (totalcat(i) * 2) - 1) * fontheight + (fontheight / 2) + borderoffsety(i)), colour&("white"), BF
                            m = 0
                            DO
                                m = m + 1
                                IF m = selected(i) THEN
                                    COLOR colour&("white"), colour&("yellow")
                                ELSE
                                    COLOR colour&("black"), colour&("white")
                                END IF
                                LOCATE firstline + linetbp(i) + (m * 2) - 2, xcharacter + xoffset(i) + LEN(text$(i)) + 1
                                PRINT menu$(i, m)
                            LOOP UNTIL m = totalcat(i)
                            LINE (basex(i), (firstline + linetbp(i) - 1) * fontheight - (fontheight / 3) + borderoffsety(i))-(endx(i), (firstline + linetbp(i) + (totalcat(i) * 2) - 1) * fontheight + (fontheight / 2) + borderoffsety(i)), colour&("black"), B
                        END IF

                        'variables for dropdown triangle (right side, you know?)
                        p1x(i) = (xcharacter + xoffset(i) + LEN(text$(i)) + maxcatlength(i) - 1) * fontwidth
                        p1y(i) = (firstline + linetbp(i) - 1) * fontheight + (fontheight / 3) + borderoffsety - 3
                        p2x(i) = p1x(i) + fontwidth
                        p2y(i) = p1y(i)
                        p3x(i) = p1x(i) + ((p2x(i) - p1x(i)) / 2)
                        p3y(i) = p1y(i) + SQR(((fontwidth / 2) * (fontwidth / 2)) + (fontwidth * fontwidth)) - (fontheight / 4)

                        'triangle
                        LINE (p1x(i), p1y(i))-(p2x(i), p2y(i)), colour&("black")
                        LINE (p1x(i), p1y(i))-(p3x(i), p3y(i)), colour&("black")
                        LINE (p2x(i), p2y(i))-(p3x(i), p3y(i)), colour&("black")
                        PAINT (p1x(i) + ((p2x(i) - p1x(i)) / 2), p1y(i) + ((p3y(i) - p1y(i)) / 2)), colour&("black"), colour&("black")
                    END IF
                CASE IS = "input"
                    checki = 0
                    DO
                        checki = checki + 1
                        IF overlaptrigger(checki) = 1 AND checki < i THEN
                            overlaptrigger(checki) = 0
                            firstprint(i) = 0
                            selectedall = 0
                        END IF
                    LOOP UNTIL checki = maxm
                    COLOR colour&("black"), colour&("white")
                    IF firstprint(i) = 0 THEN
                        firstprint(i) = 1
                        IF fill$(i) <> "" AND c(i) = 0 THEN
                            DO
                                c(i) = c(i) + 1
                                char$(i, c(i)) = MID$(fill$(i), c(i), 1)
                            LOOP UNTIL c(i) = LEN(fill$(i))
                            gbf(i) = c(i)
                            LOCATE firstline + linetbp(i), xcharacter + xoffset(i)
                            PRINT text$(i);
                            IF selectedall = 0 THEN
                                COLOR colour&("black"), colour&("white")
                            ELSE
                                COLOR colour&("white"), colour&("black")
                            END IF
                            PRINT fill$(i);
                        END IF
                        printback:
                        IF backspace(i) = 1 AND c(i) > 0 AND selectedall = 0 THEN
                            LINE ((xcharacter + xoffset(i) + LEN(text$(i)) + editpos(i) - 1) * fontwidth + (fontwidth / 2), (firstline + linetbp(i) - 1) * fontheight - (fontheight / 3))-((xcharacter + xoffset(i) + LEN(text$(i)) + editpos(i) - 1) * fontwidth + (fontwidth / 2) + 2, (firstline + linetbp(i)) * fontheight + (fontheight / 2)), colour&("white"), BF
                            backspace(i) = 0
                            COLOR colour&("black"), colour&("white")
                            LOCATE firstline + linetbp(i), xcharacter + xoffset(i) + LEN(text$(i)) + c(i) - 1
                            PRINT " ";
                            LOCATE firstline + linetbp(i), xcharacter + xoffset(i)
                            PRINT text$(i);
                            COLOR colour&("black"), colour&("white")
                            gbf(i) = 0
                            trigger(i) = 0
                            DO
                                gbf(i) = gbf(i) + 1
                                IF gbf(i) <> editpos(i) THEN
                                    IF trigger(i) = 1 THEN
                                        char$(i, gbf(i)) = char$(i, gbf(i) + 1)
                                    END IF
                                    PRINT char$(i, gbf(i));
                                ELSE
                                    char$(i, gbf(i)) = char$(i, gbf(i) + 1)
                                    PRINT char$(i, gbf(i));
                                    trigger(i) = 1
                                END IF
                            LOOP UNTIL gbf(i) >= c(i) - 1
                            char$(i, c(i)) = ""
                            c(i) = c(i) - 1
                        ELSEIF backspace(i) = 1 AND c(i) > 0 AND selectedall = 1 THEN
                            COLOR colour&("black"), colour&("white")
                            selectedall = 0
                            gbf(i) = 0
                            DO
                                gbf(i) = gbf(i) + 1
                                char$(i, gbf(i)) = ""
                            LOOP UNTIL gbf(i) = c(i)
                            gbf(i) = c(i)
                            c(i) = 0
                            LINE ((xcharacter + xoffset(i) + LEN(text$(i)) - 1) * fontwidth, (firstline + linetbp(i) - 1) * fontheight - (fontheight / 3))-((xcharacter + xoffset(i) + LEN(text$(i)) + gbf(i) + 1) * fontwidth + (fontwidth / 2) + 2, (firstline + linetbp(i)) * fontheight + (fontheight / 2)), colour&("white"), BF
                            backspace(i) = 0
                            LOCATE firstline + linetbp(i), xcharacter + xoffset(i)
                            PRINT text$(i);
                            IF fill$(i) <> "" THEN
                                DO
                                    c(i) = c(i) + 1
                                    char$(i, c(i)) = MID$(fill$(i), c(i), 1)
                                LOOP UNTIL c(i) = LEN(fill$(i))
                                gbf(i) = c(i)
                                PRINT fill$(i);
                            END IF
                        ELSE
                            LOCATE firstline + linetbp(i), xcharacter + xoffset(i)
                            COLOR colour&("black"), colour&("white")
                            PRINT text$(i);
                        END IF
                        Taste$ = ""
                    END IF
                    IF activei = i THEN 'only checks keys if input is active
                        loopback:
                        IF Taste$ <> "" THEN
                            ac = 0
                            DO
                                ac = ac + 1
                                IF ac$(ac) = Taste$ THEN
                                    c(i) = c(i) + 1
                                    char$(i, c(i)) = Taste$
                                END IF
                            LOOP UNTIL ac = alch
                        END IF
                        editpos(i) = c(i)

                        'prints the new character if g has changed
                        IF gbf(i) <> c(i) OR selectedall <> colorbf THEN
                            IF gbf(i) < c(i) OR selectedall <> colorbf THEN
                                LOCATE firstline + linetbp(i), xcharacter + xoffset(i)
                                COLOR colour&("black"), colour&("white")
                                PRINT text$(i);
                                IF selectedall = 0 THEN
                                    COLOR colour&("black"), colour&("white")
                                ELSE
                                    COLOR colour&("white"), colour&("black")
                                END IF
                                IF c(i) > 0 THEN
                                    gbf(i) = 0
                                    DO
                                        gbf(i) = gbf(i) + 1
                                        PRINT char$(i, gbf(i));
                                    LOOP UNTIL gbf(i) = c(i)
                                END IF
                            END IF
                        END IF
                        gbf(i) = c(i)

                        'blinking line after text
                        IF TIMER MOD 2 = 0 THEN
                            LINE ((xcharacter + xoffset(i) + LEN(text$(i)) + editpos(i) - 1) * fontwidth + (fontwidth / 2), (firstline + linetbp(i) - 1) * fontheight + 1)-((xcharacter + xoffset(i) + LEN(text$(i)) + editpos(i) - 1) * fontwidth + (fontwidth / 2) + 2, (firstline + linetbp(i)) * fontheight - 1), colour&("black"), BF
                        ELSE
                            LINE ((xcharacter + xoffset(i) + LEN(text$(i)) + editpos(i) - 1) * fontwidth + (fontwidth / 2), (firstline + linetbp(i) - 1) * fontheight + 1)-((xcharacter + xoffset(i) + LEN(text$(i)) + editpos(i) - 1) * fontwidth + (fontwidth / 2) + 2, (firstline + linetbp(i)) * fontheight - 1), colour&("white"), BF
                        END IF
                        SELECT CASE Taste$
                            CASE IS = CHR$(8)
                                backspace(i) = 1
                                GOTO printback
                            CASE IS = CHR$(3) 'copies the input data
                                _CLIPBOARD$ = ""
                                p = 0
                                DO
                                    p = p + 1
                                    _CLIPBOARD$ = _CLIPBOARD$ + char$(i, p)
                                LOOP UNTIL p = c(i)
                            CASE IS = CHR$(22) 'pastes data into input
                                IF _CLIPBOARD$ <> "" THEN
                                    p = 0
                                    DO
                                        p = p + 1
                                        c(i) = c(i) + 1
                                        char$(i, c(i)) = MID$(_CLIPBOARD$, p, 1)
                                    LOOP UNTIL p = LEN(_CLIPBOARD$)
                                END IF
                            CASE IS = CHR$(0) + CHR$(75)
                                IF editpos(i) > 1 THEN
                                    editpos(i) = editpos(i) - 1
                                END IF
                            CASE IS = CHR$(0) + CHR$(77)
                                IF editpos(i) < c(i) THEN
                                    editpos(i) = editpos(i) + 1
                                END IF
                            CASE IS = CHR$(13)
                                IF activei = maxm THEN
                                    status$(activei) = "finished"
                                ELSE
                                    activei = activei + 1
                                END IF
                            CASE IS = CHR$(0) + CHR$(59)
                                colorbf = selectedall
                                IF selectedall = 1 THEN
                                    selectedall = 0
                                ELSE
                                    selectedall = 1
                                END IF
                                firstprint(i) = 1
                        END SELECT
                    ELSE
                        IF _MOUSEINPUT = -1 THEN
                            IF _MOUSEBUTTON(1) = -1 THEN
                                IF _MOUSEX > (xcharacter + xoffset(i)) * fontwidth + (fontwidth / 2) AND _MOUSEX < (xcharacter + xoffset(i) + LEN(text$(i)) + c(i)) * fontwidth + (fontwidth / 2) THEN
                                    IF _MOUSEY > (firstline + linetbp(i) - 1) * fontheight + 1 AND _MOUSEY < (firstline + linetbp(i)) * fontheight - 1 THEN
                                        activei = i
                                    END IF
                                END IF
                            END IF
                            'deletes blinking line after text
                            LINE ((xcharacter + xoffset(i) + LEN(text$(i)) + editpos(i) - 1) * fontwidth + (fontwidth / 2), (firstline + linetbp(i) - 1) * fontheight + 1)-((xcharacter + xoffset(i) + LEN(text$(i)) + editpos(i) - 1) * fontwidth + (fontwidth / 2) + 2, (firstline + linetbp(i)) * fontheight - 1), colour&("white"), BF
                        END IF
                    END IF
            END SELECT
        LOOP UNTIL i = maxm
    LOOP UNTIL status$(maxii) = "finished" OR maxii = 0 OR Taste$ = CHR$(27)
    IF Taste$ = CHR$(27) THEN endparameter$ = "aborted"
    'ERASE status$
    i = 0
    DO
        i = i + 1
        SELECT CASE type$(i)
            CASE IS = "input"
                IF c(i) > 0 THEN
                    maxg(i) = c(i)
                    c(i) = 0
                    DO
                        c(i) = c(i) + 1
                        UserInput$(i) = UserInput$(i) + char$(i, c(i))
                    LOOP UNTIL c(i) = maxg(i)
                    c(i) = 0
                    f = 0
                    DO
                        f = f + 1
                        char$(i, f) = ""
                    LOOP UNTIL char$(i, f + 1) = ""
                ELSE
                    UserInput$(i) = ""
                END IF
        END SELECT
    LOOP UNTIL i = maxm
END SUB

SUB NewText (linetbp, xoffset, text$, style)
    i = i + 1
    inter(i) = 0
    type$(i) = "text"
    linetbp(i) = linetbp * 2
    xoffset(i) = xoffset
    text$(i) = text$
    style(i) = style
    status$(i) = ""
END SUB

SUB NewSlider (linetbp, xoffset, minval, maxval, text$)
    i = i + 1
    inter(i) = 1
    maxii = i
    type$(i) = "slider"
    linetbp(i) = linetbp * 2
    minval(i) = minval
    maxval(i) = maxval
    xoffset(i) = xoffset
    text$(i) = text$
    basex(i) = (xcharacter + xoffset(i) + LEN(text$(i))) * fontwidth + (fontwidth / 2) 'change this to fit your setup
    value(i) = maxval(i)
    endx(i) = basex(i) + (maxx / 4) + 4
    status$(i) = ""
END SUB

SUB NewButton (linetbp, xoffset, text$, style, kind$, destination$)
    i = i + 1
    maxii = i
    inter(i) = 1
    type$(i) = "button"
    linetbp(i) = linetbp * 2
    xoffset(i) = xoffset
    borderoffsetx(i) = 0
    borderoffsety(i) = 0
    text$(i) = text$
    basex(i) = (xcharacter + xoffset(i) - 1) * fontwidth - (fontwidth / 2) + borderoffsetx(i)
    endx(i) = basex(i) + (LEN(text$(i)) + 1) * fontwidth + (fontwidth / 2)
    basey(i) = (firstline + linetbp(i) - 1) * fontheight - (fontheight / 2) + borderoffsety(i)
    endy(i) = (firstline + linetbp(i)) * fontheight + (fontheight / 3) + borderoffsety(i)
    kind$(i) = kind$
    destination$(i) = destination$
    style(i) = style
    defaultstyle(i) = style
    status$(i) = ""
END SUB

SUB NewDropdown (linetbp, xoffset, text$, menufile$)
    i = i + 1
    inter(i) = 1
    maxii = i
    type$(i) = "dropdown"
    menufile$(i) = menufile$
    linetbp(i) = linetbp * 2
    xoffset(i) = xoffset
    borderoffsetx(i) = 0
    borderoffsety(i) = -2
    text$(i) = text$
    basex(i) = (xcharacter + xoffset(i) + LEN(text$(i)) - 1) * fontwidth - (fontwidth / 2) + borderoffsetx(i) 'left end of dropdown box
    status$(i) = ""
END SUB

SUB InputSub (linetbp, xoffset, text$, fill$)
    i = i + 1
    inter(i) = 1
    maxii = i
    IF maxg(i) > 0 THEN
        c(i) = 0
        DO
            c(i) = c(i) + 1
            char$(i, c(i)) = ""
        LOOP UNTIL c(i) = maxg(i)
        c(i) = 0
        maxg(i) = 0
    END IF
    type$(i) = "input"
    linetbp(i) = linetbp * 2
    fill$(i) = fill$
    text$(i) = text$
    xoffset(i) = xoffset
    status$(i) = ""
END SUB

SUB RunGraph (nodeTV$, flip)
    cutID nodeTV$
    ml = 0

    'search the node for outgoing links
    OPEN "data\net\" + cutCAT$ + "\" + cutSCAT$ + "\" + nodeTV$ + ".datinst" FOR INPUT AS #1
    DO
        INPUT #1, line$
        IF MID$(line$, 1, 5) = "link." THEN
            ml = ml + 1
            mainlink$(ml) = line$
            detP (line$)
            mainlinktext$(ml) = MID$(line$, p(9) + 1, LEN(line$) - p(9))
            mainlinkname$(ml) = MID$(line$, p(5) + 1, p(6) - p(5) - 1)
        END IF
        IF MID$(line$, 1, 5) = "name." THEN
            nodeNAME$ = MID$(line$, 6, LEN(line$) - 5)
        END IF
    LOOP UNTIL EOF(1) = -1
    CLOSE #1

    'search the index, then each individual file for incoming links to the relevant node
    OPEN "data\index.datnet" FOR INPUT AS #1
    DO
        INPUT #1, indexID$
        cutID indexID$
        OPEN "data\net\" + cutCAT$ + "\" + cutSCAT$ + "\" + indexID$ + ".datinst" FOR INPUT AS #2
        DO
            INPUT #2, line$
            IF MID$(line$, 1, 5) = "link." THEN
                detP (line$)
                IF MID$(line$, p(6) + 1, 5) = nodeTV$ THEN
                    duplicate = 0
                    IF ml > 0 THEN
                        mlc = 0
                        DO
                            mlc = mlc + 1
                            IF MID$(line$, p(4) + 1, p(5) - p(4) - 1) = mainlinktext$(mlc) THEN
                                duplicate = 1
                            END IF
                        LOOP UNTIL mlc = ml
                    END IF
                    IF duplicate = 0 THEN
                        ml = ml + 1
                        mainlink$(ml) = line$
                        mainlinktext$(ml) = MID$(line$, p(4) + 1, p(5) - p(4) - 1)
                        mainlinkname$(ml) = MID$(line$, p(5) + 1, p(6) - p(5) - 1)
                    ELSE
                        duplicate = 0
                    END IF
                END IF
            END IF
        LOOP UNTIL EOF(2) = -1
        CLOSE #2
    LOOP UNTIL EOF(1) = -1
    CLOSE #1
    maxml = ml

    'ml = 0
    'DO
    '    ml = ml + 1
    '    PRINT mainlink$(ml)
    'LOOP UNTIL ml = maxml

    graphscale = 100 + (5 * maxml)

    printgraph:
    COLOR colour&("white"), colour&("black")

    midmainx = maxx / 2
    midmainy = (maxy / 4) * 3

    IF maxml > 0 THEN
        ml = 0
        DO
            ml = ml + 1
            IF style(ml) = 0 THEN COLOR colour&("white"), colour&("black") ELSE COLOR colour&("black"), colour&("white")
            IF flip = 1 THEN
                mainlinky(ml) = midmainy - (SIN(0.5 * _PI + ((ml / maxml) * 2 * _PI)) * (graphscale * (-1)))
            ELSE
                mainlinky(ml) = midmainy + (SIN(0.5 * _PI + ((ml / maxml) * 2 * _PI)) * (graphscale * (-1)))
            END IF
            mainlinkx(ml) = midmainx + (SIN((ml / maxml) * 2 * _PI) * graphscale)
            thickness = 3
            IF mainlinkx(ml) = midmainx THEN thickness = 0.5 'halves thickness if it's a vertical line
            IF mainlinky(ml) = midmainy THEN thickness = 0.5 'halves thickness if it's a horizontal line
            th = 0
            DO
                LINE ((INT(mainlinkx(ml) / fontwidth) * fontwidth) - th, INT(mainlinky(ml) / fontheight) * fontheight - (fontheight / 2))-((INT(midmainx / fontwidth) * fontwidth) - th, INT(midmainy / fontheight) * fontheight - (fontheight / 2)), _RGB(15, 15, 15, 255)
                LINE ((INT(mainlinkx(ml) / fontwidth) * fontwidth) + th, INT(mainlinky(ml) / fontheight) * fontheight - (fontheight / 2))-((INT(midmainx / fontwidth) * fontwidth) + th, INT(midmainy / fontheight) * fontheight - (fontheight / 2)), _RGB(15, 15, 15, 255)
                th = th + 0.5
            LOOP UNTIL th = thickness
            LOCATE INT(mainlinky(ml) / fontheight), INT(mainlinkx(ml) / fontwidth) - (LEN(mainlinktext$(ml)) / 2) + 1
            PRINT mainlinktext$(ml)
            IF stylebf(ml) <> style(ml) THEN
                IF flip = 1 THEN
                    linknamey(ml) = midmainy - (SIN(0.5 * _PI + ((ml / maxml) * 2.25 * _PI)) * (graphscale * (-0.5)))
                ELSE
                    linknamey(ml) = midmainy + (SIN(0.5 * _PI + ((ml / maxml) * 2.25 * _PI)) * (graphscale * (-0.5)))
                END IF
                linknamex(ml) = midmainx + (SIN((ml / maxml) * 2.5 * _PI) * (graphscale * 0.5))
                IF style(ml) = 1 THEN
                    LOCATE INT(linknamey(ml) / fontheight), INT(linknamex(ml) / fontwidth)
                    PRINT mainlinkname$(ml)
                ELSE
                    COLOR colour&("white"), colour&("white")
                    LOCATE INT(linknamey(ml) / fontheight), INT(linknamex(ml) / fontwidth)
                    PRINT mainlinkname$(ml)
                    'LINE (INT(linknamey(ml) / fontheight) * fontheight, INT(linknamex(ml) / fontwidth) * fontwidth)-(INT(linknamey(ml) / fontheight + 1) * fontheight, INT(linknamex(ml) / fontwidth + LEN(mainlinkname$(ml))) * fontwidth), colour&("white"), BF
                END IF
            END IF
            stylebf(ml) = style(ml)
        LOOP UNTIL ml = maxml
    ELSE
        COLOR colour&("white"), colour&("red")
        LOCATE INT(midmainy / fontheight) + 1, INT(midmainx / fontwidth) - 12
        PRINT "[ no links to this node. ]"
        COLOR colour&("white")
    END IF

    COLOR colour&("white"), colour&("black")
    LOCATE INT(midmainy / fontheight), INT(midmainx / fontwidth) - (LEN(nodeNAME$) / 2) + 1
    PRINT nodeNAME$
    'LINE (midmainx - 5, midmainy - 5)-(midmainx + 5, midmainy + 5), _RGB(0, 127, 127, 255), BF

    'delete-button
    IF bstyle(1) = 1 THEN COLOR colour&("white"), colour&("red"): ELSE COLOR colour&("red"), colour&("white")
    LOCATE INT((midmainy - graphscale * 1.4) / fontheight), xcharacter
    IF delconfirm = 0 THEN PRINT "* DELETE NODE": ELSE PRINT "*   SURE ?   "
    'edit-button
    IF bstyle(2) = 1 THEN COLOR colour&("white"), colour&("red"): ELSE COLOR colour&("red"), colour&("white")
    LOCATE INT((midmainy - graphscale * 1.4) / fontheight), xcharacter + 15
    IF editmode = 0 THEN PRINT "    * EDIT    ": ELSE PRINT "* SAVE CHANGES"

    'main-button
    IF bstyle(3) = 1 THEN COLOR colour&("white"), colour&("orange"): ELSE COLOR colour&("orange"), colour&("white")
    LOCATE INT((midmainy - graphscale * 1.4) / fontheight), xcharacter - 13 - 12
    PRINT "* NODE FIELD"

    'selection loop
    DO
        IF resized = 1 THEN GOTO printgraph
        IF _MOUSEINPUT = -1 THEN
            'selection for links and nodes
            IF maxml > 0 THEN
                ml = 0
                DO
                    ml = ml + 1
                    IF _MOUSEY > (INT(mainlinky(ml) / fontheight) - 2) * fontheight AND _MOUSEY < (INT(mainlinky(ml) / fontheight) + 1) * fontheight THEN
                        IF _MOUSEX > (INT(mainlinkx(ml) / fontwidth) - (LEN(mainlinktext$(ml)) / 2)) * fontwidth AND _MOUSEX < (INT(mainlinkx(ml) / fontwidth) + (LEN(mainlinktext$(ml)) / 2)) * fontwidth THEN
                            IF style(ml) = 0 THEN style(ml) = 1: GOTO printgraph
                        ELSE
                            IF style(ml) = 1 THEN style(ml) = 0: GOTO printgraph
                        END IF
                    ELSE
                        IF style(ml) = 1 THEN style(ml) = 0: GOTO printgraph
                    END IF
                LOOP UNTIL ml = maxml
            END IF

            'selection for extra buttons
            'LOCATE 1, 1: PRINT _MOUSEX, _MOUSEY
            'LOCATE 2, 1: PRINT xcharacter * fontwidth, (xcharacter + 12) * fontwidth
            IF _MOUSEY > (INT((midmainy - graphscale * 1.4) / fontheight) - 1) * fontheight AND _MOUSEY < INT((midmainy - graphscale * 1.4) / fontheight) * fontheight THEN
                IF _MOUSEX > xcharacter * fontwidth AND _MOUSEX < (xcharacter + 12) * fontwidth THEN 'delete button
                    IF bstyle(1) = 0 THEN bstyle(1) = 1: GOTO printgraph
                ELSE
                    IF bstyle(1) = 1 THEN bstyle(1) = 0: GOTO printgraph
                END IF
                IF _MOUSEX > (xcharacter + 15) * fontwidth AND _MOUSEX < (xcharacter + 27) * fontwidth THEN 'edit button
                    IF bstyle(2) = 0 THEN bstyle(2) = 1: GOTO printgraph
                ELSE
                    IF bstyle(2) = 1 THEN bstyle(2) = 0: GOTO printgraph
                END IF
                IF _MOUSEX > (xcharacter - 13 - 12) * fontwidth AND _MOUSEX < (xcharacter - 13) * fontwidth THEN 'main-button
                    IF bstyle(3) = 0 THEN bstyle(3) = 1: GOTO printgraph
                ELSE
                    IF bstyle(3) = 1 THEN bstyle(3) = 0: GOTO printgraph
                END IF
            ELSE
                IF bstyle(1) = 1 THEN bstyle(1) = 0: GOTO printgraph
                IF bstyle(2) = 1 THEN bstyle(2) = 0: GOTO printgraph
                IF bstyle(3) = 1 THEN bstyle(3) = 0: GOTO printgraph
            END IF

            'style(ml) works as selected variable if = 1
            IF _MOUSEBUTTON(1) = -1 THEN
                IF maxml > 0 THEN
                    ml = 0
                    DO
                        ml = ml + 1
                        IF style(ml) = 1 THEN
                            detP (mainlink$(ml))
                            IF nodeTV$ = MID$(mainlink$(ml), p(1) + 1, 5) THEN
                                reruninst$ = MID$(mainlink$(ml), p(6) + 1, 5)
                            ELSE
                                reruninst$ = MID$(mainlink$(ml), p(1) + 1, 5)
                            END IF
                            'ERASE mainlink$, mainlinktext$, mainlinky, mainlinkx
                            IF flip = 0 THEN flip = 1: ELSE flip = 0
                            viewNode reruninst$, flip
                            endgraph = 1
                        END IF
                    LOOP UNTIL ml = maxml
                END IF
                b = 0
                DO
                    b = b + 1
                    IF bstyle(b) = 1 THEN
                        SELECT CASE b
                            CASE IS = 1 'delete node
                                IF delconfirm = 1 THEN
                                    OPEN "data\index.datnet" FOR OUTPUT AS #1
                                    node = 0
                                    DO
                                        node = node + 1
                                        IF nodeID$(node) <> nodeTV$ THEN
                                            WRITE #1, nodeID$(node)
                                        END IF
                                    LOOP UNTIL node = maxnodes
                                    KILL "data\net\" + cutCAT$ + "\" + cutSCAT$ + "\" + nodeTV$ + ".datinst"
                                    COLOR colour&("black"), colour&("white")
                                    CLS
                                    _PUTIMAGE (0, 120 - (40 / 3))-(logox, logoy), c%
                                    LOCATE firstline, xcharacter
                                    PRINT "[ node deleted. ]"
                                    _DELAY 2
                                    delconfirm = 0
                                    endgraph = 1
                                ELSE
                                    delconfirm = 1: GOTO printgraph
                                END IF
                            CASE IS = 2 'edit node
                                editNode (nodeTV$)
                                endgraph = 1
                                'idk??
                            CASE IS = 3 'go to node overview
                                nodeOverview (nodeTV$)
                                IF endnode$ <> "console" THEN
                                    endparameter2$ = "view" + endnode$
                                ELSE
                                    IF endnode$ = "console" THEN
                                        endparameter2$ = "console"
                                    END IF
                                END IF
                                endgraph = 1
                        END SELECT
                    END IF
                LOOP UNTIL b = 3
            END IF
        END IF
        Taste$ = INKEY$
        SELECT CASE Taste$
            CASE IS = "c"
                endgraph = 1
            CASE IS = CHR$(27)
                endgraph = 1
            CASE IS = CHR$(13)
                endgraph = 1
        END SELECT
    LOOP UNTIL endgraph = 1
END SUB

SUB detP (detString$)
    p = 0
    pcount = 0
    p(0) = 0
    IF detString$ <> "" THEN
        DO
            p = p + 1
            IF MID$(detString$, p, 1) = "." THEN pcount = pcount + 1: p(pcount) = p
        LOOP UNTIL p = LEN(detString$)
    END IF
    pcount = pcount + 1
    p(pcount) = LEN(detString$) + 1
END SUB

FUNCTION checkLicense (license$)
    devkey$ = "D9814E56-AF344032-A7FDF24B-D947CECE"
    IF license$ = devkey$ THEN
        checkLicense = 1
    ELSE
        shellcmd$ = "cmd /c curl http://api.gumroad.com/v2/licenses/verify -d " + CHR$(34) + "product_permalink=XXun" + CHR$(34) + " -d " + CHR$(34) + "license_key=" + license$ + CHR$(34) + " > license.txt"
        SHELL shellcmd$
        DO: LOOP UNTIL _FILEEXISTS("license.txt") = -1
        OPEN "license.txt" FOR INPUT AS #1
        IF EOF(1) = 0 THEN
            DO
                LINE INPUT #1, licensecallback$
                p = 0
                u = 0
                o = 0
                DO
                    p = p + 1
                    IF MID$(licensecallback$, p, 1) = CHR$(34) THEN
                        u = p
                        DO: u = u + 1: LOOP UNTIL MID$(licensecallback$, u, 1) = CHR$(34)
                        attribute$ = MID$(licensecallback$, p + 1, u - p - 1)
                        IF attribute$ <> "purchase" AND attribute$ <> "custom_fields" AND attribute$ <> "How did you discover Datanet?" AND attribute$ <> "variants" THEN
                            o = u
                            DO: o = o + 1: LOOP UNTIL MID$(licensecallback$, o, 1) = "," OR MID$(licensecallback$, o, 1) = "}"
                            IF MID$(licensecallback$, o - 1, 1) = CHR$(34) THEN
                                value$ = MID$(licensecallback$, u + 3, o - u - 4)
                            ELSE
                                value$ = MID$(licensecallback$, u + 2, o - u - 2)
                            END IF
                            p = o
                            SELECT CASE attribute$
                                CASE IS = "success": success$ = value$
                                CASE IS = "uses": uses = VAL(value$)
                                CASE IS = "seller_id": sellerID$ = value$
                                CASE IS = "product_id": productID$ = value$
                                CASE IS = "product_name": productname$ = value$
                                CASE IS = "permalink": permalink$ = value$
                                CASE IS = "product_permalink": productpermalink$ = value$
                                CASE IS = "email": email$ = value$
                                CASE IS = "price": price = VAL(value$)
                                CASE IS = "currency": currency$ = value$
                                CASE IS = "quantity": quantity = VAL(value$)
                                CASE IS = "order_number": ordernumber = VAL(value$)
                                CASE IS = "sale_id": saleID$ = value$
                                CASE IS = "sale_timestamp": saletimestamp$ = value$
                                CASE IS = "purchaser_id": purchaserID = VAL(value$)
                                CASE IS = "test": test$ = value$
                                CASE IS = "How did you discover Datanet?": discovery$ = value$
                                CASE IS = "license_key": licensekey$ = value$
                                CASE IS = "ip_country": IPcountry$ = value$
                                CASE IS = "is_gift_receiver_purchase": isgift$ = value$
                                CASE IS = "refunded": refunded$ = value$
                                CASE IS = "disputed": disputed$ = value$
                                CASE IS = "dispute_won": disputewon$ = value$
                                CASE IS = "id": id$ = value$
                                CASE IS = "created_at": createdat$ = value$
                                CASE IS = "variants": variants$ = value$
                                CASE IS = "chargebacked": chargebacked$ = value$
                                CASE IS = "ended_at": endedat$ = value$
                                CASE IS = "failed_at": failedat$ = value$
                            END SELECT
                        ELSE
                            DO: p = p + 1: LOOP UNTIL MID$(licensecallback$, p, 1) = "{" OR MID$(licensecallback$, p, 1) = "[" OR MID$(licensecallback$, p, 1) = ","
                        END IF
                        attribute$ = ""
                        value$ = ""
                    END IF
                LOOP UNTIL p >= LEN(licensecallback$)
            LOOP UNTIL EOF(1) = -1
        END IF
        CLOSE #1
        IF success$ = "true" AND uses < 100 AND productname$ = "Datanet" AND permalink$ = "XXun" AND licensekey$ = license$ AND endedat$ = "" AND failedat$ = "" THEN
            KILL "license.txt"
            checkLicense = 1
        ELSE
            checkLicense = 0
        END IF
    END IF
END FUNCTION

FUNCTION colour& (name$)
    SELECT CASE name$
        CASE IS = "fg"
            colour& = colour&("black")
        CASE IS = "bg"
            colour& = colour&("white")
        CASE IS = "white"
            colour& = _RGBA(237, 237, 237, 255)
        CASE IS = "black"
            colour& = _RGBA(15, 15, 15, 255)
        CASE IS = "magenta"
            colour& = _RGBA(255, 50, 100, 255)
        CASE IS = "orange"
            colour& = _RGBA(255, 133, 0, 255)
        CASE IS = "red"
            colour& = _RGBA(216, 33, 17, 255)
        CASE IS = "green"
            colour& = _RGBA(39, 211, 50, 255)
        CASE IS = "blue"
            colour& = _RGBA(44, 166, 255, 255)
        CASE IS = "purple"
            colour& = _RGBA(222, 89, 238, 255)
        CASE IS = "yellow"
            colour& = _RGBA(238, 177, 0, 255)
        CASE IS = "transparent"
            colour& = _RGBA(0, 0, 0, 0)
    END SELECT
END FUNCTION

FUNCTION resized
    IF _RESIZE = -1 THEN
        maxx = _RESIZEWIDTH
        IF maxx < 1280 THEN
            maxx = 1280
        END IF
        maxy = maxx / 16 * 9
        SCREEN _NEWIMAGE(maxx, maxy, 32)
        logox = maxx / 3
        logoy = (500 / 1920) * logox
        maxrows = INT(maxx / fontwidth)
        maxlines = INT(maxy / fontheight) - 4
        firstline = INT(120 / fontheight)
        xcharacter = INT((maxx / 2.7) / fontwidth)
        twidth = maxx / 1.5
        theight = twidth / 2
        centerwindow
        COLOR colour&("fg"), colour&("bg")
        CLS
        resized = 1
    ELSE
        resized = 0
    END IF
END FUNCTION

SUB centerwindow
    _SCREENMOVE _DESKTOPWIDTH / 2 - (_WIDTH / 2), _DESKTOPHEIGHT / 2 - (_HEIGHT / 2)
END SUB

SUB resizeWindow
    maxx = (_DESKTOPWIDTH / 2) * windowscale
    maxy = maxx / 16 * 9
    logox = maxx / 3
    logoy = (500 / 1920) * logox

    SCREEN _NEWIMAGE(maxx, maxy, 32)
    DO: LOOP UNTIL _SCREENEXISTS
    _TITLE "DATANET"
    _SCREENMOVE (_DESKTOPWIDTH / 2) - (maxx / 2), (_DESKTOPHEIGHT / 2) - (maxy / 2)

    _FONT fr&
    fontwidth = _FONTWIDTH(fr&)
    maxrows = INT(maxx / fontwidth)
    maxlines = INT(maxy / fontheight) - 4
    firstline = INT(120 / fontheight)
    xcharacter = INT((maxx / 2.7) / fontwidth)

    twidth = maxx / 1.5
    theight = twidth / 2
END SUB

'FUNCTION GetOpenFileName$ (Title$, InitialDir$, Filter$, FilterIndex, Flags&, hWnd&)
'    '  Title$      - The dialog title.
'    '  InitialDir$ - If this left blank, it will use the directory where the last opened file is
'    '  located. Specify ".\" if you want to always use the current directory.
'    '  Filter$     - File filters separated by pipes (|) in the same format as using VB6 common dialogs.
'    '  FilterIndex - The initial file filter to use. Will be altered by user during the call.
'    '  Flags&      - Dialog flags. Will be altered by the user during the call.
'    '  hWnd&       - Your program's window handle that should be aquired by the FindWindow function.
'    '
'    ' Returns: Blank when cancel is clicked otherwise, the file name selected(i)by the user.
'    ' FilterIndex and Flags& will be changed depending on the user's selections.

'    DIM OpenCall AS FILEDIALOGTYPE ' Needed for dialog call

'    fFilter$ = Filter$
'    FOR R = 1 TO LEN(fFilter$) ' Replace the pipes with character zero
'        IF MID$(fFilter$, R, 1) = "|" THEN MID$(fFilter$, R, 1) = CHR$(0)
'    NEXT R
'    fFilter$ = fFilter$ + CHR$(0)

'    lpstrFile$ = STRING$(2048, 0) ' For the returned file name
'    lpstrDefExt$ = STRING$(10, 0) ' Extension will not be added when this is not specified
'    OpenCall.lStructSize = LEN(OpenCall)
'    OpenCall.hwndOwner = hWnd&
'    OpenCall.lpstrFilter = _OFFSET(fFilter$)
'    OpenCall.nFilterIndex = FilterIndex
'    OpenCall.lpstrFile = _OFFSET(lpstrFile$)
'    OpenCall.nMaxFile = LEN(lpstrFile$) - 1
'    OpenCall.lpstrFileTitle = OpenCall.lpstrFile
'    OpenCall.nMaxFileTitle = OpenCall.nMaxFile
'    OpenCall.lpstrInitialDir = _OFFSET(InitialDir$)
'    OpenCall.lpstrTitle = _OFFSET(Title$)
'    OpenCall.lpstrDefExt = _OFFSET(lpstrDefExt$)
'    OpenCall.flags = Flags&

'    Result = GetOpenFileNameA&(OpenCall) '            Do Open File dialog call!

'    IF Result THEN ' Trim the remaining zeros
'        GetOpenFileName$ = LEFT$(lpstrFile$, INSTR(lpstrFile$, CHR$(0)) - 1)
'        Flags& = OpenCall.flags
'        FilterIndex = OpenCall.nFilterIndex
'    END IF
'END FUNCTION

'FUNCTION GetSaveFileName$ (Title$, InitialDir$, Filter$, FilterIndex, Flags&, hWnd&)
'    '  Title$      - The dialog title.
'    '  InitialDir$ - If this left blank, it will use the directory where the last opened file is
'    '     located. Specify ".\" if you want to always use the current directory.
'    '  Filter$     - File filters separated by pipes (|) in the same format as VB6 common dialogs.
'    '  FilterIndex - The initial file filter to use. Will be altered by user during the call.
'    '  Flags&      - Dialog flags. Will be altered by the user during the call.
'    '  hWnd&       - Your program's window handle that should be aquired by the FindWindow function.

'    ' Returns: Blank when cancel is clicked otherwise, the file name entered by the user.
'    ' FilterIndex and Flags& will be changed depending on the user's selections.

'    DIM SaveCall AS FILEDIALOGTYPE ' Needed for dialog call

'    fFilter$ = Filter$
'    FOR R = 1 TO LEN(fFilter$) ' Replace the pipes with zeros
'        IF MID$(fFilter$, R, 1) = "|" THEN MID$(fFilter$, R, 1) = CHR$(0)
'    NEXT R
'    fFilter$ = fFilter$ + CHR$(0)

'    lpstrFile$ = STRING$(2048, 0) ' For the returned file name
'    lpstrDefExt$ = STRING$(10, 0) ' Extension will not be added when this is not specified
'    SaveCall.lStructSize = LEN(SaveCall)
'    SaveCall.hwndOwner = hWnd&
'    SaveCall.lpstrFilter = _OFFSET(fFilter$)
'    SaveCall.nFilterIndex = FilterIndex
'    SaveCall.lpstrFile = _OFFSET(lpstrFile$)
'    SaveCall.nMaxFile = LEN(lpstrFile$) - 1
'    SaveCall.lpstrFileTitle = SaveCall.lpstrFile
'    SaveCall.nMaxFileTitle = SaveCall.nMaxFile
'    SaveCall.lpstrInitialDir = _OFFSET(InitialDir$)
'    SaveCall.lpstrTitle = _OFFSET(Title$)
'    SaveCall.lpstrDefExt = _OFFSET(lpstrDefExt$)
'    SaveCall.flags = Flags&

'    Result& = GetSaveFileNameA&(SaveCall) ' Do dialog call!

'    IF Result& THEN ' Trim the remaining zeros
'        GetSaveFileName$ = LEFT$(lpstrFile$, INSTR(lpstrFile$, CHR$(0)) - 1)
'        Flags& = SaveCall.flags
'        FilterIndex = SaveCall.nFilterIndex
'    END IF
'END FUNCTION
