$EXEICON:'..\Targon Industries\DATANET\internal\ico\Datanet_1.ico'
$RESIZE:ON
REM $DYNAMIC

'file list by SMcNeill
DECLARE CUSTOMTYPE LIBRARY "code\direntry"
    FUNCTION load_dir& (s AS STRING)
    FUNCTION has_next_entry& ()
    SUB close_dir ()
    SUB get_next_entry (s AS STRING, flags AS LONG, file_size AS LONG)
END DECLARE
REDIM Dir(0) AS STRING, File(0) AS STRING

loadconfig

REDIM SHARED AS INTEGER screenresx, screenresy, winresx, winresy
screenresx = _DESKTOPWIDTH
screenresy = _DESKTOPHEIGHT
winresx = screenresx * 0.5 'to be replaced with config-based factor
winresy = screenresy * 0.5
SCREEN _NEWIMAGE(winresx, winresy, 32)
_SCREENMOVE (screenresx / 2) - (_WIDTH(0) / 2), (screenresy / 2) - (_HEIGHT(0) / 2)
DO: LOOP UNTIL _SCREENEXISTS
_TITLE "Datanet"

'Variables
TYPE internalsettings
    trimstrings AS _BYTE
    fps AS INTEGER
END TYPE
TYPE internal
    setting AS internalsettings
END TYPE
REDIM SHARED internal AS internal

TYPE global
    AS STRING nodepath, internalpath, skinpath
    AS _UNSIGNED _INTEGER64 maxnodeid
    AS _FLOAT margin, padding, round, stroke
END TYPE
REDIM SHARED global AS global

TYPE mouse
    AS _FLOAT x, y
    AS _BYTE left, right
    AS INTEGER scroll
END TYPE
REDIM SHARED mouse AS mouse

'Data structure
TYPE node
    AS STRING id, name, type, date, time
END TYPE
TYPE nodelink
    AS STRING parent, name, child
END TYPE

REDIM SHARED directories(0) AS STRING
REDIM SHARED nodefiles(0) AS STRING

'UI
TYPE element
    AS _BYTE show, acceptinput, allownumbers, allowtext, allowspecial, selected
    AS STRING x, y, w, h, style, name, text, buffer, type, color, hovercolor, action, angle, view
    value AS _FLOAT
END TYPE
REDIM SHARED element(0) AS element
TYPE uisum
    AS _FLOAT x, y
    AS STRING f 'determines "focus"
END TYPE
REDIM SHARED uisum(0) AS uisum
REDIM SHARED AS STRING viewname(0), bufferchar, activeelement, currentview
REDIM SHARED AS _BYTE invokedelete

TYPE rectangle
    AS _FLOAT x, y, w, h
END TYPE

resetallvalues

'------------- TESTING AREA ------------



'-------------- MAIN AREA --------------

loadui
DO
    CLS
    checkresize
    checkcontrols
    displayview currentview
    _LIMIT internal.setting.fps
LOOP
SLEEP

SUB checkresize
    IF _RESIZE THEN
        winresx = _RESIZEWIDTH
        winresy = _RESIZEHEIGHT
        IF (winresx <> _WIDTH(0) OR winresy <> _HEIGHT(0)) THEN
            setwindow winresx, winresy
        END IF
    END IF
END SUB

SUB setwindow (winresx AS INTEGER, winresy AS INTEGER)
    SCREEN _NEWIMAGE(winresx, winresy, 32)
    DO: LOOP UNTIL _SCREENEXISTS
END SUB

SUB checkcontrols
    checkmouse
    checkkeyboard
END SUB

SUB checkmouse
    mouse.scroll = 0
    DO
        mouse.x = _MOUSEX
        mouse.y = _MOUSEY
        mouse.scroll = mouse.scroll + _MOUSEWHEEL
        mouse.left = _MOUSEBUTTON(1)
        mouse.right = _MOUSEBUTTON(2)
    LOOP WHILE _MOUSEINPUT
END SUB

SUB checkkeyboard
    keyhit = _KEYHIT
    IF keyhit < 0 THEN keyhit = 0

    bufferchar = ""
    invokedelete = 0
    invokeempty = 0

    'element-specific
    IF UBOUND(element) > 0 THEN
        e = 0: DO: e = e + 1
            IF _TRIM$(element(e).name) = activeelement THEN
                IF keyhit >= 48 AND keyhit <= 58 AND element(e).allownumbers THEN
                    bufferchar = CHR$(keyhit)
                ELSEIF ((keyhit >= 64 AND keyhit <= 91) OR (keyhit >= 96 AND keyhit <= 123) OR keyhit = 32) AND element(e).allowtext THEN
                    bufferchar = CHR$(keyhit)
                ELSEIF ((keyhit >= 33 AND keyhit <= 47) OR (keyhit >= 58 AND keyhit <= 64) OR (keyhit >= 91 AND keyhit <= 96) OR (keyhit >= 123 AND keyhit <= 126)) AND element(e).allowspecial THEN
                    bufferchar = CHR$(keyhit)
                END IF
            END IF
        LOOP UNTIL e = UBOUND(element)
    END IF

    'general
    SELECT CASE keyhit
        CASE 8: invokedelete = -1 'backspace
        CASE 9 'tab
            IF UBOUND(element) > 0 THEN
                e = 0: DO: e = e + 1
                    IF _TRIM$(element(e).name) = activeelement THEN
                        IF element(e + 1).view = currentview THEN activeelement = element(e + 1).name
                    END IF
                LOOP UNTIL e = UBOUND(element) - 1
            END IF
        CASE 21248: invokeempty = -1 'delete
    END SELECT
END SUB

SUB displayview (dview AS STRING) 'displays a "view"
    LINE (0, 0)-(_WIDTH(0), _HEIGHT(0)), col&("bg1"), BF
    IF UBOUND(element) > 0 THEN
        DO: e = e + 1
            IF _TRIM$(element(e).view) = _TRIM$(dview) THEN
                displayelement element(e), e, ""
            END IF
        LOOP UNTIL e = UBOUND(element)
    END IF
    resetsum
    _DISPLAY
END SUB

SUB displayelement (this AS element, e AS INTEGER, arguments AS STRING) 'parses abstract coordinates into discrete coordinates
    IF this.show = -1 THEN

        'mouse-dependent
        DIM AS LONG drawcolor
        IF this.name = activeelement AND bufferchar <> "" THEN
            this.buffer = this.buffer + bufferchar
        ELSEIF this.name = activeelement AND invokedelete THEN
            this.buffer = MID$(this.buffer, 1, LEN(this.buffer) - 1)
        ELSEIF this.selected AND invokeempty THEN
            this.buffer = ""
        END IF

        'script-dependent recursive functions
        DIM AS _FLOAT x, y, w, h
        x = VAL(getexpos$(e))
        y = VAL(geteypos$(e))
        w = VAL(getewidth$(e))
        h = VAL(geteheight$(e))

        IF mouse.x > x AND mouse.y > y AND mouse.x < x + w AND mouse.y < y + h THEN
            drawcolor = col&(this.hovercolor)
            IF _KEYDOWN(100303) OR _KEYDOWN(100304) THEN 'Shift + Mouse = Select
                IF mouse.left THEN
                    IF this.selected = -1 THEN this.selected = 0 ELSE this.selected = -1
                    DO: m = _MOUSEINPUT: LOOP UNTIL _MOUSEBUTTON(1) = 0
                END IF
            ELSE 'No Shift + Mouse = Trigger action if existent
                IF mouse.left AND this.action <> "" THEN
                    dothis "action=" + this.action
                ELSEIF mouse.left AND this.action = "" THEN
                    activeelement = this.name
                END IF
            END IF
        ELSEIF this.name = activeelement OR this.selected THEN
            drawcolor = col&("blue")
        ELSE
            drawcolor = col&(this.color)
        END IF

        IF debug = -1 THEN rectangle "x=" + lst$(x) + ";y=" + lst$(y) + ";w=" + lst$(w) + ";h=" + lst$(h) + ";round=0;style=" + this.style + ";angle=" + this.angle, _RGBA(255, 0, 50, 255)

        SELECT CASE this.type
            CASE "button"
                rectangle "x=" + lst$(x) + ";y=" + lst$(y) + ";w=" + lst$(w) + ";h=" + lst$(h) + ";style=" + this.style + ";angle=" + this.angle, drawcolor
                COLOR drawcolor, col&("t")
                _PRINTSTRING (x + (2 * global.padding), y + global.padding), this.text + " " + this.buffer
            CASE "input"
                underlinedistance = -2
                LINE (x + _FONTWIDTH, y + h + underlinedistance)-(x + w, y + h + underlinedistance), drawcolor
                COLOR drawcolor, col&("t")
                _PRINTSTRING (x + (2 * global.padding), y + global.padding), this.text + " " + this.buffer
            CASE "text"
                _PRINTSTRING (x + (2 * global.padding), y + global.padding), this.text + " " + this.buffer
            CASE "time"
                _PRINTSTRING (x + (2 * global.padding), y + global.padding), TIME$
            CASE "date"
                _PRINTSTRING (x + (2 * global.padding), y + global.padding), DATE$
        END SELECT
    END IF
END SUB

FUNCTION getexpos$ (e AS INTEGER)
    IF (element(e).x = "previous.right" OR element(e).x = "prev.r" OR element(e).x = "flex") AND e > 1 THEN
        getexpos$ = lst$(VAL(getexpos$(e - 1)) + VAL(getewidth$(e - 1)) + global.margin)
    ELSEIF (element(e).x = "previous.left" OR element(e).x = "prev.l" OR element(e).x = "-flex") AND e > 1 THEN
        getexpos$ = lst$(VAL(getexpos$(e - 1)) - VAL(getewidth$(e)) - global.margin)
    ELSEIF (element(e).x = "right" OR element(e).x = "r") THEN
        getexpos$ = lst$(_WIDTH(0) - VAL(getewidth$(e)) - global.margin)
    ELSEIF (element(e).x = "margin" OR element(e).x = "left" OR element(e).x = "l" OR element(e).x = "0") THEN
        getexpos$ = lst$(global.margin)
    ELSE
        getexpos$ = element(e).x
    END IF
END FUNCTION

FUNCTION geteypos$ (e AS INTEGER)
    IF (element(e).y = "previous.bottom" OR element(e).y = "prev.b" OR element(e).y = "flex") AND e > 1 THEN
        geteypos$ = lst$(VAL(geteypos$(e - 1)) + VAL(geteheight$(e - 1)) + global.margin)
    ELSEIF (element(e).y = "previous.top" OR element(e).y = "prev.t" OR element(e).y = "-flex") AND e > 1 THEN
        geteypos$ = lst$(VAL(geteypos$(e - 1)) - VAL(geteheight$(e)) - global.margin)
    ELSEIF (element(e).y = "bottom" OR element(e).y = "b") THEN
        geteypos$ = lst$(_HEIGHT(0) - VAL(geteheight$(e)) - global.margin)
    ELSEIF (element(e).y = "margin" OR element(e).y = "top" OR element(e).y = "t" OR element(e).y = "0") THEN
        geteypos$ = lst$(global.margin)
    ELSE
        geteypos$ = element(e).y
    END IF
END FUNCTION

FUNCTION getewidth$ (e AS INTEGER)
    IF element(e).w = "flex" OR element(e).w = "f" THEN 'you would normally want this one for text-based elements
        getewidth$ = lst$(VAL(element(e).w) + (_FONTWIDTH * (LEN(element(e).text) + 2 + LEN(element(e).buffer))) + (2 * global.padding))
    ELSE
        getewidth$ = element(e).w
    END IF
END FUNCTION

FUNCTION geteheight$ (e AS INTEGER)
    IF element(e).h = "" THEN element(e).h = "25"
    geteheight$ = element(e).h
END FUNCTION

SUB dothis (arguments AS STRING)
    DIM AS STRING nodeorigin, nodetype, nodetarget, linkname
    nodeorigin = getargument$(arguments, "nodeorigin")
    nodetype = getargument$(arguments, "nodetype")
    nodetarget = getargument$(arguments, "nodetarget")
    linkname = getargument$(arguments, "linkname")
    action$ = getargument$(arguments, "action")
    SELECT CASE action$
        CASE "add.node"
            IF set(nodetarget) AND set(nodetype) THEN
                add.node "target=" + nodetarget + ";type=" + nodetype
            ELSE
                dothis "action=view.add.node"
            END IF
        CASE "add.nodelink"
            IF set(nodeorigin) AND set(linkname) AND set(nodetarget) THEN
                add.nodelink "origin=" + nodeorigin + ";name=" + linkname + ";target=" + nodetarget
            ELSE
                dothis "action=view.add.nodelink"
            END IF
        CASE "quit"
            SYSTEM
    END SELECT
    SELECT CASE MID$(action$, 1, INSTR(action$, ".") - 1)
        CASE "view"
            currentview = MID$(action$, INSTR(action$, ".") + 1, LEN(action$))
    END SELECT
END SUB

SUB add.node (arguments AS STRING)
    DIM this AS node
    this.name = getargument$(arguments, "target")
    this.type = getargument$(arguments, "type")
    this.date = DATE$
    this.time = TIME$
    this.id = lst$(global.maxnodeid + 1)
    IF set(this.name) AND set(this.type) THEN
        writenode "", this
        getmaxnodeid
    END IF
END SUB

SUB writenode (arguments AS STRING, this AS node)
    filen = FREEFILE
    OPEN path$(this.id) FOR OUTPUT AS #filen
    WRITE #filen, "date=" + this.date
    WRITE #filen, "time=" + this.time
    WRITE #filen, "name=" + this.name
    WRITE #filen, "type=" + this.type
    CLOSE #filen
END SUB

SUB add.nodelink (arguments AS STRING)
    DIM this AS nodelink

END SUB

SUB loadconfig
    'TBD
END SUB

SUB loadui
    REDIM _PRESERVE viewname(0) AS STRING

    freen = FREEFILE
    OPEN global.internalpath + "\views.dui" FOR INPUT AS #freen
    IF EOF(freen) = 0 THEN
        DO: lview = lview + 1
            REDIM _PRESERVE viewname(UBOUND(viewname) + 1) AS STRING
            INPUT #freen, viewname(lview)
        LOOP UNTIL EOF(freen) = -1
    ELSE
        PRINT "internal/views.dui is empty, could not load UI!"
        SLEEP: SYSTEM
    END IF
    CLOSE #freen

    IF UBOUND(viewname) > 0 THEN
        lview = 0: DO: lview = lview + 1
            freen = FREEFILE
            viewfile$ = global.internalpath + "\" + viewname(lview) + ".dui"
            IF _FILEEXISTS(viewfile$) THEN
                OPEN viewfile$ FOR INPUT AS #freen
                IF EOF(freen) = 0 THEN
                    DO
                        INPUT #freen, uielement$

                        REDIM _PRESERVE element(UBOUND(element) + 1) AS element
                        eub = UBOUND(element)
                        element(eub).view = viewname(lview)
                        element(eub).type = getargument$(uielement$, "type")
                        element(eub).allownumbers = getargumentv(uielement$, "allownumbers")
                        element(eub).allowtext = getargumentv(uielement$, "allowtext")
                        element(eub).allowspecial = getargumentv(uielement$, "allowspecial")
                        element(eub).name = getargument$(uielement$, "name")
                        element(eub).x = getargument$(uielement$, "x")
                        element(eub).y = getargument$(uielement$, "y")
                        element(eub).w = getargument$(uielement$, "w")
                        element(eub).h = getargument$(uielement$, "h")
                        element(eub).color = getargument$(uielement$, "color")
                        element(eub).hovercolor = getargument$(uielement$, "hovercolor")
                        element(eub).style = getargument$(uielement$, "style")
                        element(eub).text = getargument$(uielement$, "text")
                        element(eub).action = getargument$(uielement$, "action")
                        element(eub).angle = getargument$(uielement$, "angle")
                        element(eub).buffer = getargument$(uielement$, "buffer")
                        element(eub).selected = 0
                        element(eub).show = -1
                    LOOP UNTIL EOF(freen)
                END IF
                CLOSE #freen
                PRINT "Successfully loaded UI for view " + lst$(lview) + "!"
            ELSE
                PRINT "Error loading UI for view " + lst$(lview)
            END IF
        LOOP UNTIL lview = UBOUND(viewname)
        currentview = viewname(1)
    ELSE
        PRINT "Could not load UI!"
        SLEEP: SYSTEM
    END IF
END SUB

SUB resetallvalues
    'internal settings
    internal.setting.trimstrings = -1
    internal.setting.fps = 20

    'data structure
    global.internalpath = "internal"
    global.nodepath = "nodes"
    skin$ = "default"
    global.skinpath = "skins\" + skin$
    getmaxnodeid
    global.margin = 10
    global.padding = 5
    global.round = 3
    global.stroke = 4
END SUB

SUB GetFileList (SearchDirectory AS STRING, DirList() AS STRING, FileList() AS STRING)
    CONST IS_DIR = 1
    CONST IS_FILE = 2
    DIM flags AS LONG, file_size AS LONG
    REDIM DirList(100), FileList(1000)
    DirCount = 0: FileCount = 0
    IF load_dir(SearchDirectory + CHR$(0)) THEN
        DO
            length = has_next_entry
            IF length > -1 THEN
                nam$ = SPACE$(length)
                get_next_entry nam$, flags, file_size
                IF flags AND IS_DIR THEN
                    DirCount = DirCount + 1
                    IF DirCount > UBOUND(DirList) THEN REDIM _PRESERVE DirList(UBOUND(DirList) + 100)
                    DirList(DirCount) = nam$
                ELSEIF flags AND IS_FILE THEN
                    FileCount = FileCount + 1
                    IF FileCount > UBOUND(filelist) THEN REDIM _PRESERVE FileList(UBOUND(filelist) + 100)
                    FileList(FileCount) = nam$
                END IF
            END IF
        LOOP UNTIL length = -1
        close_dir
    ELSE
    END IF
    REDIM _PRESERVE DirList(DirCount)
    REDIM _PRESERVE FileList(FileCount)
END SUB

SUB getmaxnodeid
    generatefilearrays
    IF UBOUND(nodefiles) > 0 THEN
        DO: f = f + 1
            checkval = VAL(MID$(nodefiles(f), 1, INSTR(nodefiles(f), ".")))
            IF checkval > global.maxnodeid THEN global.maxnodeid = checkval
        LOOP UNTIL f = UBOUND(nodefiles)
    END IF
END SUB

SUB generatefilearrays
    REDIM directories(0) AS STRING
    REDIM nodefiles(0) AS STRING
    GetFileList global.nodepath, directories(), nodefiles()
END SUB

FUNCTION getargument$ (basestring AS STRING, argument AS STRING)
    getargument$ = stringvalue$(basestring, argument)
END FUNCTION

FUNCTION getargumentv (basestring AS STRING, argument AS STRING)
    getargumentv = VAL(stringvalue$(basestring, argument))
END FUNCTION

FUNCTION path$ (nodename AS STRING)
    path$ = global.nodepath + "\" + nodename + ".node"
END FUNCTION

FUNCTION set (tocheck AS STRING) 'just returns if a string variable has a value or not
    IF LTRIM$(RTRIM$(tocheck)) <> "" THEN
        set = -1: EXIT FUNCTION
    ELSE
        set = 0: EXIT FUNCTION
    END IF
END FUNCTION

SUB resetsum 'explicitly doesn't use REDIM because that bugs out
    IF UBOUND(uisum) > 0 THEN
        DO: u = u + 1
            uisum(u).x = 0
            uisum(u).y = 0
            uisum(u).f = ""
        LOOP UNTIL u = UBOUND(uisum)
    END IF
    REDIM _PRESERVE uisum(0) AS uisum
END SUB

FUNCTION stringvalue$ (basestring AS STRING, argument AS STRING)
    IF LEN(basestring) > 0 THEN
        p = 1: DO
            IF MID$(basestring, p, LEN(argument)) = argument THEN
                endpos = INSTR(p + LEN(argument), basestring, ";")
                IF endpos = 0 THEN endpos = LEN(basestring) ELSE endpos = endpos - 1 'means that no comma has been found. taking the entire rest of the string as argument value.

                startpos = INSTR(p + LEN(argument), basestring, "=")
                IF startpos > endpos THEN
                    startpos = p + LEN(argument)
                ELSE
                    IF startpos = 0 THEN startpos = p + LEN(argument) ELSE startpos = startpos + 1 'means that no equal sign has been found. taking value right from the end of the argument name.
                END IF

                IF internal.setting.trimstrings = -1 THEN
                    stringvalue$ = LTRIM$(RTRIM$(MID$(basestring, startpos, endpos - startpos + 1)))
                    EXIT FUNCTION
                ELSE
                    stringvalue$ = MID$(basestring, startpos, endpos - startpos + 1)
                    EXIT FUNCTION
                END IF
            END IF
            finder = INSTR(p + 1, basestring, ";") + 1
            IF finder > 1 THEN p = finder ELSE stringvalue$ = "": EXIT FUNCTION
        LOOP UNTIL p >= LEN(basestring)
    END IF
END FUNCTION

SUB drawshape (arguments AS STRING, clr AS LONG)
    DIM AS _FLOAT x, y, w, h, thickness
    x = getargumentv(arguments, "x")
    y = getargumentv(arguments, "y")
    w = getargumentv(arguments, "w")
    h = getargumentv(arguments, "h")
    thickness = getargumentv(arguments, "thickness")
    IF thickness = 0 THEN thickness = global.stroke
    SELECT CASE getargument$(arguments, "shape")
        CASE "+"
            LINE (x + (w / 2) - (thickness / 2), y)-(x + (w / 2) + (thickness / 2), y + h), clr, BF
            LINE (x, y + (h / 2) - (thickness / 2))-(x + w, y + (h / 2) + (thickness / 2)), clr, BF
        CASE "x"
    END SELECT
END SUB

SUB rectangle (arguments AS STRING, clr AS LONG)
    x = getargumentv(arguments, "x")
    y = getargumentv(arguments, "y")
    w = getargumentv(arguments, "w")
    h = getargumentv(arguments, "h")
    round$ = getargument$(arguments, "round")
    rotation = getargumentv(arguments, "angle")
    rotation = rotation / 180 * _PI
    IF round$ = "" THEN
        round = global.round
    ELSE
        round = VAL(round$)
    END IF
    SELECT CASE UCASE$(getargument$(arguments, "style"))
        CASE "BF"
            rectangleoutline x, y, w, h, round, rotation, clr
            PAINT (x + (w / 2), y + (h / 2)), clr, clr
        CASE "B"
            rectangleoutline x, y, w, h, round, rotation, clr
    END SELECT
END SUB

SUB rectangleoutline (x, y, w, h, round, rotation, clr AS LONG)
    IF w < h THEN min = w ELSE min = h
    IF round > min / 2 THEN round = min / 2
    distance = SQR((w ^ 2) + (h ^ 2)) / 2 'distance to center point
    rotation = rotation - (_PI / 2)
    rounddistance = distance - SQR(round ^ 2 + round ^ 2)
    cx = x + (w / 2)
    cy = y + (h / 2)
    detail = _PI / 2 * round
    angle1 = ATN((h / 2) / (w / 2))
    angle2 = ((_PI) - (2 * angle1)) / 2
    rotation = rotation - angle1
    DO
        corner = corner + 1
        IF corner MOD 2 = 0 THEN
            cangle = angle2 * 2
            offset = -_PI / 4
        ELSE
            cangle = angle1 * 2
            offset = _PI / 4
        END IF

        rcf = rotation + anglebase
        rcfp1 = rotation + anglebase + cangle

        px = cx + (rounddistance * SIN(rcf))
        py = cy - (rounddistance * COS(rcf))

        px1 = cx + (rounddistance * SIN(rcfp1))
        py1 = cy - (rounddistance * COS(rcfp1))

        startangle = -(_PI / 2) + ((cangle / 2))
        endangle = ((cangle / 2))

        'uses endangle of previous corner to connect to startangle of next
        LINE (px + (SIN(endangle + rcf) * round), py - (COS(endangle + rcf) * round))-(px1 + (SIN(startangle + rcfp1 + offset) * round), py1 - (COS(startangle + rcfp1 + offset) * round)), clr

        angle = startangle + rcf
        DO: angle = angle + ((0.5 * _PI) / detail)
            PSET (px + (SIN(angle) * round), py - (COS(angle) * round)), clr
        LOOP UNTIL angle >= startangle + rcf + (_PI / 2)

        anglebase = anglebase + cangle
    LOOP UNTIL corner = 4
END SUB

FUNCTION lst$ (number AS _FLOAT)
    lst$ = LTRIM$(STR$(number))
END FUNCTION

FUNCTION col& (colour AS STRING)
    SELECT CASE colour
        CASE "t"
            col& = _RGBA(0, 0, 0, 0)
        CASE "ui"
            col& = _RGBA(0, 0, 0, 255)
        CASE "ui2"
            col& = _RGBA(150, 150, 150, 255)
        CASE "bg1"
            col& = _RGBA(230, 230, 230, 255)
        CASE "bg2"
            col& = _RGBA(160, 160, 160, 255)
        CASE "green"
            col& = _RGBA(33, 166, 0, 255)
        CASE "red"
            col& = _RGBA(255, 0, 55, 255)
        CASE "blue"
            col& = _RGBA(0, 144, 255, 255)
        CASE ELSE
            red$ = MID$(colour, 1, INSTR(colour, ";") - 1)
            green$ = MID$(colour, LEN(red$) + 1, INSTR(LEN(red$) + 1, colour, ";") - 1)
            blue$ = MID$(colour, LEN(red$ + ";" + green$) + 1, INSTR(LEN(red$ + ";" + green$) + 1, colour, ";") - 1)
            alpha$ = MID$(colour, LEN(red$ + ";" + green$ + ";" + blue$) + 1, INSTR(LEN(red$ + ";" + green$ + ";" + blue$) + 1, colour, ";") - 1)
            col& = _RGBA(VAL(red$), VAL(green$), VAL(blue$), VAL(alpha$))
    END SELECT
END FUNCTION
