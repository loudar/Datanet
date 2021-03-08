REM $DYNAMIC
SCREEN _NEWIMAGE(1920, 1080, 32)

'file list by SMcNeill
DECLARE CUSTOMTYPE LIBRARY "code\direntry"
    FUNCTION load_dir& (s AS STRING)
    FUNCTION has_next_entry& ()
    SUB close_dir ()
    SUB get_next_entry (s AS STRING, flags AS LONG, file_size AS LONG)
END DECLARE
REDIM Dir(0) AS STRING, File(0) AS STRING

'Variables
TYPE internalsettings
    trimstrings AS _BYTE
END TYPE
TYPE internal
    setting AS internalsettings
END TYPE
REDIM SHARED internal AS internal

TYPE global
    AS STRING nodepath, internalpath
    AS _UNSIGNED _INTEGER64 maxnodeid
    AS _FLOAT margin, round
END TYPE
REDIM SHARED global AS global

'Data structure
TYPE node
    AS STRING id, name, category, date, time
END TYPE
TYPE nodelink
    AS STRING parent, name, child
END TYPE

REDIM SHARED directories(0) AS STRING
REDIM SHARED nodefiles(0) AS STRING

'UI
TYPE element
    AS _BYTE view, acceptinput
    AS STRING x, y, w, h, style, name, text, type, color
    value AS _FLOAT
END TYPE
REDIM SHARED element(0) AS element
TYPE uisum
    AS _FLOAT x, y
    AS STRING f 'determines "focus"
END TYPE
REDIM SHARED uisum(0) AS uisum

TYPE rectangle
    AS _FLOAT x, y, w, h
END TYPE

resetallvalues
loadui
CLS

DO
    displayview 1
LOOP
SLEEP

SUB checkui (arguments AS STRING)
    DO: e = e + 1
        checkelement element(e), arguments
    LOOP UNTIL e = UBOUND(element)
END SUB

SUB checkelement (this AS element, arguments AS STRING)
    IF this.view = -1 AND this.acceptinput = -1 THEN

    END IF
END SUB

SUB displayview (dview AS INTEGER)
    resetsum
    SELECT CASE dview
        CASE 1
            DO: e = e + 1
                displayelement element(e), ""
            LOOP UNTIL e = UBOUND(element)
    END SELECT
END SUB

SUB displayelement (this AS element, arguments AS STRING) 'parses abstract coordinates into discrete coordinates
    IF this.view = -1 THEN
        DIM AS _FLOAT x, y, w, h
        IF MID$(this.w, 1, 4) = "flex" THEN
            w = VAL(this.w) + (_FONTWIDTH * (LEN(this.text) + 2))
        ELSE
            w = VAL(this.w)
        END IF
        h = VAL(this.h)

        IF MID$(this.x, 1, 4) = "flex" THEN
            IF MID$(this.y, 1, 4) = "flex" THEN
                'what the fuck do i do here? difficult to determine because having flex in both positions effectively doesn't position it at all
                'doesn't work like html, stacks elements based on position and not on
            ELSE
                x = findsum("y=" + this.y, w)
                y = VAL(this.y)
            END IF
        ELSE
            x = VAL(this.x)
            IF MID$(this.y, 1, 4) = "flex" THEN
                y = findsum("x=" + this.x, h)
            ELSE
                y = VAL(this.y)
            END IF
        END IF

        LOCATE 1
        PRINT this.type, x, y, w, h

        SELECT CASE this.type
            CASE "button"
                rectangle "x=" + lst$(x) + ",y=" + lst$(y) + ",w=" + lst$(w) + ",h=" + lst$(h) + ",color=" + this.color + ",style=" + this.style
            CASE "input"
                rectangle "x=" + lst$(x) + ",y=" + lst$(y) + ",w=" + lst$(w) + ",h=" + lst$(h) + ",color=" + this.color + ",style=" + this.style
        END SELECT
    END IF
END SUB

FUNCTION findsum (arguments AS STRING, addvalue AS _FLOAT)
    checksumx = getargumentv(arguments, "x")
    checksumy = getargumentv(arguments, "y")
    IF checksumx > 0 THEN
        IF UBOUND(uisum) > 0 THEN
            DO: u = u + 1
                IF uisum(u).x = checksumx AND uisum(u).f = "x" THEN
                    sumfound = u
                END IF
            LOOP UNTIL u = UBOUND(uisum)
        ELSE
            REDIM _PRESERVE uisum(UBOUND(uisum) + 1) AS uisum
            sumfound = UBOUND(uisum)
        END IF
        IF sumfound = 0 THEN
            REDIM _PRESERVE uisum(UBOUND(uisum) + 1) AS uisum
            sumfound = UBOUND(uisum)
        END IF
        uisum(sumfound).x = checksumx
        uisum(sumfound).y = uisum(sumfound).y + global.margin + addvalue
        uisum(sumfound).f = "x"
        findsum = uisum(sumfound).y - addvalue
    ELSEIF checksumy > 0 THEN
        IF UBOUND(uisum) > 0 THEN
            DO: u = u + 1
                IF uisum(u).y = checksumy AND uisum(u).f = "y" THEN
                    sumfound = u
                END IF
            LOOP UNTIL u = UBOUND(uisum)
        ELSE
            REDIM _PRESERVE uisum(UBOUND(uisum) + 1) AS uisum
            sumfound = UBOUND(uisum)
        END IF
        IF sumfound = 0 THEN
            REDIM _PRESERVE uisum(UBOUND(uisum) + 1) AS uisum
            sumfound = UBOUND(uisum)
        END IF
        uisum(sumfound).x = uisum(sumfound).x + global.margin + addvalue
        uisum(sumfound).y = checksumy
        uisum(sumfound).f = "y"
        findsum = uisum(sumfound).y - addvalue
    ELSE
        findsum = global.margin
    END IF
END FUNCTION

SUB resetsum
    REDIM uisum(0) AS uisum
END SUB

SUB add.node (arguments AS STRING)
    DIM this AS node
    this.name = getargument$(arguments, "nodename")
    this.category = getargument$(arguments, "nodecategory")
    this.date = DATE$
    this.time = TIME$
    this.id = lst$(global.maxnodeid + 1)
    IF this.name <> "" AND this.category <> "" THEN
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
    WRITE #filen, "category=" + this.category
    CLOSE #filen
END SUB

SUB add.nodelink (arguments AS STRING)
    DIM this AS nodelink

END SUB

SUB loadui
    DO: lview = lview + 1
        freen = FREEFILE
        viewfile$ = global.internalpath + "\" + lst$(lview) + ".dui"
        IF _FILEEXISTS(viewfile$) THEN
            OPEN viewfile$ FOR INPUT AS #freen
            INPUT #freen, uielement$

            REDIM _PRESERVE element(UBOUND(element) + 1) AS element
            eub = UBOUND(element)
            element(eub).type = getargument$(uielement$, "type")
            element(eub).name = getargument$(uielement$, "name")
            element(eub).x = getargument$(uielement$, "x")
            element(eub).y = getargument$(uielement$, "y")
            element(eub).w = getargument$(uielement$, "w")
            element(eub).h = getargument$(uielement$, "h")
            element(eub).color = getargument$(uielement$, "color")
            element(eub).style = getargument$(uielement$, "style")
            element(eub).view = -1

            CLOSE #freen
        ELSE
            PRINT "Error loading UI for view " + lst$(lview)
            _DELAY 2
        END IF
    LOOP UNTIL lview = 3
END SUB

SUB resetallvalues
    'internal settings
    internal.setting.trimstrings = -1

    'data structure
    global.internalpath = "internal"
    global.nodepath = "nodes"
    getmaxnodeid
    global.margin = 10
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

'FUNCTION stringvalue$ (basestring AS STRING, argument AS STRING)
'    DIM buffer AS STRING
'    buffer = MID$(basestring, INSTR(basestring, argument) + LEN(argument))
'    IF INSTR(buffer, ";") THEN
'        buffer = MID$(buffer, 1, INSTR(buffer, ";") - 1)
'    END IF
'    IF INSTR(buffer, "=") THEN
'        buffer = MID$(buffer, INSTR(buffer, "=") + 1)
'    END IF
'    stringvalue$ = _TRIM$(buffer)
'END FUNCTION

FUNCTION stringvalue$ (basestring AS STRING, argument AS STRING)
    IF LEN(basestring) > 0 THEN
        DO: p = p + 1
            IF MID$(basestring, p, LEN(argument)) = argument THEN
                endpos = INSTR(p + LEN(argument), basestring, ",")
                IF endpos = 0 THEN endpos = LEN(basestring) ELSE endpos = endpos - 1 'means that no comma has been found. taking the entire rest of the string as argument value.

                startpos = INSTR(p + LEN(argument), basestring, "=")
                IF startpos > endpos THEN
                    startpos = p + LEN(argument)
                ELSE
                    IF startpos = 0 THEN startpos = p + LEN(argument) ELSE startpos = startpos + 1 'means that no equal sign has been found. taking value right from the end of the argument name.
                END IF

                IF internal.setting.trimstrings = -1 THEN
                    stringvalue$ = LTRIM$(RTRIM$(MID$(basestring, startpos, endpos - startpos + 1)))
                ELSE
                    stringvalue$ = MID$(basestring, startpos, endpos - startpos + 1)
                END IF
            END IF
        LOOP UNTIL p = LEN(basestring)
    END IF
END FUNCTION

SUB rectangle (arguments AS STRING) 'lx, ly, ux, uy, round, clr&, outline$
    lx = VAL(getargument(arguments, "x"))
    ly = VAL(getargument(arguments, "y"))
    ux = lx + VAL(getargument(arguments, "w"))
    uy = ly + VAL(getargument(arguments, "h"))
    round$ = getargument$(arguments, "round")
    IF round$ = "" THEN
        round = global.round
    ELSE
        round = VAL(round$)
    END IF
    clr& = col&(getargument$(arguments, "color"))
    SELECT CASE getargument$(arguments, "style")
        CASE "BF"
            rectangleoutline lx, ly, ux, uy, round, clr&
            PAINT (lx + ((ux - lx) / 2), ly + ((uy - ly) / 2)), clr&, clr&
        CASE "B"
            rectangleoutline lx, ly, ux, uy, round, clr&
    END SELECT
END SUB

SUB rectangleoutline (lx, ly, ux, uy, round, clr&)
    IF round > 0 THEN
        'corners:
        'lx-ly
        x = -0.5 * _PI
        DO: x = x + ((0.5 * _PI) / round / detail)
            PSET ((lx + round) + (SIN(x) * round), (ly + round) - (COS(x) * round)), clr&
        LOOP UNTIL x >= 0
        'lx-uy
        x = -0.5 * _PI
        DO: x = x + ((0.5 * _PI) / round / detail)
            PSET ((lx + round) + (SIN(x) * round), (uy - round) + (COS(x) * round)), clr&
        LOOP UNTIL x >= 0
        'ux-ly
        x = -0.5 * _PI
        DO: x = x + ((0.5 * _PI) / round / detail)
            PSET ((ux - round) - (SIN(x) * round), (ly + round) - (COS(x) * round)), clr&
        LOOP UNTIL x >= 0
        'ux-uy
        x = -0.5 * _PI
        DO: x = x + ((0.5 * _PI) / round / detail)
            PSET ((ux - round) - (SIN(x) * round), (uy - round) + (COS(x) * round)), clr&
        LOOP UNTIL x >= 0
        'lines:
        LINE (lx + round, ly)-(ux - round, ly), clr&
        LINE (lx + round, uy)-(ux - round, uy), clr&
        LINE (lx, ly + round)-(lx, uy - round), clr&
        LINE (ux, ly + round)-(ux, uy - round), clr&
    ELSE
        LINE (lx, ly)-(ux, uy), clr&, B
    END IF
END SUB

FUNCTION lst$ (number AS _FLOAT)
    lst$ = LTRIM$(STR$(number))
END FUNCTION

FUNCTION col& (colour AS STRING)
    SELECT CASE colour
        CASE "ui"
            col& = _RGBA(255, 255, 255, 255)
        CASE "bg1"
            col& = _RGBA(20, 20, 20, 255)
        CASE ELSE
            red$ = MID$(colour, 1, INSTR(colour, ";") - 1)
            green$ = MID$(colour, LEN(red$) + 1, INSTR(LEN(red$) + 1, colour, ";") - 1)
            blue$ = MID$(colour, LEN(red$ + ";" + green$) + 1, INSTR(LEN(red$ + ";" + green$) + 1, colour, ";") - 1)
            alpha$ = MID$(colour, LEN(red$ + ";" + green$ + ";" + blue$) + 1, INSTR(LEN(red$ + ";" + green$ + ";" + blue$) + 1, colour, ";") - 1)
            col& = _RGBA(VAL(red$), VAL(green$), VAL(blue$), VAL(alpha$))
    END SELECT
END FUNCTION
