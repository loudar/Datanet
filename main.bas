$EXEICON:'..\Targon Industries\DATANET\internal\ico\Datanet_1.ico'
$RESIZE:ON
REM $DYNAMIC

_SCREENHIDE
'file list by SMcNeill
DECLARE CUSTOMTYPE LIBRARY "code\direntry"
    FUNCTION load_dir& (s AS STRING)
    FUNCTION has_next_entry& ()
    SUB close_dir ()
    SUB get_next_entry (s AS STRING, flags AS LONG, file_size AS LONG)
END DECLARE

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
    AS STRING nodepath, internalpath, license
    AS _UNSIGNED _INTEGER64 maxnodeid
    AS _FLOAT margin, padding, round, stroke
    AS _BYTE licensestatus
END TYPE
REDIM SHARED global AS global

TYPE mouse
    AS _FLOAT x, y
    AS _BYTE left, right
    AS INTEGER scroll
    AS _UNSIGNED _INTEGER64 lefttime, righttime
END TYPE
REDIM SHARED mouse AS mouse

'Data structure
TYPE node
    AS STRING name, type, date, time
END TYPE
TYPE nodelink
    AS STRING origin, name, target, date, time
END TYPE

'UI
TYPE element
    AS _BYTE show, acceptinput, allownumbers, allowtext, allowspecial, selected, state, deselect
    AS STRING x, y, w, h, style, name, text, buffer, type, color, hovercolor, action, angle, view, round, hovertext, padding, url
    AS INTEGER sel_start, sel_end, cursor, items, hovertextwait, hoverx, hovery
    AS _UNSIGNED _INTEGER64 scroll
    AS _FLOAT statelock, hovertime
    AS LONG drawcolor
    AS _BYTE contextopen, allowcontextclose
    AS INTEGER contextx, contexty
    value AS _FLOAT
END TYPE
REDIM SHARED elements(0) AS element
TYPE uisum
    AS _FLOAT x, y
    AS STRING f 'determines "focus"
END TYPE
REDIM SHARED uisum(0) AS uisum
REDIM SHARED AS STRING viewname(0), currentview
TYPE invoke
    AS _BYTE delete, back, select, right, left, deselect, jumptoend, jumptofront
END TYPE
REDIM SHARED AS LONG font_normal, font_big
REDIM SHARED AS INTEGER elementlock, activeelement
REDIM SHARED AS _BYTE contextopen, lockuicall

TYPE rectangle
    AS _FLOAT x, y, w, h
END TYPE

resetallvalues

REDIM SHARED AS INTEGER screenresx, screenresy, winresx, winresy
screenresx = _DESKTOPWIDTH
screenresy = _DESKTOPHEIGHT
winresx = screenresx * 0.5 'to be replaced with config-based factor
winresy = screenresy * 0.5
SCREEN _NEWIMAGE(winresx, winresy, 32)
_SCREENMOVE (screenresx / 2) - (winresx / 2), (screenresy / 2) - (winresy / 2)
_SCREENSHOW
DO: LOOP UNTIL _SCREENEXISTS
_TITLE "Datanet"


'------------- TESTING AREA ------------



'-------------- MAIN AREA --------------

loadui
DO
    CLS
    checkresize
    checkmouse
    keyhit = checkkeyboard
    displayview keyhit
    _LIMIT internal.setting.fps
LOOP

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

SUB checkmouse
    mouse.scroll = 0
    DO
        mouse.x = _MOUSEX
        mouse.y = _MOUSEY
        mouse.scroll = mouse.scroll + _MOUSEWHEEL
        mouse.left = _MOUSEBUTTON(1)
        IF mouse.left THEN mouse.lefttime = TIMER
        mouse.right = _MOUSEBUTTON(2)
        IF mouse.right THEN mouse.righttime = TIMER
    LOOP WHILE _MOUSEINPUT
END SUB

FUNCTION checkkeyboard
    DIM keyhit AS _UNSIGNED _INTEGER64
    keyhit = _KEYHIT
    IF keyhit < 0 THEN keyhit = 0
    checkkeyboard = keyhit
END FUNCTION

SUB displayview (keyhit AS INTEGER) 'displays a "view"
    LINE (0, 0)-(_WIDTH(0), _HEIGHT(0)), col&("bg1"), BF
    IF UBOUND(elements) > 0 THEN
        DO: e = e + 1
            IF elements(e).view = currentview THEN
                displayelement e, keyhit
            END IF
        LOOP UNTIL e = UBOUND(elements)
    END IF
    IF UBOUND(elements) > 0 THEN
        e = 0: DO: e = e + 1
            IF elements(e).view = currentview THEN
                displaymenu e, keyhit
            END IF
        LOOP UNTIL e = UBOUND(elements)
    END IF
    _DISPLAY
END SUB

SUB displaymenu (elementindex AS INTEGER, keyhit AS INTEGER)
    DIM this AS element
    this = elements(elementindex)

    DIM coord AS rectangle
    getcoord coord, elementindex

    IF mouse.right AND mouseinbounds(coord) THEN
        opencontext this
    END IF
    IF NOT mouse.right THEN this.allowcontextclose = -1
    IF this.contextopen AND contextopen THEN displaycontext this

    IF TIMER - this.hovertime >= this.hovertextwait AND this.hovertime > 0 AND this.hovertext <> "" AND NOT this.contextopen THEN
        hoverpadding = 3
        hoveryoffset = -_FONTHEIGHT(font_normal)
        LINE (mouse.x - hoverpadding, mouse.y - hoverpadding + hoveryoffset)-(mouse.x + (LEN(this.hovertext) * _FONTWIDTH(font_normal)) + hoverpadding, mouse.y + _FONTHEIGHT(font_normal) + hoverpadding + hoveryoffset), col&("bg1"), BF
        LINE (mouse.x - hoverpadding, mouse.y - hoverpadding + hoveryoffset)-(mouse.x + (LEN(this.hovertext) * _FONTWIDTH(font_normal)) + hoverpadding, mouse.y + _FONTHEIGHT(font_normal) + hoverpadding + hoveryoffset), col&("ui"), B
        COLOR col&("ui"), col&("t")
        _PRINTSTRING (mouse.x, mouse.y + hoveryoffset), this.hovertext
    END IF

    elements(elementindex) = this 'needed to save the changed data into the original elements array
END SUB

SUB opencontext (this AS element)
    this.contextopen = -1
    contextopen = -1
    this.contextx = mouse.x
    this.contexty = mouse.y
    this.allowcontextclose = 0
END SUB

SUB getcoord (coord AS rectangle, elementindex AS INTEGER)
    coord.x = VAL(getexpos$(elementindex))
    coord.y = VAL(geteypos$(elementindex))
    coord.w = VAL(getewidth$(elementindex))
    coord.h = VAL(geteheight$(elementindex))
END SUB

SUB displaycontext (this AS element)
    REDIM _PRESERVE contextdata(0) AS STRING
    getcontextarray contextdata(), this
    maxlen = getmaxstringlen(contextdata())
    contextpadding = 6
    contextwidth = (maxlen * _FONTWIDTH(font_normal)) + (2 * contextpadding)
    contextheight = (UBOUND(contextdata) * _FONTHEIGHT(font_normal)) + (2 * contextpadding)

    DIM contextcoord AS rectangle
    contextcoord.x = this.contextx - 1
    contextcoord.y = this.contexty - 1
    contextcoord.w = contextwidth
    contextcoord.h = contextheight
    IF (mouse.left OR mouse.right) AND NOT mouseinbounds(contextcoord) AND this.allowcontextclose THEN this.contextopen = 0: contextopen = 0

    IF UBOUND(contextdata) > 0 THEN
        LINE (contextcoord.x, contextcoord.y)-(contextcoord.x + contextcoord.w, contextcoord.y + contextcoord.h), col&("bg1"), BF
        LINE (contextcoord.x, contextcoord.y)-(contextcoord.x + contextcoord.w, contextcoord.y + contextcoord.h), col&("ui"), B

        contextentry = 0: DO: contextentry = contextentry + 1
            DIM entrycoord AS rectangle
            entrycoord.x = contextcoord.x
            entrycoord.y = contextcoord.y + ((_FONTHEIGHT(font_normal) + contextpadding) * (contextentry - 1))
            entrycoord.w = contextcoord.w
            entrycoord.h = _FONTHEIGHT(font_normal) + contextpadding

            IF mouseinbounds(entrycoord) THEN
                IF mouse.left THEN uicall LCASE$(contextdata(contextentry)), this: lockuicall = -1 ELSE lockuicall = 0
                COLOR col&("blue"), col&("t")
                LINE (entrycoord.x + 1, entrycoord.y + 1)-(entrycoord.x + entrycoord.w - 1, entrycoord.y + entrycoord.h - 1), col&("bg3"), BF
            ELSE
                COLOR col&("ui"), col&("t")
            END IF

            _PRINTSTRING (this.contextx + contextpadding, entrycoord.y + contextpadding), contextdata(contextentry)
        LOOP UNTIL contextentry = UBOUND(contextdata)
    END IF
END SUB

SUB getcontextarray (array() AS STRING, this AS element)
    IF textselectable(this) THEN addtostringarray array(), "Copy"
    IF typable(this) THEN addtostringarray array(), "Paste"
    IF this.action <> "" THEN addtostringarray array(), "Activate"
END SUB

SUB addtostringarray (array() AS STRING, toadd AS STRING)
    REDIM _PRESERVE array(UBOUND(array) + 1) AS STRING
    array(UBOUND(array)) = toadd
END SUB

FUNCTION getmaxstringlen (array() AS STRING)
    DIM buffer AS _UNSIGNED _INTEGER64
    IF UBOUND(array) > 0 THEN
        DO: i = i + 1
            IF LEN(array(i)) > buffer THEN
                buffer = LEN(array(i))
            END IF
        LOOP UNTIL i = UBOUND(array)
        getmaxstringlen = buffer: EXIT FUNCTION
    ELSE
        getmaxstringlen = 0: EXIT FUNCTION
    END IF
END FUNCTION

FUNCTION getbufferchar$ (this AS element, keyhit AS INTEGER)
    IF isnumchar(keyhit) AND this.allownumbers THEN
        getbufferchar$ = CHR$(keyhit)
    ELSEIF istextchar(keyhit) AND this.allowtext THEN
        getbufferchar$ = CHR$(keyhit)
    ELSEIF isspecialchar(keyhit) AND this.allowspecial THEN
        getbufferchar$ = CHR$(keyhit)
    END IF
END FUNCTION

SUB displayelement (elementindex AS INTEGER, keyhit AS INTEGER) 'parses abstract coordinates into discrete coordinates
    DIM this AS element
    DIM bufferchar AS STRING
    DIM invoke AS invoke
    this = elements(elementindex)

    IF active(elementindex) THEN bufferchar = getbufferchar(this, keyhit)

    'general
    SELECT CASE keyhit
        CASE 8: invoke.back = -1 'backspace
        CASE 9 'tab
            IF shiftdown THEN
                waituntilreleased keyhit
                activeelement = getlastelement(currentview, activeelement)
            ELSE
                waituntilreleased keyhit
                activeelement = getnextelement(currentview, activeelement)
            END IF
        CASE 13 'enter
            waituntilreleased keyhit
            buffer$ = ""
            IF this.action <> "" THEN buffer$ = "action=" + this.action + ";"
            IF shiftdown THEN
                activeelement = getnextelement(currentview, activeelement)
            ELSE
                dothis buffer$ + getcurrentinputvalues$(-1)
            END IF
        CASE 21248: invoke.delete = -1 'delete
        CASE 19200: invoke.left = -1 'left arrow
        CASE 19712: invoke.right = -1 'right arrow
        CASE 20224: invoke.jumptoend = -1 'end key
        CASE 18176: invoke.jumptofront = -1 'home key
    END SELECT

    elementkeyhandling this, elementindex, bufferchar, invoke

    DIM coord AS rectangle
    getcoord coord, elementindex

    elementmousehandling this, elementindex, invoke, coord
    IF isexception(this.name) THEN this.buffer = getexceptionvalue$(this.name)
    drawelement this, elementindex, coord, invoke

    elements(elementindex) = this
END SUB

FUNCTION isexception (elementname AS STRING)
    SELECT CASE elementname
        CASE "licensestatus": isexception = -1
        CASE ELSE: isexception = 0
    END SELECT
END FUNCTION

FUNCTION getexceptionvalue$ (elementname AS STRING)
    SELECT CASE elementname
        CASE "licensestatus": getexceptionvalue$ = switchword$("active", global.licensestatus)
    END SELECT
END FUNCTION

SUB drawelement (this AS element, elementindex AS INTEGER, coord AS rectangle, invoke AS invoke)
    _FONT font_normal
    SELECT CASE this.type
        CASE "button"
            rectangle "x=" + lst$(coord.x) + ";y=" + lst$(coord.y) + ";w=" + lst$(coord.w) + ";h=" + lst$(coord.h) + ";style=" + this.style + ";angle=" + this.angle + ";round=" + this.round, this.drawcolor
            coord.x = coord.x + (2 * global.padding)
            coord.y = coord.y + global.padding
            IF LCASE$(this.style) = "bf" THEN
                COLOR col&("bg1"), col&("t")
            ELSE
                COLOR this.drawcolor, col&("t")
            END IF
            _PRINTSTRING (coord.x, coord.y), this.text + " " + this.buffer
        CASE "input"
            underlinedistance = -2
            LINE (coord.x, coord.y + coord.h + underlinedistance)-(coord.x + coord.w - (2 * global.padding), coord.y + coord.h + underlinedistance), this.drawcolor
            COLOR this.drawcolor, col&("t")
            _PRINTSTRING (coord.x, coord.y), this.text + " " + this.buffer
        CASE "text"
            COLOR this.drawcolor, col&("t")
            _PRINTSTRING (coord.x, coord.y), this.text + " " + this.buffer
        CASE "time"
            COLOR this.drawcolor, col&("t")
            this.buffer = TIME$
            _PRINTSTRING (coord.x, coord.y), this.text + " " + this.buffer
        CASE "date"
            COLOR this.drawcolor, col&("t")
            this.buffer = DATE$
            _PRINTSTRING (coord.x, coord.y), this.text + " " + this.buffer
        CASE "title"
            _FONT font_big
            COLOR this.drawcolor, col&("t")
            _PRINTSTRING (coord.x, coord.y), this.text
            _FONT font_normal
        CASE "line"
            LINE (coord.x, coord.y)-(coord.x + coord.w - global.padding, coord.y), this.drawcolor
        CASE "box"
            rectangle "x=" + lst$(coord.x) + ";y=" + lst$(coord.y) + ";w=" + lst$(coord.w) + ";h=" + lst$(coord.h) + ";style=" + this.style + ";angle=" + this.angle + ";round=" + this.round, this.drawcolor
        CASE "switch"
            boxsize = _FONTHEIGHT(font_normal) * 0.75
            boxoffset = 0
            coord.w = coord.w + boxsize + global.margin
            IF this.state = -1 THEN this.style = "bf" ELSE this.style = "b"
            rectangle "x=" + lst$(coord.x) + ";y=" + lst$(coord.y + boxoffset) + ";w=" + lst$(boxsize) + ";h=" + lst$(boxsize) + ";style=" + this.style + ";round=0", this.drawcolor
            COLOR this.drawcolor, col&("t")
            _PRINTSTRING (coord.x + boxsize + global.margin, coord.y), this.text + " " + switchword$("on", this.state)
        CASE "list"
            rectangle "x=" + lst$(coord.x) + ";y=" + lst$(coord.y) + ";w=" + lst$(coord.w) + ";h=" + lst$(coord.h) + ";style=" + this.style + ";angle=" + this.angle + ";round=" + this.round, this.drawcolor
            coord.x = coord.x + (2 * global.padding)
            coord.y = coord.y + global.padding
            SELECT CASE this.name
                CASE "nodelist"
                    searchnode$ = getargument$(getcurrentinputvalues$(0), "nodetarget")
                    nodecount = getnodecount(searchnode$)
                    DIM AS STRING nodearray(0)
                    getnodearray nodearray(), searchnode$
                    IF UBOUND(nodearray) > 0 THEN
                        IF this.scroll > UBOUND(nodearray) THEN this.scroll = UBOUND(nodearray)
                        IF this.scroll < 0 THEN this.scroll = 0
                        n = this.scroll
                        DO: n = n + 1
                            listitemy = coord.y + ((global.margin + _FONTHEIGHT(font_normal)) * (n - 1))

                            IF mouse.x > coord.x - (2 * global.padding) AND mouse.x < coord.x + coord.w - (2 * global.padding) AND mouse.y > listitemy AND mouse.y < listitemy + _FONTHEIGHT(font_normal) THEN
                                COLOR col&("blue"), col&("t")
                            ELSE
                                COLOR this.drawcolor, col&("t")
                            END IF

                            IF listitemy + _FONTHEIGHT(font_normal) < coord.y + coord.h THEN
                                'fetch node info based on array
                                DIM AS STRING listitem(3)
                                listitem(1) = nodearray(n)
                                listitem(2) = getnodeinfo$("date", listitem(1)) + " @ " + getnodeinfo$("time", listitem(1))
                                listitem(3) = getnodeinfo$("type", listitem(1))

                                'display node info
                                IF UBOUND(listitem) > 0 THEN
                                    li = 0: DO: li = li + 1
                                        listitemx = coord.x + ((coord.w / UBOUND(listitem)) * (li - 1))
                                        _PRINTSTRING (listitemx, listitemy), listitem(li)
                                    LOOP UNTIL li = UBOUND(listitem)
                                END IF
                                ERASE listitem 'clean up array, will otherwise produce errors when dimming again
                            END IF
                        LOOP UNTIL n = UBOUND(nodearray) OR (listitemy + _FONTHEIGHT(font_normal) >= coord.y - global.padding + coord.h)
                        this.items = n - this.scroll
                    END IF
                CASE "linklist"
            END SELECT
    END SELECT

    'display selection/cursor
    IF textselectable(this) AND active(elementindex) THEN
        IF longselection(this) THEN
            COLOR this.drawcolor, col&("selected")
            minx = min(this.sel_start, this.sel_end)
            maxx = max(this.sel_start, this.sel_end)
            _PRINTSTRING (coord.x + (_FONTWIDTH(font_normal) * (LEN(this.text) + min(this.sel_start, this.sel_end))), coord.y), MID$(this.buffer, min(this.sel_start, this.sel_end), max(this.sel_start, this.sel_end) - min(this.sel_start, this.sel_end) + 1)
        ELSE
            IF (TIMER - mouse.lefttime) MOD 2 = 0 AND typable(this) THEN
                cursoroffset = -1
                cursorx = coord.x + (_FONTWIDTH(font_normal) * (LEN(this.text) + this.cursor + 1)) + cursoroffset
                LINE (cursorx, coord.y - 2)-(cursorx, coord.y + _FONTHEIGHT(font_normal) + 2), this.drawcolor, BF
            END IF
        END IF
    END IF
    IF (this.sel_start OR this.sel_end) AND mouse.left = 0 THEN
        this.deselect = -1
    END IF
END SUB

FUNCTION active (elementindex AS INTEGER)
    IF elementindex = activeelement THEN active = -1 ELSE active = 0
END FUNCTION

FUNCTION selectable (this AS element)
    IF this.type = "input" OR this.type = "button" THEN selectable = -1 ELSE selectable = 0
END FUNCTION

FUNCTION typable (this AS element)
    IF this.type = "input" THEN typable = -1 ELSE typable = 0
END FUNCTION

FUNCTION textselectable (this AS element)
    IF this.type = "input" OR this.type = "text" OR this.type = "time" OR this.type = "date" THEN textselectable = -1 ELSE textselectable = 0
END FUNCTION

FUNCTION longselection (this AS element)
    IF this.sel_start > 0 AND this.sel_end > 0 AND (this.sel_start <> this.sel_end) THEN longselection = -1 ELSE longselection = 0
END FUNCTION

SUB waituntilreleased (keyhit AS INTEGER)
    DO: LOOP UNTIL _KEYDOWN(keyhit) = 0: keyhit = 0
END SUB

FUNCTION getlastelement (viewtogetfrom AS STRING, elementindex AS INTEGER)
    IF elementindex = 1 THEN e = UBOUND(elements) + 1 ELSE e = elementindex
    IF UBOUND(elements) > 0 THEN
        DO: e = e - 1
            IF elements(e).view = viewtogetfrom AND selectable(elements(e)) THEN getlastelement = e: EXIT FUNCTION
        LOOP UNTIL e = 1
    END IF
END FUNCTION

FUNCTION getnextelement (viewtogetfrom AS STRING, elementindex AS INTEGER)
    IF elementindex = UBOUND(elements) THEN e = 0 ELSE e = elementindex
    IF UBOUND(elements) > 0 THEN
        DO: e = e + 1
            IF elements(e).view = viewtogetfrom AND selectable(elements(e)) THEN getnextelement = e: EXIT FUNCTION
        LOOP UNTIL e = UBOUND(elements)
    END IF
END FUNCTION

FUNCTION getmaxelement (viewtogetfrom AS STRING)
    IF UBOUND(elements) > 0 THEN
        DO: e = e + 1
            IF elements(e).view = viewtogetfrom THEN
                buffer = e
            END IF
        LOOP UNTIL e = UBOUND(elements)
    END IF
    getmaxelement = buffer
END FUNCTION

FUNCTION istextchar (keyhit AS INTEGER)
    IF (keyhit >= ASC("A") AND keyhit <= ASC("Z")) OR (keyhit >= ASC("a") AND keyhit <= ASC("z")) OR keyhit = ASC(" ") THEN istextchar = -1 ELSE istextchar = 0
END FUNCTION

FUNCTION isnumchar (keyhit AS INTEGER)
    IF keyhit >= ASC("0") AND keyhit <= ASC("9") THEN isnumchar = -1 ELSE isnumchar = 0
END FUNCTION

FUNCTION isspecialchar (keyhit AS INTEGER)
    IF (keyhit >= ASC("!") AND keyhit <= ASC("~") AND NOT istextchar(keyhit) AND NOT isnumchar(keyhit)) THEN isspecialchar = -1 ELSE isspecialchar = 0
END FUNCTION

SUB elementmousehandling (this AS element, elementindex AS INTEGER, invoke AS invoke, coord AS rectangle)
    IF this.hovertextwait = 0 THEN this.hovertextwait = 1
    IF mouseinbounds(coord) THEN
        IF this.hovertime = 0 OR this.hoverx <> mouse.x OR this.hovery <> mouse.y THEN this.hovertime = TIMER: this.hoverx = mouse.x: this.hovery = mouse.y
    ELSE
        this.hovertime = 0
    END IF
    IF mouseinbounds(coord) AND (elementlock = 0 OR elementlock = elementindex) AND NOT contextopen THEN
        IF ctrldown THEN
            IF mouse.left AND selectable(this) THEN
                IF this.selected = -1 THEN this.selected = 0 ELSE this.selected = -1
                DO: m = _MOUSEINPUT: LOOP UNTIL _MOUSEBUTTON(1) = 0
            END IF
        ELSE
            IF mouse.left AND this.action <> "" THEN
                dothis "action=" + this.action + ";" + getcurrentinputvalues$(-1)
            ELSEIF mouse.left AND this.type = "switch" AND this.statelock = 0 THEN
                IF this.state = 0 THEN this.state = -1 ELSE this.state = 0
                this.statelock = -1
            ELSEIF mouse.left AND this.action = "" AND NOT active(elementindex) AND textselectable(this) THEN
                activeelement = elementindex
                this.sel_start = 0: this.sel_end = 0
            END IF

            IF mouse.scroll THEN
                this.scroll = this.scroll + mouse.scroll
            END IF
        END IF

        IF textselectable(this) THEN
            sel_leftbound = coord.x + ((LEN(this.text) + 1) * _FONTWIDTH(font_normal))
            sel_rightbound = coord.x + ((LEN(this.text) + 1 + LEN(this.buffer)) * _FONTWIDTH(font_normal))
            IF mouse.x > sel_leftbound AND mouse.x < sel_rightbound AND mouse.left AND this.action = "" THEN
                charcount = (sel_rightbound - sel_leftbound) / _FONTWIDTH(font_normal)
                mousehoverchar = INT((((mouse.x - sel_leftbound) / (sel_rightbound - sel_leftbound)) * charcount) + 0.5)

                IF this.deselect THEN
                    this.deselect = 0
                    this.sel_start = 0: this.sel_end = 0
                END IF
                IF this.sel_start THEN
                    this.sel_end = mousehoverchar
                    this.cursor = this.sel_end
                ELSE
                    this.sel_start = mousehoverchar
                    this.cursor = mousehoverchar
                END IF

                elementlock = elementindex 'locks all actions to only the current element
            END IF
        END IF
        IF NOT active(elementindex) THEN this.drawcolor = col&(this.hovercolor)
    ELSEIF active(elementindex) THEN
        this.drawcolor = col&("blue")
    ELSEIF this.selected THEN
        this.drawcolor = col&("green")
    ELSE
        this.drawcolor = col&(this.color)
    END IF
    IF mouse.left = 0 THEN elementlock = 0: this.statelock = 0
END SUB

FUNCTION mouseinbounds (coord AS rectangle)
    IF mouse.x > coord.x AND mouse.x < coord.x + coord.w AND mouse.y > coord.y AND mouse.y < coord.y + coord.h THEN mouseinbounds = -1 ELSE mouseinbounds = 0
END FUNCTION

SUB uicall (func AS STRING, this AS element)
    IF NOT lockuicall THEN
        SELECT CASE func
            CASE "select all"
                this.sel_start = 1: this.sel_end = LEN(this.buffer)
            CASE "paste"
                IF longselection(this) THEN
                    paste this.buffer, this.sel_start, this.sel_end
                ELSE
                    this.buffer = MID$(this.buffer, 1, this.cursor) + _CLIPBOARD$ + MID$(this.buffer, this.cursor + 1, LEN(this.buffer))
                END IF
            CASE "copy"
                IF longselection(this) THEN
                    copy this.buffer, this.sel_start, this.sel_end
                ELSE
                    _CLIPBOARD$ = this.buffer
                END IF
            CASE "search"
                IF this.name = "commandline" THEN this.buffer = "nodetarget=": this.cursor = LEN(this.buffer)
        END SELECT
    END IF
END SUB

SUB elementkeyhandling (this AS element, elementindex AS INTEGER, bufferchar AS STRING, invoke AS invoke)
    IF (this.selected OR active(elementindex)) AND invoke.delete AND shiftdown THEN 'delete the entire buffer with shift+delete
        this.buffer = ""
        this.cursor = 0
        this.sel_start = 0: this.sel_end = 0
    END IF
    IF this.cursor > LEN(this.buffer) THEN this.cursor = LEN(this.buffer)
    IF this.sel_start > LEN(this.buffer) THEN this.sel_start = LEN(this.buffer)
    IF this.sel_end > LEN(this.buffer) THEN this.sel_end = LEN(this.buffer)

    IF active(elementindex) = 0 THEN EXIT SUB

    'BELOW CODE WILL ONLY RUN IF ELEMENT IS ACTIVE!

    IF bufferchar <> "" THEN
        IF ctrldown THEN 'ctrl
            SELECT CASE LCASE$(bufferchar)
                CASE "a"
                    uicall "select all", this
                CASE "v" 'paste something into an input field
                    uicall "paste", this
                CASE "c" 'copy something from an input field
                    uicall "copy", this
                CASE "f" 'replace buffer with "nodetarget="
                    uicall "search", this
                CASE ELSE
                    insertbufferchar this, bufferchar
            END SELECT
        ELSE
            IF longselection(this) THEN
                sel_start = min(this.sel_start, this.sel_end)
                sel_end = max(this.sel_start, this.sel_end)
                this.buffer = deletepart$(this.buffer, sel_start, sel_end)
                this.cursor = sel_start - 1
            END IF
            insertbufferchar this, bufferchar
        END IF
    ELSE
        IF ctrldown THEN
            IF invoke.left THEN this.cursor = _INSTRREV(MID$(this.buffer, 1, this.cursor - 1), " ")
            IF invoke.right THEN this.cursor = INSTR(MID$(this.buffer, this.cursor + 1, LEN(this.buffer)), " ") + this.cursor
        ELSE
            IF invoke.left THEN IF this.cursor > 0 THEN this.cursor = this.cursor - 1
            IF invoke.right THEN IF this.cursor < LEN(this.buffer) THEN this.cursor = this.cursor + 1
        END IF
        IF invoke.jumptoend THEN this.cursor = LEN(this.buffer)
        IF invoke.jumptofront THEN this.cursor = 0
    END IF

    'selection management
    IF longselection(this) AND (invoke.delete OR invoke.back) THEN 'deleting with selection
        sel_start = min(this.sel_start, this.sel_end)
        sel_end = max(this.sel_start, this.sel_end)
        this.buffer = deletepart$(this.buffer, sel_start, sel_end)
        this.cursor = sel_start - 1
        resetselection this
    ELSE 'deleting only one character
        IF invoke.back AND this.cursor > 0 THEN 'backspace
            this.buffer = MID$(this.buffer, 1, this.cursor - 1) + MID$(this.buffer, this.cursor + 1, LEN(this.buffer))
            this.cursor = this.cursor - 1
            resetselection this
        ELSEIF invoke.delete AND this.cursor < LEN(this.buffer) THEN 'delete
            this.buffer = MID$(this.buffer, 1, this.cursor) + MID$(this.buffer, this.cursor + 2, LEN(this.buffer))
            resetselection this
        END IF
    END IF
END SUB

FUNCTION deletepart$ (basestring AS STRING, delstart AS INTEGER, delend AS INTEGER)
    deletepart$ = MID$(basestring, 1, delstart - 1) + MID$(basestring, delend + 1, LEN(basestring))
END FUNCTION

SUB resetselection (this AS element)
    this.sel_start = 0: this.sel_end = 0
END SUB

SUB paste (basestring AS STRING, clipstart AS INTEGER, clipend AS INTEGER)
    clipbuffer = min(clipstart, clipend)
    clipend = max(clipstart, clipend)
    clipstart = clipbuffer
    basestring = MID$(basestring, 1, clipstart - 1) + _CLIPBOARD$ + MID$(basestring, clipend + 1, LEN(basestring))
END SUB

SUB copy (basestring AS STRING, clipstart AS INTEGER, clipend AS INTEGER)
    cliplength = ABS(clipstart - clipend) + 1
    clipstart = min(clipstart, clipend)
    _CLIPBOARD$ = MID$(basestring, clipstart, cliplength)
END SUB

FUNCTION min (a, b)
    IF a < b THEN min = a ELSE min = b
END FUNCTION

FUNCTION max (a, b)
    IF a > b THEN max = a ELSE max = b
END FUNCTION

SUB insertbufferchar (this AS element, insert AS STRING)
    this.buffer = MID$(this.buffer, 1, this.cursor) + insert + MID$(this.buffer, this.cursor + 1, LEN(this.buffer))
    this.cursor = this.cursor + 1
    resetselection this
END SUB

FUNCTION ctrldown
    IF _KEYDOWN(100305) OR _KEYDOWN(100306) THEN ctrldown = -1 ELSE ctrldown = 0
END FUNCTION

FUNCTION shiftdown
    IF _KEYDOWN(100303) OR _KEYDOWN(100304) THEN shiftdown = -1 ELSE shiftdown = 0
END FUNCTION

FUNCTION getnodeinfo$ (attribute AS STRING, target AS STRING)
    file$ = path$(target)
    IF _FILEEXISTS(file$) THEN
        freen = FREEFILE
        OPEN file$ FOR INPUT AS #freen
        IF EOF(freen) = 0 THEN
            DO
                INPUT #freen, nodeline$
                info$ = getargument$(nodeline$, attribute)
                IF info$ <> "" THEN
                    CLOSE #freen
                    getnodeinfo$ = info$
                    EXIT FUNCTION
                END IF
            LOOP UNTIL EOF(freen) = -1
        END IF
        CLOSE #freen
    END IF
END FUNCTION

SUB getnodelinkarray (array() AS STRING, target AS STRING)
    file$ = path$(target)
    IF _FILEEXISTS(file$) THEN
        freen = FREEFILE
        OPEN file$ FOR INPUT AS #freen
        IF EOF(freen) = 0 THEN
            DO
                INPUT #freen, nodeline$
                IF MID$(nodeline$, 1, 5) = "link:" THEN
                    addtostringarray array(), MID$(nodeline$, 6, LEN(nodeline$))
                END IF
            LOOP UNTIL EOF(freen) = -1
        END IF
        CLOSE #freen
    END IF
END SUB

FUNCTION switchword$ (word AS STRING, state AS _BYTE)
    DIM AS STRING state1, state2
    SELECT CASE LCASE$(word)
        CASE "on"
            state1 = "On"
            state2 = "Off"
        CASE "active"
            state1 = "Active"
            state2 = "Inactive"
    END SELECT
    IF state = -1 THEN
        switchword$ = state1
    ELSE
        switchword$ = state2
    END IF
END FUNCTION

FUNCTION getcurrentinputvalues$ (killbuffer AS _BYTE)
    DIM buffer AS STRING
    IF UBOUND(elements) > 0 THEN
        DO: e = e + 1
            IF elements(e).view = currentview AND elements(e).name <> "commandline" THEN
                IF elements(e).buffer <> "" THEN buffer = buffer + elements(e).name + "=" + elements(e).buffer + ";"
                IF killbuffer THEN elements(e).buffer = ""
                IF elements(e).url <> "" THEN buffer = buffer + "url=" + elements(e).url + ";"
            ELSEIF elements(e).view = currentview AND elements(e).name = "commandline" THEN
                buffer = buffer + elements(e).buffer
                IF killbuffer THEN elements(e).buffer = ""
            END IF
            IF elements(e).type = "input" AND killbuffer THEN
                elements(e).cursor = 0
                elements(e).sel_start = 0
                elements(e).sel_end = 0
            END IF
        LOOP UNTIL e = UBOUND(elements)
    END IF
    getcurrentinputvalues$ = buffer
END FUNCTION

FUNCTION getexpos$ (e AS INTEGER)
    IF (elements(e).x = "previousright" OR elements(e).x = "prevr" OR elements(e).x = "flex") AND e > 1 THEN
        getexpos$ = lst$(VAL(getexpos$(e - 1)) + VAL(getewidth$(e - 1)) + global.margin)
    ELSEIF (elements(e).x = "previousleft" OR elements(e).x = "prevl" OR elements(e).x = "-flex") AND e > 1 THEN
        getexpos$ = lst$(VAL(getexpos$(e - 1)) - VAL(getewidth$(e)) - global.margin)
    ELSEIF (elements(e).x = "previous" OR elements(e).x = "p" OR elements(e).x = "prev") AND e > 1 THEN
        getexpos$ = getexpos$(e - 1)
    ELSEIF (elements(e).x = "right" OR elements(e).x = "r") THEN
        getexpos$ = lst$(_WIDTH(0) - VAL(getewidth$(e)) - global.margin)
    ELSEIF (elements(e).x = "margin" OR elements(e).x = "m" OR elements(e).x = "left" OR elements(e).x = "l" OR elements(e).x = "0") THEN
        getexpos$ = lst$(global.margin)
    ELSE
        getexpos$ = elements(e).x
    END IF
END FUNCTION

FUNCTION geteypos$ (e AS INTEGER)
    IF (elements(e).y = "previousbottom" OR elements(e).y = "prevb" OR elements(e).y = "pb" OR elements(e).y = "flex") AND e > 1 THEN
        geteypos$ = lst$(VAL(geteypos$(e - 1)) + VAL(geteheight$(e - 1)) + global.margin)
    ELSEIF (elements(e).y = "previoustop" OR elements(e).y = "prevt" OR elements(e).y = "pt" OR elements(e).y = "-flex") AND e > 1 THEN
        geteypos$ = lst$(VAL(geteypos$(e - 1)) - VAL(geteheight$(e)) - global.margin)
    ELSEIF (elements(e).y = "previous" OR elements(e).y = "p" OR elements(e).y = "prev") AND e > 1 THEN
        geteypos$ = geteypos$(e - 1)
    ELSEIF (elements(e).y = "bottom" OR elements(e).y = "b") THEN
        geteypos$ = lst$(_HEIGHT(0) - VAL(geteheight$(e)) - global.margin)
    ELSEIF (elements(e).y = "margin" OR elements(e).y = "m" OR elements(e).y = "top" OR elements(e).y = "t" OR elements(e).y = "0") THEN
        geteypos$ = lst$(global.margin)
    ELSE
        geteypos$ = elements(e).y
    END IF
END FUNCTION

FUNCTION getewidth$ (e AS INTEGER)
    IF elements(e).w = "flex" OR elements(e).w = "f" THEN 'you would normally want this one for text-based elements
        getewidth$ = lst$(VAL(elements(e).w) + (_FONTWIDTH * (LEN(elements(e).text) + 2 + LEN(elements(e).buffer))) + (2 * global.padding))
    ELSEIF elements(e).w = "full" THEN
        getewidth$ = lst$(_WIDTH(0) - VAL(getexpos$(e)) - global.margin)
    ELSE
        getewidth$ = elements(e).w
    END IF
END FUNCTION

FUNCTION geteheight$ (e AS INTEGER)
    IF elements(e).type = "title" THEN geteheight$ = lst$(_FONTHEIGHT(font_big) + (2 * global.padding)): EXIT FUNCTION
    IF elements(e).h = "0" THEN
        geteheight$ = lst$(_FONTHEIGHT(font_normal) + (2 * global.padding))
    ELSEIF (elements(e).h = "nextt" OR elements(e).h = "next.top" OR elements(e).h = "nt") AND e < UBOUND(elements) THEN
        geteheight$ = lst$(VAL(geteypos$(e + 1)) - (2 * global.margin) - VAL(geteypos$(e)))
    ELSE
        geteheight$ = elements(e).h
    END IF
END FUNCTION

FUNCTION getepadding$ (e AS INTEGER)
    IF elements(e).padding = "" THEN
        getepadding$ = lst$(global.padding)
    ELSE
        getepadding$ = elements(e).padding
    END IF
END FUNCTION

SUB dothis (arguments AS STRING)
    DIM AS STRING nodeorigin, nodetype, nodetarget, linkname, license, url
    nodeorigin = getargument$(arguments, "nodeorigin")
    nodetype = getargument$(arguments, "nodetype")
    nodetarget = getargument$(arguments, "nodetarget")
    linkname = getargument$(arguments, "linkname")
    action$ = getargument$(arguments, "action")
    license = getargument$(arguments, "license")
    url = getargument$(arguments, "url")
    SELECT CASE action$
        CASE "add.node"
            IF set(nodetarget) AND set(nodetype) THEN
                nodecount = getnodecount(nodetarget)
                IF nodecount > 0 THEN 'adds a new node with the same name, but writes it into a separate file
                    nodecount = nodecount + 1
                    nodetarget = nodetarget + "_" + lst$(nodecount)
                END IF
                add.node "target=" + nodetarget + ";type=" + nodetype
                dothis "action=view.main"
            ELSE
                dothis "action=view.add.node"
            END IF
        CASE "add.nodelink"
            IF set(nodeorigin) AND set(linkname) AND set(nodetarget) THEN
                add.nodelink "origin=" + nodeorigin + ";name=" + linkname + ";target=" + nodetarget
                dothis "action=view.main"
            ELSE
                dothis "action=view.add.nodelink"
            END IF
        CASE "remove.node"
            IF set(nodetarget) THEN
                remove.node "target=" + nodetarget
                dothis "action=view.main"
            ELSE
                dothis "action=view.remove.node"
            END IF
        CASE "remove.nodelink"
            IF set(nodeorigin) AND set(linkname) AND set(nodetarget) THEN
                remove.nodelink "origin=" + nodeorigin + ";name=" + linkname + ";target=" + nodetarget
                dothis "action=view.main"
            ELSE
                dothis "action=view.remove.nodelink"
            END IF
        CASE "add.license"
            IF set(license) THEN
                add.license "license=" + license
                dothis "action=view.main"
            ELSE
                dothis "action=view.add.license"
            END IF
        CASE "web"
            openbrowser url
        CASE "check.license"
            loadconfig
        CASE "saveconfig"
            saveconfig
        CASE "quit"
            SYSTEM
    END SELECT
    SELECT CASE MID$(action$, 1, INSTR(action$, ".") - 1)
        CASE "view"
            currentview = MID$(action$, INSTR(action$, ".") + 1, LEN(action$))
    END SELECT
END SUB

SUB add.license (arguments AS STRING)
    DIM license AS STRING
    license = getargument$(arguments, "license")
    IF checkLicense(license) THEN
        global.license = _DEFLATE$(license)
        license = ""
        saveconfig
    END IF
END SUB

SUB add.node (arguments AS STRING)
    DIM this AS node
    this.name = getargument$(arguments, "target")
    this.type = getargument$(arguments, "type")
    this.date = DATE$
    this.time = TIME$
    IF set(this.name) AND set(this.type) THEN
        writenode "", this
        getmaxnodeid
    END IF
END SUB

SUB writenode (arguments AS STRING, this AS node)
    file$ = path$(this.name)
    IF _FILEEXISTS(file$) THEN
        DO: nodecount = nodecount + 1
            file$ = path$(this.name + "_" + lst$(nodecount))
        LOOP UNTIL _FILEEXISTS(file$) = 0
    END IF
    filen = FREEFILE
    OPEN file$ FOR OUTPUT AS #filen
    WRITE #filen, "date=" + this.date
    WRITE #filen, "time=" + this.time
    WRITE #filen, "name=" + this.name
    WRITE #filen, "type=" + this.type
    CLOSE #filen
END SUB

SUB remove.node (arguments AS STRING)

    file$ = path$(getargument$(arguments, "target"))
    IF _FILEEXISTS(file$) THEN
        KILL file$
    END IF
END SUB

SUB remove.nodelink (arguments AS STRING)
    'TODO
END SUB

SUB getnodearray (array() AS STRING, search AS STRING)
    DIM searchn AS STRING
    file$ = path$(search)
    IF _FILEEXISTS(file$) THEN
        REDIM _PRESERVE array(UBOUND(array) + 1) AS STRING: array(UBOUND(array)) = search
        DO: nodecount = nodecount + 1
            searchn = search + "_" + lst$(nodecount)
            file$ = path$(searchn)
            IF _FILEEXISTS(file$) THEN REDIM _PRESERVE array(UBOUND(array) + 1) AS STRING: array(UBOUND(array)) = searchn
        LOOP UNTIL _FILEEXISTS(file$) = 0
    END IF
END SUB

FUNCTION getnodecount (arguments AS STRING) 'counts the amount of nodes with a given name <target>
    DIM AS STRING nodetarget
    DIM AS INTEGER buffer
    nodetarget = getargument$(arguments, "target")
    file$ = path$(nodetarget)
    IF _FILEEXISTS(file$) THEN
        buffer = buffer + 1
        DO: nodecount = nodecount + 1
            file$ = path$(nodetarget + "_" + lst$(nodecount))
            IF _FILEEXISTS(file$) THEN buffer = buffer + 1
        LOOP UNTIL _FILEEXISTS(file$) = 0
        getnodecount = buffer
        EXIT FUNCTION
    ELSE
        getnodecount = 0
        EXIT FUNCTION
    END IF
END FUNCTION

SUB add.nodelink (arguments AS STRING) 'adds a new node link, but doesn't check if it exists already!
    DIM this AS nodelink
    this.target = getargument$(arguments, "target")
    this.name = getargument$(arguments, "name")
    this.origin = getargument$(arguments, "origin")
    this.date = DATE$
    this.time = TIME$
    IF set(this.target) AND set(this.name) AND set(this.origin) THEN
        writenodelink "", this
        getmaxnodeid
    END IF
END SUB

SUB writenodelink (arguments AS STRING, this AS nodelink) 'writes a nodelink to a given node
    file$ = path$(this.origin)
    filen = FREEFILE
    OPEN file$ FOR OUTPUT AS #filen
    WRITE #filen, "link:date=" + this.date + ";time=" + this.time + ";origin=" + this.origin + ";name=" + this.name + ";target=" + this.target
    CLOSE #filen
END SUB

SUB loadconfig
    configfile$ = global.internalpath + "\config.dst"
    prooffile configfile$, -1
    freen = FREEFILE
    OPEN configfile$ FOR INPUT AS #freen
    IF EOF(freen) = 0 THEN
        DO
            INPUT #freen, configline$
            config$ = config$ + configline$
        LOOP UNTIL EOF(freen) = -1
    END IF
    CLOSE #freen

    global.padding = getargumentv(config$, "padding")
    global.margin = getargumentv(config$, "margin")
    global.round = getargumentv(config$, "round")
    global.stroke = getargumentv(config$, "stroke")
    global.license = getargument$(config$, "license")
    IF checkLicense(_INFLATE$(global.license)) = 0 THEN setlicense "", 0: saveconfig
    IF global.license <> "" THEN global.licensestatus = -1
END SUB

SUB saveconfig
    config$ = "round=" + lst$(global.round) + ";margin=" + lst$(global.margin) + ";padding=" + lst$(global.padding) + ";stroke=" + lst$(global.stroke) + ";license=" + global.license
    config$ = config$
    configfile$ = global.internalpath + "\config.dst"
    freen = FREEFILE
    OPEN configfile$ FOR OUTPUT AS #freen
    PRINT #freen, config$
    CLOSE #freen
END SUB

SUB loadfonts
    'fontr$ = "C:\Windows\Fonts\consola.ttf"
    fontr$ = "internal\fonts\PTMono-Regular.ttf" 'replace with file loaded from config file
    fonteb$ = "internal\fonts\OpenSans-ExtraBold.ttf"
    font_normal = _LOADFONT(fontr$, 16, "MONOSPACE")
    font_big = _LOADFONT(fonteb$, 48)
    _FONT font_normal
END SUB

SUB loadui
    REDIM _PRESERVE viewname(0) AS STRING

    loadfonts

    freen = FREEFILE
    viewfile$ = global.internalpath + "\views.dui"
    prooffile viewfile$, -1
    OPEN viewfile$ FOR INPUT AS #freen
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

                        REDIM _PRESERVE elements(UBOUND(elements) + 1) AS element
                        eub = UBOUND(elements)
                        elements(eub).view = viewname(lview)
                        elements(eub).type = getargument$(uielement$, "type")
                        elements(eub).allownumbers = getargumentv(uielement$, "allownumbers")
                        elements(eub).allowtext = getargumentv(uielement$, "allowtext")
                        elements(eub).allowspecial = getargumentv(uielement$, "allowspecial")
                        elements(eub).name = getargument$(uielement$, "name")
                        elements(eub).x = getargument$(uielement$, "x")
                        elements(eub).y = getargument$(uielement$, "y")
                        elements(eub).w = getargument$(uielement$, "w")
                        elements(eub).h = getargument$(uielement$, "h")
                        elements(eub).color = getargument$(uielement$, "color")
                        elements(eub).hovercolor = getargument$(uielement$, "hovercolor")
                        elements(eub).style = getargument$(uielement$, "style")
                        elements(eub).text = getargument$(uielement$, "text")
                        elements(eub).action = getargument$(uielement$, "action")
                        elements(eub).angle = getargument$(uielement$, "angle")
                        elements(eub).buffer = getargument$(uielement$, "buffer")
                        elements(eub).round = getargument$(uielement$, "round")
                        elements(eub).hovertext = getargument$(uielement$, "hovertext")
                        elements(eub).hovertextwait = getargumentv(uielement$, "hovertextwait")
                        elements(eub).padding = getargument$(uielement$, "padding")
                        elements(eub).url = getargument$(uielement$, "url")
                        elements(eub).selected = 0

                        IF elements(eub).type = "input" AND activeelement = 0 THEN
                            activeelement = eub
                        END IF

                        IF elements(eub).name = "license" THEN
                            elements(eub).buffer = global.license
                        END IF
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
    internal.setting.fps = 60

    'data structure
    global.internalpath = "internal"
    global.nodepath = "nodes"
    proofpath global.internalpath
    proofpath global.nodepath
    getmaxnodeid
    loadconfig
END SUB

SUB proofpath (pathtoproof AS STRING) 'creates a folder if it doesn't exist
    IF _DIREXISTS(pathtoproof) = 0 THEN
        MKDIR pathtoproof
    END IF
END SUB

SUB prooffile (filetoproof AS STRING, giveerror AS _BYTE) 'creates a file if it doesn't exist
    IF _FILEEXISTS(filetoproof) = 0 THEN
        IF giveerror THEN PRINT "The following file could not be found, the program might not work as intended: " + filetoproof: _DELAY 2
        freen = FREEFILE
        OPEN filetoproof FOR OUTPUT AS #freen
        CLOSE #freen
    END IF
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
    REDIM directories(0) AS STRING
    REDIM nodefiles(0) AS STRING
    GetFileList global.nodepath, directories(), nodefiles()
    IF UBOUND(nodefiles) > 0 THEN
        DO: f = f + 1
            checkval = VAL(MID$(nodefiles(f), 1, INSTR(nodefiles(f), ".")))
            IF checkval > global.maxnodeid THEN global.maxnodeid = checkval
        LOOP UNTIL f = UBOUND(nodefiles)
    END IF
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
            rectangleoutline x, y, w, h, round, rotation, clr, _PI / 20
            PAINT (x + (w / 2), y + (h / 2)), clr, clr
        CASE "B"
            rectangleoutline x, y, w, h, round, rotation, clr, 0
    END SELECT
END SUB

SUB rectangleoutline (x, y, w, h, round, rotation, clr AS LONG, bfadjust)
    IF w < h THEN mininmum = w ELSE mininmum = h
    IF round > mininmum / 2 THEN round = mininmum / 2
    distance = SQR((w ^ 2) + (h ^ 2)) / 2 'distance to center point
    rotation = rotation - (_PI / 2)
    rounddistance = distance - SQR(round ^ 2 + round ^ 2)
    cx = x + (w / 2)
    cy = y + (h / 2)
    detail = _PI * round 'how many pixels are calculated for one rounded corner
    angle1 = ATN((h / 2) / (w / 2))
    angle2 = ((_PI) - (2 * angle1)) / 2
    rotation = rotation - angle1
    DO
        corner = corner + 1
        IF corner MOD 2 = 0 THEN 'alternates between the two different possible angles within a rectangle
            cangle = angle2 * 2
            offset = -_PI / 4
        ELSE
            cangle = angle1 * 2
            offset = _PI / 4
        END IF

        'rcf = round corner factor, adds the angle progressively together to go "around" the rectangle based off the middle
        rcf = rotation + anglebase
        rcfp1 = rotation + anglebase + cangle

        px = cx + (rounddistance * SIN(rcf))
        py = cy - (rounddistance * COS(rcf))

        px1 = cx + (rounddistance * SIN(rcfp1))
        py1 = cy - (rounddistance * COS(rcfp1))

        'start is left end of rounding, end is right end of rounding
        startangle = -(_PI / 2) + ((cangle / 2))
        endangle = ((cangle / 2))

        'uses endangle of current corner to connect to startangle of next
        LINE (px + (SIN(endangle + rcf) * round), py - (COS(endangle + rcf) * round))-(px1 + (SIN(startangle + rcfp1 + offset) * round), py1 - (COS(startangle + rcfp1 + offset) * round)), clr

        'draws the curves on the corners pixel by pixel
        angle = startangle + rcf - bfadjust
        DO: angle = angle + ((0.5 * _PI) / detail)
            PSET (px + (SIN(angle) * round), py - (COS(angle) * round)), clr
        LOOP UNTIL angle >= startangle + rcf + (_PI / 2) + bfadjust

        anglebase = anglebase + cangle
    LOOP UNTIL corner = 4
END SUB

FUNCTION lst$ (number AS _FLOAT)
    lst$ = LTRIM$(STR$(number))
END FUNCTION

FUNCTION checkLicense (license$)
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
    KILL "license.txt"
    IF success$ = "true" AND productname$ = "Datanet" AND permalink$ = "XXun" AND licensekey$ = license$ AND endedat$ = "" AND failedat$ = "" THEN
        checkLicense = -1
    ELSE
        checkLicense = 0
    END IF
END FUNCTION

SUB openbrowser (url AS STRING)
    SHELL _HIDE "rundll32 url.dll,FileProtocolHandler " + url
END SUB

SUB setlicense (license AS STRING, status AS _BYTE)
    global.license = license
    global.licensestatus = status
END SUB

FUNCTION col& (colour AS STRING)
    SELECT CASE colour
        CASE "t"
            col& = _RGBA(0, 0, 0, 0)
        CASE "ui"
            col& = _RGBA(20, 20, 20, 255)
        CASE "ui2"
            col& = _RGBA(150, 150, 150, 255)
        CASE "selected"
            col& = _RGBA(200, 200, 220, 255)
        CASE "bg1"
            col& = _RGBA(230, 230, 230, 255)
        CASE "bg2"
            col& = _RGBA(160, 160, 160, 255)
        CASE "bg3"
            col& = _RGBA(220, 220, 220, 255)
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
