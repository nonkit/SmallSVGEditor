Module SmallSVGEditorModule
    Dim title, debug, gw, gh, __Not, WQ, qt, CR, LF, LT, UPPER, LOWER, DIGIT, LCHAR, TCHAR, shape, nShapes, cont, param, clicked, __in, out, c, obj, icut, icopy, ipaste, selectedshape, i, __select, shp, _x, arrow_dx, _y, arrow_dy, x, y, a, cx, cy, len, width, height, angle, mxM, myM, mxD, myD, _i, nPinch, pnch, pinch, r, nMenu, mnu, menu, func, selecteditem, mode, repeat, clipboard, index, iMin, iMax, scale, shX, shY, resize, w, h, bcolor, pwidth, pcolor, xt, xmin, ymin, x1, x2, y1, y2, pen, nPen, pw, size, color, grid, colorGrid, stepGrid, sizeMenu, fromMenu, cxMenu, cyMenu, xMenu, yMenu, imenubar, margin, url, itemname, msWait, t, x3, y3, oItem, dx, dy, _iMax, _mxD, _myD, _mxM, _myM, p, match, mxU, myU, xmax, ymax, sizePinch, shWidth, shHeight, xPinch, yPinch, shAngle, moved, released, oFrame, _x1, _y1, _x2, _y2, _x3, _y3, sp, buf, rx, ry, sR, sColor, sG, sB, sHex, iR, iDec, iG, iB, rLightness, rN2, rSaturation, rN1, rH, rHue, iValue, rR, rG, rB, rMax, rMin, UNDEFINED, rRC, rGC, rBC, rV, nPalette, pltt, palette, tPalette, maxPalette, iSlider, iHue, iLightness, iSaturation, level, iRed, iGreen, iBlue, red, green, blue, iPalette, BORDERCOLOR, oRect, min, max, left, top, TOPY, caption, DELTAY, LEFTX, CAPTIONCOLOR, oNewColor, numSlider, slider, oPalette, sldr, colorInit, POPUPCOLOR, oPopup, oOK, oCancel, oCaption, oRectCurrent, oColor, cancel, TEXTCOLOR, oFilename, OPACITY, oText, fifok, oMsg, subname, typed, filename, fifos, fifoc, _shift, _ctrl, keys, shift, ctrl, key, iLen, iPtr, cmr, _p, handler, pc, bc, attr, dmu, smrc, pSave, tag, findNext, pTag, lTag, pAttr, lAttr, pEnd, pEq, pQ, txt, pL, pR, pKw, pStyle, kw, value, pValue, pColon, isSpace, __char, BOXCOLOR, SLITCOLOR, ox, oy, alpha, silverlight, s, fs, _cx, _cy, mag, x0, x0_, x1_, y0 As Primitive
    Sub Main()
        ' Small SVG Editor
        ' Copyright © 2012-2014 Nonki Takahashi.  The MIT License.
        ' Version 1.96b
        ' Repository https://git01.codeplex.com/smallsvgeditor
        ' Last update 2014-12-01
        ' Program ID TLW744-6
        '
        ' History:
        '  - Created from Shapes 1.7b.
        '  - Changed Open/Save/Color popup design.
        '  - Supported SVG for Open/Save.
        '
        ' TODO:
        '  [✔] Support grouping tag (element), defs tag and use tag
        '  [ ] Support text tag (element)
        '  [✔] Change file format from .sb (.smallbasic) to .svg in ReadShapes
        '  [ ] Check illegal behavior in Silverlight environment
        '  [ ] Refine to avoid Publish error (too large)
        '  [ ] Sort subroutines
        '  [ ] Bug fix for Silverlight:  Lines come different place
        '  [ ] Bug fix for Silverlight:  Lines move after click
        '  [ ] Bug fix for Silverlight:  Rotated triangles move after click
        '
        title = "Small SVG Editor 1.96b"
        GraphicsWindow.Title = title
        debug = false
        SB_Workaround()
        gw = 598
        gh = 428
        GraphicsWindow.Width = gw
        GraphicsWindow.Height = gh
        __Not = "False=True;True=False;"
        WQ = Text.GetCharacter(34)
        qt = WQ
        CR = Text.GetCharacter(13)
        LF = Text.GetCharacter(10)
        LT = Text.GetCharacter(60)
        UPPER = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        LOWER = "abcdefghijkomnopqrstuvwxyz"
        DIGIT = "0123456789"
        LCHAR = UPPER + LOWER + "_"
        TCHAR = LCHAR + DIGIT
        Popup_Init()
        CS_InitPalette() ' initialize palette for color slider
        DrawMenu()
        shape = ""
        nShapes = 0
        cont = true ' continue
        Mouse_Init()
        KB_Init()
        param = "down=True;move=False;up=True;" ' wait to click
        Mouse_SetHandler()
        While cont
            If clicked Then ' mouse clicked
                DetectClickedObject()
                DoObject()
                param = "down=True;move=False;up=True;"
                Mouse_SetHandler()
            ElseIf __in > out Then
                KB_InKey()
                If c = CType("^x", Primitive) Then
                    obj = "menu" + icut
                    DoMenu()
                ElseIf c = CType("^c", Primitive) Then
                    obj = "menu" + icopy
                    DoMenu()
                ElseIf c = CType("^v", Primitive) Then
                    obj = "menu" + ipaste
                    DoMenu()
                ElseIf c = CType("DEL", Primitive) Then
                    If selectedshape <> CType("", Primitive) Then
                        DeleteSelectedShape()
                        i = selectedshape
                        __select = false
                        ShapeSelect() ' removes pinches
                    End If
                ElseIf (c = CType("LEFT", Primitive)) Or (c = CType("RIGHT", Primitive)) Or (c = CType("UP", Primitive)) Or (c = CType("DOWN", Primitive)) Then
                    If selectedshape <> CType("", Primitive) Then
                        i = selectedshape
                        shp = shape(i)
                        _x = shp("x") + arrow_dx(c)
                        _y = shp("y") + arrow_dy(c)
                        MoveShape()
                    End If
                End If
                param = "down=True;move=False;up=True;"
                Mouse_SetHandler()
            Else
                Program.Delay(100)
            End If
        End While
        ' end of program
    End Sub
    Sub CalcDetectBorder()
        ' param i - index of shapes
        shp = shape(i)
        If shp("func") = CType("line", Primitive) Then ' line
            x = shp("x2") - shp("x1")
            y = shp("y2") - shp("y1")
            Math_CartesianToPolar()
            If 180 <= a Then
                a = a - 180
            End If
            shp("angle") = a
            cx = shp("x") + (Microsoft.SmallBasic.Library.Math.Abs(x) / 2)
            cy = shp("y") + (Microsoft.SmallBasic.Library.Math.Abs(y) / 2)
            len = Microsoft.SmallBasic.Library.Math.SquareRoot((x * x) + (y * y))
            shp("_x0") = Microsoft.SmallBasic.Library.Math.Floor(cx - (len / 2))
            shp("_x1") = Microsoft.SmallBasic.Library.Math.Floor(cx + (len / 2))
            shp("_y0") = cy - 4
            shp("_y1") = cy + 4
        Else
            If shp("func") = CType("tri", Primitive) Then ' triangle
                shp("width") = shp("x3")
                shp("height") = shp("y2")
            End If
            shp("_x0") = shp("x")
            shp("_y0") = shp("y")
            shp("_x1") = shp("x") + shp("width")
            shp("_y1") = shp("y") + shp("height")
        End If
        shape(i) = shp
    End Sub
    Sub CalcPinchPos()
        ' param i - shape index
        ' return mxM, myM - center of pinch
        shp = shape(i)
        _x = shp("x")
        _y = shp("y")
        width = shp("width")
        height = shp("height")
        angle = shp("angle")
        param = "width=0;height=0;angle=" + angle + ";"
        param("cx") = _x + (width / 2)
        param("cy") = _y + (height / 2)
        Stack.PushValue("local", x)
        Stack.PushValue("local", y)
        Stack.PushValue("local", _x)
        Stack.PushValue("local", _y)
        param("x") = param("cx")
        param("y") = param("cy") - 10
        Shapes_CalcRotatePos()
        mxM = x
        myM = y
        _y = Stack.PopValue("local")
        _x = Stack.PopValue("local")
        y = Stack.PopValue("local")
        x = Stack.PopValue("local")
    End Sub
    Sub CalcVertexes()
        ' param i - pinch index
        ' param angle - angle of a shape
        ' param selectedshape - parent shape index of pinch
        ' param shape[] - shape data
        ' return mxM, myM - free vertex
        ' return mxD, myD - fixed vertex
        shp = shape(selectedshape)
        If shp("func") = CType("line", Primitive) Then
            If i = 1 Then
                mxM = shp("x") + shp("x1")
                myM = shp("y") + shp("y1")
                mxD = shp("x") + shp("x2")
                myD = shp("y") + shp("y2")
            ElseIf i = 2 Then
                mxM = shp("x") + shp("x2")
                myM = shp("y") + shp("y2")
                mxD = shp("x") + shp("x1")
                myD = shp("y") + shp("y1")
            End If
        Else
            mxM = shp("x")
            myM = shp("y")
            mxD = mxM
            myD = myM
            If (i = 1) Or (i = 3) Then
                mxD = mxD + shp("width")
            End If
            If (i = 1) Or (i = 2) Then
                myD = myD + shp("height")
            End If
            If (i = 2) Or (i = 4) Then
                mxM = mxM + shp("width")
            End If
            If (i = 3) Or (i = 4) Then
                myM = myM + shp("height")
            End If
            param = "x=" + mxD + ";y=" + myD + ";width=0;height=0;angle=" + angle
            param = param + ";cx=" + ((mxD + mxM) / 2) + ";cy=" + ((myD + myM) / 2) + ";"
            Shapes_CalcRotatePos()
            mxD = x
            myD = y
            param("x") = mxM
            param("y") = myM
            Shapes_CalcRotatePos()
            mxM = x
            myM = y
        End If
    End Sub
    Sub DeleteSelectedShape()
        ' param selectedshape
        shp = shape(selectedshape)
        Shapes.Remove(shp("obj"))
        nShapes = nShapes - 1
        For SmallSVGEditorModule._i = selectedshape To nShapes
            shape(_i) = shape(_i + 1)
        Next
        shape(nShapes + 1) = ""
        selectedshape = ""
    End Sub
    Sub DetectClickedObject()
        ' mxD, myD - clicked position
        ' return obj - name of object (menu, shape or pinch)
        Stack.PushValue("local", i)
        obj = ""
        For SmallSVGEditorModule.i = 1 To nPinch
            pnch = pinch(i)
            If (pnch("_x0") <= mxD) And (mxD <= pnch("_x1")) And (pnch("_y0") <= myD) And (myD <= pnch("_y1")) Then
                obj = "pinch" + i
                GoTo dco_exit
            End If
        Next
        For SmallSVGEditorModule.i = nShapes To 1 Step -1
            shp = shape(i)
            param = "x=" + mxD + ";y=" + myD + ";width=0;height=0;"
            param("cx") = (shp("_x0") + shp("_x1")) / 2
            param("cy") = (shp("_y0") + shp("_y1")) / 2
            param("angle") = -shp("angle")
            Shapes_CalcRotatePos()
            If (shp("_x0") <= x) And (x <= shp("_x1")) And (shp("_y0") <= y) And (y <= shp("_y1")) Then
                If (shp("func") = CType("rect", Primitive)) Or (shp("func") = CType("line", Primitive)) Then
                    obj = "shape" + i
                    GoTo dco_exit
                ElseIf shp("func") = CType("ell", Primitive) Then
                    x = (x - param("cx")) / shp("width") * 2
                    y = (y - param("cy")) / shp("height") * 2
                    r = Microsoft.SmallBasic.Library.Math.SquareRoot((x * x) + (y * y))
                    If r <= 1 Then
                        obj = "shape" + i
                        GoTo dco_exit
                    End If
                ElseIf shp("func") = CType("tri", Primitive) Then
                    x = (x - param("cx")) / shp("width") * 2
                    y = (y - shp("_y1")) / shp("height")
                    r = Microsoft.SmallBasic.Library.Math.Abs(x) + Microsoft.SmallBasic.Library.Math.Abs(y)
                    If (r <= 1) And (y <= 0) Then
                        obj = "shape" + i
                        GoTo dco_exit
                    End If
                End If
            End If
        Next
        For SmallSVGEditorModule.i = 1 To nMenu
            mnu = menu(i)
            If (mnu("func") <> CType("", Primitive)) And (mnu("_x0") <= mxD) And (mxD <= mnu("_x1")) And (mnu("_y0") <= myD) And (myD <= mnu("_y1")) Then
                obj = "menu" + i
                GoTo dco_exit
            End If
        Next
dco_exit:
        If (obj = CType("", Primitive)) And (selectedshape <> CType("", Primitive)) Then
            i = selectedshape
            __select = false
            ShapeSelect()
        End If
        i = Stack.PopValue("local")
    End Sub
    Sub DoMenu() ' if a menu item clicked do the funciton
        If Text.StartsWith(obj, "menu") Then
            param = "down=False;move=False;up=False;" ' wait button pushed
            Mouse_SetHandler()
            i = Text.GetSubTextToEnd(obj, 5)
            obj = ""
            mnu = menu(i)
            func = mnu("func")
            __select = true
            ItemSelect() ' shows menu item frame
            If (selecteditem = i) And Text.IsSubText("rect|ell|tri|line", func) Then
                If mode = CType("repeat", Primitive) Then
                    mode = "single"
                    Shapes.HideShape(repeat(func))
                Else
                    mode = "repeat"
                    Shapes.ShowShape(repeat(func))
                End If
            Else
                mode = "single"
            End If
            selecteditem = i
            If Text.IsSubText("open|save|cut|paste|rect|ell|tri|line", func) And (selectedshape <> CType("", Primitive)) Then
                i = selectedshape
                __select = false
                ShapeSelect() ' removes pinches if a shape selected
                If func = CType("cut", Primitive) Then
                    selectedshape = i
                End If
            End If
            If func = CType("open", Primitive) Then ' open shapes from file
                ReadShapes()
            ElseIf func = CType("save", Primitive) Then
                WriteShapes()
            ElseIf func = CType("cut", Primitive) Then
                If selectedshape <> CType("", Primitive) Then
                    clipboard = shape(selectedshape)
                    DeleteSelectedShape()
                End If
            ElseIf func = CType("copy", Primitive) Then
                If selectedshape <> CType("", Primitive) Then
                    clipboard = shape(selectedshape)
                    index = "1=x;2=y;3=_x0;4=_x1;5=_y0;6=_y1;"
                    For SmallSVGEditorModule._i = 1 To 6
                        clipboard(index(_i)) = clipboard(index(_i)) + 10
                    Next
                End If
            ElseIf func = CType("paste", Primitive) Then
                If clipboard <> CType("", Primitive) Then
                    nShapes = nShapes + 1
                    shape(nShapes) = clipboard
                    index = "1=x;2=y;3=_x0;4=_x1;5=_y0;6=_y1;"
                    For SmallSVGEditorModule._i = 1 To 6
                        clipboard(index(_i)) = clipboard(index(_i)) + 10
                    Next
                    iMin = nShapes
                    iMax = nShapes
                    scale = 1
                    shX = 0
                    shY = 0
                    Shapes_Add()
                    i = nShapes
                    __select = true
                    ShapeSelect()
                End If
            ElseIf (func = CType("rect", Primitive)) Or (func = CType("ell", Primitive)) Or (func = CType("tri", Primitive)) Then
                ' rectangle, ellipse or triangle
                While true
                    WaitToClick() ' to get mxD, myD
                    DetectClickedObject()
                    If Text.StartsWith(obj, "menu") Then
                        Goto dm_exit ' cancel to register shape
                    End If
                    obj = ""
                    mxM = mxD
                    myM = myD
                    angle = 0
                    WaitToReleaseS() ' to get mxU, myU
                    resize = false
                    nShapes = nShapes + 1
                    i = nShapes ' to set shape[nShapes]
                    RegisterShapeData()
                    If (w = 0) And (h = 0) Then
                        shape(i) = ""
                        nShapes = nShapes - 1
                        Goto dm_exit ' cansel to register zero sized shape
                    End If
                    GraphicsWindow.BrushColor = bcolor
                    GraphicsWindow.PenWidth = pwidth
                    If pwidth > 0 Then
                        GraphicsWindow.PenColor = pcolor
                    End If
                    shp = shape(nShapes)
                    If func = CType("rect", Primitive) Then
                        shp("obj") = Shapes.AddRectangle(w, h)
                    ElseIf func = CType("ell", Primitive) Then
                        shp("obj") = Shapes.AddEllipse(w, h)
                    ElseIf func = CType("tri", Primitive) Then
                        shp("obj") = Shapes.AddTriangle(xt, 0, 0, h, w, h)
                    End If
                    Shapes.Move(shp("obj"), xmin, ymin)
                    shape(nShapes) = shp
                    If mode = CType("single", Primitive) Then
                        Goto dm_exit
                    End If
                End While
            ElseIf func = CType("line", Primitive) Then
                While true
                    WaitToClick() ' to get mxD, myD
                    DetectClickedObject()
                    If Text.StartsWith(obj, "menu") Then
                        Goto dm_exit ' cancel to register line
                    End If
                    obj = ""
                    mxM = mxD
                    myM = myD
                    WaitToReleaseS() ' to get mxU, myU
                    nShapes = nShapes + 1
                    i = nShapes ' to set shape[nShapes]
                    RegisterShapeData()
                    If (x1 = x2) And (y1 = y2) Then
                        shape(i) = ""
                        nShapes = nShapes - 1
                        Goto dm_exit ' cansel to register zero sized line
                    End If
                    GraphicsWindow.BrushColor = bcolor
                    GraphicsWindow.PenWidth = pwidth
                    If pwidth > 0 Then
                        GraphicsWindow.PenColor = pcolor
                    End If
                    shp = shape(nShapes)
                    shp("obj") = Shapes.AddLine(x1, y1, x2, y2)
                    Shapes.Move(shp("obj"), xmin, ymin)
                    shape(nShapes) = shp
                    If mode = CType("single", Primitive) Then
                        Goto dm_exit
                    End If
                End While
            ElseIf func = CType("pw", Primitive) Then
                If pen = nPen Then
                    pen = 1
                Else
                    pen = pen + 1
                End If
                pwidth = pw(pen)
                mnu = menu(i)
                x = mnu("_x0")
                y = mnu("_y0")
                size = mnu("_x1") - x
                GraphicsWindow.BrushColor = "#EEEEEE"
                GraphicsWindow.FillRectangle(x, y, size, size)
                DrawMenuItem()
            ElseIf func = CType("pc", Primitive) Then
                color = pcolor
                CS_ShowPopup()
                pcolor = color
                DrawMenuItem()
            ElseIf func = CType("bc", Primitive) Then
                color = bcolor
                CS_ShowPopup()
                bcolor = color
                DrawMenuItem()
            ElseIf func = CType("gr", Primitive) Then
                grid = __Not(grid)
                If grid Then
                    colorGrid = "#EEEEEE"
                    stepGrid = 10
                    DrawGrid()
                    colorGrid = "#AAAAAA"
                    stepGrid = 50
                    DrawGrid()
                    colorGrid = "#666666"
                    stepGrid = 100
                    DrawGrid()
                Else
                    GraphicsWindow.BrushColor = "White"
                    GraphicsWindow.FillRectangle(0, sizeMenu + 20, gw, gh - sizeMenu - 20)
                End If
            ElseIf (func = CType("menubar", Primitive)) And (selectedshape <> CType("", Primitive)) Then
                i = selectedshape
                __select = false
                ShapeSelect()
            End If
            If (selectedshape <> CType("", Primitive)) And ((func = CType("pw", Primitive)) Or (func = CType("pc", Primitive)) Or (func = CType("bc", Primitive))) Then
                i = selectedshape
                __select = false
                ShapeSelect()
                shp = shape(i)
                If func = CType("pw", Primitive) Then
                    shp("pw") = pwidth
                ElseIf func = CType("pc", Primitive) Then
                    shp("pc") = pcolor
                ElseIf func = CType("bc", Primitive) Then
                    shp("bc") = color
                End If
                shape(i) = shp
                iMin = i ' to re-size Shapes
                iMax = nShapes ' to keep z-order of Shapes
                Shapes_Remove()
                scale = 1
                shX = 0
                shY = 0
                Shapes_Add()
                __select = true
                ShapeSelect()
            End If
dm_exit:
            i = selecteditem
            __select = false
            ItemSelect() ' removes menu item frame
            If Text.IsSubText("rect|ell|tri|line", func) Then
                Shapes.HideShape(repeat(func))
            End If
        End If
    End Sub
    Sub DoObject()
        ' param obj - clicked object
        While obj <> CType("", Primitive)
            fromMenu = false
            DoMenu()
            If obj <> CType("", Primitive) Then
                DoShape()
            End If
            If obj <> CType("", Primitive) Then
                DoPinch()
            End If
        End While
    End Sub
    Sub DoPinch() ' if a pinch clicked then rotate or re-size a shape
        ' param obj - clicked object
        ' param selectedshape - parent shape index of the pinch
        If Text.StartsWith(obj, "pinch") Then
            i = Text.GetSubTextToEnd(obj, 6)
            obj = ""
            If i = 5 Then ' rotate a shape
                i = selectedshape
                WaitToReleaseR() ' to get angle
                shp = shape(i)
                Shapes.Rotate(shp("obj"), angle)
                shp("angle") = Microsoft.SmallBasic.Library.Math.Floor(angle)
                shape(i) = shp
                __select = false
                ShapeSelect() ' remove pinches
                __select = true
                ShapeSelect() ' show pinches
            Else
                shp = shape(selectedshape)
                angle = shp("angle")
                CalcVertexes() ' to get mxM, myM, mxD, myD
                func = shp("func")
                WaitToReleaseS() ' to get mxU, myU
                i = selectedshape
                __select = false
                ShapeSelect() ' remove pinches
                ' selectedshape is broken in ShapeSelect() so use i instead
                RegisterShapeMetrics() ' re-size shape[i]
                iMin = i ' to re-size Shapes
                iMax = nShapes ' to keep z-order of Shapes
                Shapes_Remove()
                scale = 1
                shX = 0
                shY = 0
                Shapes_Add()
                __select = true
                ShapeSelect() ' show pinches
            End If
        End If
    End Sub
    Sub DoShape() ' if a shape clicked then move it
        If Text.StartsWith(obj, "shape") Then
            If selectedshape <> CType("", Primitive) Then ' if other shape selected
                i = selectedshape
                __select = false
                ShapeSelect() ' removes pinches
            End If
            i = Text.GetSubTextToEnd(obj, 6) ' shape index
            __select = true
            ShapeSelect() ' shows pinches
            Mouse_SetHandler()
            WaitToReleaseM() ' for moving a shape
            clicked = false
            obj = ""
        End If
    End Sub
    Sub DrawGrid()
        ' param colorGrid - color for grid
        ' param stepGrid - step for grid
        GraphicsWindow.PenWidth = 1
        GraphicsWindow.PenColor = colorGrid
        For SmallSVGEditorModule.x = 0 To gw - 1 Step stepGrid
            GraphicsWindow.DrawLine(x, sizeMenu + 20, x, gh)
        Next
        For SmallSVGEditorModule.y = sizeMenu + 20 To gh - 1 Step stepGrid
            GraphicsWindow.DrawLine(0, y, gw, y)
        Next
    End Sub
    Sub DrawMenu()
        ' return menu[] - array of menu data
        pwidth = GraphicsWindow.PenWidth
        cxMenu = 6
        cyMenu = 6
        sizeMenu = 40
        nMenu = 13
        grid = false
        GraphicsWindow.BrushColor = "#EEEEEE"
        GraphicsWindow.FillRectangle(0, 0, gw, 20 + sizeMenu)
        pw = "1=2;2=4;3=8;4=16;5=0;6=1;" ' pen width
        pen = 1 ' pen width index
        nPen = 6 ' number of pen width
        For SmallSVGEditorModule.i = 1 To nMenu
            xMenu = cxMenu + (Microsoft.SmallBasic.Library.Math.Floor((i - 1) / 1) * (sizeMenu + 4))
            yMenu = cyMenu + (Microsoft.SmallBasic.Library.Math.Remainder(i - 1, 1) * (sizeMenu + 14))
            GraphicsWindow.BrushColor = "#EEEEEE"
            GraphicsWindow.FillRectangle(xMenu, yMenu, sizeMenu, sizeMenu)
            mnu = menu(i)
            mnu("_x0") = xMenu
            mnu("_y0") = yMenu
            mnu("_x1") = xMenu + sizeMenu
            mnu("_y1") = yMenu + sizeMenu
            menu(i) = mnu
            DrawMenuItem()
        Next
        nMenu = nMenu + 1
        imenubar = nMenu
        mnu = menu(i)
        mnu("_x0") = 0
        mnu("_y0") = 0
        mnu("_x1") = gw
        mnu("_y1") = 20 + sizeMenu
        mnu("func") = "menubar"
        menu(i) = mnu
    End Sub
    Sub DrawMenuItem()
        ' param i - item number
        ' param pwidth - pen width
        ' param pcolor - pen color
        ' param bcolor - brush color
        margin = 4
        mnu = menu(i)
        x = mnu("_x0")
        y = mnu("_y0")
        size = mnu("_x1") - x
        GraphicsWindow.PenColor = "Black"
        GraphicsWindow.PenWidth = 2
        GraphicsWindow.FontBold = false
        GraphicsWindow.FontSize = 8
        url = "http://www.nonkit.com/smallbasic.files/"
        If i = 1 Then
            mnu("func") = "open"
            GraphicsWindow.DrawImage(url + "open.png", x, y)
            itemname(i) = "Open"
        ElseIf i = 2 Then
            mnu("func") = "save"
            GraphicsWindow.DrawImage(url + "save.png", x, y)
            Program.Delay(msWait) ' ? for Silverlight
            itemname(i) = "Save"
        ElseIf i = 3 Then
            icut = i ' for short cut key
            mnu("func") = "cut"
            ' initialize shapes
            Shapes_Init()
            nShapes = Microsoft.SmallBasic.Library.Array.GetItemCount(shape)
            ' add shapes
            scale = 0.11
            iMin = 1
            iMax = nShapes
            Shapes_Add()
            x = x + 14
            Program.Delay(msWait)
            Shapes_Move()
            x = x - 14
            iMax = 3
            For SmallSVGEditorModule.t = 0 To 360 * 0.3
                angle = 30 - (30 * Microsoft.SmallBasic.Library.Math.Cos(t * Microsoft.SmallBasic.Library.Math.Pi / 180))
                Shapes_Rotate()
            Next
            itemname(i) = "Cut"
        ElseIf i = 4 Then
            icopy = i ' for short cut key
            mnu("func") = "copy"
            GraphicsWindow.DrawImage(url + "copy.png", x, y)
            Program.Delay(msWait) ' ? for Silverlight
            itemname(i) = "Copy"
        ElseIf i = 5 Then
            ipaste = i ' for short cut key
            mnu("func") = "paste"
            GraphicsWindow.DrawImage(url + "paste.png", x, y)
            Program.Delay(msWait) ' ? for Silverlight
            itemname(i) = "Paste"
        ElseIf i = 6 Then
            mnu("func") = "rect"
            GraphicsWindow.DrawRectangle(x + margin, y + margin, size - (margin * 2), size - (margin * 2))
            itemname(i) = "Rectangle"
            repeat("rect") = Shapes.AddImage(url + "repeat.png")
            Shapes.Move(repeat("rect"), x, y) ' for consecutive shapes addition
            Shapes.HideShape(repeat("rect"))
        ElseIf i = 7 Then
            mnu("func") = "ell"
            GraphicsWindow.DrawEllipse(x + margin, y + margin, size - (margin * 2), size - (margin * 2))
            itemname(i) = "Ellipse"
            repeat("ell") = Shapes.AddImage(url + "repeat.png")
            Shapes.Move(repeat("ell"), x, y) ' for consecutive shapes addition
            Shapes.HideShape(repeat("ell"))
        ElseIf i = 8 Then
            mnu("func") = "tri"
            x1 = x + (size / 2)
            y1 = y + margin
            x2 = x + margin
            y2 = y + size - margin
            x3 = x + size - margin
            y3 = y + size - margin
            GraphicsWindow.DrawTriangle(x1, y1, x2, y2, x3, y3)
            itemname(i) = "Triangle"
            repeat("tri") = Shapes.AddImage(url + "repeat.png")
            Shapes.Move(repeat("tri"), x, y) ' for consecutive shapes addition
            Shapes.HideShape(repeat("tri"))
        ElseIf i = 9 Then
            mnu("func") = "line"
            x1 = x + margin
            y1 = y + margin
            x2 = x + size - margin
            y2 = y + size - margin
            GraphicsWindow.DrawLine(x1, y1, x2, y2)
            itemname(i) = "Line"
            repeat("line") = Shapes.AddImage(url + "repeat.png")
            Shapes.Move(repeat("line"), x, y) ' for consecutive shapes addition
            Shapes.HideShape(repeat("line"))
        ElseIf i = 10 Then
            mnu("func") = "pw"
            GraphicsWindow.PenWidth = pwidth
            x1 = x + margin
            y1 = y + (size / 2)
            x2 = x + size - margin
            y2 = y + (size / 2)
            GraphicsWindow.DrawLine(x1, y1, x2, y2)
            itemname(i) = "Pen Width"
        ElseIf i = 11 Then
            mnu("func") = "pc"
            margin = 6
            GraphicsWindow.PenWidth = 4
            GraphicsWindow.PenColor = pcolor
            GraphicsWindow.DrawRectangle(x + margin, y + margin, size - (margin * 2), size - (margin * 2))
            itemname(i) = "Pen Color"
        ElseIf i = 12 Then
            mnu("func") = "bc"
            GraphicsWindow.BrushColor = bcolor
            GraphicsWindow.FillRectangle(x + margin, y + margin, size - (margin * 2), size - (margin * 2))
            GraphicsWindow.PenColor = "Black"
            GraphicsWindow.PenWidth = 2
            GraphicsWindow.DrawRectangle(x + margin, y + margin, size - (margin * 2), size - (margin * 2))
            itemname(i) = "Brush Color"
        ElseIf i = 13 Then
            mnu("func") = "gr"
            GraphicsWindow.PenColor = "#AAAAAA"
            GraphicsWindow.PenWidth = 1
            For SmallSVGEditorModule._x = x + margin To x + size - margin Step (size - (margin * 2)) / 4
                GraphicsWindow.DrawLine(_x, y + margin, _x, y + size - margin)
            Next
            For SmallSVGEditorModule._y = y + margin To y + size - margin Step (size - (margin * 2)) / 4
                GraphicsWindow.DrawLine(x + margin, _y, x + size - margin, _y)
            Next
            itemname(i) = "Grid"
        End If
        menu(i) = mnu
        If (itemname(i) <> CType("", Primitive)) And (oItem(i) = CType("", Primitive)) Then
            GraphicsWindow.BrushColor = "Black"
            oItem(i) = Shapes.AddText(itemname(i))
            Shapes.Move(oItem(i), x + margin, y + size)
        End If
        GraphicsWindow.FontBold = true
        GraphicsWindow.FontSize = 12
    End Sub
    Sub ItemSelect()
        ' i - menu index
        ' select - "True" if selected
        If i <> imenubar Then
            If __select Then
                GraphicsWindow.PenColor = "Gray"
            Else
                GraphicsWindow.PenColor = "#EEEEEE"
            End If
            GraphicsWindow.PenWidth = 2
            mnu = menu(i)
            x = mnu("_x0") - 1
            y = mnu("_y0") - 1
            width = mnu("_x1") - x + 1
            height = mnu("_y1") - y + 1
            GraphicsWindow.DrawRectangle(x, y, width, height)
        End If
    End Sub
    Sub MoveShape()
        ' param i - shape index
        ' param _x, _y - new position of the shape
        shp = shape(i)
        dx = _x - shp("x")
        dy = _y - shp("y")
        shp("x") = _x
        shp("y") = _y
        shp("_x0") = shp("_x0") + dx
        shp("_x1") = shp("_x1") + dx
        shp("_y0") = shp("_y0") + dy
        shp("_y1") = shp("_y1") + dy
        shape(i) = shp
        Shapes.Move(shp("obj"), _x, _y)
        If shp("func") = CType("line", Primitive) Then
            _iMax = 2
        Else
            _iMax = 5
        End If
        For SmallSVGEditorModule._i = 1 To _iMax
            pnch = pinch(_i)
            pnch("_x0") = pnch("_x0") + dx
            pnch("_x1") = pnch("_x1") + dx
            pnch("_y0") = pnch("_y0") + dy
            pnch("_y1") = pnch("_y1") + dy
            pinch(_i) = pnch
            Shapes.Move(pnch("obj"), pnch("_x0"), pnch("_y0"))
        Next
    End Sub
    Sub NormalizePos()
        ' param mxD, myD - fixed vertex of a shape rotated
        ' param mxM, myM - opposite vertex of a shape rotated
        ' param angle - angle of a shape
        ' return _mxD, _myD - fixed vertex of a shape not rotated
        ' return _mxM, _myM - opposite vertex of a shape not rotated
        param = "x=" + mxD + ";y=" + myD + ";width=0;height=0;"
        param("cx") = (mxD + mxM) / 2
        param("cy") = (myD + myM) / 2
        param("angle") = -angle
        Shapes_CalcRotatePos()
        _mxD = Microsoft.SmallBasic.Library.Math.Floor(x)
        _myD = Microsoft.SmallBasic.Library.Math.Floor(y)
        param("x") = mxM
        param("y") = myM
        Shapes_CalcRotatePos()
        _mxM = Microsoft.SmallBasic.Library.Math.Floor(x)
        _myM = Microsoft.SmallBasic.Library.Math.Floor(y)
    End Sub
    Sub ReadShapes()
        File_Open()
        ' param buf - SVG buffer
        scale = 1
        iMin = 1
        iMax = 0
        p = 1
        Parse_Header()
        Parse_Space()
        Parse_Defs()
        While match
            Parse_Space()
            Parse_Rect()
            If match Then
                iMax = iMax + 1
                shape(iMax) = shp
            End If
            If __Not(match) Then
                Parse_Ellipse()
                If match Then
                    iMax = iMax + 1
                    shape(iMax) = shp
                End If
            End If
            If __Not(match) Then
                Parse_Polygon()
                If match Then
                    iMax = iMax + 1
                    shape(iMax) = shp
                End If
            End If
            If __Not(match) Then
                Parse_Line()
                If match Then
                    iMax = iMax + 1
                    shape(iMax) = shp
                End If
            End If
        End While
        Parse_Use()
        Shapes_Add()
    End Sub
    Sub RegisterShapeData()
        ' param i - index of shapes
        ' param func - "rect", "ell", "tri" or "line"
        ' return shape[i] - shape data
        shp = shape(i)
        shp("func") = func
        shape(i) = shp
        RegisterShapeStyle()
        RegisterShapeMetrics()
    End Sub
    Sub RegisterShapeMetrics()
        ' param i - index of shapes
        ' param func - "rect", "ell", "tri" or "line"
        ' param mxD, myD - fixed vertex
        ' param mxU, myU - opposite vertex
        ' return shape[i] - shape data
        ' return xmin, ymin, w, h - shape position and size
        ' return xt - top position for triangle
        ' return x1, y1, x2, y2 - line position
        shp = shape(i)
        If func = CType("line", Primitive) Then ' line
            xmin = Microsoft.SmallBasic.Library.Math.Min(mxD, mxU)
            ymin = Microsoft.SmallBasic.Library.Math.Min(myD, myU)
            xmax = Microsoft.SmallBasic.Library.Math.Max(mxD, mxU)
            ymax = Microsoft.SmallBasic.Library.Math.Max(myD, myU)
            x1 = mxD - xmin
            y1 = myD - ymin
            x2 = mxU - xmin
            y2 = myU - ymin
            shp("x1") = x1
            shp("y1") = y1
            shp("x2") = x2
            shp("y2") = y2
        Else
            mxM = mxU
            myM = myU
            angle = shp("angle")
            NormalizePos()
            xmin = Microsoft.SmallBasic.Library.Math.Min(_mxD, _mxM)
            ymin = Microsoft.SmallBasic.Library.Math.Min(_myD, _myM)
            xmax = Microsoft.SmallBasic.Library.Math.Max(_mxD, _mxM)
            ymax = Microsoft.SmallBasic.Library.Math.Max(_myD, _myM)
            w = xmax - xmin
            h = ymax - ymin
            shp("width") = w
            shp("height") = h
        End If
        shp("x") = xmin
        shp("y") = ymin
        If func = CType("tri", Primitive) Then ' triangle
            xt = Microsoft.SmallBasic.Library.Math.Floor((xmax - xmin) / 2) ' x top
            shp("x1") = xt
            shp("y1") = 0
            shp("x2") = 0
            shp("y2") = h
            shp("x3") = w
            shp("y3") = h
        End If
        shape(i) = shp
        CalcDetectBorder()
    End Sub
    Sub RegisterShapeStyle()
        ' param i - index of shapes
        ' param pwidth  - pen width
        ' param pcolor - pen color
        ' param bcolor - brush color
        ' return shape[i] - shape data
        shp = shape(i)
        shp("pw") = pwidth
        If pwidth > 0 Then
            shp("pc") = pcolor
        Else
            shp("pc") = ""
        End If
        If func <> CType("line", Primitive) Then ' rectangle, ellipse or triangle
            shp("bc") = bcolor
        End If
        shape(i) = shp
    End Sub
    Sub ShapeSelect()
        ' Show or remove pinches for a selected shape
        ' param i - shape index
        ' param select - "True" if selected
        ' return selectedshape - selected shape index
        If __select Then
            shp = shape(i)
            Stack.PushValue("local", x)
            Stack.PushValue("local", y)
            GraphicsWindow.PenColor = "Black"
            GraphicsWindow.PenWidth = 1
            sizePinch = 10
            selectedshape = i
            shX = shp("x")
            shY = shp("y")
            GraphicsWindow.BrushColor = "Lime"
            If shp("func") = CType("line", Primitive) Then
                nPinch = 2
                For SmallSVGEditorModule._i = 1 To nPinch
                    pnch = pinch(_i)
                    pnch("obj") = Shapes.AddEllipse(sizePinch, sizePinch)
                    x = shX + shp("x" + _i) - (sizePinch / 2)
                    y = shY + shp("y" + _i) - (sizePinch / 2)
                    Shapes.Move(pnch("obj"), x, y)
                    pnch("_x0") = x
                    pnch("_y0") = y
                    pnch("_x1") = x + sizePinch
                    pnch("_y1") = y + sizePinch
                    pinch(_i) = pnch
                Next
            Else
                pnch = pinch(5)
                pnch("obj") = Shapes.AddEllipse(sizePinch, sizePinch)
                shWidth = shp("width")
                shHeight = shp("height")
                param("cx") = shX + (shWidth / 2)
                param("cy") = shY + (shHeight / 2)
                param("angle") = shp("angle")
                param("x") = shX + (shWidth / 2) - (sizePinch / 2)
                param("y") = shY - 30 - (sizePinch / 2)
                param("width") = sizePinch
                param("height") = sizePinch
                Shapes_CalcRotatePos()
                Shapes.Move(pnch("obj"), x, y)
                pnch("_x0") = x
                pnch("_y0") = y
                pnch("_x1") = x + sizePinch
                pnch("_y1") = y + sizePinch
                pinch(5) = pnch
                nPinch = 5
                xPinch = "1=0;2=" + shWidth + ";3=0;4=" + shWidth + ";"
                yPinch = "1=0;2=0;3=" + shHeight + ";4=" + shHeight + ";"
                GraphicsWindow.BrushColor = "#639AE7"
                For SmallSVGEditorModule._i = 1 To 4
                    pnch = pinch(_i)
                    pnch("obj") = Shapes.AddEllipse(sizePinch, sizePinch)
                    param("x") = shX + xPinch(_i) - (sizePinch / 2)
                    param("y") = shY + yPinch(_i) - (sizePinch / 2)
                    Shapes_CalcRotatePos()
                    Shapes.Move(pnch("obj"), x, y)
                    pnch("_x0") = x
                    pnch("_y0") = y
                    pnch("_x1") = x + sizePinch
                    pnch("_y1") = y + sizePinch
                    pinch(_i) = pnch
                Next
                shape(i) = shp
            End If
            y = Stack.PopValue("local")
            x = Stack.PopValue("local")
        Else
            selectedshape = ""
            For SmallSVGEditorModule._i = 1 To nPinch
                pnch = pinch(_i)
                Shapes.Remove(pnch("obj"))
            Next
            nPinch = 0
        End If
    End Sub
    Sub WaitToClick()
        ' return mxD, myD - clicked point
        param = "down=True;move=False;up=False;" ' wait to click
        Mouse_SetHandler()
        While clicked = CType(false, Primitive)
            Program.Delay(100)
        End While
    End Sub
    Sub WaitToReleaseM() ' for moving a shape
        ' param i - shape index
        ' param mxD, myD - fixed vertex of a shape
        ' return mxU, myU - opposite vertex of a shape
        GraphicsWindow.PenWidth = 1
        GraphicsWindow.PenColor = "Black"
        GraphicsWindow.BrushColor = "White"
        param = "down=False;move=True;" ' for moving a shape
        Mouse_SetHandler()
        shp = shape(i)
        func = shp("func")
        _x = shp("x")
        _y = shp("y")
        shAngle = shp("angle")
        mxM = mxD
        myM = myD
        moved = true
        While released = CType(false, Primitive)
            If moved Then
                param = "move=False;" ' while moving a shape
                Mouse_SetHandler()
                _x = shp("x") + mxM - mxD
                _y = shp("y") + myM - myD
                If oFrame(func) = CType("", Primitive) Then
                    If func = CType("rect", Primitive) Then
                        oFrame(func) = Shapes.AddRectangle(shp("width"), shp("height"))
                    ElseIf func = CType("ell", Primitive) Then
                        oFrame(func) = Shapes.AddEllipse(shp("width"), shp("height"))
                    ElseIf func = CType("tri", Primitive) Then
                        _x1 = shp("x1")
                        _y1 = shp("y1")
                        _x2 = shp("x2")
                        _y2 = shp("y2")
                        _x3 = shp("x3")
                        _y3 = shp("y3")
                        oFrame(func) = Shapes.AddTriangle(_x1, _y1, _x2, _y2, _x3, _y3)
                    ElseIf func = CType("line", Primitive) Then
                        _x1 = shp("x1")
                        _y1 = shp("y1")
                        _x2 = shp("x2")
                        _y2 = shp("y2")
                        oFrame(func) = Shapes.AddLine(_x1, _y1, _x2, _y2)
                        Shapes.SetOpacity(oFrame(func), 50)
                    End If
                End If
                If (func = CType("rect", Primitive)) Or (func = CType("ell", Primitive)) Or (func = CType("tri", Primitive)) Then
                    Shapes.SetOpacity(oFrame(func), 0)
                    Shapes.Move(oFrame(func), _x, _y)
                    Shapes.Rotate(oFrame(func), shAngle)
                    Shapes.SetOpacity(oFrame(func), 50)
                ElseIf func = CType("line", Primitive) Then
                    Shapes.Move(oFrame(func), _x, _y)
                End If
                param = "move=True;" ' for next moving a shape
                Mouse_SetHandler()
            Else
                Program.Delay(100)
            End If
        End While
        param = "move=False;up=False;" ' mouse released
        Mouse_SetHandler()
        MoveShape()
        If oFrame(func) <> CType("", Primitive) Then
            Shapes.Remove(oFrame(func))
            oFrame(func) = ""
        End If
    End Sub
    Sub WaitToReleaseR() ' for rotating a shape
        ' param i - shape index
        ' return angle - angle for rotation
        GraphicsWindow.PenWidth = 1
        GraphicsWindow.PenColor = "Black"
        GraphicsWindow.BrushColor = "White"
        param = "down=False;move=True;up=True;" ' for rotating a shape / wait to release
        Mouse_SetHandler()
        CalcPinchPos() ' into mxM, myM
        cx = param("cx")
        cy = param("cy")
        shp = shape(i)
        func = shp("func")
        If func = CType("tri", Primitive) Then
            x1 = shp("x1")
            y1 = shp("y1")
            x2 = shp("x2")
            y2 = shp("y2")
            x3 = shp("x3")
            y3 = shp("y3")
        End If
        moved = true
        If oFrame(func) <> CType("", Primitive) Then
            Shapes.Remove(oFrame(func))
        End If
        If func = CType("rect", Primitive) Then
            oFrame(func) = Shapes.AddRectangle(width, height)
        ElseIf func = CType("ell", Primitive) Then
            oFrame(func) = Shapes.AddEllipse(width, height)
        ElseIf func = CType("tri", Primitive) Then
            oFrame(func) = Shapes.AddTriangle(x1, y1, x2, y2, x3, y3)
        End If
        Shapes.SetOpacity(oFrame(func), 0)
        Shapes.Move(oFrame(func), _x, _y)
        While released = CType(false, Primitive)
            If moved Then
                param = "move=False;" ' while sizing a shape
                Mouse_SetHandler()
                x = mxM - cx
                y = myM - cy
                If (x <> 0) Or (y <> 0) Then
                    Math_CartesianToPolar()
                    angle = Microsoft.SmallBasic.Library.Math.Floor(a + 90)
                    If angle >= 360 Then
                        angle = angle - 360
                    End If
                End If
                Shapes.Rotate(oFrame(func), angle)
                Shapes.SetOpacity(oFrame(func), 50)
                param = "move=True;" ' for next sizing a shape
                Mouse_SetHandler()
            Else
                Program.Delay(100)
            End If
        End While
        param = "move=False;up=False;" ' mouse released
        Mouse_SetHandler()
        If oFrame(func) <> CType("", Primitive) Then
            Shapes.Remove(oFrame(func))
            oFrame(func) = ""
        End If
    End Sub
    Sub WaitToReleaseS() ' for sizing a shape
        ' param func - "rect", "ell", "tri" or "line" because shape index may not decided
        ' param mxD, myD - fixed vertex of a shape
        ' param mxM, myM - opposite vertex of a shape
        ' param angle - angle of a shape
        ' return mxU, myU - opposite vertex of a shape
        GraphicsWindow.PenWidth = 1
        GraphicsWindow.PenColor = "Black"
        GraphicsWindow.BrushColor = "White"
        param = "down=False;move=True;up=True;" ' for sizing a shape / wait to release
        Mouse_SetHandler()
        moved = true
        While released = CType(false, Primitive)
            If moved Then
                param = "move=False;" ' while sizing a shape
                Mouse_SetHandler()
                If (func = CType("rect", Primitive)) Or (func = CType("ell", Primitive)) Or (func = CType("tri", Primitive)) Then
                    If oFrame(func) <> CType("", Primitive) Then
                        Shapes.Remove(oFrame(func))
                    End If
                    NormalizePos()
                    xmin = Microsoft.SmallBasic.Library.Math.Min(_mxD, _mxM)
                    ymin = Microsoft.SmallBasic.Library.Math.Min(_myD, _myM)
                    xmax = Microsoft.SmallBasic.Library.Math.Max(_mxD, _mxM)
                    ymax = Microsoft.SmallBasic.Library.Math.Max(_myD, _myM)
                    If func = CType("rect", Primitive) Then
                        oFrame(func) = Shapes.AddRectangle(xmax - xmin, ymax - ymin)
                    ElseIf func = CType("ell", Primitive) Then
                        oFrame(func) = Shapes.AddEllipse(xmax - xmin, ymax - ymin)
                    ElseIf func = CType("tri", Primitive) Then
                        oFrame(func) = Shapes.AddTriangle((xmax - xmin) / 2, 0, 0, ymax - ymin, xmax - xmin, ymax - ymin)
                    End If
                    Shapes.SetOpacity(oFrame(func), 0)
                    Shapes.Move(oFrame(func), xmin, ymin)
                    Shapes.Rotate(oFrame(func), angle)
                    Shapes.SetOpacity(oFrame(func), 50)
                ElseIf func = CType("line", Primitive) Then
                    If oFrame(func) <> CType("", Primitive) Then
                        Shapes.Remove(oFrame(func))
                    End If
                    oFrame(func) = Shapes.AddLine(mxD, myD, mxM, myM)
                    Shapes.SetOpacity(oFrame(func), 50)
                End If
                param = "move=True;" ' for next sizing a shape
                Mouse_SetHandler()
            Else
                Program.Delay(100)
            End If
        End While
        param = "move=False;up=False;" ' mouse released
        Mouse_SetHandler()
        If oFrame(func) <> CType("", Primitive) Then
            Shapes.Remove(oFrame(func))
            oFrame(func) = ""
        End If
    End Sub
    Sub WriteShapes()
        Stack.PushValue("local", i)
        sp(4) = "    "
        sp(6) = "      "
        sp(8) = "        "
        buf = LT + "svg width=" + WQ + gw + WQ + " "
        buf = buf + "height=" + WQ + gh + WQ + ">" + CR + LF
        buf = buf + "  " + LT + "defs>" + CR + LF
        buf = buf + sp(4) + LT + "g id=" + WQ + "g1" + WQ + ">" + CR + LF
        If nShapes > 0 Then
            shp = shape(1)
            xmin = shp("x")
            ymin = shp("y")
            xmax = shp("x")
            ymax = shp("y")
        End If
        For SmallSVGEditorModule.i = 2 To nShapes
            shp = shape(i)
            If shp("x") < xmin Then
                xmin = shp("x")
            End If
            If shp("y") < ymin Then
                ymin = shp("y")
            End If
            If xmax < shp("x") Then
                xmax = shp("x")
            End If
            If ymax < shp("y") Then
                ymax = shp("y")
            End If
        Next
        For SmallSVGEditorModule.i = 1 To nShapes
            shp = shape(i)
            If shp("func") = CType("rect", Primitive) Then
                buf = buf + sp(6) + LT + "rect "
                buf = buf + "x=" + WQ + Microsoft.SmallBasic.Library.Math.Floor(shp("x") + (shp("pw") / 2) - xmin) + WQ + " "
                buf = buf + "y=" + WQ + Microsoft.SmallBasic.Library.Math.Floor(shp("y") + (shp("pw") / 2) - ymin) + WQ + " "
                buf = buf + "width=" + WQ + (shp("width") - shp("pw")) + WQ + " "
                buf = buf + "height=" + WQ + (shp("height") - shp("pw")) + WQ + " "
                If (shp("angle") <> 0) And (shp("angle") <> CType("", Primitive)) Then
                    buf = buf + CR + LF + sp(8)
                    buf = buf + "transform=" + WQ + "rotate(" + shp("angle") + " "
                    buf = buf + Microsoft.SmallBasic.Library.Math.Floor((shp("width") / 2) + shp("x") - xmin) + " "
                    buf = buf + Microsoft.SmallBasic.Library.Math.Floor((shp("height") / 2) + shp("y") - ymin) + ")" + WQ + " "
                End If
                buf = buf + CR + LF + sp(8)
                buf = buf + "style=" + WQ + "fill:" + Text.ConvertToLowerCase(shp("bc")) + ";"
            ElseIf shp("func") = CType("ell", Primitive) Then
                buf = buf + sp(6) + LT + "ellipse "
                cx = Microsoft.SmallBasic.Library.Math.Floor((shp("width") / 2) + shp("x") - xmin)
                cy = Microsoft.SmallBasic.Library.Math.Floor((shp("height") / 2) + shp("y") - ymin)
                rx = Microsoft.SmallBasic.Library.Math.Floor((shp("width") - shp("pw")) / 2)
                ry = Microsoft.SmallBasic.Library.Math.Floor((shp("height") - shp("pw")) / 2)
                buf = buf + "cx=" + WQ + cx + WQ + " "
                buf = buf + "cy=" + WQ + cy + WQ + " "
                buf = buf + "rx=" + WQ + rx + WQ + " "
                buf = buf + "ry=" + WQ + ry + WQ + " "
                If (shp("angle") <> 0) And (shp("angle") <> CType("", Primitive)) Then
                    buf = buf + CR + LF + sp(8)
                    buf = buf + "transform=" + WQ + "rotate(" + shp("angle") + " "
                    buf = buf + cx + " "
                    buf = buf + cy + ")" + WQ + " "
                End If
                buf = buf + CR + LF + sp(8)
                buf = buf + "style=" + WQ + "fill:" + Text.ConvertToLowerCase(shp("bc")) + ";"
            ElseIf shp("func") = CType("tri", Primitive) Then
                buf = buf + sp(6) + LT + "polygon points=" + WQ
                buf = buf + (shp("x1") + shp("x") - xmin) + ","
                buf = buf + (shp("y1") + shp("y") - ymin) + " "
                buf = buf + (shp("x2") + shp("x") - xmin) + ","
                buf = buf + (shp("y2") + shp("y") - ymin) + " "
                buf = buf + (shp("x3") + shp("x") - xmin) + ","
                buf = buf + (shp("y3") + shp("y") - ymin) + WQ + " "
                If (shp("angle") <> 0) And (shp("angle") <> CType("", Primitive)) Then
                    buf = buf + CR + LF + sp(8)
                    buf = buf + "transform=" + WQ + "rotate(" + shp("angle") + " "
                    buf = buf + (shp("x1") + shp("x") - xmin) + " "
                    buf = buf + Microsoft.SmallBasic.Library.Math.Floor(((shp("y2") - shp("y1")) / 2) + shp("y") - ymin) + ")" + WQ + " "
                End If
                buf = buf + CR + LF + sp(8)
                buf = buf + "style=" + WQ + "fill:" + Text.ConvertToLowerCase(shp("bc")) + ";"
            ElseIf shp("func") = CType("line", Primitive) Then
                buf = buf + sp(6) + LT + "line "
                buf = buf + "x1=" + WQ + (shp("x1") + shp("x") - xmin) + WQ + " "
                buf = buf + "y1=" + WQ + (shp("y1") + shp("y") - ymin) + WQ + " "
                buf = buf + "x2=" + WQ + (shp("x2") + shp("x") - xmin) + WQ + " "
                buf = buf + "y2=" + WQ + (shp("y2") + shp("y") - ymin) + WQ + " "
                buf = buf + CR + LF + sp(8)
                buf = buf + "style=" + WQ
            End If
            If shp("pw") > 0 Then
                buf = buf + "stroke:" + Text.ConvertToLowerCase(shp("pc")) + ";"
            End If
            buf = buf + "stroke-width:" + shp("pw") + WQ + "/>" + CR + LF
        Next
        buf = buf + sp(4) + LT + "/g>" + CR + LF
        buf = buf + "  " + LT + "/defs>" + CR + LF
        buf = buf + "  " + LT + "use x=" + xmin + " y=" + ymin + " xlink:href=" + WQ + "#g1" + WQ + " />" + CR + LF
        buf = buf + LT + "/svg>" + CR + LF
        File_Save()
        i = Stack.PopValue("local")
    End Sub
    Sub Color_ColorToRGB()
        ' Color | Convert Color to RGB
        ' param sColor - "#rrggbb"
        ' return iR, iG, iB - [0, 255]
        sR = Text.GetSubText(sColor, 2, 2)
        sG = Text.GetSubText(sColor, 4, 2)
        sB = Text.GetSubText(sColor, 6, 2)
        sHex = sR
        Math_Hex2Dec()
        iR = iDec
        sHex = sG
        Math_Hex2Dec()
        iG = iDec
        sHex = sB
        Math_Hex2Dec()
        iB = iDec
    End Sub
    Sub Color_HSLtoRGB()
        ' Color | Convert HSL to RGB
        ' param rHue - [0, 360) or UNDEFINED
        ' param rLightness - [0, 1]
        ' param rSaturation - [0, 1]
        ' return iR, iG, iB - RGB color
        ' return sColor - "#rrggbb"
        If rLightness <= CType(0.5, Primitive) Then
            rN2 = rLightness * (1 + rSaturation)
        Else
            rN2 = rLightness + rSaturation - (rLightness * rSaturation)
        End If
        rN1 = (2 * rLightness) - rN2
        If rSaturation = 0 Then
            iR = Microsoft.SmallBasic.Library.Math.Round(rLightness * 255)
            iG = Microsoft.SmallBasic.Library.Math.Round(rLightness * 255)
            iB = Microsoft.SmallBasic.Library.Math.Round(rLightness * 255)
        Else
            rH = rHue + 120
            Color_Value()
            iR = iValue
            rH = rHue
            Color_Value()
            iG = iValue
            rH = rHue - 120
            Color_Value()
            iB = iValue
        End If
        sColor = GraphicsWindow.GetColorFromRGB(iR, iG, iB)
    End Sub
    Sub Color_RGBtoHSL()
        ' Color | Convert RGB to HSL
        ' param sColor - "#rrggbb"
        ' return rHue - [0, 360) or UNDEFINED
        ' return rLightness - (0, 1)
        ' return rSaturation - (0, 1)
        Color_ColorToRGB()
        ' rR = iR / 255 ' occurs Math.Max() bug
        rR = Microsoft.SmallBasic.Library.Math.Round(iR / 255 * 10000) / 10000
        ' rG = iG / 255 ' occurs Math.Max() bug
        rG = Microsoft.SmallBasic.Library.Math.Round(iG / 255 * 10000) / 10000
        ' rB = iB / 255 ' occurs Math.Max() bug
        rB = Microsoft.SmallBasic.Library.Math.Round(iB / 255 * 10000) / 10000
        rMax = Microsoft.SmallBasic.Library.Math.Max(rR, rG)
        rMax = Microsoft.SmallBasic.Library.Math.Max(rMax, rB)
        rMin = Microsoft.SmallBasic.Library.Math.Min(rR, rG)
        rMin = Microsoft.SmallBasic.Library.Math.Min(rMin, rB)
        rLightness = (rMax + rMin) / 2
        If rMax = rMin Then ' rR = rG = rB
            rSaturation = 0
            rHue = UNDEFINED
        Else
            If rLightness <= CType(0.5, Primitive) Then
                rSaturation = (rMax - rMin) / (rMax + rMin)
            Else
                rSaturation = (rMax - rMin) / (2 - rMax - rMin)
            End If
            rRC = (rMax - rR) / (rMax - rMin)
            rGC = (rMax - rG) / (rMax - rMin)
            rBC = (rMax - rB) / (rMax - rMin)
            If rR = rMax Then ' between Yellow and Magenta
                rHue = rBC - rGC
            ElseIf rG = rMax Then
                rHue = 2 + rRC - rBC
            ElseIf rB = rMax Then
                rHue = 4 + rGC - rRC
            Else
                TextWindow.WriteLine("Error:")
                TextWindow.WriteLine("rMax=" + rMax)
                TextWindow.WriteLine("rR=" + rR + ",sR=" + sR)
                TextWindow.WriteLine("rG=" + rG + ",sG=" + sG)
                TextWindow.WriteLine("rB=" + rB + ",sB=" + sB)
            End If
            rHue = rHue * 60
            If rHue < 0 Then
                rHue = rHue + 360
            End If
        End If
    End Sub
    Sub Color_Value()
        ' Color | Function value
        ' param rN1, rN2
        ' param rH - [-120, 480)
        ' return iValue - 0..255
        If rH >= 360 Then
            rH = rH - 360
        End If
        If rH < 0 Then
            rH = rH + 360
        End If
        If rH < 60 Then
            rV = rN1 + ((rN2 - rN1) * rH / 60)
        ElseIf rH < 180 Then
            rV = rN2
        ElseIf rH < 240 Then
            rV = rN1 + ((rN2 - rN1) * (240 - rH) / 60)
        Else
            rV = rN1
        End If
        iValue = Microsoft.SmallBasic.Library.Math.Round(rV * 255)
    End Sub
    Sub CS_AddColorToPalette()
        ' Color Selector | Add color to palette
        ' param color - color to set
        ' param maxPalette
        ' param nPalette
        ' param palette
        ' param tPalette - target palette
        Stack.PushValue("local", i)
        For SmallSVGEditorModule.i = 1 To nPalette
            pltt = palette(i)
            If color = pltt("color") Then
                GoTo csactp_not_new_color
            End If
        Next
        pltt = palette(tPalette)
        pltt("color") = color
        palette(tPalette) = pltt
        If nPalette < maxPalette Then
            nPalette = nPalette + 1
        End If
        tPalette = tPalette + 1
        If maxPalette < tPalette Then
            tPalette = 1
        End If
csactp_not_new_color:
        i = Stack.PopValue("local")
    End Sub
    Sub CS_AdjustSlider()
        ' Color Selector | Adjust slider
        ' param iSlider - moved slider
        Stack.PushValue("local", iSlider)
        If (iSlider = iHue) Or (iSlider = iLightness) Or (iSlider = iSaturation) Then
            If iSlider = iHue Then
                Slider_GetLevel()
                rHue = level
            ElseIf iSlider = iLightness Then
                Slider_GetLevel()
                rLightness = level / 100
            Else
                Slider_GetLevel()
                rSaturation = level / 100
            End If
            Color_HSLtoRGB()
            iSlider = iRed
            level = iR
            Slider_SetLevel()
            iSlider = iGreen
            level = iG
            Slider_SetLevel()
            iSlider = iBlue
            level = iB
            Slider_SetLevel()
        Else
            CS_GetColorFromSlider()
            sColor = GraphicsWindow.GetColorFromRGB(red, green, blue)
            Color_RGBtoHSL()
            If rHue = UNDEFINED Then
                rHue = 0
            End If
            iSlider = iHue
            level = Microsoft.SmallBasic.Library.Math.Floor(rHue)
            Slider_SetLevel()
            iSlider = iSaturation
            level = Microsoft.SmallBasic.Library.Math.Floor(rSaturation * 100)
            Slider_SetLevel()
            iSlider = iLightness
            level = Microsoft.SmallBasic.Library.Math.Floor(rLightness * 100)
            Slider_SetLevel()
        End If
        iSlider = Stack.PopValue("local")
    End Sub
    Sub CS_DoObject()
        ' Color Selector | Do object
        ' param - obj
        While obj <> CType("", Primitive)
            CS_DoSlider()
            If obj <> CType("", Primitive) Then
                CS_DoPalette()
            End If
        End While
    End Sub
    Sub CS_DoPalette()
        ' Color Selector | Do palette
        ' param obj - clicked object
        If Text.StartsWith(obj, "palette") Then
            iPalette = Text.GetSubTextToEnd(obj, 8)
            pltt = palette(iPalette)
            color = pltt("color")
            CS_SetColorToSlider() ' set color to slider
            CS_ShowNewColor() ' show new color name
            CS_DrawColorRect() ' draw new color rectangle
            obj = ""
            param = "down=True;move=False;up=False;" ' wait to click
            Mouse_SetHandler()
        End If
    End Sub
    Sub CS_DoSlider()
        ' Color Selector | Do slider
        ' param obj - clicked object
        ' param iSlider - index of slider
        If Text.StartsWith(obj, "slider") Then
            Slider_WaitToRelease()
            obj = ""
            param = "down=True;move=False;up=False;" ' wait to click
            Mouse_SetHandler()
        End If
    End Sub
    Sub CS_DrawColorRect()
        ' Color Selector | Draw color rectangle
        ' param color - color of rectangle
        ' param x, y, width, height - position and size of rectangle
        ' return oRect - rectangle object
        GraphicsWindow.BrushColor = color
        GraphicsWindow.PenColor = BORDERCOLOR
        If oRect <> CType("", Primitive) Then
            Shapes.Remove(oRect)
        End If
        oRect = Shapes.AddRectangle(width, height)
        Shapes.Move(oRect, x, y)
    End Sub
    Sub CS_DrawPalette()
        ' Color Selector | Draw palette
        ' param palette[] - color palette
        ' param nPalette - number of color in palette
        ' param x, y, width, height - position and size of rectangle
        ' return oPalette[] - palette object array
        Stack.PushValue("local", i)
        GraphicsWindow.PenColor = BORDERCOLOR
        For SmallSVGEditorModule.i = 1 To nPalette
            pltt = palette(i)
            GraphicsWindow.BrushColor = pltt("color")
            pltt("oCell") = Shapes.AddRectangle(width, height)
            dx = Microsoft.SmallBasic.Library.Math.Remainder(i - 1, 8) * (width + 4)
            dy = Microsoft.SmallBasic.Library.Math.Floor((i - 1) / 8) * (height + 4)
            Shapes.Move(pltt("oCell"), x + dx, y + dy)
            pltt("x") = x + dx
            pltt("y") = y + dy
            pltt("width") = width
            pltt("height") = height
            palette(i) = pltt
        Next
        i = Stack.PopValue("local")
    End Sub
    Sub CS_GetColorFromSlider()
        ' Color Selector | get color from slider
        ' return color
        Stack.PushValue("local", iSlider)
        iSlider = iRed ' slider index
        Slider_GetLevel()
        red = level
        iSlider = iGreen ' slider index
        Slider_GetLevel()
        green = level
        iSlider = iBlue ' slider index
        Slider_GetLevel()
        blue = level
        color = GraphicsWindow.GetColorFromRGB(red, green, blue)
        iSlider = Stack.PopValue("local")
    End Sub
    Sub CS_Init()
        ' Color Selector | Initialize sliders
        width = 256
        min = 0
        max = 255
        left = 190
        ' add red slider
        top = TOPY
        caption = "R"
        Slider_Add()
        iRed = iSlider ' index of slider
        ' add green slider
        top = top + DELTAY
        caption = "G"
        Slider_Add()
        iGreen = iSlider ' index of slider
        ' add blue slider
        top = top + DELTAY
        caption = "B"
        Slider_Add()
        iBlue = iSlider ' index of slider
        ' add hue slider
        width = 360
        top = top + DELTAY
        max = 360
        caption = "H"
        Slider_Add()
        iHue = iSlider ' index of slider
        ' add saturation slider
        width = 100
        top = top + DELTAY
        max = 100
        caption = "S"
        Slider_Add()
        iSaturation = iSlider ' index of slider
        ' add lightness slider
        width = 100
        top = top + DELTAY
        max = 100
        caption = "L"
        Slider_Add()
        iLightness = iSlider ' index of slider
        ' draw color rectangle
        CS_GetColorFromSlider()
        CS_ShowNewColor()
        x = LEFTX
        y = TOPY + (DELTAY * 4)
        width = 100
        height = 100
        CS_DrawColorRect()
        ' add text box
        GraphicsWindow.BrushColor = CAPTIONCOLOR
        top = y + height + 4
        oNewColor = Shapes.AddText("")
        Shapes.Move(oNewColor, LEFTX, top)
    End Sub
    Sub CS_DumpSlider()
        ' Color Selector | Dump slider for debug
        For SmallSVGEditorModule.i = 1 To numSlider
            TextWindow.WriteLine("slider" + i)
            TextWindow.WriteLine(slider(i))
        Next
    End Sub
    Sub CS_InitPalette()
        ' Color Selector | Initialize palette
        ' This subroutine should be called before CS_ShowPopup().
        pcolor = GraphicsWindow.PenColor
        If Text.GetLength(pcolor) = 9 Then ' for Silverlight
            pcolor = "#" + Text.GetSubText(pcolor, 4, 6)
        End If
        bcolor = GraphicsWindow.BrushColor
        If Text.GetLength(bcolor) = 9 Then ' for Silverlight
            bcolor = "#" + Text.GetSubText(bcolor, 4, 6)
        End If
        maxPalette = 16 ' max cell number of palette
        nPalette = 2 ' number of palette in use
        tPalette = 3 ' index of update target cell
        pltt = palette(1)
        pltt("color") = pcolor
        palette(1) = pltt
        pltt = palette(2)
        pltt("color") = bcolor
        palette(2) = pltt
    End Sub
    Sub CS_RemovePalette()
        ' Color Selector | Remove palette
        ' param nPalette - number of color in palette
        ' return oPalette[] - palette object array
        Stack.PushValue("local", i)
        For SmallSVGEditorModule.i = 1 To nPalette
            oPalette = "Palette" + i
            pltt = palette(i)
            Shapes.Remove(pltt("oCell"))
        Next
        i = Stack.PopValue("local")
    End Sub
    Sub CS_RemoveSliders()
        ' Color Selector | Remove sliders
        For SmallSVGEditorModule.iSlider = 1 To numSlider
            Slider_Remove()
        Next
        numSlider = 0
    End Sub
    Sub CS_SearchClickedObject()
        ' Color Selector | Check slider clicked
        ' param mxD, myD - clicked point
        ' return obj - clicked slider or palette
        ' return iSlider - index if obj is slider
        ' return iPalette - index if obj is palette
        Stack.PushValue("local", i)
        For SmallSVGEditorModule.iSlider = 1 To numSlider
            obj = "slider" + iSlider
            sldr = slider(iSlider)
            x2 = sldr("x2")
            y2 = sldr("y2")
            x3 = sldr("x3")
            y3 = sldr("y3")
            If (x2 <= mxD) And (mxD <= x3) And (y2 <= myD) And (myD <= y3) Then
                GoTo scco_obj_found
            End If
        Next
        For SmallSVGEditorModule.iPalette = 1 To nPalette
            obj = "palette" + iPalette
            pltt = palette(iPalette)
            x2 = pltt("x")
            y2 = pltt("y")
            x3 = pltt("x") + pltt("width")
            y3 = pltt("y") + pltt("height")
            If (x2 <= mxD) And (mxD <= x3) And (y2 <= myD) And (myD <= y3) Then
                GoTo scco_obj_found
            End If
        Next
        obj = ""
scco_obj_found:
        i = Stack.PopValue("local")
    End Sub
    Sub CS_SetColorToSlider()
        ' Color Selector | Set color to slider
        ' param color
        Stack.PushValue("local", iSlider)
        sColor = color
        Color_ColorToRGB()
        iSlider = iRed
        level = iR
        Slider_SetLevel()
        iSlider = iGreen
        level = iG
        Slider_SetLevel()
        iSlider = iBlue
        level = iB
        Slider_SetLevel()
        CS_AdjustSlider()
        iSlider = Stack.PopValue("local")
    End Sub
    Sub CS_ShowNewColor()
        ' Color Selector | Show new color
        ' param oColor
        ' param color
        Shapes.SetText(oNewColor, color)
    End Sub
    Sub CS_ShowPopup()
        ' Color Selector | Show popup
        ' param color - current color
        ' return color - new color
        ' define constant
        Stack.PushValue("local", cont)
        colorInit = color ' initial color
        GraphicsWindow.PenWidth = 2
        GraphicsWindow.PenColor = POPUPCOLOR
        GraphicsWindow.BrushColor = POPUPCOLOR
        oPopup = Shapes.AddRectangle(gw, gh)
        Shapes.SetOpacity(oPopup, 64)
        Shapes.Move(oPopup, LEFTX - 10, TOPY - 10)
        GraphicsWindow.BrushColor = CAPTIONCOLOR
        oOK = Controls.AddButton("OK", gw - 100, gh - 34)
        oCancel = Controls.AddButton("Cancel", gw - 60, gh - 34)
        AddHandler Controls.ButtonClicked, AddressOf CS_OnButtonClicked
        CS_Init()
        Stack.PushValue("local", y)
        y = TOPY
        color = colorInit
        CS_DrawColorRect() ' original color
        oRectCurrent = oRect
        oRect = "" ' keep current color
        'GraphicsWindow.SetPixel(0, 0, colorInit)
        'color = GraphicsWindow.GetPixel(0, 0)
        If Text.GetLength(color) = 9 Then ' for Silverlight
            color = "#" + Text.GetSubText(color, 4, 6)
        End If
        'GraphicsWindow.SetPixel(0, 0, "LightGray")
        GraphicsWindow.BrushColor = CAPTIONCOLOR
        oColor = Shapes.AddText(colorInit)
        Shapes.Move(oColor, x, y + height + 2)
        'If color <> colorInit Then
        '  oColor2 = Shapes.AddText(color)
        '  Shapes.Move(oColor2, x, y + height + 14)
        'EndIf
        y = Stack.PopValue("local")
        CS_SetColorToSlider()
        CS_DrawColorRect() ' draw new color rectangle
        CS_ShowNewColor() ' show new color name
        Stack.PushValue("local", x)
        Stack.PushValue("local", y)
        Stack.PushValue("local", width)
        Stack.PushValue("local", height)
        x = x + width + 30
        y = TOPY + (height * 2) + 24
        width = 30
        height = 30
        CS_DrawPalette()
        height = Stack.PopValue("local")
        width = Stack.PopValue("local")
        y = Stack.PopValue("local")
        x = Stack.PopValue("local")
        cont = true ' continue
        param = "down=True;move=False;up=False;" ' wait click
        Mouse_SetHandler()
        While cont
            If clicked Then
                CS_SearchClickedObject()
                CS_DoObject()
                clicked = false
            Else
                Program.Delay(100)
            End If
        End While
        If cancel Then
            color = colorInit
        Else
            CS_AddColorToPalette()
        End If
        CS_RemovePalette()
        CS_RemoveSliders()
        Shapes.Remove(oColor)
        'Shapes.Remove(oColor2)
        Shapes.Remove(oNewColor)
        Shapes.Remove(oRectCurrent)
        Shapes.Remove(oRect)
        Controls.Remove(oOK)
        Controls.Remove(oCancel)
        Shapes.Remove(oPopup)
        cont = Stack.PopValue("local")
    End Sub
    Sub CS_OnButtonClicked()
        ' Color Selector | Event handler on button clicked
        cont = false
        If Controls.LastClickedButton = oCancel Then
            cancel = true
        Else
            cancel = false
        End If
    End Sub
    Sub File_Open()
        ' File | Show output program to save
        Stack.PushValue("local", cont)
        GraphicsWindow.PenWidth = 0
        GraphicsWindow.BrushColor = POPUPCOLOR
        oPopup = Shapes.AddRectangle(gw, gh)
        Shapes.SetOpacity(oPopup, 64)
        Shapes.Move(oPopup, LEFTX - 10, TOPY - 10)
        GraphicsWindow.BrushColor = CAPTIONCOLOR
        oCaption = Shapes.AddText("Filename")
        Shapes.Move(oCaption, LEFTX, TOPY + 4)
        GraphicsWindow.BrushColor = TEXTCOLOR
        oFilename = Controls.AddTextBox(LEFTX + 80, TOPY)
        Shapes.SetOpacity(oFilename, OPACITY)
        Controls.SetSize(oFilename, 300, 24)
        oText = Controls.AddMultiLineTextBox(LEFTX, TOPY + 30)
        Controls.SetSize(oText, gw - 20, gh - 84)
        Shapes.SetOpacity(oText, OPACITY)
        oOK = Controls.AddButton("OK", gw - 40, gh - 34)
        AddHandler Controls.ButtonClicked, AddressOf File_OnButtonClicked
        AddHandler Controls.TextTyped, AddressOf File_OnTextTyped
        subname = "Shapes_Init"
        typed = false
        cont = true ' continue
        While cont
            If typed Then
                filename = Controls.GetTextBoxText(oFilename)
                If __Not(Text.IsSubText(filename, ":")) And __Not(Text.IsSubText(filename, "\")) Then
                    filename = Program.Directory + "\" + filename
                End If
                buf = ""
                buf = File.ReadContents(filename)
                Controls.SetTextBoxText(oText, buf)
                typed = false
            Else
                Program.Delay(200)
            End If
        End While
        buf = Controls.GetTextBoxText(oText)
        Controls.Remove(oCaption)
        Controls.Remove(oFilename)
        Controls.Remove(oText)
        Controls.Remove(oOK)
        Controls.Remove(oPopup)
        cont = Stack.PopValue("local")
    End Sub
    Sub File_OnTextTyped()
        ' File | Textbox event handler
        If Controls.LastTypedTextBox = oFilename Then
            typed = true
        End If
    End Sub
    Sub File_Save()
        ' File | Show output program to save
        ' param buf - program buffer
        ' define constant
        Stack.PushValue("local", cont)
        GraphicsWindow.PenWidth = 0
        GraphicsWindow.BrushColor = POPUPCOLOR
        oPopup = Shapes.AddRectangle(gw, gh)
        Shapes.SetOpacity(oPopup, OPACITY)
        Shapes.Move(oPopup, LEFTX - 10, TOPY - 10)
        GraphicsWindow.BrushColor = TEXTCOLOR
        oText = Controls.AddMultiLineTextBox(LEFTX, TOPY)
        Controls.SetSize(oText, gw - 20, gh - 50)
        Shapes.SetOpacity(oText, OPACITY)
        Controls.SetTextBoxText(oText, buf)
        oOK = Controls.AddButton("OK", gw - 40, gh - 34)
        GraphicsWindow.BrushColor = CAPTIONCOLOR
        oMsg = Shapes.AddText("Click textbox above, push Ctrl+A, Ctrl+C to copy and save to your editor")
        Shapes.Move(oMsg, LEFTX, gh - 28)
        cont = true ' continue
        AddHandler Controls.ButtonClicked, AddressOf File_OnButtonClicked
        While cont
            Program.Delay(500)
        End While
        Controls.Remove(oText)
        Controls.Remove(oMsg)
        Controls.Remove(oOK)
        Controls.Remove(oPopup)
        cont = Stack.PopValue("local")
    End Sub
    Sub File_OnButtonClicked()
        ' File | Button event handler
        cont = false
    End Sub
    Sub KB_FlushFIFO()
        ' Keyborad | Flush keyboard buffer (FIFO)
        For SmallSVGEditorModule.out = out + 1 To __in
            fifok(out) = ""
            fifos(out) = ""
            fifoc(out) = ""
        Next
    End Sub
    Sub KB_InKey()
        ' Keyboard | In key
        ' return c - input key
        c = ""
        If __in > out Then
            out = out + 1
            c = fifok(out)
            _shift = fifos(out)
            _ctrl = fifoc(out)
            fifok(out) = ""
            fifos(out) = ""
            fifoc(out) = ""
            If Text.GetLength(c) > 1 Then
                If Microsoft.SmallBasic.Library.Array.ContainsIndex(keys, c) Then
                    c = keys(_shift + c)
                Else
                    c = "<" + c + ">"
                End If
            ElseIf _shift = CType("", Primitive) Then
                c = Text.ConvertToLowerCase(c)
            End If
            c = Text.Append(_ctrl, c)
        End If
    End Sub
    Sub KB_Init()
        ' Keyboard | Initialization for Shapes (use only ^x, ^c, ^v, del and arrow keys)
        shift = ""
        ctrl = ""
        __in = 0
        out = 0
        keys = "Delete=DEL;Left=LEFT;Right=RIGHT;Up=UP;Down=DOWN;"
        arrow_dx = "LEFT=-1;RIGHT=1;UP=0;DOWN=0;"
        arrow_dy = "LEFT=0;RIGHT=0;UP=-1;DOWN=1;"
        AddHandler GraphicsWindow.KeyDown, AddressOf KB_OnKeyDown
        AddHandler GraphicsWindow.KeyUp, AddressOf KB_OnKeyUp
    End Sub
    Sub KB_OnKeyDown()
        ' Keyboard | Key down event handler
        key = GraphicsWindow.LastKey
        If (key = CType("LeftShift", Primitive)) Or (key = CType("RightShift", Primitive)) Then
            shift = "+"
        ElseIf (key = CType("LeftCtrl", Primitive)) Or (key = CType("RightCtrl", Primitive)) Then
            ctrl = "^"
        Else
            __in = __in + 1
            fifok(__in) = key
            fifos(__in) = shift
            fifoc(__in) = ctrl
        End If
    End Sub
    Sub KB_OnKeyUp()
        ' Keyboard | Key up event handler
        key = GraphicsWindow.LastKey
        If (key = CType("LeftShift", Primitive)) Or (key = CType("RightShift", Primitive)) Then
            shift = ""
        ElseIf (key = CType("LeftCtrl", Primitive)) Or (key = CType("RightCtrl", Primitive)) Then
            ctrl = ""
        End If
    End Sub
    Sub Math_CartesianToPolar()
        ' Math | convert cartesian coodinate to polar coordinate
        ' param x, y - cartesian coordinate
        ' return r, a - polar coordinate
        r = Microsoft.SmallBasic.Library.Math.SquareRoot((x * x) + (y * y))
        If (x = 0) And (y > 0) Then
            a = 90 ' [degree]
        ElseIf (x = 0) And (y < 0) Then
            a = -90
        Else
            a = Microsoft.SmallBasic.Library.Math.ArcTan(y / x) * 180 / Microsoft.SmallBasic.Library.Math.Pi
        End If
        If x < 0 Then
            a = a + 180
        ElseIf (x > 0) And (y < 0) Then
            a = a + 360
        End If
    End Sub
    Sub Math_Hex2Dec()
        ' Math | Convert hexadecimal to decimal
        ' param sHex
        ' return iDec
        iDec = 0
        iLen = Text.GetLength(sHex)
        For SmallSVGEditorModule.iPtr = 1 To iLen
            iDec = (iDec * 16) + Text.GetIndexOf("0123456789ABCDEF", Text.GetSubText(sHex, iPtr, 1)) - 1
        Next
    End Sub
    Sub Mouse_Init()
        ' Mouse | Initialize for common event handler
        clicked = false
        moved = false
        released = false
        If debug Then
            Timer.Interval = 200
            AddHandler Timer.Tick, AddressOf Mouse_OnTick
        End If
    End Sub
    Sub Mouse_SetHandler()
        ' Mouse | Set or reset common event handler
        ' param["down"] - "True" if set, "False" if reset
        ' param["move"] - "True" if set, "False" if reset
        ' param["up"] -  - "True" if set, "False" if reset
        ' return clicked - "False" if set MouseDown
        ' return moved - "False" if set MouseMove
        ' return released - "False" if set MouseUp
        ' return dmu - which handlers are set for debug
        If param("up") Then
            released = false
            AddHandler GraphicsWindow.MouseUp, AddressOf Mouse_OnUp
            handler("up") = "U"
        ElseIf param("up") = CType(false, Primitive) Then
            AddHandler GraphicsWindow.MouseUp, AddressOf Mouse_DoNothing
            handler("up") = ""
        End If
        If param("down") Then
            clicked = false
            AddHandler GraphicsWindow.MouseDown, AddressOf Mouse_OnDown
            handler("down") = "D"
        ElseIf param("down") = CType(false, Primitive) Then
            AddHandler GraphicsWindow.MouseDown, AddressOf Mouse_DoNothing
            handler("down") = ""
        End If
        If param("move") Then
            moved = false
            AddHandler GraphicsWindow.MouseMove, AddressOf Mouse_OnMove
            handler("move") = "M"
        ElseIf param("move") = CType(false, Primitive) Then
            AddHandler GraphicsWindow.MouseMove, AddressOf Mouse_DoNothing
            handler("move") = ""
        End If
        dmu = handler("down") + handler("move") + handler("up")
        If debug Then
            smrc = " set "
        End If
    End Sub
    Sub Mouse_OnDown()
        ' Mouse | Common event handler on mouse down
        ' return mxD, myD - position on mouse down
        mxD = Microsoft.SmallBasic.Library.Math.Floor(GraphicsWindow.MouseX)
        myD = Microsoft.SmallBasic.Library.Math.Floor(GraphicsWindow.MouseY)
        clicked = true
        released = false
        If debug Then
            smrc = " clicked " + mxD + "," + myD
        End If
    End Sub
    Sub Mouse_DoNothing()
        ' Mouse | Common event handler to do nothing
    End Sub
    Sub Mouse_OnMove()
        ' Mouse | Common event handler on mouse move
        ' return mxM, myM - position on mouse move
        mxM = Microsoft.SmallBasic.Library.Math.Floor(GraphicsWindow.MouseX)
        myM = Microsoft.SmallBasic.Library.Math.Floor(GraphicsWindow.MouseY)
        moved = true
        If debug Then
            smrc = " moved " + mxM + "," + myM
        End If
    End Sub
    Sub Mouse_OnTick()
        ' Mouse | debug routine
        If clicked Then
            cmr = "C"
        Else
            cmr = ""
        End If
        If moved Then
            cmr = cmr + "M"
        End If
        If released Then
            cmr = cmr + "R"
        End If
        GraphicsWindow.Title = title + smrc + " " + dmu + " " + cmr
    End Sub
    Sub Mouse_OnUp()
        ' Mouse | Common event handler on mouse up
        ' return mxU, myU - position on mouse up
        mxU = Microsoft.SmallBasic.Library.Math.Floor(GraphicsWindow.MouseX)
        myU = Microsoft.SmallBasic.Library.Math.Floor(GraphicsWindow.MouseY)
        released = true
        If debug Then
            smrc = " released " + mxU + "," + myU
        End If
    End Sub
    Sub Parse_Defs()
        ' param buf - SVG buffer
        ' param p - pointer to SVG buffer
        ' return match - "True" if match
        match = false
        If Text.StartsWith(Text.GetSubTextToEnd(buf, p), LT + "defs>") Then
            Stack.PushValue("local", p)
            p = p + 6
            Parse_Space()
            match = false
            If Text.StartsWith(Text.GetSubTextToEnd(buf, p), LT + "g id=" + WQ + "g1" + WQ + ">") Then
                p = p + 11
                match = true
            End If
            _p = Stack.PopValue("local")
            If __Not(match) Then
                p = _p
            End If
        End If
    End Sub
    Sub Parse_Ellipse()
        ' param buf - SVG buffer
        ' param p - pointer to SVG buffer
        ' return match - "True" if match
        ' return shp - shape entry
        match = false
        If Text.StartsWith(Text.GetSubTextToEnd(buf, p), LT + "ellipse") Then
            param = "tag=ellipse;"
            Parse_FindTag()
            Parse_GetAttrAndText()
            cx = attr("cx")
            cy = attr("cy")
            rx = attr("rx")
            ry = attr("ry")
            Parse_SetStyle()
            shp = ""
            shp("func") = "ell"
            shp("x") = cx - rx - Microsoft.SmallBasic.Library.Math.Floor(pw / 2)
            shp("y") = cy - ry - Microsoft.SmallBasic.Library.Math.Floor(pw / 2)
            shp("width") = (2 * rx) + pw
            shp("height") = (2 * ry) + pw
            shp("pw") = pw
            shp("pc") = pc
            shp("bc") = bc
            p = p + len
            match = true
        End If
    End Sub
    Sub Parse_FindTag()
        ' find tag from html buffer
        ' param["tag"] - tag name
        ' param["class"] - class name
        ' param p - pointer for buffer
        ' param buf - html buffer
        ' return tag - found tag
        pSave = p
        tag = ""
        findNext = true
        While findNext
            findNext = false ' tag may be not found
            pTag = Text.GetIndexOf(Text.GetSubTextToEnd(buf, p), LT + param("tag"))
            If 0 < pTag Then
                lTag = Text.GetLength(param("tag")) + 1
                pTag = p + pTag - 1
                len = Text.GetIndexOf(Text.GetSubTextToEnd(buf, pTag), "/" + param("tag") + ">")
                If len = 0 Then
                    lTag = 1
                    len = Text.GetIndexOf(Text.GetSubTextToEnd(buf, pTag), "/>")
                End If
                If param("class") = CType("", Primitive) Then
                    len = len + lTag
                    tag = Text.GetSubText(buf, pTag, len)
                    findNext = false ' found the tag
                ElseIf 0 < len Then
                    findNext = true ' tag may have different class
                    len = len + lTag
                    attr = "class=" + qt + param("class") + qt
                    pAttr = pTag + lTag + 1
                    lAttr = Text.GetLength(attr)
                    If Text.GetSubText(buf, pAttr, lAttr) = attr Then
                        tag = Text.GetSubText(buf, pTag, len)
                        findNext = false ' found the tag
                    End If
                End If
                p = pTag + len
            End If
        End While
        If tag = CType("", Primitive) Then
            p = pSave
        End If
    End Sub
    Sub Parse_GetAttrAndText()
        ' get attributes and text from given tag
        ' param tag - given tag
        ' return attr[] - array of attributes in the tag
        ' return txt - text in the tag
        ' return len - length of the tag
        pTag = Text.GetIndexOf(tag, " ") + 1
        pEnd = Text.GetIndexOf(tag, ">")
        attr = ""
        While pTag <= pEnd
            Parse_SkipSpaceInTag()
            pEq = Text.GetIndexOf(Text.GetSubTextToEnd(tag, pTag), "=")
            If 0 < pEq Then
                pEq = pTag + pEq - 1
                If Text.GetSubText(tag, pEq + 1, 1) = qt Then
                    pQ = Text.GetIndexOf(Text.GetSubTextToEnd(tag, pEq + 2), qt)
                    If 0 < pQ Then
                        pQ = pEq + 2 + pQ - 1
                        attr(Text.GetSubText(tag, pTag, pEq - pTag)) = Text.GetSubText(tag, pEq + 2, pQ - pEq - 2)
                        pTag = pQ + 2
                    End If
                End If
            Else
                pTag = pEnd + 1
            End If
        End While
        If (pEnd + 1) < pTag Then
            pTag = pEnd + 1
        End If
        len = Text.GetLength(tag)
        txt = ""
        While pTag <= len
            pL = Text.GetIndexOf(Text.GetSubTextToEnd(tag, pTag), LT)
            If pL = 0 Then
                txt = Text.Append(txt, Text.GetSubTextToEnd(tag, pTag))
                pTag = len + 1
            Else
                pL = pTag + pL - 1
                txt = Text.Append(txt, Text.GetSubText(tag, pTag, pL - pTag))
                pR = Text.GetIndexOf(Text.GetSubTextToEnd(tag, pTag), ">")
                If 0 < pR Then
                    pTag = pTag + pR
                Else
                    pTag = len + 1
                End If
            End If
        End While
    End Sub
    Sub Parse_GetStyleAttr()
        ' param kw - keyword
        ' param attr["style"] - style attribute
        ' param pStyle - pointer to search in style attribute
        ' return value - value
        pKw = Text.GetIndexOf(Text.GetSubTextToEnd(attr("style"), pStyle), kw)
        If pKw = 0 Then
            value = ""
        Else
            pValue = pStyle + pKw + Text.GetLength(kw)
            pColon = Text.GetIndexOf(Text.GetSubTextToEnd(attr("style"), pValue), ";")
            If pColon = 0 Then
                pColon = Text.GetLength(kw) + 1
            End If
            value = Text.GetSubText(attr("style"), pValue, pColon - 1)
        End If
    End Sub
    Sub Parse_Header()
        ' param buf - SVG buffer
        ' param p - pointer to SVG buffer
        ' return match - "True" if match
        ' return shp - shape entry
        If Text.StartsWith(Text.GetSubTextToEnd(buf, p), LT + "svg") Then
            len = Text.GetIndexOf(Text.GetSubTextToEnd(buf, p), ">")
            If 0 < len Then
                tag = Text.GetSubText(buf, p, len)
                Parse_GetAttrAndText()
                width = attr("width")
                height = attr("height")
                p = p + len
                match = true
            End If
        End If
    End Sub
    Sub Parse_Line()
        ' param buf - SVG buffer
        ' param p - pointer to SVG buffer
        ' return match - "True" if match
        ' return shp - shape entry
        match = false
        If Text.StartsWith(Text.GetSubTextToEnd(buf, p), LT + "line") Then
            param = "tag=line;"
            Parse_FindTag()
            Parse_GetAttrAndText()
            Parse_SetStyle()
            shp = ""
            shp("func") = "line"
            shp("x1") = attr("x1")
            shp("y1") = attr("y1")
            shp("x2") = attr("x2")
            shp("y2") = attr("y2")
            shp("pw") = pw
            shp("pc") = pc
            p = p + len
            match = true
        End If
    End Sub
    Sub Parse_Polygon()
        ' param buf - SVG buffer
        ' param p - pointer to SVG buffer
        ' return match - "True" if match
        ' return shp - shape entry
        match = false
        If Text.StartsWith(Text.GetSubTextToEnd(buf, p), LT + "polygon") Then
            param = "tag=polygon;"
            Parse_FindTag()
            Parse_GetAttrAndText()
            x = attr("x")
            y = attr("y")
            width = attr("width")
            height = attr("height")
            Parse_SetStyle()
            shp = ""
            shp("func") = "tri"
            shp("x") = x - Microsoft.SmallBasic.Library.Math.Floor(pw / 2)
            shp("y") = y - Microsoft.SmallBasic.Library.Math.Floor(pw / 2)
            shp("width") = width + pw
            shp("height") = height + pw
            shp("pw") = pw
            shp("pc") = pc
            shp("bc") = bc
            p = p + len
            match = true
        End If
    End Sub
    Sub Parse_SetStyle()
        pStyle = 1
        kw = "fill"
        Parse_GetStyleAttr()
        bc = value
        kw = "stroke"
        Parse_GetStyleAttr()
        pc = value
        kw = "stroke-width"
        Parse_GetStyleAttr()
        pw = value
    End Sub
    Sub Parse_SkipSpaceInTag()
        ' param pTag - pointer to tag
        ' param tag - tag
        ' return pTag - updated pointer to tag
        isSpace = true
        While isSpace
            __char = Text.GetSubText(tag, pTag, 1)
            If Text.IsSubText(" " + CR + LF, __char) Then
                pTag = pTag + 1
            Else
                isSpace = false
            End If
        End While
    End Sub
    Sub Parse_Space()
        ' param p - pointer to buffer
        ' param buf - buffer
        ' return p - updated pointer to buffer
        isSpace = true
        While isSpace
            __char = Text.GetSubText(buf, p, 1)
            If Text.IsSubText(" " + CR + LF, __char) Then
                p = p + 1
            Else
                isSpace = false
            End If
        End While
    End Sub
    Sub Parse_Rect()
        ' param buf - SVG buffer
        ' param p - pointer to SVG buffer
        ' return match - "True" if match
        ' return shp - shape entry
        match = false
        If Text.StartsWith(Text.GetSubTextToEnd(buf, p), LT + "rect") Then
            param = "tag=rect;"
            Parse_FindTag()
            Parse_GetAttrAndText()
            x = attr("x")
            y = attr("y")
            width = attr("width")
            height = attr("height")
            Parse_SetStyle()
            shp = ""
            shp("func") = "rect"
            shp("x") = x - Microsoft.SmallBasic.Library.Math.Floor(pw / 2)
            shp("y") = y - Microsoft.SmallBasic.Library.Math.Floor(pw / 2)
            shp("width") = width + pw
            shp("height") = height + pw
            shp("pw") = pw
            shp("pc") = pc
            shp("bc") = bc
            p = p + len
            match = true
        End If
    End Sub
    Sub Parse_Use()
        shX = 0
        shY = 0
    End Sub
    Sub Popup_Init()
        TOPY = 10 ' top y
        LEFTX = 10 ' left x
        DELTAY = 36 ' delta y
        OPACITY = 70
        POPUPCOLOR = "LightGray"
        CAPTIONCOLOR = "Black"
        TEXTCOLOR = "Black"
        BORDERCOLOR = "#666666"
        BOXCOLOR = "LightGray"
        SLITCOLOR = "#555555"
        UNDEFINED = "N/A"
    End Sub
    Sub SB_RotateWorkaround()
        ' Small Basic | Rotate workaround for Silverlight
        ' param shp - current shape
        ' param x, y - original coordinate
        ' param alpha - angle [radian]
        ' returns x, y - workaround coordinate
        If shp("func") = CType("tri", Primitive) Then
            x1 = -Microsoft.SmallBasic.Library.Math.Floor(shp("x3") / 2)
            y1 = -Microsoft.SmallBasic.Library.Math.Floor(shp("y3") / 2)
        ElseIf shp("func") = CType("line", Primitive) Then
            x1 = -Microsoft.SmallBasic.Library.Math.Floor(Microsoft.SmallBasic.Library.Math.Abs(shp("x1") - shp("x2")) / 2)
            y1 = -Microsoft.SmallBasic.Library.Math.Floor(Microsoft.SmallBasic.Library.Math.Abs(shp("y1") - shp("y2")) / 2)
        End If
        ox = x - x1
        oy = y - y1
        x = (x1 * Microsoft.SmallBasic.Library.Math.Cos(alpha)) - (y1 * Microsoft.SmallBasic.Library.Math.Sin(alpha)) + ox
        y = (x1 * Microsoft.SmallBasic.Library.Math.Sin(alpha)) + (y1 * Microsoft.SmallBasic.Library.Math.Cos(alpha)) + oy
    End Sub
    Sub SB_Workaround()
        ' Small Basic | Workaround for Silverlight
        ' returns silverlight - "True" if in remote
        color = GraphicsWindow.GetPixel(0, 0)
        If Text.GetLength(color) > 7 Then
            silverlight = true
            msWait = 300
        Else
            silverlight = false
        End If
    End Sub
    Sub Shapes_Init()
        ' Shapes | Initialize scissors shapes data for menu icon
        ' return shX, shY - current position of shapes
        ' return shape - array of shapes
        shX = 230 ' x offset
        shY = 30 ' y offset
        shape = ""
        shape(1) = "func=tri;x=45;y=0;x1=22;y1=0;x2=0;y2=213;x3=44;y3=213;bc=#6E6E6E;pw=0;"
        shape(2) = "func=rect;x=45;y=212;width=15;height=47;bc=#6E6E6E;pw=0;"
        shape(3) = "func=ell;x=0;y=235;width=66;height=104;bc=#EEEEEE;pc=#0C95BB;pw=16;"
        shape(4) = "func=tri;x=45;y=0;x1=22;y1=0;x2=0;y2=213;x3=44;y3=213;bc=#939393;pw=0;"
        shape(5) = "func=rect;x=75;y=212;width=14;height=49;bc=#919191;pw=0;"
        shape(6) = "func=ell;x=61;y=163;width=13;height=15;bc=#6E6E6E;pw=0;"
        shape(7) = "func=ell;x=70;y=236;width=64;height=104;bc=#EEEEEE;pc=#0C95BB;pw=16;"
    End Sub
    Sub Shapes_Add()
        ' Shapes | add shapes as shapes data
        ' param iMin, iMax - shape indices to add
        ' param shape - array of shapes
        ' param scale - 1 if same scale
        ' return shWidth, shHeight - total size of shapes
        ' return shAngle - current angle of shapes
        Stack.PushValue("local", i)
        Stack.PushValue("local", x)
        Stack.PushValue("local", y)
        Shapes_CalcWidthAndHeight()
        s = scale
        For SmallSVGEditorModule.i = iMin To iMax
            shp = shape(i)
            GraphicsWindow.PenWidth = shp("pw") * s
            If shp("pw") > 0 Then
                GraphicsWindow.PenColor = shp("pc")
            End If
            If Text.IsSubText("rect|ell|tri|text", shp("func")) Then
                GraphicsWindow.BrushColor = shp("bc")
            End If
            If shp("func") = CType("rect", Primitive) Then
                shp("obj") = Shapes.AddRectangle(shp("width") * s, shp("height") * s)
            ElseIf shp("func") = CType("ell", Primitive) Then
                shp("obj") = Shapes.AddEllipse(shp("width") * s, shp("height") * s)
            ElseIf shp("func") = CType("tri", Primitive) Then
                shp("obj") = Shapes.AddTriangle(shp("x1") * s, shp("y1") * s, shp("x2") * s, shp("y2") * s, shp("x3") * s, shp("y3") * s)
            ElseIf shp("func") = CType("line", Primitive) Then
                shp("obj") = Shapes.AddLine(shp("x1") * s, shp("y1") * s, shp("x2") * s, shp("y2") * s)
            ElseIf shp("func") = CType("text", Primitive) Then
                If silverlight Then
                    fs = Microsoft.SmallBasic.Library.Math.Floor(shp("fs") * 0.9)
                Else
                    fs = shp("fs")
                End If
                GraphicsWindow.FontSize = fs * s
                GraphicsWindow.FontName = shp("fn")
                shp("obj") = Shapes.AddText(shp("text"))
            End If
            x = shp("x")
            y = shp("y")
            shp("rx") = x
            shp("ry") = y
            If silverlight And Text.IsSubText("tri|line", shp("func")) Then
                alpha = Microsoft.SmallBasic.Library.Math.GetRadians(shp("angle"))
                SB_RotateWorkaround()
                shp("wx") = x
                shp("wy") = y
            End If
            Shapes.Move(shp("obj"), shX + (x * s), shY + (y * s))
            If Text.IsSubText("rect|ell|tri|text", shp("func")) And (shp("angle") <> 0) And (shp("angle") <> CType("", Primitive)) Then
                Shapes.Rotate(shp("obj"), shp("angle"))
            End If
            shape(i) = shp
        Next
        shAngle = 0
        y = Stack.PopValue("local")
        x = Stack.PopValue("local")
        i = Stack.PopValue("local")
    End Sub
    Sub Shapes_CalcRotatePos()
        ' Shapes | Calculate position for rotated shape
        ' param["x"], param["y"] - position of a shape
        ' param["width"], param["height"] - size of a shape
        ' param ["cx"], param["cy"] - center of rotation
        ' param ["angle"] - rotate angle
        ' return x, y - rotated position of a shape
        _cx = param("x") + (param("width") / 2)
        _cy = param("y") + (param("height") / 2)
        x = _cx - param("cx")
        y = _cy - param("cy")
        Math_CartesianToPolar()
        a = a + param("angle")
        x = r * Microsoft.SmallBasic.Library.Math.Cos(a * Microsoft.SmallBasic.Library.Math.Pi / 180)
        y = r * Microsoft.SmallBasic.Library.Math.Sin(a * Microsoft.SmallBasic.Library.Math.Pi / 180)
        _cx = x + param("cx")
        _cy = y + param("cy")
        x = _cx - (param("width") / 2)
        y = _cy - (param("height") / 2)
    End Sub
    Sub Shapes_CalcWidthAndHeight()
        ' Shapes | Calculate total width and height of shapes
        ' param iMin, iMax - shape indices to add
        ' return shWidth, shHeight - total size of shapes
        For SmallSVGEditorModule.i = iMin To iMax
            shp = shape(i)
            If (shp("func") = CType("tri", Primitive)) Or (shp("func") = CType("line", Primitive)) Then
                xmin = shp("x1")
                xmax = shp("x1")
                ymin = shp("y1")
                ymax = shp("y1")
                If shp("x2") < xmin Then
                    xmin = shp("x2")
                End If
                If xmax < shp("x2") Then
                    xmax = shp("x2")
                End If
                If shp("y2") < ymin Then
                    ymin = shp("y2")
                End If
                If ymax < shp("y2") Then
                    ymax = shp("y2")
                End If
                If shp("func") = CType("tri", Primitive) Then
                    If shp("x3") < xmin Then
                        xmin = shp("x3")
                    End If
                    If xmax < shp("x3") Then
                        xmax = shp("x3")
                    End If
                    If shp("y3") < ymin Then
                        ymin = shp("y3")
                    End If
                    If ymax < shp("y3") Then
                        ymax = shp("y3")
                    End If
                End If
                shp("width") = xmax - xmin
                shp("height") = ymax - ymin
            End If
            If i = 1 Then
                shWidth = shp("x") + shp("width")
                shHeight = shp("y") + shp("height")
            Else
                If shWidth < (shp("x") + shp("width")) Then
                    shWidth = shp("x") + shp("width")
                End If
                If shHeight < (shp("y") + shp("height")) Then
                    shHeight = shp("y") + shp("height")
                End If
            End If
            shape(i) = shp
        Next
    End Sub
    Sub Shapes_Move()
        ' Shapes | Move shapes
        ' param iMin, iMax - shape indices to add
        ' param shape - array of shapes
        ' param scale - to zoom
        ' param x, y - position to move
        ' return shX, shY - new position of shapes
        Stack.PushValue("local", i)
        s = scale
        shX = x
        shY = y
        For SmallSVGEditorModule.i = iMin To iMax
            shp = shape(i)
            If silverlight And Text.IsSubText("tri|line", shp("func")) Then
                _x = shp("wx")
                _y = shp("wy")
            Else
                _x = shp("rx")
                _y = shp("ry")
            End If
            Shapes.Move(shp("obj"), shX + (_x * s), shY + (_y * s))
        Next
        i = Stack.PopValue("local")
    End Sub
    Sub Shapes_Remove()
        ' Shapes | Remove shapes
        ' param iMin, iMax - shapes indices to remove
        ' param shape - array of shapes
        Stack.PushValue("local", i)
        For SmallSVGEditorModule.i = iMin To iMax
            shp = shape(i)
            Shapes.Remove(shp("obj"))
        Next
        i = Stack.PopValue("local")
    End Sub
    Sub Shapes_Rotate()
        ' Shapes | Rotate shapes
        ' param iMin, iMax - shapes indices to rotate
        ' param shape - array of shapes
        ' param scale - to zoom
        ' param angle - to rotate
        Stack.PushValue("local", i)
        Stack.PushValue("local", x)
        Stack.PushValue("local", y)
        s = scale
        param("angle") = angle
        param("cx") = shWidth / 2
        param("cy") = shHeight / 2
        For SmallSVGEditorModule.i = iMin To iMax
            shp = shape(i)
            param("x") = shp("x")
            param("y") = shp("y")
            param("width") = shp("width")
            param("height") = shp("height")
            Shapes_CalcRotatePos()
            shp("rx") = x
            shp("ry") = y
            If silverlight And Text.IsSubText("tri|line", shp("func")) Then
                alpha = Microsoft.SmallBasic.Library.Math.GetRadians(angle + shp("angle"))
                SB_RotateWorkaround()
                shp("wx") = x
                shp("wy") = y
            End If
            Shapes.Move(shp("obj"), shX + (x * s), shY + (y * s))
            Shapes.Rotate(shp("obj"), angle + shp("angle"))
            shape(i) = shp
        Next
        y = Stack.PopValue("local")
        x = Stack.PopValue("local")
        i = Stack.PopValue("local")
    End Sub
    Sub Slider_Add()
        ' Slider | Add slider as shapes and property
        ' param width
        ' param caption
        ' param min, max
        ' param left, top
        ' return slider[] - property of slider
        ' return iSlider - added slider index
        numSlider = numSlider + 1
        iSlider = numSlider
        ' add shapes for slider
        GraphicsWindow.BrushColor = CAPTIONCOLOR
        len = Text.GetLength(caption)
        sldr = slider(iSlider)
        sldr("oCaption") = Shapes.AddText(caption)
        Shapes.Move(sldr("oCaption"), left - ((len * 5) + 10), top + 1)
        level = Microsoft.SmallBasic.Library.Math.Floor((min + max) / 2)
        sldr("level") = level ' property
        sldr("min") = min
        sldr("max") = max
        GraphicsWindow.PenColor = BORDERCOLOR
        mag = (level - min) / (max - min)
        GraphicsWindow.BrushColor = SLITCOLOR
        sldr("oSlit") = Shapes.AddRectangle(width, 10)
        GraphicsWindow.PenColor = BORDERCOLOR
        GraphicsWindow.BrushColor = BOXCOLOR
        sldr("oBox") = Shapes.AddRectangle(10, 18)
        GraphicsWindow.BrushColor = CAPTIONCOLOR
        sldr("oLevel") = Shapes.AddText(level)
        sldr("x0") = left
        sldr("x1") = left + width
        sldr("y0") = top
        Shapes.Move(sldr("oLevel"), left + width + 5, top)
        ' move and zoom shapes for slider
        Shapes.Move(sldr("oSlit"), left, top + 4)
        slider(iSlider) = sldr
        Slider_SetLevel()
    End Sub
    Sub Slider_CallBack()
        ' Slider | Call back
        ' param iSlider - changed slider
        CS_AdjustSlider()
        CS_GetColorFromSlider()
        CS_ShowNewColor() ' show new color name
        CS_DrawColorRect() ' draw new color rectangle
    End Sub
    Sub Slider_GetLevel()
        ' Slider | Get latest level of slider
        ' param iSlider
        ' return level
        sldr = slider(iSlider)
        level = sldr("level")
    End Sub
    Sub Slider_GetMouseLevel()
        ' Slider | Get mouse level of slider
        ' param iSlider
        ' return level
        sldr = slider(iSlider)
        x0 = sldr("x0")
        x1 = sldr("x1")
        max = sldr("max")
        min = sldr("min")
        level = min + Microsoft.SmallBasic.Library.Math.Floor((max - min) * (mxM - x0) / (x1 - x0))
    End Sub
    Sub Slider_WaitToRelease()
        ' Slider | Get released point for slider moving
        ' param iSlider
        param = "down=False;move=True;up=True;" ' for slider moving / wait to release
        Mouse_SetHandler()
        While released = CType(false, Primitive)
            If moved Then
                param = "move=False;" ' while slider moving
                Mouse_SetHandler()
                sldr = slider(iSlider)
                x0_ = sldr("x0")
                x1_ = sldr("x1")
                If mxM < x0_ Then
                    mxM = x0_
                End If
                If x1_ < mxM Then
                    mxM = x1_
                End If
                Slider_GetMouseLevel() ' get mouse level of slider
                Slider_SetLevel() ' set slider level and move slider box
                Slider_CallBack()
                param = "move=True;" ' for next slider moving
                Mouse_SetHandler()
            Else
                Program.Delay(100)
            End If
        End While
        param = "move=False;up=False;" ' mouse released
        Mouse_SetHandler()
    End Sub
    Sub Slider_Remove()
        ' Slider | Remove a slider
        ' param iSlider
        sldr = slider(iSlider)
        Shapes.Remove(sldr("oCaption"))
        Shapes.Remove(sldr("oSlit"))
        Shapes.Remove(sldr("oBox"))
        Shapes.Remove(sldr("oLevel"))
    End Sub
    Sub Slider_SetLevel()
        ' Slider | Set slider level and move slider box
        ' param iSlider
        ' param level
        Stack.PushValue("local", width)
        sldr = slider(iSlider)
        x0 = sldr("x0")
        x1 = sldr("x1")
        y0 = sldr("y0")
        width = x1 - x0
        sldr("level") = level
        Shapes.SetText(sldr("oLevel"), level)
        ' move bar
        min = sldr("min")
        max = sldr("max")
        mag = (level - min) / (max - min)
        ' move box
        Shapes.Move(sldr("oBox"), x0 + Microsoft.SmallBasic.Library.Math.Floor(width * mag) - 5, y0)
        sldr("x2") = x0 + Microsoft.SmallBasic.Library.Math.Floor(width * mag) - 5
        sldr("x3") = x0 + Microsoft.SmallBasic.Library.Math.Floor(width * mag) - 5 + 10
        sldr("y2") = y0
        sldr("y3") = y0 + 18
        slider(iSlider) = sldr
        width = Stack.PopValue("local")
    End Sub
End Module
