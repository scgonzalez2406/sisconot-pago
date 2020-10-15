*
PROCEDURE errorhandl
PARAMETER m.messg, m.code,  ;
          m.progr, m.lin
SET DEVICE TO SCREEN
DEFINE WINDOW msgerr FROM 10, 02  ;
       TO 18, 78 SHADOW TITLE  ;
       'MENSAJE DE ERROR ' COLOR  ;
       SCHEME 7
DO CASE
     CASE m.code = 125
          ACTIVATE WINDOW msgerr
          @ 00, 01 SAY  ;
            'Programa: ' +  ;
            ALLTRIM(m.progr +  ;
            '.PRG')
          @ 01, 01 SAY  ;
            'Linea del Error : ' +  ;
            LTRIM(STR(m.lin))
          @ 02, 01 SAY  ;
            'Mensaje: ' +  ;
            m.messg
          @ 03, 01 SAY 'Error : ' +  ;
            LTRIM(STR(m.code))
          ms = 'Active la Impresora y Pulse una Tecla ¢'
          @ 03, 01 SAY  ;
            SPACE((WCOLS() -  ;
            LEN(ms) - 6) / 2) +  ;
            SPACE(3)
          @ 03, COL() SAY ms
          ms = 'Presione Esc para Cancelar la Impresi¢n'
          @ 04, 01 SAY  ;
            SPACE((WCOLS() -  ;
            LEN(ms) - 6) / 2) +  ;
            SPACE(3)
          @ 04, COL() SAY ms
          SET ESCAPE OFF
          WAIT ''
          IF LASTKEY() = 27
               CLOSE DATABASES
               RELEASE WINDOW  ;
                       msgerr,  ;
                       record1,  ;
                       a1, a2, a3,  ;
                       _r
               RELEASE WINDOW all
               RELEASE WINDOW
               RELEASE WINDOW
               RELEASE WINDOW
               RELEASE WINDOW
               RELEASE WINDOW
               RELEASE WINDOW
               RETURN TO MASTER
          ENDIF
          RELEASE WINDOW msgerr
          SET DEVICE TO PRINTER
          RETRY
     CASE m.code = 108
          RETRY
     CASE m.code = 1707
          RETRY
     CASE m.code = 109
          RETRY
     OTHERWISE
          ACTIVATE WINDOW msgerr
          @ 00, 01 SAY  ;
            'Programa: ' +  ;
            ALLTRIM(m.progr +  ;
            '.PRG')
          @ 01, 01 SAY  ;
            'Linea del Error : ' +  ;
            LTRIM(STR(m.lin))
          @ 02, 01 SAY  ;
            'Mensaje: ' +  ;
            m.messg
          @ 03, 01 SAY 'Error : ' +  ;
            LTRIM(STR(m.code))
          ms = 'Anote la Operaci¢n y Consulte con (SEGUNDO GONZALEZ)'
          @ 04, 01 SAY  ;
            SPACE((WCOLS() -  ;
            LEN(ms) - 6) / 2) +  ;
            SPACE(3)
          @ 04, COL() SAY ms
          @ 05, 01 SAY  ;
            'Telef.: (086)-626602 '
          SET ESCAPE OFF
          WAIT ''
ENDCASE
RELEASE WINDOW msgerr, record1,  ;
        a1, a2, a3, _r
RELEASE WINDOW
RELEASE WINDOW
RELEASE WINDOW
RELEASE WINDOW
RELEASE WINDOW
RELEASE WINDOW
to = 0
RETURN TO MASTER
*
FUNCTION string
PARAMETER stri
stri = stri + ' '
RETURN TRANSFORM(stri, '@R' +  ;
       REPLICATE('X ',  ;
       LEN(stri)))
*
FUNCTION stringv
PARAMETER stri
letra1 = 'ABCDEFGHIJKLMN¥OPQRSTUVWXYZ0123456789.-, '
letra2 = 'ÚÂ¿ÄÉË»ÍÕÑ¸ÖÒ·ÃÅ´³ÌÎ¹ºÆØµÇ×¶ÀÁÙÈÊ¼ÔÏ¾ÓÐ½±'
stri = ALLTRIM(stri)
FOR i = 1 TO LEN(stri)
     x = SUBSTR(stri, i, 1)
     IF i = 1
          stri1 = SUBSTR(letra1,  ;
                  AT(x, letra2),  ;
                  1)
     ELSE
          stri1 = stri1 +  ;
                  SUBSTR(letra1,  ;
                  AT(x, letra2),  ;
                  1)
     ENDIF
ENDFOR
RETURN stri1
*
FUNCTION stringr
PARAMETER stri
letra2 = 'ABCDEFGHIJKLMN¥OPQRSTUVWXYZ0123456789.-, '
letra1 = 'ÚÂ¿ÄÉË»ÍÕÑ¸ÖÒ·ÃÅ´³ÌÎ¹ºÆØµÇ×¶ÀÁÙÈÊ¼ÔÏ¾ÓÐ½±'
stri = ALLTRIM(stri)
FOR i = 1 TO LEN(stri)
     x = SUBSTR(stri, i, 1)
     IF i = 1
          stri1 = SUBSTR(letra1,  ;
                  AT(x, letra2),  ;
                  1)
     ELSE
          stri1 = stri1 +  ;
                  SUBSTR(letra1,  ;
                  AT(x, letra2),  ;
                  1)
     ENDIF
ENDFOR
RETURN stri1
*
FUNCTION help
DEFINE WINDOW xhelp FROM 09, 05  ;
       TO 19, 75 SHADOW COLOR  ;
       SCHEME 5
MOVE WINDOW xhelp CENTER
ACTIVATE WINDOW xhelp
xz1 = ' Sistema Integral de Control Academico de Notas'
xz2 = 'Copyright (C) Dise¤o de Sistemas (SISCONOT, C.A) - 1996'
xz3 = 'Versi¢n 1.00'
xz4 = 'Reservados todos los Derechos'
xz5 = 'Licencia Autorizada a ' +  ;
      l_nombre
xz6 = 'Toda  Utilizaci¢n de  este sistema que no este'
xz7 = 'Expresamente autorizada , est  prohibida '
@ 00, 0 SAY SPACE(((WCOLS() -  ;
  LEN(xz1) - 6) / 2) + 3) + xz1
@ 01, 0 SAY SPACE(((WCOLS() -  ;
  LEN(xz2) - 6) / 2) + 3) + xz2
@ 02, 0 SAY SPACE(((WCOLS() -  ;
  LEN(xz3) - 6) / 2) + 3) + xz3
@ 03, 0 SAY SPACE(((WCOLS() -  ;
  LEN(xz4) - 6) / 2) + 3) + xz4
@ 04, 0 SAY SPACE(((WCOLS() -  ;
  LEN(xz5) - 6) / 2) + 3) + xz5
@ 05, 0 SAY SPACE(((WCOLS() -  ;
  LEN(xz6) - 6) / 2) + 3) + xz6
@ 06, 0 SAY SPACE(((WCOLS() -  ;
  LEN(xz7) - 6) / 2) + 3) + xz7
xr = '[   OK   ]'
@ 07, 0 TO 07, WCOLS() DOUBLE
@ 08, 0 SAY SPACE(((WCOLS() -  ;
  LEN(xr) - 6) / 2) + 3)
@ 08, COL() GET xr COLOR ,W+/B 
CLEAR GETS
atx = INKEY(10, 'H')
RELEASE WINDOW xhelp
RETURN .T.
*
PROCEDURE phelp
PARAMETER xpopup
SET SYSMENU ON
= help()
DO CASE
     CASE ALLTRIM(UPPER(xpopup)) $  ;
          'TABLAS BASICAS'
          HELP þ tablas
     CASE ALLTRIM(UPPER(xpopup)) $  ;
          'AL£MNOS/NOTAS'
          HELP þ alumnos
     CASE ALLTRIM(UPPER(xpopup)) $  ;
          'REPORTES'
          HELP þ procesos
ENDCASE
SET SYSMENU OFF
RETURN
*
FUNCTION clave
DEFINE WINDOW acceso FROM 10, 20  ;
       TO 15, 60 SHADOW NONE  ;
       COLOR SCHEME 5
ACTIVATE WINDOW acceso
@ 00, 00 TO WROWS() - 1, WCOLS()
xyz = 'Clave de Acceso'
lk = SPACE(((WCOLS() - LEN(xyz) -  ;
     6) / 2) + 3)
@ 01, LEN(lk) SAY xyz
@ 02, 00 SAY 'Ã' + REPLICATE('Ä',  ;
  WCOLS() - 2) + '´'
SET ESCAPE OFF
x_swt1 = 0
x_con = 0
DO WHILE .T.
     x_x = 1
     x_col = 25
     x_clave = SPACE(1)
     x_c = 1
     pio = 1
     DO WHILE x_x<=6
          x_tecla = SPACE(1)
          @ 03, 03 SAY  ;
            'Introduzca la Clave: '
          tecla = 0
          SET CONSOLE OFF
          DO WHILE tecla=0
               tecla = INKEY('H')
          ENDDO
          SET CONSOLE ON
          x_tecla = CHR(tecla)
          IF LASTKEY() = 27
               pio = 2
               EXIT
          ENDIF
          IF LASTKEY() = 13
               EXIT
          ENDIF
          @ 03, x_col SAY '*'
          x_clave = x_clave +  ;
                    x_tecla
          x_x = x_x + 1
          x_col = x_col + 1
     ENDDO
     IF pio = 2
          EXIT
     ENDIF
     x_clave = ALLTRIM(x_clave)
     IF UPPER(ALLTRIM(x_clave)) =  ;
        'ACTMX'
          x_swt1 = 1
          EXIT
     ELSE
          ?? CHR(7)
          @ 04, 02 SAY '®® '  ;
            COLOR W+/RB* 
          @ 04, 08 SAY  ;
            'Clave de Acceso Denegada'
          @ 04, COL() + 4 SAY  ;
            ' ¯¯' COLOR W+/RB* 
          SET CURSOR OFF
          WAIT ''
          @ 04, 02 SAY  ;
            SPACE(WCOLS() - 3)
          @ 03, 25 SAY SPACE(8)
          x_con = x_con + 1
          IF x_con > 3
               x_swt1 = 0
               EXIT
          ELSE
               SET CURSOR ON
               LOOP
          ENDIF
     ENDIF
ENDDO
IF pio = 2
     x_swt1 = 0
ENDIF
RELEASE WINDOW acceso
RETURN x_swt1
*
FUNCTION acceso
DEFINE WINDOW acceso FROM 10, 20  ;
       TO 15, 60 SHADOW NONE  ;
       COLOR SCHEME 5
ACTIVATE WINDOW acceso
@ 00, 00 TO WROWS() - 1, WCOLS()  ;
  COLOR GR+/W 
xyz = 'Clave de Acceso'
lk = SPACE(((WCOLS() - LEN(xyz) -  ;
     6) / 2) + 3)
@ 01, LEN(lk) SAY xyz
@ 02, 00 SAY 'Ã' COLOR GR+/W 
@ 02, COL() SAY REPLICATE('Ä',  ;
  WCOLS() - 2)
@ 02, COL() SAY '´' COLOR GR+/W 
SET ESCAPE OFF
x_swt1 = 0
x_con = 0
x_x = 1
x_col = 25
x_clave = SPACE(1)
x_c = 1
pio = 1
DO WHILE x_x<=5
     x_tecla = SPACE(1)
     @ 03, 03 SAY  ;
       'Introduzca la Clave: '
     tecla = 0
     SET CONSOLE OFF
     DO WHILE tecla=0
          tecla = INKEY('H')
     ENDDO
     SET CONSOLE ON
     IF LASTKEY() = 13
          IF x_x = 1
               LOOP
          ENDIF
     ENDIF
     IF LASTKEY() = 27
          pio = 2
          EXIT
     ENDIF
     IF tecla < 0
          pio = 2
          EXIT
     ENDIF
     x_tecla = CHR(tecla)
     @ 03, x_col SAY '*'
     IF LASTKEY() = 13
          EXIT
     ENDIF
     IF x_x = 1
          x_clave = x_tecla
     ELSE
          x_clave = x_clave +  ;
                    x_tecla
     ENDIF
     x_x = x_x + 1
     x_col = x_col + 1
ENDDO
IF pio = 2
     x_clave = 'FALSO'
ENDIF
RELEASE WINDOW acceso
RETURN ALLTRIM(x_clave)
*
FUNCTION clave1
SET ESCAPE OFF
@ 03, 24 SAY '²²²²²'
x_swt1 = 0
x_con = 0
x_x = 1
x_col = 24
x_clave = SPACE(1)
x_c = 1
pio = 1
DO WHILE x_x<=5
     x_tecla = SPACE(1)
     tecla = 0
     SET CONSOLE OFF
     DO WHILE tecla=0
          tecla = INKEY('H')
     ENDDO
     SET CONSOLE ON
     IF LASTKEY() = 13
          IF x_x = 1
               LOOP
          ENDIF
     ENDIF
     IF LASTKEY() = 27
          LOOP
     ENDIF
     x_tecla = CHR(tecla)
     @ 03, x_col SAY CHR(tecla)
     IF LASTKEY() = 13
          EXIT
     ENDIF
     IF x_x = 1
          x_clave = x_tecla
     ELSE
          x_clave = x_clave +  ;
                    x_tecla
     ENDIF
     x_x = x_x + 1
     x_col = x_col + 1
ENDDO
m.clave = x_clave
KEYBOARD CHR(13)
RETURN .T.
*
PROCEDURE calculador
STORE 0 TO _CALCMEM
SET SYSMENU ON
ACTIVATE WINDOW calculator
CLEAR GETS
SET SYSMENU OFF
RETURN
*
PROCEDURE calendario
ACTIVATE WINDOW calendar
CLEAR GETS
RETURN
*
FUNCTION printer
DEFINE WINDOW printer FROM 16, 19  ;
       TO 20, 64 SHADOW COLOR  ;
       SCHEME 5
ACTIVATE WINDOW printer
swt = 0
SET ESCAPE OFF
DO WHILE .T.
     msg = 'Prepare la Impresora y Coloquela en Linea'
     xyz = SPACE(((WCOLS() -  ;
           LEN(msg) - 6) / 2) +  ;
           3)
     @ 00, LEN(xyz) SAY msg
     msg = 'Salida Por Impresora'
     xyz = SPACE(((WCOLS() -  ;
           LEN(msg) - 6) / 2) +  ;
           3)
     @ 01, LEN(xyz) SAY msg
     msg = 'ESC = Cancelar ³ ENTER = Imprimir'
     xyz = SPACE(((WCOLS() -  ;
           LEN(msg) - 6) / 2) +  ;
           3)
     @ 02, LEN(xyz) SAY msg
     te = 0
     DO WHILE te<>13 .AND. te<>27
          te = INKEY('H')
     ENDDO
     IF te = 13
          x_ok = SYS(13)
          IF x_ok = 'OFFLINE'
               CLEAR
               SET BELL TO 1700, 4
               ?? CHR(7)
               @ 00, 00 SAY  ;
                 SPACE(((WCOLS() -  ;
                 LEN('Error') -  ;
                 6) / 2) + 3)
               @ 00, COL() SAY  ;
                 'Error'
               msg = 'Impresora No Activada'
               xyz = SPACE(((WCOLS() -  ;
                     LEN(msg) -  ;
                     6) / 2) +  ;
                     3)
               @ 01, LEN(xyz) SAY  ;
                 msg
               msg = 'Presione Cualquier Tecla Para continuar'
               xyz = SPACE(((WCOLS() -  ;
                     LEN(msg) -  ;
                     6) / 2) +  ;
                     3)
               @ 02, LEN(xyz) SAY  ;
                 msg
               a = INKEY(7, 'h')
               SET BELL OFF
               CLEAR
               LOOP
          ELSE
               swt = 1
               EXIT
          ENDIF
     ELSE
          swt = 2
          EXIT
     ENDIF
ENDDO
SET ESCAPE OFF
RELEASE WINDOW printer
RETURN swt
*
PROCEDURE bbackup
SAVE SCREEN TO jole
DEFINE WINDOW backup FROM 09, 12  ;
       TO 14, 63 SHADOW COLOR  ;
       SCHEME 5
ACTIVATE WINDOW backup
DO WHILE .T.
     unid = 'A'
     jk1 = '*-------- Respaldo del Sistema ---------*'
     @ 0, 0 SAY SPACE(((WCOLS() -  ;
       LEN(jk1) - 6) / 2) + 3) +  ;
       jk1
     @ 1, 1 SAY  ;
       'Unidad donde se hara el Respaldo: '  ;
       GET unid PICTURE  ;
       '@!M A,B,C'
     READ
     IF LASTKEY() = 27
          RELEASE WINDOW
          CLOSE DATABASES
          RETURN
     ENDIF
     IF unid $ 'AB'
          @ 2, 2 SAY  ;
            'Inserte un Diskette formateado en la unidad ' +  ;
            unid + ':'
     ENDIF
     jk2 = 'Presione Cualquier Tecla para iniciar el Respaldo'
     @ 3, 0 SAY SPACE(((WCOLS() -  ;
       LEN(jk2) - 6) / 2) + 3) +  ;
       jk2
     SAVE SCREEN TO xgf
     atx = 0
     SET CONSOLE OFF
     DO WHILE atx=0
          atx = INKEY('H')
     ENDDO
     SET CONSOLE ON
     IF LASTKEY() = 27
          p = salida()
          IF p = 2
               RELEASE WINDOW  ;
                       backup
               CLOSE DATABASES
               RELEASE WINDOW
          ENDIF
     ENDIF
     EXIT
ENDDO
RELEASE WINDOW backup
HIDE POPUP ALL
@ 00, 00 FILL TO 24, 79 COLOR W+/ ;
  B 
?? CHR(7)
IF unid = 'A'
     RUN pkzip -ex& a:respaldo.zip *.dbf
ENDIF
IF unid = 'B'
     RUN pkzip -ex& b:respaldo.zip *.dbf
ENDIF
IF unid = 'C'
     RUN pkzip -ex& respaldo.zip *.dbf
ENDIF
RESTORE SCREEN FROM jole
RETURN
*
PROCEDURE brestore
SAVE SCREEN TO xbackup
DEFINE WINDOW backup FROM 09, 05  ;
       TO 14, 75 SHADOW COLOR  ;
       SCHEME 5
ACTIVATE WINDOW backup
DO WHILE .T.
     unid = 'A'
     jk1 = '*------ Restaura desde del Respaldo-------*'
     @ 0, 0 SAY SPACE(((WCOLS() -  ;
       LEN(jk1) - 6) / 2) + 3) +  ;
       jk1
     @ 1, 1 SAY  ;
       'Unidad desde donde se Restaurar : '  ;
       GET unid PICTURE  ;
       '@!M A,B,C'
     READ
     IF LASTKEY() = 27
          RELEASE WINDOW
          CLOSE DATABASES
          RETURN
     ENDIF
     IF unid $ 'AB'
          @ 2, 2 SAY  ;
            'Inserte Diskette que continene el Respaldo en la unidad ' +  ;
            unid + ':'
     ENDIF
     jk2 = 'Presione Cualquier Tecla para iniciar el Restore:'
     @ 3, 0 SAY SPACE(((WCOLS() -  ;
       LEN(jk2) - 6) / 2) + 3) +  ;
       jk2
     SAVE SCREEN TO xgf
     atx = 0
     SET CONSOLE OFF
     DO WHILE atx=0
          atx = INKEY('HM')
     ENDDO
     SET CONSOLE ON
     IF LASTKEY() = 27
          p = salida()
          IF p = 2
               RELEASE WINDOW  ;
                       backup
               CLOSE DATABASES
               RELEASE WINDOW
          ENDIF
     ENDIF
     EXIT
ENDDO
?? CHR(7)
RELEASE WINDOW backup
HIDE POPUP ALL
@ 00, 00 FILL TO 24, 79 COLOR W+/ ;
  B 
IF unid = 'A'
     RUN pkunzip a:respaldo.zip  ;
         c:\sistema
ENDIF
IF unid = 'B'
     RUN pkunzip b:respaldo.zip  ;
         c:\sistema
ENDIF
IF unid = 'C'
     RUN pkunzip respaldo.zip
ENDIF
RESTORE SCREEN FROM xbackup
RETURN
*
FUNCTION salida
DEFINE WINDOW xsalida FROM 15, 20  ;
       TO 19, 60 SHADOW COLOR  ;
       SCHEME 7
ACTIVATE WINDOW xsalida
msg = 'Desea Continuar con el proceso'
xcont = 1
ycol1 = SPACE(((WCOLS() -  ;
        LEN(msg) - 6) / 2) + 3)
@ 01, LEN(ycol1) SAY msg COLOR  ;
  SCHEME 7
@ 02, 03 GET xcont SIZE 2, 13, 8  ;
  FUNCTION  ;
  '*H \!\[ Si ];\!\[ No ]' COLOR  ;
  SCHEME 7
READ CYCLE OBJECT 2
DO CASE
     CASE xcont = 1
          a = 1
     CASE xcont = 2
          a = 2
     OTHERWISE
          a = 1
ENDCASE
RELEASE WINDOW xsalida
RETURN a
*
FUNCTION acepta
PARAMETER xmen
SAVE SCREEN TO tsalva
DEFINE WINDOW acp FROM 18, 20 TO  ;
       21, 60 SHADOW COLOR SCHEME  ;
       7
ACTIVATE WINDOW acp
xcont = 1
@ 00, 00 SAY SPACE(((WCOLS() -  ;
  LEN(xmen) - 6) / 2) + 3) + xmen  ;
  COLOR SCHEME 7
@ 01, 03 GET xcont SIZE 2, 13, 8  ;
  FUNCTION  ;
  '*H \!\[ Si ];\!\[ No ]' COLOR  ;
  SCHEME 7
READ CYCLE OBJECT 1
IF LASTKEY() = 27
     xcont = 2
ENDIF
DO CASE
     CASE xcont = 1
          a = 1
     CASE xcont = 2
          a = 2
     OTHERWISE
          a = 1
ENDCASE
RELEASE WINDOW acp
RESTORE SCREEN FROM tsalva
RETURN a
*
FUNCTION acepta1
PARAMETER xmen
SAVE SCREEN TO tsalva
DEFINE WINDOW acp FROM 04, 05 TO  ;
       07, 45 SHADOW COLOR SCHEME  ;
       7
ACTIVATE WINDOW acp
xcont = 1
@ 00, 00 SAY SPACE(((WCOLS() -  ;
  LEN(xmen) - 6) / 2) + 3) + xmen  ;
  COLOR SCHEME 7
@ 01, 03 GET xcont SIZE 2, 13, 8  ;
  FUNCTION  ;
  '*H \!\[ Si ];\!\[ No ]' COLOR  ;
  SCHEME 7
READ CYCLE OBJECT 1
IF LASTKEY() = 27
     xcont = 2
ENDIF
DO CASE
     CASE xcont = 1
          a = 1
     CASE xcont = 2
          a = 2
     OTHERWISE
          a = 1
ENDCASE
RELEASE WINDOW acp
RESTORE SCREEN FROM tsalva
RETURN a
*
FUNCTION herror
PARAMETER msg
DEFINE WINDOW xherror FROM 03, 46  ;
       TO 06, 77 SHADOW COLOR  ;
       SCHEME 7
ACTIVATE WINDOW xherror
xyz = SPACE(((WCOLS() - LEN(msg) -  ;
      6) / 2) + 3)
@ 00, LEN(xyz) SAY msg COLOR GR+/ ;
  R,,
msg = 'Pulse una Tecla'
xyz = SPACE(((WCOLS() - LEN(msg) -  ;
      6) / 2) + 3)
@ 01, LEN(xyz) SAY msg COLOR GR+/ ;
  R,,
xyz = INKEY(5, 'H')
RELEASE WINDOW xherror
RETURN .T.
*
FUNCTION msgerro
PARAMETER errmsg
IF LEN(errmsg) > 30
     rmes = 'Error'
     xcol1 = SPACE(((80 -  ;
             LEN(errmsg) - 6) /  ;
             2) + 3)
     xcol2 = LEN(xcol1) +  ;
             LEN(errmsg)
     DEFINE WINDOW msg FROM 16,  ;
            LEN(xcol1) -  ;
            IIF(LEN(xcol1) < 5, 3,  ;
            5) TO 20, xcol2 + 4  ;
            SHADOW COLOR SCHEME  ;
            7
     ACTIVATE WINDOW msg
     @ 01, 00 SAY '®®' COLOR GR+/ ;
       R*,,
     @ 01, WCOLS() - 2 SAY '¯¯'  ;
       COLOR GR+/R*,,
     ycol1 = SPACE(((WCOLS() -  ;
             LEN(rmes) - 6) / 2) +  ;
             3)
     ycol2 = LEN(ycol1) +  ;
             LEN(rmes)
     @ 00, LEN(ycol1) SAY rmes  ;
       COLOR GR+/R* 
     xyz = SPACE(((WCOLS() -  ;
           LEN(errmsg) - 6) / 2) +  ;
           3)
     @ 01, LEN(xyz) SAY errmsg
     errmsg = 'Pulse una tecla para Continuar'
     xcol1 = SPACE(((WCOLS() -  ;
             LEN(errmsg) - 6) /  ;
             2) + 3)
     xcol2 = LEN(xcol1) +  ;
             LEN(errmsg)
     @ 02, LEN(xcol1) SAY errmsg  ;
       COLOR GR+/R,,
ELSE
     rmes = 'Error'
     rtu = 'Pulse cualquier tecla para continuar'
     xcol1 = SPACE(((80 -  ;
             LEN(rtu) - 6) / 2) +  ;
             3)
     xcol2 = LEN(xcol1) +  ;
             LEN(rtu)
     DEFINE WINDOW msg FROM 16,  ;
            LEN(xcol1) -  ;
            IIF(LEN(xcol1) < 5, 3,  ;
            5) TO 20, xcol2 + 4  ;
            SHADOW COLOR SCHEME  ;
            7
     ACTIVATE WINDOW msg
     @ 01, 00 SAY '®®' COLOR GR+/ ;
       R*,,
     @ 01, WCOLS() - 2 SAY '¯¯'  ;
       COLOR GR+/R*,,
     ycol1 = SPACE(((WCOLS() -  ;
             LEN(rmes) - 6) / 2) +  ;
             3)
     ycol2 = LEN(ycol1) +  ;
             LEN(rmes)
     @ 00, LEN(ycol1) SAY rmes  ;
       COLOR GR+/R* 
     xcol1 = SPACE(((WCOLS() -  ;
             LEN(errmsg) - 6) /  ;
             2) + 3)
     xcol2 = LEN(xcol1) +  ;
             LEN(errmsg)
     @ 01, LEN(xcol1) SAY errmsg  ;
       COLOR GR+/R,,
     errmsg = 'Pulse una tecla para Continuar'
     xcol1 = SPACE(((WCOLS() -  ;
             LEN(errmsg) - 6) /  ;
             2) + 3)
     xcol2 = LEN(xcol1) +  ;
             LEN(errmsg)
     @ 02, LEN(xcol1) SAY errmsg
ENDIF
asd = 0
DO WHILE asd=0
     asd = INKEY('H')
ENDDO
RELEASE WINDOW msg
RETURN .T.
*
FUNCTION fecha
PARAMETER xfil, xcol
SET CENTURY ON
no = DOW(DATE())
DO CASE
     CASE no = 1
          fecha = 'DOM'
     CASE no = 2
          fecha = 'LUN'
     CASE no = 3
          fecha = 'MAR'
     CASE no = 4
          fecha = 'MIE'
     CASE no = 5
          fecha = 'JUE'
     CASE no = 6
          fecha = 'VIE'
     CASE no = 7
          fecha = 'SAB'
ENDCASE
@ xfil, xcol SAY fecha + '  ' +  ;
  DTOC(DATE()) COLOR GR+/R 
RETURN .T.
*
FUNCTION meses
PARAMETER xfecha
IF EMPTY(xfecha)
     xfecha = DATE()
ENDIF
yfecha = CMONTH(xfecha)
DO CASE
     CASE yfecha = 'January' .OR.  ;
          yfecha = 'Enero'
          xfecha = 'Enero'
     CASE yfecha = 'February'  ;
          .OR. yfecha =  ;
          'Febrero'
          xfecha = 'Febrero'
     CASE yfecha = 'March' .OR.  ;
          yfecha = 'Marzo'
          xfecha = 'Marzo'
     CASE yfecha = 'April' .OR.  ;
          yfecha = 'Abril'
          xfecha = 'Abril'
     CASE yfecha = 'May' .OR.  ;
          yfecha = 'Mayo'
          xfecha = 'Mayo'
     CASE yfecha = 'June' .OR.  ;
          yfecha = 'Junio'
          xfecha = 'Junio'
     CASE yfecha = 'July' .OR.  ;
          yfecha = 'Julio'
          xfecha = 'Julio'
     CASE yfecha = 'August' .OR.  ;
          yfecha = 'Agosto'
          xfecha = 'Agosto'
     CASE yfecha = 'September'  ;
          .OR. yfecha =  ;
          'Septiembre'
          xfecha = 'Septiembre'
     CASE yfecha = 'October' .OR.  ;
          yfecha = 'Octubre'
          xfecha = 'Octubre'
     CASE yfecha = 'November'  ;
          .OR. yfecha =  ;
          'Noviembre'
          xfecha = 'Noviembre'
     CASE yfecha = 'December'  ;
          .OR. yfecha =  ;
          'Diciembre'
          xfecha = 'Diciembre'
     OTHERWISE
          yfecha = CMONTH(xfecha)
ENDCASE
RETURN xfecha
*
FUNCTION semana
PARAMETER x_fecha
IF EMPTY(x_fecha)
     x_fecha = DATE()
ENDIF
SET CENTURY ON
no = DOW(x_fecha)
DO CASE
     CASE no = 1
          fecha = 'Domingo'
     CASE no = 2
          fecha = 'Lunes'
     CASE no = 3
          fecha = 'Martes'
     CASE no = 4
          fecha = 'Miercoles'
     CASE no = 5
          fecha = 'Jueves'
     CASE no = 6
          fecha = 'Viernes'
     CASE no = 7
          fecha = 'Sabado'
ENDCASE
RETURN fecha
*
FUNCTION semana1
PARAMETER x_fecha
IF EMPTY(x_fecha)
     x_fecha = DATE()
ENDIF
SET CENTURY ON
no = DOW(x_fecha)
DO CASE
     CASE no = 1
          fecha = 7
     CASE no = 2
          fecha = 1
     CASE no = 3
          fecha = 2
     CASE no = 4
          fecha = 3
     CASE no = 5
          fecha = 4
     CASE no = 6
          fecha = 5
     CASE no = 7
          fecha = 6
ENDCASE
RETURN fecha
*
FUNCTION fox_acp
PARAMETER wzalrtmess
PRIVATE alrtbtn
m.alrtbtn = 2
DEFINE WINDOW _qec1ij2t7 AT 0, 0  ;
       SIZE 4, 50 FONT  ;
       'MS Sans Serif', 10 STYLE  ;
       'B' FLOAT NOCLOSE SHADOW  ;
       TITLE WTITLE() NOMINIMIZE  ;
       COLOR SCHEME 5
MOVE WINDOW _qec1ij2t7 CENTER
ACTIVATE WINDOW NOSHOW _qec1ij2t7
@ 1, (50 - TXTWIDTH(wzalrtmess)) /  ;
  2 SAY wzalrtmess FONT  ;
  'MS Sans Serif', 10 STYLE 'B'
@ 2, 0 TO 2, WCOLS() DOUBLE
@ 3, 14 GET m.alrtbtn FONT  ;
  'MS Sans Serif', 8 STYLE 'B'  ;
  SIZE 1.769 , 8.667 , 1.333   ;
  PICTURE  ;
  '@*HT \<Aceptar;\?\!\<Cancelar'
ACTIVATE WINDOW _qec1ij2t7
READ CYCLE MODAL
RELEASE WINDOW _qec1ij2t7
RETURN m.alrtbtn
*
FUNCTION boxfield
PARAMETER popdbf, varname
m.savearea = SELECT()
in_area = SELECT()
filtro = FILTER()
SELECT 0
USE EXCLUSIVE (popdbf) AGAIN
i = 1
varname = UPPER(varname)
tag_found = .F.
DO WHILE  .NOT. EMPTY(TAG(i))  ;
   .AND.  .NOT. tag_found
     tag_found = (TAG(i) ==  ;
                 varname)
     IF  .NOT. tag_found
          i = i + 1
     ENDIF
ENDDO
IF  .NOT. tag_found
     DO WHILE .T.
          IF FLOCK()
               index on (&varname) tag;
(varname)
               UNLOCK
               EXIT
          ENDIF
     ENDDO
ENDIF
set filter to &filtro
SET ORDER TO (varname)
GOTO TOP
DO CASE
     CASE COL() < SCOLS() / 2
          s_col = SCOLS() / 2 + 1
          e_col = SCOLS() - 1
          s_row = 5
          e_row = SROWS() - 3
     CASE COL() >= SCOLS() / 2
          s_col = 2
          e_col = SCOLS() / 2 - 1
          s_row = 5
          e_row = SROWS() - 3
ENDCASE
DEFINE WINDOW dbfwin FROM s_row,  ;
       s_col TO e_row, e_col GROW  ;
       FLOAT CLOSE ZOOM SHADOW  ;
       TITLE PROPER(varname) +  ;
       ' BoxField' MINIMIZE COLOR  ;
       W+/BG,W+/B,GR+/W,GR+/W,GR+/ ;
       N,GR+/N,W+/B,W+/B 
ON KEY LABEL enter keyboard chr(23)
BROWSE NOAPPEND NOEDIT NODELETE  ;
       WINDOW dbfwin
ON KEY LABEL enter
IF LASTKEY() <> 27
     retval = &varname
ELSE
     retval = &varname
ENDIF
RELEASE WINDOW dbfwin
SELECT (m.savearea)
RETURN retval
*
FUNCTION boxfieldt
PARAMETER popdbf, varname
PRIVATE ALL
m.savearea = SELECT()
in_area = SELECT()
SELECT 0
USE EXCLUSIVE (popdbf) AGAIN
i = 1
varname = UPPER(varname)
tag_found = .F.
DO WHILE  .NOT. EMPTY(TAG(i))  ;
   .AND.  .NOT. tag_found
     tag_found = (TAG(i) ==  ;
                 varname)
     IF  .NOT. tag_found
          i = i + 1
     ENDIF
ENDDO
IF  .NOT. tag_found
     DO WHILE .T.
          IF FLOCK()
               index on (&varname) tag;
(varname)
               UNLOCK
               EXIT
          ENDIF
     ENDDO
ENDIF
SET ORDER TO (varname)
GOTO TOP
DO CASE
     CASE COL() < SCOLS() / 2
          s_col = SCOLS() / 2 + 1
          e_col = SCOLS() - 1
          s_row = 5
          e_row = SROWS() - 3
     CASE COL() >= SCOLS() / 2
          s_col = 2
          e_col = SCOLS() / 2 - 1
          s_row = 5
          e_row = SROWS() - 3
ENDCASE
SET FILTER TO practica = 'ET'
DEFINE WINDOW dbfwin FROM s_row,  ;
       s_col TO e_row, e_col GROW  ;
       FLOAT CLOSE ZOOM SHADOW  ;
       TITLE PROPER(varname) +  ;
       ' BoxField' MINIMIZE COLOR  ;
       W+/BG,W+/B,GR+/W,GR+/W,GR+/ ;
       N,GR+/N,W+/B,W+/B 
ON KEY LABEL enter keyboard chr(23)
BROWSE NOAPPEND NOEDIT NODELETE  ;
       WINDOW dbfwin
ON KEY LABEL enter
IF LASTKEY() <> 27
     retval = &varname
ELSE
     retval = &varname
ENDIF
RELEASE WINDOW dbfwin
SET FILTER TO
SELECT (m.savearea)
RETURN retval
*
FUNCTION boxfield2
PARAMETER popdbf, varname
PRIVATE ALL
m.savearea = SELECT()
in_area = SELECT()
SELECT 0
USE EXCLUSIVE (popdbf) AGAIN
i = 1
varname = UPPER(varname)
tag_found = .F.
DO WHILE  .NOT. EMPTY(TAG(i))  ;
   .AND.  .NOT. tag_found
     tag_found = (TAG(i) ==  ;
                 varname)
     IF  .NOT. tag_found
          i = i + 1
     ENDIF
ENDDO
IF  .NOT. tag_found
     DO WHILE .T.
          IF FLOCK()
               index on (&varname) tag;
(varname)
               UNLOCK
               EXIT
          ENDIF
     ENDDO
ENDIF
SET ORDER TO (varname)
GOTO TOP
DO CASE
     CASE COL() < SCOLS() / 2
          s_col = SCOLS() / 2 + 1
          e_col = SCOLS() - 1
          s_row = 5
          e_row = SROWS() - 3
     CASE COL() >= SCOLS() / 2
          s_col = 2
          e_col = SCOLS() / 2 - 1
          s_row = 5
          e_row = SROWS() - 3
ENDCASE
SET FILTER TO practica = 'N '
DEFINE WINDOW dbfwin FROM s_row,  ;
       s_col TO e_row, e_col GROW  ;
       FLOAT CLOSE ZOOM SHADOW  ;
       TITLE PROPER(varname) +  ;
       ' BoxField' MINIMIZE COLOR  ;
       W+/BG,W+/B,GR+/W,GR+/W,GR+/ ;
       N,GR+/N,W+/B,W+/B 
ON KEY LABEL enter keyboard chr(23)
BROWSE NOAPPEND NOEDIT NODELETE  ;
       WINDOW dbfwin
ON KEY LABEL enter
IF LASTKEY() <> 27
     retval = &varname
ELSE
     retval = &varname
ENDIF
RELEASE WINDOW dbfwin
SET FILTER TO
SELECT (m.savearea)
RETURN retval
*
FUNCTION espere
DEFINE WINDOW a5 FROM 17, 20 TO  ;
       20, 60 SHADOW COLOR SCHEME  ;
       5
ACTIVATE WINDOW a5
ex = 'Por favor espere un momento '
ey = 'Estoy Procesando'
@ 0, 0 SAY SPACE((WCOLS() -  ;
  LEN(ex) - 6) / 2) + SPACE(3) +  ;
  ex COLOR W+/BG 
@ 1, 0 SAY SPACE((WCOLS() -  ;
  LEN(ey) - 6) / 2) + '..' COLOR  ;
  W+/BG* 
@ 1, COL() SAY ey COLOR W+/BG 
@ 1, COL() SAY '..' COLOR W+/BG* 
RETURN .T.
*
FUNCTION relock1
PARAMETER m.prompt
PRIVATE m.therm_x1, m.therm_y1,  ;
        m.therm_x2, m.therm_y2
m.therm_x1 = 16
m.therm_y1 = 15
m.therm_x2 = 22
m.therm_y2 = 64
DEFINE WINDOW thermomete FROM  ;
       m.therm_x1, m.therm_y1 TO  ;
       m.therm_x2, m.therm_y2  ;
       FLOAT SHADOW DOUBLE COLOR  ;
       SCHEME 5
ACTIVATE WINDOW thermomete
m.g_thermwid = 46
@ 0, 3 SAY m.prompt
@ 1, 13 SAY 'Porcentaje: '
@ 2, 1 TO 4, m.g_thermwid
SHOW WINDOW TOP thermomete
RETURN .T.
*
FUNCTION relock2
PARAMETER percent
PRIVATE m.blocks, nblocks
m.g_thermwid = 46
nblocks = (percent / 100) *  ;
          (m.g_thermwid - 4)
@ 1, 25 SAY percent PICTURE  ;
  '9999'
@ 1, COL() SAY '%'
@ 3, 3 SAY REPLICATE('Û',  ;
  nblocks)
RETURN .T.
*
FUNCTION relock3
RELEASE WINDOW thermomete
RETURN .T.
*
FUNCTION letra
PARAMETER mnota
DO CASE
     CASE EMPTY(mnota)
          mletra = '*********'
     CASE mnota = 'P '
          mletra = 'PENDIENTE'
     CASE mnota = '- '
          mletra = 'EXONERADA'
     CASE VAL(mnota) < 10
          mletra = 'PENDIENTE'
     CASE VAL(mnota) = 10
          mletra = 'DIEZ'
     CASE VAL(mnota) = 11
          mletra = 'ONCE'
     CASE VAL(mnota) = 12
          mletra = 'DOCE'
     CASE VAL(mnota) = 13
          mletra = 'TRECE'
     CASE VAL(mnota) = 14
          mletra = 'CATORCE'
     CASE VAL(mnota) = 15
          mletra = 'QUINCE'
     CASE VAL(mnota) = 16
          mletra = 'DIECISEIS'
     CASE VAL(mnota) = 17
          mletra = 'DIECISIETE'
     CASE VAL(mnota) = 18
          mletra = 'DIECIOCHO'
     CASE VAL(mnota) = 19
          mletra = 'DIECINUEVE'
     CASE VAL(mnota) = 20
          mletra = 'VEINTE'
ENDCASE
RETURN mletra
*
FUNCTION letras
PARAMETER xmonto
PRIVATE ALL
unidad0 = ' '
unidad1 = 'UN'
unidad2 = 'DOS'
unidad3 = 'TRES'
unidad4 = 'CUATRO'
unidad5 = 'CINCO'
unidad6 = 'SEIS'
unidad7 = 'SIETE'
unidad8 = 'OCHO'
unidad9 = 'NUEVE'
decena00 = ' '
decena10 = 'DIEZ'
decena11 = 'ONCE'
decena12 = 'DOCE'
decena13 = 'TRECE'
decena14 = 'CATORCE'
decena15 = 'QUINCE'
decena16 = 'DIECISEIS'
decena17 = 'DIECISIETE'
decena18 = 'DIECIOCHO'
decena19 = 'DIECINUEVE'
decena20 = 'VEINTE'
decena30 = 'TREINTA'
decena40 = 'CUARENTA'
decena50 = 'CINCUENTA'
decena60 = 'SESENTA'
decena70 = 'SETENTA'
decena80 = 'OCHENTA'
decena90 = 'NOVENTA'
centena0 = ' '
centena1 = 'CIEN'
centena2 = 'DOSCIENTOS'
centena3 = 'TRECIENTOS'
centena4 = 'CUATROCIENTOS'
centena5 = 'QUINIENTOS'
centena6 = 'SEISCIENTOS'
centena7 = 'SETECIENTOS'
centena8 = 'OCHOCIENTOS'
centena9 = 'NOVECIENTOS'
IF xmonto < 0
     xmonto = xmonto * -1
ENDIF
xdecimal = LTRIM(STR(xmonto, 12,  ;
           2))
xdecimal = RIGHT(xdecimal, 2)
numero = INT(xmonto)
xnumero = LTRIM(STR(numero, 9))
xnumero = IIF(LEN(xnumero) < 9,  ;
          REPLICATE('0', 9 -  ;
          LEN(xnumero)) + xnumero,  ;
          xnumero)
STORE ' ' TO let1
FOR i = 1 TO 3
     lk = LTRIM(STR(i))
     b&lk=substr(xnumero,i,1)
ENDFOR
IF b3 = '0' .AND. b2 = '0'
     t = b2 + b3
     let1=centena&b1
ELSE
     IF b1 = '1'
          centena1 = 'CIENTO'
     ENDIF
     let1=centena&b1
     IF VAL(b2 + b3) >= 20
          t = b2 + '0'
          let1=alltrim(let1)+' '+trim(decena&t)
          let1=let1+iif(b3<>'0',' Y '+unidad&b3,unidad&b3)
     ELSE
          IF VAL(b2 + b3) < 10
               let1=let1+' '+unidad&b3
          ELSE
               t = b2 + b3
               let1=let1+' '+trim(decena&t)
          ENDIF
     ENDIF
ENDIF
IF VAL(b1 + b2 + b3) > 0
     let1 = let1 + ' MILLONES'
ENDIF
centena1 = 'CIEN'
xnumero = SUBSTR(xnumero, 4, 6)
FOR i = 1 TO 3
     lk = LTRIM(STR(i))
     b&lk=substr(xnumero,i,1)
ENDFOR
IF b3 = '0' .AND. b2 = '0'
     t = b2 + b3
     let1=let1+' '+centena&b1
ELSE
     IF b1 = '1'
          centena1 = 'CIENTO'
     ENDIF
     let1=let1+' '+centena&b1
     IF VAL(b2 + b3) >= 20
          t = b2 + '0'
          let1=let1+' '+trim(decena&t)
          let1=let1+iif(b3<>'0',' Y '+unidad&b3,unidad&b3)
     ELSE
          IF VAL(b2 + b3) < 10
               let1=let1+' '+unidad&b3
          ELSE
               t = b2 + b3
               let1=let1+' '+trim(decena&t)
          ENDIF
     ENDIF
ENDIF
IF VAL(b1 + b2 + b3) > 0
     let1 = let1 + ' MIL'
ENDIF
centena1 = 'CIEN'
x = 1
FOR i = 4 TO 6
     lk = LTRIM(STR(x))
     b&lk=substr(xnumero,i,1)
     x = x + 1
ENDFOR
IF b3 = '0' .AND. b2 = '0'
     t = b2 + b3
     let1=let1+' '+centena&b1
ELSE
     IF b1 = '1'
          centena1 = 'CIENTO'
     ENDIF
     let1=let1+' '+centena&b1
     IF VAL(b2 + b3) >= 20
          t = b2 + '0'
          let1=let1+' '+trim(decena&t)
          let1=let1+iif(b3<>'0',' Y '+unidad&b3,unidad&b3)
     ELSE
          IF VAL(b2 + b3) < 10
               let1=let1+' '+unidad&b3
          ELSE
               t = b2 + b3
               let1=let1+' '+trim(decena&t)
          ENDIF
     ENDIF
ENDIF
let1 = let1 + ' BOLIVARES CON ' +  ;
       xdecimal + '/100'
RETURN let1
*
FUNCTION busqueda
DEFINE WINDOW busqueda FROM 05,  ;
       05 TO 10, 75 FLOAT CLOSE  ;
       SHADOW TITLE  ;
       'Busqueda de Alumnos'  ;
       MINIMIZE COLOR SCHEME 5
ACTIVATE WINDOW busqueda
DEFINE WINDOW wzlocate FROM 1, 1  ;
       TO 20, 60 FONT  ;
       'MS Sans Serif', 8 GROW  ;
       FLOAT CLOSE ZOOM SHADOW  ;
       SYSTEM COLOR W+/BG,W+/B,GR+/ ;
       W,GR+/W,GR+/N,GR+/N,W+/B,W+/ ;
       B 
MOVE WINDOW wzlocate CENTER
m_cedula = SPACE(12)
m_nombre = SPACE(40)
m.curarea = SELECT()
m.arch = SYS(2015)
SELECT (m.curarea)
SET ORDER TO nombres
copy struc to &m.arch
GOTO TOP
DO WHILE .T.
     @ 01, 01 SAY  ;
       'Apellidos/Nombres: ' GET  ;
       m_nombre PICTURE '@!'
     @ 02, 01 TO 02, WCOLS()  ;
       DOUBLE
     @ 03, 28 GET m.choice  ;
       DEFAULT 1 SIZE 1, 10, 5  ;
       PICTURE '@*HN \<Ok' COLOR  ;
       SCHEME 5
     READ
     IF  .NOT. EMPTY(m_nombre)
          lon = LEN(ALLTRIM(m_nombre))
          bus = ALLTRIM(m_nombre)
          SELECT 18
          use &m.arch
          SELECT (m.curarea)
          SEEK bus
          IF  .NOT. FOUND()
               = msgerro( ;
                 'No existen Datos con esta operaci¢n' ;
                 )
               LOOP
          ENDIF
          DO WHILE  .NOT. EOF()
               SELECT (m.curarea)
               IF  .NOT. bus $  ;
                   SUBSTR(nombres,  ;
                   1, lon)
                    EXIT
               ENDIF
               SCATTER MEMO  ;
                       MEMVAR
               SELECT 18
               DO WHILE .T.
                    IF FLOCK()
                         APPEND BLANK
                         GATHER MEMVAR  ;
                                MEMO
                         UNLOCK
                         EXIT
                    ENDIF
               ENDDO
               SELECT (m.curarea)
               SKIP
          ENDDO
          SELECT 18
          GOTO TOP
          ON KEY LABEL enter keyboard;
chr(23)
          BROWSE NOMENU NOEDIT  ;
                 NODELETE WINDOW  ;
                 wzlocate TITLE  ;
                 'Buscar Alumnos'  ;
                 NOREFRESH
          ON KEY LABEL enter
          m_cedula = cedula
     ELSE
          ON KEY LABEL enter keyboard;
chr(23)
          BROWSE NOMENU NOEDIT  ;
                 NODELETE WINDOW  ;
                 wzlocate TITLE  ;
                 'Buscar Alumnos'  ;
                 NOREFRESH
          ON KEY LABEL enter
          m_cedula = cedula
     ENDIF
     EXIT
ENDDO
RELEASE WINDOW wzlocate, busqueda
SELECT 18
USE
SELECT (m.curarea)
SET ORDER TO cedula
GOTO TOP
RETURN m_cedula
*
FUNCTION busqued
DEFINE WINDOW busqueda FROM 05,  ;
       05 TO 10, 75 FLOAT CLOSE  ;
       SHADOW TITLE  ;
       'Busqueda de Alumnos'  ;
       MINIMIZE COLOR SCHEME 5
ACTIVATE WINDOW busqueda
DEFINE WINDOW wzlocate FROM 1, 1  ;
       TO 20, 60 FONT  ;
       'MS Sans Serif', 8 GROW  ;
       FLOAT CLOSE ZOOM SHADOW  ;
       SYSTEM COLOR W+/BG,W+/B,GR+/ ;
       W,GR+/W,GR+/N,GR+/N,W+/B,W+/ ;
       B 
MOVE WINDOW wzlocate CENTER
m_cedula = SPACE(12)
m_nombre = SPACE(40)
GOTO TOP
DO WHILE .T.
     @ 01, 01 SAY  ;
       'Apellidos/Nombres: ' GET  ;
       m_nombre PICTURE '@!'
     @ 02, 01 TO 02, WCOLS()  ;
       DOUBLE
     @ 03, 28 GET m.choice  ;
       DEFAULT 1 SIZE 1, 10, 5  ;
       PICTURE '@*HN \<Ok' COLOR  ;
       SCHEME 5
     READ
     IF  .NOT. EMPTY(m_nombre)
          lon = LEN(ALLTRIM(m_nombre))
          bus = ALLTRIM(m_nombre)
          ON KEY LABEL enter keyboard;
chr(23)
          BROWSE FOR bus $  ;
                 SUBSTR(da.nombres,  ;
                 1, lon) FIELDS  ;
                 ma.ced,  ;
                 da.nombres  ;
                 NOMENU NOEDIT  ;
                 NODELETE WINDOW  ;
                 wzlocate TITLE  ;
                 'Buscar Alumnos'
          ON KEY LABEL enter
          m_cedula = ma.ced
     ELSE
          ON KEY LABEL enter keyboard;
chr(23)
          BROWSE FIELDS ma.ced,  ;
                 da.nombres  ;
                 NOMENU NOEDIT  ;
                 NODELETE WINDOW  ;
                 wzlocate TITLE  ;
                 'Buscar Alumnos'
          ON KEY LABEL enter
          m_cedula = ma.ced
     ENDIF
     EXIT
ENDDO
RELEASE WINDOW wzlocate, busqueda
SET ORDER TO cedcur
GOTO TOP
RETURN m_cedula
*
FUNCTION tiempo
PARAMETER xfechai, xfechap
ddias = DAY(xfechap)
dmes = MONTH(xfechap)
dano = YEAR(xfechap)
tdias = DAY(xfechai)
tmes = MONTH(xfechai)
tano = YEAR(xfechai)
STORE 0 TO gdias, gmes, gano
IF ddias < tdias
     ddias = ddias + 30
     dmes = dmes - 1
ENDIF
gdias = ddias - tdias
IF dmes < tmes
     dmes = dmes + 12
     dano = dano - 1
ENDIF
gmes = dmes - tmes
gano = dano - tano
xmeses = (gano * 12) + gmes
RETURN STR(gdias, 2) + '-' +  ;
       STR(gmes, 2) + '-' +  ;
       STR(gano, 2)
*
FUNCTION asigrept
PARAMETER y_mat, y_asi
y_asi = ALLTRIM(y_asi)
yi = 1
DO WHILE yi<=LEN(y_asi)
     yyy = SUBSTR(y_asi, yi, 2)
     IF yyy = y_mat
          RETURN .T.
     ENDIF
     yi = yi + 2
ENDDO
RETURN .F.
*
