CLOSE DATABASES
SET DELETED ON
SET TALK OFF
SET SAFETY OFF
SET BELL OFF
SET MESSAGE TO 23 CENTER
SET BELL TO 1700, 4
SELECT 1
USE datos
SET ORDER TO cedula
SELECT 3
use mapo&lapmat
SET ORDER TO sec
DEFINE WINDOW sclr FROM 11, 10 TO  ;
       18, 70 FLOAT NOCLOSE  ;
       SHADOW TITLE  ;
       'Emisi¢n de Carnet Estudiantil (CARNET1)'  ;
       NOMINIMIZE COLOR SCHEME 5
ACTIVATE WINDOW sclr
m.fecha = DATE()
m.codigo = SPACE(1)
m.seccion = 'A'
m.curso = '32011'
valida1 = 'ABCDEFGHIJKLMNOPQRSTUVXYZ'
SAVE SCREEN TO jol
raya = CHR(27) + CHR(45) + '1'
noraya = CHR(27) + CHR(45) + '0'
ng = CHR(27) + CHR(69)
ngn = CHR(27) + CHR(70)
ano1 = SUBSTR(lapmat, 1, 2)
ano2 = SUBSTR(lapmat, 3, 2)
ch1 = LTRIM(STR(YEAR(CTOD( ;
      '01/01/' + ano1))))
ch2 = RIGHT(lapmat, 2)
swtp = 0
scrip = CHR(27) + CHR(83) + '1'
nscrip = CHR(27) + CHR(84)
cpi12 = CHR(27) + CHR(77)
cpi10 = CHR(27) + CHR(80)
italic = CHR(27) + CHR(52)
nitalic = CHR(27) + CHR(53)
DO WHILE .T.
     RESTORE SCREEN FROM jol
     m.lap = '1er Lapso'
     @ 1, 1 SAY  ;
       'Fecha Expedici¢n: ' GET  ;
       m.fecha PICTURE '@!'
     @ 2, 1 SAY  ;
       'Menci¢n         : ' GET  ;
       m.curso DEFAULT '31022'  ;
       PICTURE  ;
       '@!M 32011,31018,31019,30000,31000'
     @ 3, 1 SAY  ;
       'Secci¢n         : '
     @ 4, 1 TO 4, WCOLS() DOUBLE
     @ 5, 1 SAY  ;
       'Presione Esc para cancelar la operaci¢n'
     READ
     IF LASTKEY() = 27
          RELEASE WINDOW sclr
          CLOSE DATABASES
          RETURN
     ENDIF
     DO CASE
          CASE m.curso = '32011'
               m.mencion = 'BASICA'
          CASE m.curso = '31018'
               m.mencion = 'CIENCIAS'
          CASE m.curso = '31019'
               m.mencion = 'HUMANIDADES'
          CASE m.curso = '30000'
               m.mencion = 'PRE-ESCOLAR'
          CASE m.curso = '31000'
               m.mencion = 'EDUC. BASICA'
     ENDCASE
     @ 2, 28 SAY m.mencion
     IF m.curso = '32011'
          @ 3, 1 SAY  ;
            'Secci¢n         : '  ;
            GET m.codigo DEFAULT  ;
            '7' PICTURE  ;
            '@!M 7,8,9'
     ELSE
          IF m.curso = '31000'
               @ 3, 1 SAY  ;
                 'Secci¢n         : '  ;
                 GET m.codigo  ;
                 DEFAULT '1'  ;
                 PICTURE  ;
                 '@!M 1,2,3,4,5,6'
          ELSE
               @ 3, 1 SAY  ;
                 'Secci¢n         : '  ;
                 GET m.codigo  ;
                 DEFAULT '1'  ;
                 PICTURE  ;
                 '@!M 1,2'
          ENDIF
     ENDIF
     @ 3, COL() GET m.seccion  ;
       DEFAULT 'A' PICTURE '@!'  ;
       VALID m.seccion $ valida1  ;
       ERROR  ;
       'La Secci¢n es invalida'
     READ
     IF LASTKEY() = 27
          RELEASE WINDOW sclr
          CLOSE DATABASES
          RETURN
     ENDIF
     SELECT 3
     SET ORDER TO sec
     xbusca = m.curso + m.codigo +  ;
              m.seccion
     IF  .NOT. SEEK(xbusca)
          = msgerro( ;
            'No existen alumnos asignados a esta secci¢n, Verifique' ;
            )
          LOOP
     ENDIF
     p = printer()
     IF p = 2
          RELEASE WINDOW sclr
          CLOSE DATABASES
          RETURN
     ENDIF
     SET DEVICE TO PRINTER
     @ 00, 00 SAY CHR(18) + ' '
     pos = 1
     m.secx = m.codigo +  ;
              m.seccion
     DO WHILE  .NOT. EOF()
          SELECT 3
          IF status = 'X'
               SELECT 3
               SKIP
               LOOP
          ENDIF
          IF cur <> m.curso
               SELECT 3
               SKIP
               LOOP
          ENDIF
          IF seccionr <> m.secx
               EXIT
          ENDIF
          xcedula = ced
          SELECT 1
          IF  .NOT. SEEK(xcedula)
               SELECT 3
               SKIP
               LOOP
          ENDIF
          m.nombres = nombres
          m.apel = SUBSTR(nombres,  ;
                   1, AT(',',  ;
                   nombres, 1) -  ;
                   1)
          m.nomb = SUBSTR(nombres,  ;
                   AT(',',  ;
                   nombres, 1) +  ;
                   1,  ;
                   LEN(nombres))
          m.apel = ALLTRIM(m.apel)
          m.nomb = ALLTRIM(m.nomb)
          xnombre = TRIM(nombres)
          m.sexo = sexo
          m.fechanac = fechanac
          m.lugarnac = lugarnac
          m.cedrep = rep_legal
          m.nomrep = ALLTRIM(representa)
          SELECT 3
          m.codigo = SUBSTR(seccionr,  ;
                     1, 1)
          m.seccci = SUBSTR(seccionr,  ;
                     2, 1)
          m.curso = cur
          DO CASE
               CASE pos = 1
                    @ 00, 00 SAY  ;
                      ' ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿'
                    @ 01, 00 SAY  ;
                      ' ³ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»³ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»³'
                    @ 02, 00 SAY  ;
                      ' ³º                          ÚÄÄÄÄÄÄÄÄÄ¿º³º                          ÚÄÄÄÄÄÄÄÄÄ¿º³'
                    @ 02, 03 SAY  ;
                      ng + italic +  ;
                      'UNIDAD EDUCATIVA' +  ;
                      nitalic +  ;
                      ngn
                    @ 02, 53 SAY  ;
                      ng + cpi12 +  ;
                      'CARNET DEL REPRESENTANTE' +  ;
                      cpi10 +  ;
                      ngn
                    @ 03, 00 SAY  ;
                      ' ³º                          ³         ³º³º                          ³         ³º³'
                    @ 03, 03 SAY  ;
                      ng + italic +  ;
                      'COLEGIO "LOEFLING"' +  ;
                      nitalic +  ;
                      ngn
                    @ 04, 00 SAY  ;
                      ' ³º                          ³         ³º³º                          ³         ³º³'
                    @ 04, 03 SAY  ;
                      ng + italic +  ;
                      'O.M.A.D  PD00600602' +  ;
                      nitalic +  ;
                      ngn
                    @ 05, 00 SAY  ;
                      ' ³º                          ³         ³º³º                          ³         ³º³'
                    @ 06, 00 SAY  ;
                      ' ³º                          ³         ³º³º                          ³         ³º³'
                    @ 06, 12 SAY  ;
                      m.apel  ;
                      PICTURE  ;
                      '@!s17'
                    @ 06, 03 SAY  ;
                      ng + cpi12 +  ;
                      'APELLIDOS:' +  ;
                      cpi10 +  ;
                      ngn
                    @ 07, 00 SAY  ;
                      ' ³º                          ³         ³º³º   __________________     ³         ³º³'
                    @ 07, 12 SAY  ;
                      m.nomb  ;
                      PICTURE  ;
                      '@!s17'
                    @ 07, 03 SAY  ;
                      ng + cpi12 +  ;
                      'NOMBRES  :' +  ;
                      cpi10 +  ;
                      ngn
                    @ 08, 00 SAY  ;
                      ' ³º                          ÀÄÄÄÄÄÄÄÄÄÙº³º                          ÀÄÄÄÄÄÄÄÄÄÙº³'
                    @ 08, 12 SAY  ;
                      xcedula  ;
                      PICTURE  ;
                      '@!'
                    @ 08, 03 SAY  ;
                      ng + cpi12 +  ;
                      'C.I. N§  :' +  ;
                      cpi10 +  ;
                      ngn
                    @ 08, 60 SAY  ;
                      CHR(15) +  ;
                      scrip +  ;
                      'DIRECTOR ACADEMICO' +  ;
                      nscrip +  ;
                      CHR(18) +  ;
                      CHR(20)
                    @ 09, 00 SAY  ;
                      ' ³ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶³ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶³'
                    @ 10, 00 SAY  ;
                      ' ³º                                     º³º                                     º³'
                    @ 10, 09 SAY  ;
                      m.codigo  ;
                      PICTURE  ;
                      '!'
                    @ 10, 16 SAY  ;
                      m.seccci  ;
                      PICTURE  ;
                      '!'
                    @ 10, 25 SAY  ;
                      m.mencion  ;
                      PICTURE  ;
                      '@!'
                    @ 10, 52 SAY  ;
                      m.nomrep  ;
                      PICTURE  ;
                      '@!'
                    @ 10, 03 SAY  ;
                      ng + cpi12 +  ;
                      'GRADO:   SECC.:   MENCION:                      NOMBRES  :' +  ;
                      cpi10 +  ;
                      ngn
                    @ 11, 00 SAY  ;
                      ' ³º                                     º³º                                     º³'
                    @ 11, 14 SAY  ;
                      ch1 + '-' +  ;
                      ch2 PICTURE  ;
                      '@!'
                    @ 11, 52 SAY  ;
                      m.cedrep  ;
                      PICTURE  ;
                      '@!'
                    @ 11, 03 SAY  ;
                      ng + cpi12 +  ;
                      'A¥O ESCOLAR:                                    C.I N§.  :' +  ;
                      cpi10 +  ;
                      ngn
                    @ 12, 00 SAY  ;
                      ' ³º                                     º³º                                     º³'
                    @ 12, 52 SAY  ;
                      l_director
                    @ 12, 02 SAY  ;
                      ng + cpi12 +  ;
                      '                                                 DIRECTOR :' +  ;
                      cpi10 +  ;
                      ngn
                    @ 13, 00 SAY  ;
                      ' ³ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼³ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼³'
                    @ 14, 00 SAY  ;
                      ' ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´'
               CASE pos = 2
                    @ 15, 00 SAY  ;
                      ' ³ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»³ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»³'
                    @ 16, 00 SAY  ;
                      ' ³º                          ÚÄÄÄÄÄÄÄÄÄ¿º³º                          ÚÄÄÄÄÄÄÄÄÄ¿º³'
                    @ 16, 03 SAY  ;
                      ng + italic +  ;
                      'UNIDAD EDUCATIVA' +  ;
                      nitalic +  ;
                      ngn
                    @ 16, 53 SAY  ;
                      ng + cpi12 +  ;
                      'CARNET DEL REPRESENTANTE' +  ;
                      cpi10 +  ;
                      ngn
                    @ 17, 00 SAY  ;
                      ' ³º                          ³         ³º³º                          ³         ³º³'
                    @ 17, 03 SAY  ;
                      ng + italic +  ;
                      'COLEGIO "LOEFLING"' +  ;
                      nitalic +  ;
                      ngn
                    @ 18, 00 SAY  ;
                      ' ³º                          ³         ³º³º                          ³         ³º³'
                    @ 18, 03 SAY  ;
                      ng + italic +  ;
                      'O.M.A.D  PD00600602' +  ;
                      nitalic +  ;
                      ngn
                    @ 19, 00 SAY  ;
                      ' ³º                          ³         ³º³º                          ³         ³º³'
                    @ 20, 00 SAY  ;
                      ' ³º                          ³         ³º³º                          ³         ³º³'
                    @ 20, 12 SAY  ;
                      m.apel  ;
                      PICTURE  ;
                      '@!s17'
                    @ 20, 03 SAY  ;
                      ng + cpi12 +  ;
                      'APELLIDOS:' +  ;
                      cpi10 +  ;
                      ngn
                    @ 21, 00 SAY  ;
                      ' ³º                          ³         ³º³º   __________________     ³         ³º³'
                    @ 21, 12 SAY  ;
                      m.nomb  ;
                      PICTURE  ;
                      '@!s17'
                    @ 21, 03 SAY  ;
                      ng + cpi12 +  ;
                      'NOMBRES  :' +  ;
                      cpi10 +  ;
                      ngn
                    @ 22, 00 SAY  ;
                      ' ³º                          ÀÄÄÄÄÄÄÄÄÄÙº³º                          ÀÄÄÄÄÄÄÄÄÄÙº³'
                    @ 22, 12 SAY  ;
                      xcedula  ;
                      PICTURE  ;
                      '@!'
                    @ 22, 03 SAY  ;
                      ng + cpi12 +  ;
                      'C.I. N§  :' +  ;
                      cpi10 +  ;
                      ngn
                    @ 22, 60 SAY  ;
                      CHR(15) +  ;
                      scrip +  ;
                      'DIRECTOR ACADEMICO' +  ;
                      nscrip +  ;
                      CHR(18) +  ;
                      CHR(20)
                    @ 23, 00 SAY  ;
                      ' ³ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶³ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶³'
                    @ 24, 00 SAY  ;
                      ' ³º                                     º³º                                     º³'
                    @ 24, 09 SAY  ;
                      m.codigo  ;
                      PICTURE  ;
                      '!'
                    @ 24, 16 SAY  ;
                      m.seccci  ;
                      PICTURE  ;
                      '!'
                    @ 24, 25 SAY  ;
                      m.mencion  ;
                      PICTURE  ;
                      '@!'
                    @ 24, 52 SAY  ;
                      m.nomrep  ;
                      PICTURE  ;
                      '@!'
                    @ 24, 03 SAY  ;
                      ng + cpi12 +  ;
                      'GRADO:   SECC.:   MENCION:                      NOMBRES  :' +  ;
                      cpi10 +  ;
                      ngn
                    @ 25, 00 SAY  ;
                      ' ³º                                     º³º                                     º³'
                    @ 25, 14 SAY  ;
                      ch1 + '-' +  ;
                      ch2 PICTURE  ;
                      '@!'
                    @ 25, 52 SAY  ;
                      m.cedrep  ;
                      PICTURE  ;
                      '@!'
                    @ 25, 03 SAY  ;
                      ng + cpi12 +  ;
                      'A¥O ESCOLAR:                                    C.I N§.  :' +  ;
                      cpi10 +  ;
                      ngn
                    @ 26, 00 SAY  ;
                      ' ³º                                     º³º                                     º³'
                    @ 26, 52 SAY  ;
                      l_director
                    @ 26, 02 SAY  ;
                      ng + cpi12 +  ;
                      '                                                 DIRECTOR :' +  ;
                      cpi10 +  ;
                      ngn
                    @ 27, 00 SAY  ;
                      ' ³ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼³ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼³'
                    @ 28, 00 SAY  ;
                      ' ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´'
               CASE pos = 3
                    @ 29, 00 SAY  ;
                      ' ³ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»³ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»³'
                    @ 30, 00 SAY  ;
                      ' ³º                          ÚÄÄÄÄÄÄÄÄÄ¿º³º                          ÚÄÄÄÄÄÄÄÄÄ¿º³'
                    @ 30, 03 SAY  ;
                      ng + italic +  ;
                      'UNIDAD EDUCATIVA' +  ;
                      nitalic +  ;
                      ngn
                    @ 30, 53 SAY  ;
                      ng + cpi12 +  ;
                      'CARNET DEL REPRESENTANTE' +  ;
                      cpi10 +  ;
                      ngn
                    @ 31, 00 SAY  ;
                      ' ³º                          ³         ³º³º                          ³         ³º³'
                    @ 31, 03 SAY  ;
                      ng + italic +  ;
                      'COLEGIO "LOEFLING"' +  ;
                      nitalic +  ;
                      ngn
                    @ 32, 00 SAY  ;
                      ' ³º                          ³         ³º³º                          ³         ³º³'
                    @ 32, 03 SAY  ;
                      ng + italic +  ;
                      'O.M.A.D  PD00600602' +  ;
                      nitalic +  ;
                      ngn
                    @ 33, 00 SAY  ;
                      ' ³º                          ³         ³º³º                          ³         ³º³'
                    @ 34, 00 SAY  ;
                      ' ³º                          ³         ³º³º                          ³         ³º³'
                    @ 34, 12 SAY  ;
                      m.apel  ;
                      PICTURE  ;
                      '@!s17'
                    @ 34, 03 SAY  ;
                      ng + cpi12 +  ;
                      'APELLIDOS:' +  ;
                      cpi10 +  ;
                      ngn
                    @ 35, 00 SAY  ;
                      ' ³º                          ³         ³º³º   __________________     ³         ³º³'
                    @ 35, 12 SAY  ;
                      m.nomb  ;
                      PICTURE  ;
                      '@!s17'
                    @ 35, 03 SAY  ;
                      ng + cpi12 +  ;
                      'NOMBRES  :' +  ;
                      cpi10 +  ;
                      ngn
                    @ 36, 00 SAY  ;
                      ' ³º                          ÀÄÄÄÄÄÄÄÄÄÙº³º                          ÀÄÄÄÄÄÄÄÄÄÙº³'
                    @ 36, 12 SAY  ;
                      xcedula  ;
                      PICTURE  ;
                      '@!'
                    @ 36, 03 SAY  ;
                      ng + cpi12 +  ;
                      'C.I. N§  :' +  ;
                      cpi10 +  ;
                      ngn
                    @ 36, 60 SAY  ;
                      CHR(15) +  ;
                      scrip +  ;
                      'DIRECTOR ACADEMICO' +  ;
                      nscrip +  ;
                      CHR(18) +  ;
                      CHR(20)
                    @ 37, 00 SAY  ;
                      ' ³ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶³ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶³'
                    @ 38, 00 SAY  ;
                      ' ³º                                     º³º                                     º³'
                    @ 38, 09 SAY  ;
                      m.codigo  ;
                      PICTURE  ;
                      '!'
                    @ 38, 16 SAY  ;
                      m.seccci  ;
                      PICTURE  ;
                      '!'
                    @ 38, 25 SAY  ;
                      m.mencion  ;
                      PICTURE  ;
                      '@!'
                    @ 38, 52 SAY  ;
                      m.nomrep  ;
                      PICTURE  ;
                      '@!'
                    @ 38, 03 SAY  ;
                      ng + cpi12 +  ;
                      'GRADO:   SECC.:   MENCION:                      NOMBRES  :' +  ;
                      cpi10 +  ;
                      ngn
                    @ 39, 00 SAY  ;
                      ' ³º                                     º³º                                     º³'
                    @ 39, 14 SAY  ;
                      ch1 + '-' +  ;
                      ch2 PICTURE  ;
                      '@!'
                    @ 39, 52 SAY  ;
                      m.cedrep  ;
                      PICTURE  ;
                      '@!'
                    @ 39, 03 SAY  ;
                      ng + cpi12 +  ;
                      'A¥O ESCOLAR:                                    C.I N§.  :' +  ;
                      cpi10 +  ;
                      ngn
                    @ 40, 00 SAY  ;
                      ' ³º                                     º³º                                     º³'
                    @ 40, 52 SAY  ;
                      l_director
                    @ 40, 02 SAY  ;
                      ng + cpi12 +  ;
                      '                                                 DIRECTOR :' +  ;
                      cpi10 +  ;
                      ngn
                    @ 41, 00 SAY  ;
                      ' ³ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼³ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼³'
                    @ 42, 00 SAY  ;
                      ' ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´'
               CASE pos = 4
                    @ 43, 00 SAY  ;
                      ' ³ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»³ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»³'
                    @ 44, 00 SAY  ;
                      ' ³º                          ÚÄÄÄÄÄÄÄÄÄ¿º³º                          ÚÄÄÄÄÄÄÄÄÄ¿º³'
                    @ 44, 03 SAY  ;
                      ng + italic +  ;
                      'UNIDAD EDUCATIVA' +  ;
                      nitalic +  ;
                      ngn
                    @ 44, 53 SAY  ;
                      ng + cpi12 +  ;
                      'CARNET DEL REPRESENTANTE' +  ;
                      cpi10 +  ;
                      ngn
                    @ 45, 00 SAY  ;
                      ' ³º                          ³         ³º³º                          ³         ³º³'
                    @ 45, 03 SAY  ;
                      ng + italic +  ;
                      'COLEGIO "LOEFLING"' +  ;
                      nitalic +  ;
                      ngn
                    @ 46, 00 SAY  ;
                      ' ³º                          ³         ³º³º                          ³         ³º³'
                    @ 46, 03 SAY  ;
                      ng + italic +  ;
                      'O.M.A.D  PD00600602' +  ;
                      nitalic +  ;
                      ngn
                    @ 47, 00 SAY  ;
                      ' ³º                          ³         ³º³º                          ³         ³º³'
                    @ 48, 00 SAY  ;
                      ' ³º                          ³         ³º³º                          ³         ³º³'
                    @ 48, 12 SAY  ;
                      m.apel  ;
                      PICTURE  ;
                      '@!s17'
                    @ 48, 03 SAY  ;
                      ng + cpi12 +  ;
                      'APELLIDOS:' +  ;
                      cpi10 +  ;
                      ngn
                    @ 49, 00 SAY  ;
                      ' ³º                          ³         ³º³º   __________________     ³         ³º³'
                    @ 49, 12 SAY  ;
                      m.nomb  ;
                      PICTURE  ;
                      '@!s17'
                    @ 49, 03 SAY  ;
                      ng + cpi12 +  ;
                      'NOMBRES  :' +  ;
                      cpi10 +  ;
                      ngn
                    @ 50, 00 SAY  ;
                      ' ³º                          ÀÄÄÄÄÄÄÄÄÄÙº³º                          ÀÄÄÄÄÄÄÄÄÄÙº³'
                    @ 50, 12 SAY  ;
                      xcedula  ;
                      PICTURE  ;
                      '@!'
                    @ 50, 03 SAY  ;
                      ng + cpi12 +  ;
                      'C.I. N§  :' +  ;
                      cpi10 +  ;
                      ngn
                    @ 50, 60 SAY  ;
                      CHR(15) +  ;
                      scrip +  ;
                      'DIRECTOR ACADEMICO' +  ;
                      nscrip +  ;
                      CHR(18) +  ;
                      CHR(20)
                    @ 51, 00 SAY  ;
                      ' ³ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶³ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶³'
                    @ 52, 00 SAY  ;
                      ' ³º                                     º³º                                     º³'
                    @ 52, 09 SAY  ;
                      m.codigo  ;
                      PICTURE  ;
                      '!'
                    @ 52, 16 SAY  ;
                      m.seccci  ;
                      PICTURE  ;
                      '!'
                    @ 52, 25 SAY  ;
                      m.mencion  ;
                      PICTURE  ;
                      '@!'
                    @ 52, 52 SAY  ;
                      m.nomrep  ;
                      PICTURE  ;
                      '@!'
                    @ 52, 03 SAY  ;
                      ng + cpi12 +  ;
                      'GRADO:   SECC.:   MENCION:                      NOMBRES  :' +  ;
                      cpi10 +  ;
                      ngn
                    @ 53, 00 SAY  ;
                      ' ³º                                     º³º                                     º³'
                    @ 53, 14 SAY  ;
                      ch1 + '-' +  ;
                      ch2 PICTURE  ;
                      '@!'
                    @ 53, 52 SAY  ;
                      m.cedrep  ;
                      PICTURE  ;
                      '@!'
                    @ 53, 03 SAY  ;
                      ng + cpi12 +  ;
                      'A¥O ESCOLAR:                                    C.I N§.  :' +  ;
                      cpi10 +  ;
                      ngn
                    @ 54, 00 SAY  ;
                      ' ³º                                     º³º                                     º³'
                    @ 54, 52 SAY  ;
                      l_director
                    @ 54, 02 SAY  ;
                      ng + cpi12 +  ;
                      '                                                 DIRECTOR :' +  ;
                      cpi10 +  ;
                      ngn
                    @ 55, 00 SAY  ;
                      ' ³ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼³ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼³'
                    @ 56, 00 SAY  ;
                      ' ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ'
          ENDCASE
          pos = pos + 1
          IF pos > 4
               pos = 1
          ENDIF
          SELECT 3
          SKIP
     ENDDO
     SET DEVICE TO SCREEN
     xcedula = SPACE(12)
     m.swp = 0
     m.control = 0
     m.pote = 1
     m.nac = 'V'
     m.ced = SPACE(10)
     valida1 = 'ABCDEFGHIJKLMNOPQRSTUVXYZ'
     valida2 = 'ABCDEFGHIJKLMNOPQRSTUVXYZ*'
ENDDO
*
