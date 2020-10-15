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
SET ORDER TO cedcur
DEFINE WINDOW record1 FROM 06, 00  ;
       TO 14, 79 FLOAT NOCLOSE  ;
       SHADOW NOMINIMIZE COLOR  ;
       SCHEME 5
ACTIVATE WINDOW record1
xd = 'Emisi¢n de Carnet Estudiantil'
@ 0, 0 SAY SPACE(((WCOLS() -  ;
  LEN(xd) - 6) / 2) + 3) + xd
@ 01, 00 SAY  ;
  'ÕÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¸'
@ 02, 00 SAY  ;
  '³ C‚dula  :                Apellidos y Nombres :                            ³'
@ 03, 00 SAY  ;
  '³ Sexo:   Fecha Nac.:               Lugar Nac. :                            ³'
@ 04, 00 SAY  ;
  '³ C¢digo:          Secci¢n :       Fecha de Expendici¢n:                    ³'
@ 05, 00 SAY  ;
  'ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ'
ng = CHR(27) + CHR(69)
ngn = CHR(27) + CHR(70)
xcedula = SPACE(12)
m.swp = 0
m.control = 0
m.pote = 1
ON KEY LABEL f8 keybo chr(13)
ON KEY LABEL f9 keybo chr(13)
ON KEY LABEL tab do validar with varread()
m.nac = 'V'
m.ced = SPACE(10)
valida1 = 'ABCDEFGHIJKLMNOPQRSTUVXYZ'
valida2 = 'ABCDEFGHIJKLMNOPQRSTUVXYZ*'
SAVE SCREEN TO jo
m.fecha = DATE()
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
swpt = 1
DO WHILE .T.
     RESTORE SCREEN FROM jo
     @ 02, 12 GET m.nac PICTURE  ;
       '@!M V,E,D,R' MESSAGE  ;
       'ingrese la Nacionalidad del Alumno'
     @ 02, 13 SAY '-'
     @ 02, 14 GET m.ced PICTURE  ;
       '999999999' VALID  ;
       VAL(m.ced) >= 0 MESSAGE  ;
       'Ingrese el N£mero del Cedula del Alumno'  ;
       ERROR  ;
       'Ingrese al N£mero de Cedula'
     READ
     IF LASTKEY() = -8
          SELECT 1
          GOTO TOP
          xcedula = m.nac + '-' +  ;
                    m.ced
          xcedula = busqueda()
          m.nac = SUBSTR(xcedula,  ;
                  1, 1)
          m.ced = SUBSTR(xcedula,  ;
                  3, 10)
          LOOP
     ENDIF
     IF LASTKEY() = 27
          ON KEY LABEL tab
          RELEASE WINDOW record1
          CLOSE DATABASES
          RETURN
     ENDIF
     IF VAL(m.ced) <= 0
          = msgerro( ;
            'Ingrese el Nro. de Cedula' ;
            )
          LOOP
     ENDIF
     xcedula = m.nac + '-' +  ;
               m.ced
     SELECT 1
     SET ORDER TO cedula
     SEEK xcedula
     IF  .NOT. FOUND()
          = herror( ;
            'Cedula no Existente' ;
            )
          xcedula = m.nac + '-' +  ;
                    m.ced
          xcedula = busqueda()
          m.nac = SUBSTR(xcedula,  ;
                  1, 1)
          m.ced = SUBSTR(xcedula,  ;
                  3, 10)
          LOOP
     ENDIF
     ON KEY LABEL tab
     m.nombres = nombres
     xnombre = TRIM(nombres)
     m.sexo = sexo
     m.fechanac = fechanac
     m.lugarnac = lugarnac
     @ 02, 50 SAY m.nombres  ;
       PICTURE '@!S24'
     @ 03, 08 SAY m.sexo PICTURE  ;
       '!'
     @ 03, 22 SAY m.fechanac  ;
       PICTURE '@E'
     @ 03, 49 SAY m.lugarnac  ;
       PICTURE '@!s27'
     CLEAR GETS
     SELECT 3
     SET ORDER TO ced
     SEEK xcedula
     IF  .NOT. FOUND()
          = msgerro( ;
            'Este Alumno no tine matricula cargada para este curso' ;
            )
          LOOP
     ENDIF
     IF status = 'X'
          = msgerro( ;
            'Este Alumno fue egresado de la Matricula, verifique' ;
            )
     ENDIF
     m.curso = cur
     @ 04, 10 GET m.curso PICTURE  ;
       '@!'
     m.codigo = SUBSTR(seccionr,  ;
                1, 1)
     m.seccci = SUBSTR(seccionr,  ;
                2, 1)
     IF SUBSTR(m.curso, 1, 2) =  ;
        '32'
          @ 04, 29 GET m.codigo  ;
            DEFAULT '7' PICTURE  ;
            '@!M 7,8,9'
     ELSE
          @ 04, 29 GET m.codigo  ;
            DEFAULT '1' PICTURE  ;
            '@!M 1,2'
     ENDIF
     @ 04, COL() GET m.seccci  ;
       DEFAULT 'U' PICTURE '@!'  ;
       VALID m.seccci $ valida1  ;
       ERROR  ;
       'La Secci¢n es Invalida'
     CLEAR GETS
     @ 04, 56 GET m.fecha PICTURE  ;
       '@E'
     READ
     IF LASTKEY() = 27
          ON KEY LABEL tab
          RELEASE WINDOW record1
          CLOSE DATABASES
          RETURN
     ENDIF
     p = printer()
     IF p = 2
          RELEASE WINDOW sclr
          CLOSE DATABASES
          RETURN
     ENDIF
     SELECT 1
     m.nombres = nombres
     m.apel = SUBSTR(nombres, 1,  ;
              AT(',', nombres, 1) -  ;
              1)
     m.nomb = SUBSTR(nombres,  ;
              AT(',', nombres, 1) +  ;
              1, LEN(nombres))
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
     SET DEVICE TO PRINTER
     @ 00, 00 SAY CHR(18) +  ;
       CHR(20) + ' '
     @ 00, 00 SAY  ;
       ' ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿'
     @ 01, 00 SAY  ;
       ' ³ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»³ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»³'
     @ 02, 00 SAY  ;
       ' ³º                          ÚÄÄÄÄÄÄÄÄÄ¿º³º                          ÚÄÄÄÄÄÄÄÄÄ¿º³'
     @ 02, 03 SAY ng + italic +  ;
       'UNIDAD EDUCATIVA' +  ;
       nitalic + ngn
     @ 02, 53 SAY ng + cpi12 +  ;
       'CARNET DEL REPRESENTANTE' +  ;
       cpi10 + ngn
     @ 03, 00 SAY  ;
       ' ³º                          ³         ³º³º                          ³         ³º³'
     @ 03, 03 SAY ng + italic +  ;
       'COLEGIO "LOEFLING"' +  ;
       nitalic + ngn
     @ 04, 00 SAY  ;
       ' ³º                          ³         ³º³º                          ³         ³º³'
     @ 04, 03 SAY ng + italic +  ;
       'O.M.A.D  PD00600602' +  ;
       nitalic + ngn
     @ 05, 00 SAY  ;
       ' ³º                          ³         ³º³º                          ³         ³º³'
     @ 06, 00 SAY  ;
       ' ³º                          ³         ³º³º                          ³         ³º³'
     @ 06, 12 SAY m.apel PICTURE  ;
       '@!s17'
     @ 06, 03 SAY ng + cpi12 +  ;
       'APELLIDOS:' + cpi10 +  ;
       ngn
     @ 07, 00 SAY  ;
       ' ³º                          ³         ³º³º   __________________     ³         ³º³'
     @ 07, 12 SAY m.nomb PICTURE  ;
       '@!s17'
     @ 07, 03 SAY ng + cpi12 +  ;
       'NOMBRES  :' + cpi10 +  ;
       ngn
     @ 08, 00 SAY  ;
       ' ³º                          ÀÄÄÄÄÄÄÄÄÄÙº³º                          ÀÄÄÄÄÄÄÄÄÄÙº³'
     @ 08, 12 SAY xcedula PICTURE  ;
       '@!'
     @ 08, 03 SAY ng + cpi12 +  ;
       'C.I. N§  :' + cpi10 +  ;
       ngn
     @ 08, 60 SAY CHR(15) + scrip +  ;
       'DIRECTOR ACADEMICO' +  ;
       nscrip + CHR(18) +  ;
       CHR(20)
     @ 09, 00 SAY  ;
       ' ³ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶³ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶³'
     @ 10, 00 SAY  ;
       ' ³º                                     º³º                                     º³'
     @ 10, 09 SAY m.codigo  ;
       PICTURE '!'
     @ 10, 16 SAY m.seccci  ;
       PICTURE '!'
     @ 10, 25 SAY m.mencion  ;
       PICTURE '@!'
     @ 10, 52 SAY m.nomrep  ;
       PICTURE '@!'
     @ 10, 03 SAY ng + cpi12 +  ;
       'GRADO:   SECC.:   MENCION:                      NOMBRES  :' +  ;
       cpi10 + ngn
     @ 11, 00 SAY  ;
       ' ³º                                     º³º                                     º³'
     @ 11, 14 SAY ch1 + '-' + ch2  ;
       PICTURE '@!'
     @ 11, 52 SAY m.cedrep  ;
       PICTURE '@!'
     @ 11, 03 SAY ng + cpi12 +  ;
       'A¥O ESCOLAR:                                    C.I N§.  :' +  ;
       cpi10 + ngn
     @ 12, 00 SAY  ;
       ' ³º                                     º³º                                     º³'
     @ 12, 52 SAY l_director
     @ 12, 02 SAY ng + cpi12 +  ;
       '                                                 DIRECTOR :' +  ;
       cpi10 + ngn
     @ 13, 00 SAY  ;
       ' ³ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼³ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼³'
     @ 14, 00 SAY  ;
       ' ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ'
     @ 15, 00 SAY ' '
     SET DEVICE TO SCREEN
     xcedula = SPACE(12)
     ON KEY LABEL f8 keybo chr(13)
     ON KEY LABEL f9 keybo chr(13)
     ON KEY LABEL tab do validar with;
varread()
     m.swp = 0
     m.control = 0
     m.pote = 1
     m.nac = 'V'
     m.ced = SPACE(10)
     valida1 = 'ABCDEFGHIJKLMNOPQRSTUVXYZ'
     valida2 = 'ABCDEFGHIJKLMNOPQRSTUVXYZ*'
ENDDO
*
PROCEDURE validar
PARAMETER xvar
DO CASE
     CASE xvar = 'NAC' .OR. xvar =  ;
          'CED'
          ON KEY LABEL tab
          xcedula = m.nac + '-' +  ;
                    m.ced
          SELECT 1
          xcedula = busqueda()
          m.nac = SUBSTR(xcedula,  ;
                  1, 1)
          m.ced = SUBSTR(xcedula,  ;
                  3, 10)
          SHOW GET m.nac
          SHOW GET m.ced
ENDCASE
ON KEY LABEL tab do validar with varread()
RETURN
*
