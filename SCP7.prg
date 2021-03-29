CLOSE DATABASES
SET TALK OFF
SELECT 1
USE datos
SET ORDER TO cedula
SELECT 2
USE regpagos
SET ORDER TO cedula
SELECT 3
use mp&lapmat
SET ORDER TO cedcur
GOTO TOP
SCATTER BLANK MEMO MEMVAR FIELDS  ;
        ced, cur
SELECT 4
use ad&lapmat
SET ORDER TO cedcur
GOTO TOP
SCATTER BLANK MEMO MEMVAR FIELDS  ;
        ced, cur
DEFINE WINDOW a1 FROM 10, 10 TO  ;
       16, 70 GROW FLOAT CLOSE  ;
       ZOOM SHADOW TITLE  ;
       'Eliminaci¢n de Datos de Alumnos'  ;
       MINIMIZE COLOR SCHEME 5
ACTIVATE WINDOW a1
ON KEY LABEL f8 keybo chr(13)
ON KEY LABEL f9 keybo chr(13)
ON KEY LABEL tab do validar with varread()
m.nac = 'V'
m.ced = 0
matad = SPACE(2)
DO WHILE .T.
     matad = '00'
     CLEAR
     @ 3, 0 TO 3, WCOLS()
     @ 4, 1 SAY  ;
       'Presione Esc para cancelar la operaci¢n'
     @ 1, 1 SAY 'C‚dula : '
     @ 1, 11 GET m.nac PICTURE  ;
       '@!M V,E,C,R' MESSAGE  ;
       'ingrese la Nacionalidad del Alumno'
     @ 1, 12 SAY '-'
     @ 1, 13 GET m.ced PICTURE  ;
       '99999999' VALID m.ced >=  ;
       0 MESSAGE  ;
       'Ingrese el N£mero del Cedula del Alumno'  ;
       ERROR  ;
       'Ingrese al N£mero de Cedula'
     @ 2, 1 SAY 'Curso  : ' GET  ;
       m.cur PICTURE  ;
       '@!M 32012,31022,31023'
     READ
     IF LASTKEY() = 27 .OR.  ;
        EMPTY(m.ced)
          ON KEY LABEL tab
          CLOSE DATABASES
          RELEASE WINDOW a1
          RETURN
     ENDIF
     IF LASTKEY() = -8
          SELECT 1
          GOTO TOP
          m.ced = IIF(LEN(LTRIM(STR(m.ced,  ;
                  10))) < 10,  ;
                  LTRIM(STR(m.ced,  ;
                  10)) + SPACE(10 -  ;
                  LEN(LTRIM(STR(m.ced,  ;
                  10)))),  ;
                  LTRIM(STR(m.ced,  ;
                  10)))
          xcedula = m.nac + '-' +  ;
                    m.ced
          xcedula = busqueda()
          m.nac = SUBSTR(xcedula,  ;
                  1, 1)
          m.ced = VAL(SUBSTR(xcedula,  ;
                  3, 10))
          LOOP
     ENDIF
     IF m.ced <= 0
          = msgerro( ;
            'Ingrese el Nro. de Cedula' ;
            )
          LOOP
     ENDIF
     m.ced = IIF(LEN(LTRIM(STR(m.ced,  ;
             10))) < 10,  ;
             LTRIM(STR(m.ced,  ;
             10)) + SPACE(10 -  ;
             LEN(LTRIM(STR(m.ced,  ;
             10)))),  ;
             LTRIM(STR(m.ced,  ;
             10)))
     xcedula = m.nac + '-' +  ;
               m.ced
     SELECT 1
     SEEK xcedula
     IF  .NOT. FOUND()
          p = msgerro( ;
              'Este Alumno no esta registrado en el sistema' ;
              )
          m.ced = 0
          m.nac = ' '
          LOOP
     ENDIF
     SELECT 3
     SEEK xcedula + m.cur
     IF  .NOT. FOUND()
          p = msgerro( ;
              'No existe este alumno para este codigo' ;
              )
          m.ced = 0
          m.nac = ' '
          LOOP
     ENDIF
     SELECT 4
     SEEK xcedula + m.cur
     IF FOUND()
          matad = nromatad
     ENDIF
     SELECT 1
     @ 1, 25 SAY ALLTRIM(nombres)
     IF m.cur = '32012'
          @ 2, 25 SAY 'BASICA'
     ENDIF
     IF m.cur = '31022'
          @ 2, 25 SAY 'CIENCIAS'
     ENDIF
     IF m.cur = '31023'
          @ 2, 25 SAY  ;
            'HUMANIDADES'
     ENDIF
     SELECT 3
     p = acepta( ;
         'Esta seguro de Eliminar la Matricula?' ;
         )
     IF p = 1
          SELECT 4
          IF matad <> '00'
               = msgerro( ;
                 'Este Alumno Cursa Materias Adicionales' ;
                 )
               p = acepta( ;
                   '¨Desea Eliminar de Matricula Mat.Adic?' ;
                   )
               IF p = 1
                    SELECT 3
                    DELETE
                    SELECT 4
                    DELETE
               ELSE
                    EXIT
               ENDIF
          ELSE
               SELECT 3
               DELETE
          ENDIF
          p = acepta( ;
              'Desea Eliminar su Datos Personales?' ;
              )
          IF p = 1
               SELECT 1
               DELETE
          ENDIF
     ENDIF
     SELECT 3
     SCATTER BLANK MEMO MEMVAR  ;
             FIELDS ced, cur
     m.ced = 0
     m.nac = ' '
     SELECT 4
     SCATTER BLANK MEMO MEMVAR  ;
             FIELDS ced, cur
     m.ced = 0
     m.nac = ' '
ENDDO
CLOSE DATABASES
RELEASE WINDOW a1
RETURN
*
PROCEDURE validar
PARAMETER xvar
DO CASE
     CASE xvar = 'NAC' .OR. xvar =  ;
          'CED'
          ON KEY LABEL tab
          m.ced = IIF(LEN(LTRIM(STR(m.ced,  ;
                  10))) < 10,  ;
                  LTRIM(STR(m.ced,  ;
                  10)) + SPACE(10 -  ;
                  LEN(LTRIM(STR(m.ced,  ;
                  10)))),  ;
                  LTRIM(STR(m.ced,  ;
                  10)))
          xcedula = m.nac + '-' +  ;
                    m.ced
          SELECT 1
          xcedula = busqueda()
          m.nac = SUBSTR(xcedula,  ;
                  1, 1)
          m.ced = VAL(SUBSTR(xcedula,  ;
                  3, 10))
          SHOW GET m.nac
          SHOW GET m.ced
ENDCASE
ON KEY LABEL tab do validar with varread()
RETURN
*
