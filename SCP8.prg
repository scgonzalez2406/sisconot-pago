CLOSE DATABASES
SET TALK OFF
SELECT 1
USE datos
SET ORDER TO cedula
SELECT 3
use mp&lapmat
SET ORDER TO cedcur
GOTO TOP
SCATTER BLANK MEMO MEMVAR FIELDS  ;
        ced, cur
DEFINE WINDOW a1 FROM 10, 10 TO  ;
       17, 70 GROW FLOAT CLOSE  ;
       ZOOM SHADOW TITLE  ;
       'Actualizaci¢n de C‚dula de Alumnos'  ;
       MINIMIZE COLOR SCHEME 5
ACTIVATE WINDOW a1
DO WHILE .T.
     CLEAR
     @ 4, 0 TO 4, WCOLS()
     @ 5, 1 SAY  ;
       'Presione Esc para cancelar la operaci¢n'
     @ 1, 1 SAY 'C‚dula : ' GET  ;
       m.ced PICTURE  ;
       '!-99999999'
     @ 2, 1 SAY 'Alumno : '
     @ 3, 1 SAY 'Nueva C‚dula : '
     READ
     IF LASTKEY() = 27 .OR.  ;
        EMPTY(m.ced)
          CLOSE DATABASES
          RELEASE WINDOW a1
          RETURN
     ENDIF
     paso = 0
     SELECT 1
     IF SEEK(m.ced)
          xnombre = nombres
          @ 2, 1 SAY 'Alumno : '  ;
            GET xnombre PICTURE  ;
            '@!s40'
          CLEAR GETS
          paso = 1
     ENDIF
     SELECT 3
     IF SEEK(m.ced)
          paso = paso + 1
     ENDIF
     IF paso = 0
          p = msgerro( ;
              'Este Alumno no esta registrado en el sistema' ;
              )
          LOOP
     ENDIF
     m.nac1 = ' '
     m.ced1 = SPACE(10)
     DO WHILE .T.
          @ 3, 1 SAY  ;
            'Nueva C‚dula : ' GET  ;
            m.nac1 PICTURE  ;
            '@!M V,E,C,R'
          @ 3, COL() SAY '-'
          @ 3, COL() GET m.ced1
          READ
          IF LASTKEY() = 27
               p = salida()
               IF p = 2
                    RELEASE WINDOW
                    CLOSE DATABASES
                    RETURN
               ENDIF
          ENDIF
          IF m.ced1 = SPACE(10)
               = msgerro( ;
                 'El Nuevo n£mero de c‚dula esta en blanco, verifique' ;
                 )
               LOOP
          ENDIF
          xcedula = m.nac1 + '-' +  ;
                    m.ced1
          SELECT 1
          IF SEEK(m.ced)
               IF SEEK(xcedula)
                    = msgerro( ;
                      'Ya Existe un alumno con esta c‚dula en Datos personales, verifique' ;
                      )
                    LOOP
               ENDIF
          ENDIF
          SELECT 3
          IF SEEK(m.ced)
               IF SEEK(xcedula)
                    = msgerro( ;
                      'Ya Existe un alumno con esta c‚dula en la Matricula Actual, verifique' ;
                      )
                    LOOP
               ENDIF
          ENDIF
          EXIT
     ENDDO
     p = acepta( ;
         'Esta seguro de realizar el cambio' ;
         )
     IF p = 1
          p = acepta( ;
              'Realizo el Proceso de Cambio' ;
              )
          IF p = 1
               SELECT 1
               IF SEEK(m.ced)
                    DO WHILE .T.
                         IF RLOCK()
                              REPLACE  ;
                               cedula  ;
                               WITH  ;
                               xcedula
                              UNLOCK
                              EXIT
                         ENDIF
                    ENDDO
               ENDIF
               SELECT 3
               IF SEEK(m.ced)
                    DO WHILE .T.
                         IF RLOCK()
                              REPLACE  ;
                               ced  ;
                               WITH  ;
                               xcedula
                              UNLOCK
                              EXIT
                         ENDIF
                    ENDDO
               ENDIF
          ENDIF
     ENDIF
     SELECT 3
     SCATTER BLANK MEMO MEMVAR  ;
             FIELDS ced, cur
ENDDO
CLOSE DATABASES
RELEASE WINDOW a1
RETURN
*
