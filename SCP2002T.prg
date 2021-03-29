CLOSE DATABASES
SET DELETED ON
SET TALK OFF
SELECT 1
USE lapso1
m.lapso = lapso1
CLEAR GETS
SELECT 1
USE datos
SET ORDER TO cedula
SELECT 2
use mp&lapmat
SET ORDER TO ced
SELECT 3
USE record
SET ORDER TO cedcurso
SELECT 7
USE contdeud AGAIN ALIAS contdeud  ;
    ORDER cedula
SELECT 8
USE plancobr AGAIN ALIAS plancobr  ;
    ORDER lapcod
SELECT 11
USE detacobr AGAIN ALIAS detacobr  ;
    ORDER lapcod
SELECT 12
USE concepto AGAIN ALIAS concepto  ;
    ORDER codigo
SELECT 15
use ma&lapmat
SET ORDER TO ced
SELECT 13
USE liceo
SELECT 14
USE cobrador AGAIN ALIAS cobrador  ;
    ORDER codigo
SELECT 9
USE regpagos AGAIN ALIAS regdatos  ;
    ORDER cedula
SELECT 10
USE bancos AGAIN ALIAS bancos  ;
    ORDER codigo
DEFINE WINDOW record1 FROM 04, 00  ;
       TO 23, 79 FLOAT NOCLOSE  ;
       SHADOW TITLE  ;
       'Registro de Control de Deuda Anterior (SCP2002t)'  ;
       NOMINIMIZE COLOR SCHEME 5
xcedula = SPACE(12)
m.swp = 0
m.control = 0
m.pote = 1
ON KEY LABEL f8 keybo chr(13)
ON KEY LABEL f9 keybo chr(13)
ON KEY LABEL tab do validar with varread()
m.nac = 'V'
m.ced = 0
valida1 = 'ABCDEFGHIJKLMNOPQRSTUVXYZ '
valida2 = 'ABCDEFGHIJKLMNOPQRSTUVXYZ* '
DO WHILE .T.
     IF WVISIBLE('record1')
          ACTIVATE WINDOW SAME  ;
                   record1
     ELSE
          ACTIVATE WINDOW NOSHOW  ;
                   record1
     ENDIF
     IF m.pote = 1
          @ 00, 00 SAY  ;
            'ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ'
          @ 01, 00 SAY  ;
            'ณ Cdula  :             Apell/Nombres :                                      ณ'
          @ 02, 00 SAY  ;
            'ณ Sexo:   Fecha Nac.:            Lugar Nacmto. :                             ณ'
          @ 03, 00 SAY  ;
            'ณ Estado Nacmto:                              Ini./Est.:                     ณ'
          @ 04, 00 SAY  ;
            'ฦอออออัอออออออออออออออออออออออออออออออออออออออออออออออออออออออออัออออออออออออต'
          @ 05, 00 SAY  ;
            'ณ COD.ณ   DESCRIPCION                                           ณ   MONTO    ณ'
          @ 06, 00 SAY  ;
            'ฦอออออุอออออออออออออออออออออออออออออออออออออออออออออออออออออออออุออออออออออออต'
          @ 07, 00 SAY  ;
            'ณ 01  ณ                                                         ณ            ณ'
          @ 08, 00 SAY  ;
            'ณ 02  ณ                                                         ณ            ณ'
          @ 09, 00 SAY  ;
            'ณ 03  ณ                                                         ณ            ณ'
          @ 10, 00 SAY  ;
            'ณ 04  ณ                                                         ณ            ณ'
          @ 11, 00 SAY  ;
            'ณ 05  ณ                                                         ณ            ณ'
          @ 12, 00 SAY  ;
            'รฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤด'
          @ 13, 00 SAY  ;
            'ณ                                               TOTAL DEUDA ->  ณ            ณ'
          @ 14, 00 SAY  ;
            'ิอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออฯออออออออออออพ'
          m.pote = 2
          SAVE SCREEN TO pelota
     ENDIF
     RESTORE SCREEN FROM pelota
     @ 01, 12 GET m.nac PICTURE  ;
       '@!M V,E,C,R' MESSAGE  ;
       'ingrese la Nacionalidad del Alumno'
     @ 01, 13 SAY '-'
     @ 01, 14 GET m.ced PICTURE  ;
       '99999999' VALID m.ced >=  ;
       0 MESSAGE  ;
       'Ingrese el Nฃmero del Cedula del Alumno'  ;
       ERROR  ;
       'Ingrese al Nฃmero de Cedula'
     READ
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
     IF LASTKEY() = 27
          ON KEY LABEL tab
          RELEASE WINDOW record1
          CLOSE DATABASES
          RETURN
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
     SET ORDER TO cedula
     SEEK xcedula
     IF  .NOT. FOUND()
          = msgerro( ;
            'Este alumno no esta registrado, verifique' ;
            )
          m.nac = 'V'
          m.ced = 0
          LOOP
     ENDIF
     SCATTER MEMO MEMVAR
     m.cedula = xcedula
     m.nac1 = LEFT(m.rep_legal,  ;
              1)
     m.ced1 = VAL(SUBSTR(m.rep_legal,  ;
              3, 10))
     SELECT 7
     SET ORDER TO cedula
     IF SEEK(xcedula)
          m.saldo = total_d -  ;
                    monto_p
     ENDIF
     SCATTER MEMO MEMVAR
     m.cedula = xcedula
     m.saldo = m.total_d -  ;
               m.monto_p
     @ 13, 66 GET m.saldo PICTURE  ;
       '999,999.99'
     CLEAR GETS
     DO WHILE .T.
          @ 01, 40 GET m.nombres  ;
            PICTURE '@!S37'  ;
            MESSAGE  ;
            'Ingrese Apellidos y Nombres de Alumno'
          @ 02, 08 GET m.sexo  ;
            PICTURE '@!M M,F'  ;
            MESSAGE  ;
            'Ingrese el Sexo del Alumno'
          @ 02, 22 GET m.fechanac  ;
            PICTURE '@E' MESSAGE  ;
            'Ingrese la Fecha de Nacimiento del alumno'
          @ 02, 49 GET m.lugarnac  ;
            PICTURE '@!s29'  ;
            MESSAGE  ;
            'Ingrese el Lugar de Nacimiento del Alumno'
          @ 03, 17 GET m.estado  ;
            PICTURE '@!s27'  ;
            MESSAGE  ;
            'Ingrese el Estado de Nacimiento'
          @ 03, 57 GET m.entidad  ;
            PICTURE '@!' MESSAGE  ;
            'Ingrese El Estado de Nacimiento Abreviado ข (DF, EX)'
          CLEAR GETS
          @ 07, 10 GET  ;
            m.concepto1 PICTURE  ;
            '@!'
          @ 07, 66 GET m.monto1  ;
            PICTURE '999,999.99'
          @ 08, 10 GET  ;
            m.concepto2 PICTURE  ;
            '@!'
          @ 08, 66 GET m.monto2  ;
            PICTURE '999,999.99'
          @ 09, 10 GET  ;
            m.concepto3 PICTURE  ;
            '@!'
          @ 09, 66 GET m.monto3  ;
            PICTURE '999,999.99'
          @ 10, 10 GET  ;
            m.concepto4 PICTURE  ;
            '@!'
          @ 10, 66 GET m.monto4  ;
            PICTURE '999,999.99'
          @ 11, 10 GET  ;
            m.concepto5 PICTURE  ;
            '@!'
          @ 11, 66 GET m.monto5  ;
            PICTURE '999,999.99'
          READ
          IF LASTKEY() = 27
               p = salida()
               IF p = 2
                    ON KEY LABEL tab
                    RELEASE WINDOW  ;
                            record1
                    CLOSE DATABASES
                    RETURN
               ENDIF
               LOOP
          ENDIF
          IF EMPTY(m.cedula)
               = msgerro( ;
                 'El Nฃmero de la Cdula esta el Blanco' ;
                 )
               _CUROBJ = OBJNUM(m.cedula)
               LOOP
          ENDIF
          IF EMPTY(m.nombres)
               = msgerro( ;
                 'El Nombre esta el Blanco' ;
                 )
               _CUROBJ = OBJNUM(m.nombres)
               LOOP
          ENDIF
          IF EMPTY(m.sexo) .OR.   ;
             .NOT. m.sexo $ 'FM'
               = msgerro( ;
                 'El sexo es invalido' ;
                 )
               _CUROBJ = OBJNUM(m.sexo)
               LOOP
          ENDIF
          IF EMPTY(m.fechanac)
               = msgerro( ;
                 'Fecha de Nacimiento esta el Blanco' ;
                 )
               _CUROBJ = OBJNUM(m.fechanac)
               LOOP
          ENDIF
          m.total_d = m.monto1 +  ;
                      m.monto2 +  ;
                      m.monto3 +  ;
                      m.monto4 +  ;
                      m.monto5
          @ 13, 66 GET m.total_d  ;
            PICTURE '999,999.99'
          CLEAR GETS
          m.monto_p = 0.00 
          opc = acepta( ;
                'Acepta los Datos personales' ;
                )
          IF opc = 1
               SELECT 1
               SET ORDER TO cedula
               SEEK m.cedula
               m.cuco = IIF(FOUND(),  ;
                        0, 1)
               DO WHILE .T.
                    IF FLOCK()
                         IF m.cuco =  ;
                            1
                              APPEND  ;
                               BLANK
                         ENDIF
                         GATHER MEMVAR  ;
                                MEMO
                         UNLOCK
                         EXIT
                    ENDIF
               ENDDO
               SELECT 7
               SET ORDER TO cedula
               SEEK m.cedula
               m.cuco = IIF(FOUND(),  ;
                        0, 1)
               DO WHILE .T.
                    IF FLOCK()
                         IF m.cuco =  ;
                            1
                              APPEND  ;
                               BLANK
                         ENDIF
                         GATHER MEMVAR  ;
                                MEMO
                         UNLOCK
                         EXIT
                    ENDIF
               ENDDO
          ENDIF
          EXIT
     ENDDO
     m.nac = 'V'
     m.ced = 0
ENDDO
CLOSE DATABASES
RELEASE WINDOW
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
