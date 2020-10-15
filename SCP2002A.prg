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
USE predatos AGAIN ALIAS predatos  ;
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
SELECT 20
USE contdeud AGAIN ALIAS contdeud  ;
    ORDER ced
DEFINE WINDOW record1 FROM 01, 00  ;
       TO 23, 79 FLOAT NOCLOSE  ;
       SHADOW TITLE  ;
       'Egreso de Matricula de Pagos (SCP2002A)'  ;
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
            'ÕÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¸'
          @ 01, 00 SAY  ;
            '³ C‚dula  :             Apell/Nombres :                                      ³'
          @ 02, 00 SAY  ;
            '³ Sexo:   Fecha Nac.:            Lugar Nacmto. :                             ³'
          @ 03, 00 SAY  ;
            '³ Estado Nacmto:                              Ini./Est.:                     ³'
          @ 04, 00 SAY  ;
            '³ Representante Legal :           Apell/Nombres:                             ³'
          @ 05, 00 SAY  ;
            'ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ'
          @ 06, 00 SAY  ;
            '³              D A T O S    D E L    R E P R E S E N T A N T E               ³'
          @ 07, 00 SAY  ;
            'ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´'
          @ 08, 00 SAY  ;
            '³ Nombre de la Madre:                          ³ Profesi¢n :                 ³'
          @ 09, 00 SAY  ;
            '³ Nombre  del  Padre:                          ³ Profesi¢n :                 ³'
          @ 10, 00 SAY  ;
            'ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´'
          @ 11, 00 SAY  ;
            '³ Direcci¢n Habitaci¢n :                                                     ³'
          @ 12, 00 SAY  ;
            '³                                                                            ³'
          @ 13, 00 SAY  ;
            '³ Telefonos :                                                                ³'
          @ 14, 00 SAY  ;
            'ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ'
          @ 15, 00 SAY  ;
            '³ Menci¢n :       (           )  N£mero del Expediente:          Turno :(   )³'
          @ 16, 00 SAY  ;
            '³ Semestre: (  )                                                             ³'
          @ 17, 00 SAY  ;
            'ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾'
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
       'Ingrese el N£mero del Cedula del Alumno'  ;
       ERROR  ;
       'Ingrese al N£mero de Cedula'
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
     m.saldo = total_d - monto_p
     SELECT 2
     IF SEEK(xcedula)
          IF cierre = 'S'
               = msgerro( ;
                 'Esta Matricula de pago ya fue cerrada, verifique' ;
                 )
               m.nac = 'V'
               m.ced = 0
               LOOP
          ENDIF
     ENDIF
     SELECT 7
     SET ORDER TO ced
     IF SEEK(xcedula)
          IF total_d - monto_p >  ;
             0
               = msgerro( ;
                 'Este Alumno tiene un registro de deuda vencida pendiente, verifique' ;
                 )
               IF  .NOT.  ;
                   deuda_ven1()
                    m.nac = 'V'
                    m.ced = 0
                    LOOP
               ENDIF
          ENDIF
     ENDIF
     CLEAR GETS
     @ 01, 40 GET m.nombres  ;
       PICTURE '@!S37' MESSAGE  ;
       'Ingrese Apellidos y Nombres de Alumno'
     @ 02, 08 GET m.sexo PICTURE  ;
       '@!M M,F' MESSAGE  ;
       'Ingrese el Sexo del Alumno'
     @ 02, 22 GET m.fechanac  ;
       PICTURE '@E' MESSAGE  ;
       'Ingrese la Fecha de Nacimiento del alumno'
     @ 02, 49 GET m.lugarnac  ;
       PICTURE '@!s29' MESSAGE  ;
       'Ingrese el Lugar de Nacimiento del Alumno'
     @ 03, 17 GET m.estado  ;
       PICTURE '@!s27' MESSAGE  ;
       'Ingrese el Estado de Nacimiento'
     @ 03, 57 GET m.entidad  ;
       PICTURE '@!' MESSAGE  ;
       'Ingrese El Estado de Nacimiento Abreviado ¢ (DF, EX)'
     @ 04, 23 GET m.nac1 PICTURE  ;
       '@!M , ,V,E,C,R' MESSAGE  ;
       'ingrese la Nacionalidad del Representante Legal'
     @ 04, 24 SAY '-'
     @ 04, 25 GET m.ced1 PICTURE  ;
       '99999999' VALID m.ced1 >=  ;
       0 MESSAGE  ;
       'Ingrese el N£mero del Cedula del Representante Legal'  ;
       ERROR  ;
       'Ingrese al N£mero de Cedula'
     @ 04, 49 GET m.representa  ;
       PICTURE '@!s25' MESSAGE  ;
       'Ingrese el Nombre del Representante del Alumno en la Instituci¢n'
     m.ced1 = IIF(LEN(LTRIM(STR(m.ced1,  ;
              10))) < 10,  ;
              LTRIM(STR(m.ced1,  ;
              10)) + SPACE(10 -  ;
              LEN(LTRIM(STR(m.ced1,  ;
              10)))),  ;
              LTRIM(STR(m.ced1,  ;
              10)))
     m.rep_legal = m.nac1 + '-' +  ;
                   m.ced1
     xmonto_pag = 0
     @ 08, 22 GET m.represent1  ;
       PICTURE '@!S24' MESSAGE  ;
       'Nombre de la Madre'
     @ 08, 61 GET m.profesion1  ;
       PICTURE '@!S17' MESSAGE  ;
       'Profesi¢n u Oficio de la Madre'
     @ 09, 22 GET m.represent2  ;
       PICTURE '@!S24' MESSAGE  ;
       'Nombre del Padre'
     @ 09, 61 GET m.profesion2  ;
       PICTURE '@!S17' MESSAGE  ;
       'Profesi¢n u Oficio del Padre'
     @ 11, 26 GET m.direccion1  ;
       PICTURE '@!' MESSAGE  ;
       'Direcci¢n o Habitaci¢n'
     @ 12, 26 GET m.direccion2  ;
       PICTURE '@!' MESSAGE  ;
       'Direcci¢n o Habitaci¢n'
     @ 13, 15 GET m.telefono  ;
       PICTURE '@!' MESSAGE  ;
       'Telefonos'
     CLEAR GETS
     SELECT 2
     SET ORDER TO ced
     SEEK xcedula
     IF  .NOT. FOUND()
          = msgerro( ;
            'Este alumno no tiene registro de Matricula de Pago, verifique' ;
            )
          m.nac = 'V'
          m.ced = 0
          LOOP
     ENDIF
     IF status = 'X'
          = msgerro( ;
            'Este Alumno fue egresado de la Matricula, verifique' ;
            )
          m.nac = 'V'
          m.ced = SPACE(10)
          LOOP
     ENDIF
     SCATTER MEMO MEMVAR
     promosion = 0
     m.planco = m.plancobro
     qcurso = m.cur
     m.codigo = semestre
     m.turno = turno
     @ 15, 12 GET qcurso PICTURE  ;
       '@!M 32012,31022,31023'  ;
       MESSAGE  ;
       'Ingrese la Menci¢n'
     DO CASE
          CASE ALLTRIM(qcurso) =  ;
               '32011'
               @ 15, 19 SAY  ;
                 'BASICA'
               mmencion = 'BASICA'
          CASE ALLTRIM(qcurso) =  ;
               '31022'
               @ 15, 19 SAY  ;
                 'CIENCIAS'
               mmencion = 'CIENCIAS'
          CASE ALLTRIM(qcurso) =  ;
               '31023'
               @ 15, 19 SAY  ;
                 'HUMANIDADES'
               mmencion = 'HUMANIDADES'
     ENDCASE
     m.codplan = qcurso
     @ 15, 42 GET m.turno PICTURE  ;
       '@!M MATUTINO,VESPERTINO,NOCTURNO'
     IF SUBSTR(m.codplan, 1, 2) =  ;
        '32'
          @ 16, 13 GET m.codigo  ;
            DEFAULT '07' PICTURE  ;
            '@!M 07,08,09,10,11,12'
     ELSE
          @ 16, 13 GET m.codigo  ;
            DEFAULT '01' PICTURE  ;
            '@!M 01,02,03,04'
     ENDIF
     m.curso = qcurso
     m.cedula = xcedula
     m.ced = xcedula
     CLEAR GETS
     SELECT 2
     m.planco = plancobro
     @ 16, 44 SAY 'P/Cobro:' GET  ;
       m.planco PICTURE '@!'
     SELECT 8
     SEEK lapmat + m.planco
     mdes1 = ALLTRIM(descripcio)
     @ 16, COL() + 1 SAY mdes1  ;
       PICTURE '@!s17'
     CLEAR GETS
     opc = acepta( ;
           'Desea Retirar el Matr. Pagos' ;
           )
     IF opc = 1
          opc = acepta( ;
                'Usted esta seguro de Retirar' ;
                )
          IF opc = 1
               SELECT 2
               DO WHILE .T.
                    IF RLOCK()
                         REPLACE status  ;
                                 WITH  ;
                                 'X'
                         UNLOCK
                         EXIT
                    ENDIF
               ENDDO
          ENDIF
     ENDIF
     xcedula = SPACE(12)
     m.swp = 0
     @ 06, 00 CLEAR TO 19,  ;
       WCOLS()
     _CUROBJ = OBJNUM(xcedula)
     SHOW GETS
     m.control = 0
     m.nac = 'V'
     m.ced = 0
ENDDO
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
     CASE xvar = 'PLANCO'
          SELECT 8
          SET FILTER TO lapso = lapmat
          ON KEY LABEL tab
          m.planco = boxfield('PLANCOBR', ;
                     'CODIGO')
          SHOW GET m.planco
     CASE xvar = 'MBANCO'
          ON KEY LABEL tab
          mbanco = boxfield('BANCOS', ;
                   'CODIGO')
          SHOW GET mbanco
     CASE xvar = 'MCOBRADOR'
          ON KEY LABEL tab
          mcobrador = boxfield('COBRADOR', ;
                      'CODIGO')
          SHOW GET mcobrador
ENDCASE
ON KEY LABEL tab do validar with varread()
RETURN
*
