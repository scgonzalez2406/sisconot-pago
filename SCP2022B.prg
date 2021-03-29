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
SELECT 16
USE atrasado AGAIN ALIAS atrasado  ;
    ORDER recibo
SELECT 17
USE nroatras
DEFINE WINDOW record1 FROM 01, 00  ;
       TO 24, 79 FLOAT NOCLOSE  ;
       SHADOW TITLE  ;
       'Actualizaciขn de Registro de Inscripciขn (SCP20002)'  ;
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
lin11 = 0
lin22 = 0
lin33 = 0
mdec11 = 0
mdec22 = 0
tu1 = 0
mbandes = SPACE(30)
nopaga = .F.
DO WHILE .T.
     nopaga = .F.
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
            'ณ                                                                            ณ'
          @ 05, 00 SAY  ;
            'ฦออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออต'
          @ 06, 00 SAY  ;
            'ณ              D A T O S    D E L    R E P R E S E N T A N T E               ณ'
          @ 07, 00 SAY  ;
            'รฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤด'
          @ 08, 00 SAY  ;
            'ณ Nombre de la Madre:                          ณ Profesiขn :                 ณ'
          @ 09, 00 SAY  ;
            'ณ Nombre  del  Padre:                          ณ Profesiขn :                 ณ'
          @ 10, 00 SAY  ;
            'รฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤด'
          @ 11, 00 SAY  ;
            'ณ Direcciขn Habitaciขn :                                                     ณ'
          @ 12, 00 SAY  ;
            'ณ                                                                            ณ'
          @ 13, 00 SAY  ;
            'ณ Telefonos :                                                                ณ'
          @ 14, 00 SAY  ;
            'ฦออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออต'
          @ 15, 00 SAY  ;
            'ณ Menciขn :       (           )  Turno: (   ) Nฃmero del Expediente:         ณ'
          @ 16, 00 SAY  ;
            'ณ Semestre: (  )                                                             ณ'
          @ 17, 00 SAY  ;
            'ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ'
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
          a = acepta( ;
              'Desea Registrar este Alumno' ;
              )
          IF a = 2
               m.nac = 'V'
               m.ced = 0
               LOOP
          ENDIF
     ENDIF
     SCATTER MEMO MEMVAR
     m.cedula = xcedula
     m.nac1 = LEFT(m.rep_legal,  ;
              1)
     m.ced1 = VAL(SUBSTR(m.rep_legal,  ;
              3, 10))
     SELECT 2
     SCATTER MEMO MEMVAR
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
     SET ORDER TO cedula
     IF SEEK(xcedula)
          m.saldo = total_d -  ;
                    monto_p
          IF m.saldo > 0
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
          cocu = acepta( ;
                 'Estan Correctos los Datos Personales' ;
                 )
          IF cocu = 2
               LOOP
          ENDIF
          cocu = acepta( ;
                 'Desea cargar datos del representante' ;
                 )
          IF cocu = 1
               DO WHILE .T.
                    @ 08, 22 GET  ;
                      m.represent1  ;
                      PICTURE  ;
                      '@!S24'  ;
                      MESSAGE  ;
                      'Nombre de la Madre'
                    @ 08, 61 GET  ;
                      m.profesion1  ;
                      PICTURE  ;
                      '@!S17'  ;
                      MESSAGE  ;
                      'Profesiขn u Oficio de la Madre'
                    @ 09, 22 GET  ;
                      m.represent2  ;
                      PICTURE  ;
                      '@!S24'  ;
                      MESSAGE  ;
                      'Nombre del Padre'
                    @ 09, 61 GET  ;
                      m.profesion2  ;
                      PICTURE  ;
                      '@!S17'  ;
                      MESSAGE  ;
                      'Profesiขn u Oficio del Padre'
                    @ 11, 26 GET  ;
                      m.direccion1  ;
                      PICTURE  ;
                      '@!'  ;
                      MESSAGE  ;
                      'Direcciขn o Habitaciขn'
                    @ 12, 26 GET  ;
                      m.direccion2  ;
                      PICTURE  ;
                      '@!'  ;
                      MESSAGE  ;
                      'Direcciขn o Habitaciขn'
                    @ 13, 15 GET  ;
                      m.telefono  ;
                      PICTURE  ;
                      '@!'  ;
                      MESSAGE  ;
                      'Telefonos'
                    READ
                    IF LASTKEY() =  ;
                       27
                         p = salida()
                         IF p = 2
                              RELEASE  ;
                               WINDOW  ;
                               record1
                              CLOSE  ;
                               DATABASES
                              RETURN
                         ENDIF
                         LOOP
                    ENDIF
                    EXIT
               ENDDO
          ENDIF
          EXIT
     ENDDO
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
     SELECT 2
     SET ORDER TO ced
     SEEK xcedula
     IF status = 'X'
          = msgerro( ;
            'Este Alumno fue egresado de la Matricula, verifique' ;
            )
          m.nac = 'V'
          m.ced = SPACE(10)
          LOOP
     ENDIF
     IF cierre = 'S'
          = msgerro( ;
            'Esta Matricula de pago ya fue cerrada, verifique' ;
            )
          m.nac = 'V'
          m.ced = 0
          LOOP
     ENDIF
     IF  .NOT. FOUND()
          SELECT 2
          SCATTER MEMO MEMVAR
          promosion = 0
          m.planco = m.plancobro
          qcurso = '32012'
          m.cur = '32012'
          m.codigo = '07'
          m.semestre = m.codigo
          m.promosion = 0
          m.planco = SPACE(5)
          m.turno = 'MAT'
     ELSE
          SELECT 15
          SET ORDER TO ced
          SEEK xcedula
          IF FOUND()
               qcurso = '32012'
               m.cur = '32012'
               m.codigo = '07'
               m.semestre = m.codigo
               m.promosion = 0
               m.planco = SPACE(5)
               m.turno = 'MAT'
               m.matricula = SPACE(5)
          ELSE
               SELECT 2
               SCATTER MEMO  ;
                       MEMVAR
               promosion = 0
               m.planco = m.plancobro
               m.turno = 'MAT'
          ENDIF
     ENDIF
     qcurso = m.cur
     m.codigo = m.semestre
     m.turno = m.turno
     @ 15, 12 GET qcurso PICTURE  ;
       '@!M 32012,31022,31023'  ;
       MESSAGE  ;
       'Ingrese la Menciขn'
     DO CASE
          CASE ALLTRIM(qcurso) =  ;
               '32012'
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
     @ 15, 41 GET m.turno PICTURE  ;
       '@!M MATUTINO,VESPERTINO,NOCTURNO'
     @ 15, 68 GET m.matricula
     m.codplan = qcurso
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
     p11 = 0
     SELECT 2
     IF FOUND()
          p11 = 1
          = msgerro( ;
            'El Alumno ya esta inscrito' ;
            )
          p = acepta( ;
              'Desea realizar modificaciones' ;
              )
          IF p = 2
               opc = acepta( ;
                     'Acepta los Datos personales' ;
                     )
               IF opc = 1
                    SELECT 1
                    SET ORDER TO cedula
                    SEEK m.cedula
                    m.cuco = IIF(FOUND(),  ;
                             0,  ;
                             1)
                    DO WHILE .T.
                         IF FLOCK()
                              IF m.cuco =  ;
                                 1
                                   APPEND BLANK
                              ENDIF
                              GATHER  ;
                               MEMVAR  ;
                               MEMO
                              UNLOCK
                              EXIT
                         ENDIF
                    ENDDO
               ENDIF
               m.nac = 'V'
               m.ced = 0
               LOOP
          ENDIF
     ENDIF
     @ 15, 12 GET qcurso PICTURE  ;
       '@!M 32012,31022,31023'  ;
       MESSAGE  ;
       'Ingrese la Menciขn'
     @ 15, 41 GET m.turno PICTURE  ;
       '@!M MATUTINO,VESPERTINO,NOCTURNO'
     IF p11 = 1
          @ 15, 68 SAY  ;
            m.matricula
     ELSE
          @ 15, 68 GET  ;
            m.matricula
     ENDIF
     READ
     IF LASTKEY() = 27
          p = salida()
          IF p = 2
               opc = acepta( ;
                     'Acepta los Datos personales' ;
                     )
               IF opc = 1
                    SELECT 1
                    SET ORDER TO cedula
                    SEEK m.cedula
                    m.cuco = IIF(FOUND(),  ;
                             0,  ;
                             1)
                    m.rep_legal =  ;
                     m.nac1 +  ;
                     LTRIM(STR(m.ced1))
                    DO WHILE .T.
                         IF FLOCK()
                              IF m.cuco =  ;
                                 1
                                   APPEND BLANK
                              ENDIF
                              GATHER  ;
                               MEMVAR  ;
                               MEMO
                              UNLOCK
                              EXIT
                         ENDIF
                    ENDDO
                    m.nac = 'V'
                    m.ced = SPACE(10)
                    LOOP
               ENDIF
               ON KEY LABEL tab
               RELEASE WINDOW  ;
                       record1
               CLOSE DATABASES
               RETURN
          ENDIF
          LOOP
     ENDIF
     DO CASE
          CASE ALLTRIM(qcurso) =  ;
               '32012'
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
     m.curso = qcurso
     SELECT 2
     m.recno = RECNO()
     DO WHILE .T.
          IF SUBSTR(m.codplan, 1,  ;
             2) = '32'
               @ 16, 13 GET  ;
                 m.codigo DEFAULT  ;
                 '07' PICTURE  ;
                 '@!M 07,08,09,10,11,12'  ;
                 MESSAGE  ;
                 'Ingrese el Curso'
          ELSE
               @ 16, 13 GET  ;
                 m.codigo DEFAULT  ;
                 '01' PICTURE  ;
                 '@!M 01,02,03,04'  ;
                 MESSAGE  ;
                 'Ingrese el Curso'
          ENDIF
          READ
          IF LASTKEY() = 27
               p = salida()
               IF p = 2
                    ON KEY LABEL tab
                    RELEASE WINDOW
                    CLOSE DATABASES
                    RETURN
               ENDIF
               LOOP
          ENDIF
          m.semestre = m.codigo
          opc = acepta( ;
                'Acepta estos Datos Academicos' ;
                )
          IF opc = 2
               LOOP
          ENDIF
          EXIT
     ENDDO
     SELECT 1
     SET ORDER TO cedula
     SEEK m.cedula
     m.cuco = IIF(FOUND(), 0, 1)
     DO WHILE .T.
          IF FLOCK()
               IF m.cuco = 1
                    APPEND BLANK
               ENDIF
               GATHER MEMVAR MEMO
               UNLOCK
               EXIT
          ENDIF
     ENDDO
     SELECT 2
     m.curso = qcurso
     m.cedula = xcedula
     m.ced = xcedula
     IF  .NOT. SEEK(m.cedula)
          DEFINE WINDOW reg FROM  ;
                 00, 00 TO 24, 79  ;
                 GROW FLOAT CLOSE  ;
                 ZOOM SHADOW  ;
                 TITLE  ;
                 'Registro de Control de Pago'  ;
                 MINIMIZE SYSTEM  ;
                 COLOR SCHEME 5
          MOVE WINDOW reg CENTER
          ACTIVATE WINDOW reg
          @ 00, 00 SAY  ;
            'ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป'
          @ 01, 00 SAY  ;
            'บ Codigo del Plan:      Descripciขn:                                         บ'
          @ 02, 00 SAY  ;
            'บ Total Costo Plan Cobro:                  Fecha de Pago    :                บ'
          @ 03, 00 SAY  ;
            'บ Pagado Pre-Inscripciขn:                  Tipo Pago (E/D/C):                บ'
          @ 04, 00 SAY  ;
            'บ       Monto a Cancelar:                  Nro. Dep./Cheq   :                บ'
          @ 05, 00 SAY  ;
            'บ           Monto Pagado:                  Banco :                           บ'
          @ 06, 00 SAY  ;
            'บ                                       Cobrador :                           บ'
          @ 07, 00 SAY  ;
            'ฬอออออออัออออออออออออออออออออออออออัอออออออออออัอออออออออออออัอออออออออออออัอน'
          @ 08, 00 SAY  ;
            'บ CUOTA ณ C O N C E P T O S        ณ FECHA VEN ณ    MONTO    ณ    PAGADO   ณTบ'
          @ 08, 02 SAY  ;
            'CUOTA ณ C O N C E P T O S        ณ FECHA VEN ณ    MONTO    ณ    PAGADO   ณT'  ;
            COLOR N/W 
          @ 09, 00 SAY  ;
            'บ       ณ                          ณ           ณ             ณ             ณ บ'
          @ 10, 00 SAY  ;
            'บ       ณ                          ณ           ณ             ณ             ณ บ'
          @ 11, 00 SAY  ;
            'บ       ณ                          ณ           ณ             ณ             ณ บ'
          @ 12, 00 SAY  ;
            'บ       ณ                          ณ           ณ             ณ             ณ บ'
          @ 13, 00 SAY  ;
            'บ       ณ                          ณ           ณ             ณ             ณ บ'
          @ 14, 00 SAY  ;
            'บ       ณ                          ณ           ณ             ณ             ณ บ'
          @ 15, 00 SAY  ;
            'บ       ณ                          ณ           ณ             ณ             ณ บ'
          @ 16, 00 SAY  ;
            'บ       ณ                          ณ           ณ             ณ             ณ บ'
          @ 17, 00 SAY  ;
            'บ       ณ                          ณ           ณ             ณ             ณ บ'
          @ 18, 00 SAY  ;
            'บ       ณ                          ณ           ณ             ณ             ณ บ'
          @ 19, 00 SAY  ;
            'บ       ณ                          ณ           ณ             ณ             ณ บ'
          @ 20, 00 SAY  ;
            'บ       ณ                          ณ           ณ             ณ             ณ บ'
          @ 21, 00 SAY  ;
            'ศอออออออฯออออออออออออออออออออออออออฯอออออออออออฯอออออออออออออฯอออออออออออออฯอผ'
          SAVE SCREEN TO hol
          p = 1
          DO WHILE .T.
               @ 01, 18 GET  ;
                 m.planco PICTURE  ;
                 '@!'
               READ
               IF LASTKEY() = 27
                    p = salida()
                    IF p = 2
                         EXIT
                    ENDIF
                    LOOP
               ENDIF
               SELECT 8
               IF  .NOT.  ;
                   SEEK(lapmat +  ;
                   m.planco)
                    SELECT 8
                    SET FILTER TO lapso;
= lapmat
                    m.planco = boxfield('plancobr', ;
                               'codigo')
                    SET FILTER TO
               ENDIF
               mdes1 = ALLTRIM(descripcio)
               @ 01, 18 SAY  ;
                 m.planco PICTURE  ;
                 '@!' COLOR W+/W 
               @ 01, 36 SAY mdes1  ;
                 PICTURE '@!'
               CLEAR GETS
               xnro = nrocuotas
               mm = 1
               SELECT 11
               SET ORDER TO codcuota
               SEEK lapmat +  ;
                    m.planco +  ;
                    STR(1)
               swp = 0
               suma = 0
               li = 09
               DO WHILE  .NOT.  ;
                  EOF()
                    IF lapso <>  ;
                       lapmat
                         SELECT 11
                         SKIP
                         LOOP
                    ENDIF
                    IF codigo <>  ;
                       m.planco
                         SELECT 11
                         SKIP
                         LOOP
                    ENDIF
                    IF swp = 0
                         mcuota =  ;
                          cuota
                         swp = 1
                         @ li, 03  ;
                           SAY  ;
                           mcuota  ;
                           PICTURE  ;
                           '99'
                         mcon = concepto1
                         SELECT 12
                         SEEK mcon
                         @ li, 10  ;
                           SAY  ;
                           LEFT(descripcio,  ;
                           25)
                         SELECT 11
                         @ li, 37  ;
                           SAY  ;
                           fecha_ven  ;
                           PICTURE  ;
                           '@E'
                    ENDIF
                    IF mcuota <>  ;
                       cuota
                         lm = LTRIM(STR(mm))
                         m.total_deud =  ;
                          m.total_deud +  ;
                          suma
                         m.cuota&lm=suma
                         @ li, 48  ;
                           SAY  ;
                           suma  ;
                           PICTURE  ;
                           '99,999,999.99'
                         li = li +  ;
                              1
                         mcuota =  ;
                          cuota
                         @ li, 03  ;
                           SAY  ;
                           mcuota  ;
                           PICTURE  ;
                           '99'
                         mcon = concepto1
                         SELECT 12
                         SEEK mcon
                         @ li, 10  ;
                           SAY  ;
                           LEFT(descripcio,  ;
                           25)
                         SELECT 11
                         @ li, 37  ;
                           SAY  ;
                           fecha_ven  ;
                           PICTURE  ;
                           '@E'
                         mm = mm +  ;
                              1
                         suma = 0.00 
                         mcuota =  ;
                          cuota
                    ENDIF
                    ii = 1
                    FOR ii = 1 TO  ;
                        10
                         lk = LTRIM(STR(ii))
                         if concepto&lk=space(2)
                              LOOP
                         ENDIF
                         if monto&lk=0
                              LOOP
                         ENDIF
                         suma=suma+monto&lk
                    ENDFOR
                    SKIP
               ENDDO
               lm = LTRIM(STR(mm))
               m.total_deud = m.total_deud +  ;
                              suma
               m.cuota&lm=suma
               @ li, 48 SAY suma  ;
                 PICTURE  ;
                 '99,999,999.99'
               mmontod = m.cuota1
               EXIT
          ENDDO
          mfechap = DATE()
          mtipop = 'Efectivo'
          mnrode = SPACE(20)
          mbanco = SPACE(2)
          mmontop = 0.00 
          mdesban = SPACE(20)
          mcobrador = SPACE(2)
          m.choice = 1
          swp = 0
          p = 0
          SAVE SCREEN TO hi
          m.cuota1 = m.cuota1
          xcuotamen = m.cuota1 -  ;
                      xmonto_pag
          DO WHILE .T.
               RESTORE SCREEN  ;
                       FROM hi
               @ 02, 26 SAY  ;
                 m.total_deud  ;
                 PICTURE  ;
                 '99,999,999.99'  ;
                 COLOR W+/W 
               @ 03, 26 SAY  ;
                 xmonto_pag  ;
                 PICTURE  ;
                 '99,999,999.99'  ;
                 COLOR W+/W 
               @ 04, 26 SAY  ;
                 xcuotamen  ;
                 PICTURE  ;
                 '99,999,999.99'  ;
                 COLOR W+/W 
               @ 02, 62 GET  ;
                 mfechap PICTURE  ;
                 '@E'
               @ 03, 62 GET  ;
                 mtipop DEFAULT  ;
                 'Efectivo'  ;
                 PICTURE  ;
                 '@!M Deposito,Cheque,Efectivo'
               READ
               IF mtipop <>  ;
                  'Efectivo'
                    @ 04, 62 GET  ;
                      mnrode  ;
                      PICTURE  ;
                      '@!S15'  ;
                      VALID  ;
                      mnrode <>  ;
                      SPACE(20)  ;
                      ERROR  ;
                      'El Nฃmero de la Transacciขn esta en blanco'
                    @ 05, 50 GET  ;
                      mbanco  ;
                      PICTURE  ;
                      '@!' VALID  ;
                      mbancos(mbanco)
                    READ
                    SELECT bancos
                    IF  .NOT.  ;
                        SEEK(mbanco)
                         mbanco =  ;
                          boxfield('bancos', ;
                          'codigo')
                    ENDIF
                    SELECT 9
                    SET ORDER TO bandepo
                    IF SEEK(mbanco +  ;
                       mnrode)
                         IF status =  ;
                            'D'
                              = msgerro( ;
                                'Ya existe un registro de pagos con este numero, verifique' ;
                                )
                              LOOP
                         ENDIF
                    ENDIF
                    SELECT 10
                    @ 05, 50 SAY  ;
                      mbanco  ;
                      PICTURE  ;
                      '@!' COLOR  ;
                      W+/W 
                    @ 05, COL() +  ;
                      2 SAY  ;
                      LEFT(descripcio,  ;
                      20)
                    mdesban = ALLTRIM(descripcio)
               ENDIF
               @ 05, 26 GET  ;
                 mmontop PICTURE  ;
                 '99,999,999.99'
               @ 06, 50 GET  ;
                 mcobrador  ;
                 PICTURE '@!'  ;
                 VALID  ;
                 mcobrad(mcobrador)
               READ
               IF LASTKEY() = 27
                    p = salida()
                    IF p = 2
                         EXIT
                    ENDIF
                    LOOP
               ENDIF
               IF mmontop = 0
                    nopaga = .T.
               ENDIF
               SELECT cobrador
               IF  .NOT.  ;
                   SEEK(mcobrador)
                    mcobrador = boxfield('cobrador', ;
                                'codigo')
               ENDIF
               SELECT 14
               @ 06, 50 SAY  ;
                 mcobrador  ;
                 PICTURE '@!'  ;
                 COLOR W+/W 
               @ 06, COL() + 2  ;
                 SAY LEFT(nombres,  ;
                 20)
               mnomcob = ALLTRIM(nombres)
               opc = acepta1( ;
                     'Esta correcto este Monto a Cancelar' ;
                     )
               IF opc = 2
                    LOOP
               ENDIF
               EXIT
          ENDDO
          IF p = 2
               RELEASE WINDOW reg
               m.nac = 'V'
               m.ced = 0
               LOOP
          ENDIF
          SELECT 8
          SEEK lapmat + m.planco
          xnro1 = nrocuotas
          SELECT 11
          SET ORDER TO codcuota
          SEEK lapmat + m.planco +  ;
               STR(1)
          li = 09
          mpagado = mmontop +  ;
                    xmonto_pag
          np = 1
          FOR np = 1 TO 12
               np1 = LTRIM(STR(np))
               mpagrec&np1=0
               mpsaldo&np1=0
          ENDFOR
          m.cubre = 0
          ii = 1
          FOR ii = 1 TO xnro1
               lk = LTRIM(STR(ii))
               if mpagado >= m.cuota&lk
                    mpagado=mpagado-m.cuota&lk
                    mpagrec&lk=m.cuota&lk-pagado&lk
                    mpsaldo&lk=0
                    @li,62 say m.cuota&lk;
pict '99,999,999.99'
                    @ li, 76 SAY  ;
                      '๛'
                    m.cubre = m.cubre +  ;
                              1
                    mdec11 = mdec11 +  ;
                             1
               ELSE
                    IF mpagado >  ;
                       0
                         @ li, 62  ;
                           SAY  ;
                           mpagado  ;
                           PICTURE  ;
                           '99,999,999.99'
                         @ li, 76  ;
                           SAY  ;
                           'ํ'
                         mpsaldo&lk=m.cuota&lk-mpagado-pagado&lk
                         mpagrec&lk=mpagado
                         mdec22 =  ;
                          mdec22 +  ;
                          1
                    ENDIF
                    EXIT
               ENDIF
               li = li + 1
          ENDFOR
          opce = acepta1( ;
                 'Proceso este Pago de Inscripciขn' ;
                 )
          IF opce = 2
               RELEASE WINDOW reg
               m.nac = 'V'
               m.ced = 0
               LOOP
          ENDIF
          m.curso = qcurso
          m.cedula = xcedula
          m.cur = qcurso
          m.plancobro = m.planco
          mrecibo = 0
          SELECT 17
          DO WHILE .T.
               IF RLOCK()
                    IF nopaga =  ;
                       .F.
                         REPLACE recibo  ;
                                 WITH  ;
                                 recibo +  ;
                                 1
                         mrecibo =  ;
                          recibo
                         UNLOCK
                    ENDIF
                    EXIT
               ENDIF
          ENDDO
          SELECT 2
          mpagado = mmontop +  ;
                    xmonto_pag
          ii = 1
          FOR ii = 1 TO xnro1
               lk = LTRIM(STR(ii))
               if (mpagado+pagado&lk);
>= m.cuota&lk
                    xpagado=(m.cuota&lk-pagado&lk)
                    SELECT 9
                    DO WHILE .T.
                         IF FLOCK()
                              APPEND  ;
                               BLANK
                              REPLACE  ;
                               recibo  ;
                               WITH  ;
                               mrecibo,  ;
                               cedula  ;
                               WITH  ;
                               m.cedula
                              REPLACE  ;
                               curso  ;
                               WITH  ;
                               m.cur,  ;
                               plancobro  ;
                               WITH  ;
                               m.plancobro
                              REPLACE  ;
                               seccion  ;
                               WITH  ;
                               m.semestre,  ;
                               fecha_pago  ;
                               WITH  ;
                               mfechap
                              REPLACE  ;
                               nrocuota  ;
                               WITH  ;
                               ii,  ;
                               tipo_pago  ;
                               WITH  ;
                               mtipop,  ;
                               nro_depo  ;
                               WITH  ;
                               mnrode
                              REPLACE  ;
                               cod_banco  ;
                               WITH  ;
                               mbanco,  ;
                               monto  ;
                               WITH  ;
                               xpagado,  ;
                               transa  ;
                               WITH  ;
                               'CO'
                              REPLACE  ;
                               cobrador  ;
                               WITH  ;
                               mcobrador
                              REPLACE  ;
                               lapso  ;
                               WITH  ;
                               lapmat
                              UNLOCK
                              EXIT
                         ENDIF
                    ENDDO
                    SELECT 16
                    DO WHILE .T.
                         IF FLOCK()
                              APPEND  ;
                               BLANK
                              REPLACE  ;
                               recibo  ;
                               WITH  ;
                               mrecibo,  ;
                               cedula  ;
                               WITH  ;
                               m.cedula
                              REPLACE  ;
                               curso  ;
                               WITH  ;
                               m.cur,  ;
                               plancobro  ;
                               WITH  ;
                               m.plancobro
                              REPLACE  ;
                               seccion  ;
                               WITH  ;
                               m.semestre,  ;
                               fecha_pago  ;
                               WITH  ;
                               mfechap
                              REPLACE  ;
                               nrocuota  ;
                               WITH  ;
                               ii,  ;
                               tipo_pago  ;
                               WITH  ;
                               mtipop,  ;
                               nro_depo  ;
                               WITH  ;
                               mnrode
                              REPLACE  ;
                               cod_banco  ;
                               WITH  ;
                               mbanco,  ;
                               monto  ;
                               WITH  ;
                               xpagado,  ;
                               transa  ;
                               WITH  ;
                               'CO'
                              REPLACE  ;
                               cobrador  ;
                               WITH  ;
                               mcobrador
                              REPLACE  ;
                               lapso  ;
                               WITH  ;
                               lapmat
                              UNLOCK
                              EXIT
                         ENDIF
                    ENDDO
                    SELECT 2
                    mpagado=mpagado-(m.cuota&lk-pagado&lk)
               ELSE
                    IF mpagado >  ;
                       0
                         SELECT 9
                         DO WHILE  ;
                            .T.
                              IF FLOCK()
                                   APPEND BLANK
                                   REPLACE recibo WITH mrecibo, cedula WITH m.cedula
                                   REPLACE curso WITH m.cur, plancobro WITH m.plancobro
                                   REPLACE seccion WITH m.semestre, fecha_pago WITH mfechap
                                   REPLACE tipo_pago WITH mtipop, nro_depo WITH mnrode
                                   REPLACE cod_banco WITH mbanco, monto WITH mpagado
                                   REPLACE nrocuota WITH ii, transa WITH 'AB'
                                   REPLACE lapso WITH lapmat
                                   UNLOCK
                                   EXIT
                              ENDIF
                         ENDDO
                         SELECT 16
                         DO WHILE  ;
                            .T.
                              IF FLOCK()
                                   APPEND BLANK
                                   REPLACE recibo WITH mrecibo, cedula WITH m.cedula
                                   REPLACE curso WITH m.cur, plancobro WITH m.plancobro
                                   REPLACE seccion WITH m.semestre, fecha_pago WITH mfechap
                                   REPLACE tipo_pago WITH mtipop, nro_depo WITH mnrode
                                   REPLACE cod_banco WITH mbanco, monto WITH mpagado
                                   REPLACE nrocuota WITH ii, transa WITH 'AB'
                                   REPLACE lapso WITH lapmat
                                   UNLOCK
                                   EXIT
                              ENDIF
                         ENDDO
                    ENDIF
                    SELECT 2
                    EXIT
               ENDIF
          ENDFOR
          WAIT WINDOW  ;
               'Nro. del Recibo Generado: ' +  ;
               LTRIM(STR(mrecibo)) +  ;
               CHR(13) +  ;
               'Presione Cualquier Tecla para continuar..'
          SELECT 2
          SET ORDER TO ced
          SEEK xcedula
          m.cuco = IIF(FOUND(), 0,  ;
                   1)
          m.cur = qcurso
          SELECT 8
          xnrocuotas = nrocuotas
          m.plancobro = m.planco
          m.total_deud = 0
          m.montopagad = 0
          mm = 1
          m.nrocuotas = xnro
          m.ultimacuot = m.cubre
          SELECT 11
          SET ORDER TO codcuota
          SEEK lapmat + m.planco +  ;
               STR(1)
          swp = 0
          suma = 0
          DO WHILE  .NOT. EOF()
               IF lapso <> lapmat
                    SELECT 11
                    SKIP
                    LOOP
               ENDIF
               IF codigo <>  ;
                  m.planco
                    SELECT 11
                    SKIP
                    LOOP
               ENDIF
               IF swp = 0
                    mcuota = cuota
                    swp = 1
               ENDIF
               IF mcuota <> cuota
                    lm = LTRIM(STR(mm))
                    m.total_deud =  ;
                     m.total_deud +  ;
                     suma
                    m.cuota&lm=suma
                    mm = mm + 1
                    suma = 0.00 
                    mcuota = cuota
               ENDIF
               ii = 1
               FOR ii = 1 TO 10
                    lk = LTRIM(STR(ii))
                    if concepto&lk=space(2)
                         LOOP
                    ENDIF
                    if monto&lk=0
                         LOOP
                    ENDIF
                    suma=suma+monto&lk
               ENDFOR
               SKIP
          ENDDO
          lm = LTRIM(STR(mm))
          m.total_deud = m.total_deud +  ;
                         suma
          m.cuota&lm=suma
          m.cuco = 0
          SELECT 2
          IF  .NOT. SEEK(xcedula)
               m.cuco = 1
          ENDIF
          DO WHILE .T.
               IF FLOCK()
                    IF m.cuco = 1
                         APPEND BLANK
                    ENDIF
                    GATHER MEMVAR  ;
                           MEMO
                    mpagado = mmontop +  ;
                              xmonto_pag
                    ii = 1
                    FOR ii = 1 TO  ;
                        xnro1
                         lk = LTRIM(STR(ii))
                         if (mpagado+pagado&lk);
>= m.cuota&lk
                              mpagado=mpagado-(m.cuota&lk-pagado&lk)
                              REPLACE  ;
                               ultimacuot  ;
                               WITH  ;
                               ultimacuot +  ;
                               1
                              x=(m.cuota&lk-pagado&lk)
                              REPLACE  ;
                               monto_paga  ;
                               WITH  ;
                               monto_paga +  ;
                               x
                              repla pagado&lk;
with m.cuota&lk
                              REPLACE  ;
                               nrorecibo  ;
                               WITH  ;
                               mrecibo
                              REPLACE  ;
                               fecha_ultp  ;
                               WITH  ;
                               mfechap
                         ELSE
                              IF mmontop >  ;
                                 0
                                   repla pagado&lk with pagado&lk+mpagado
                                   REPLACE monto_paga WITH monto_paga + mpagado
                                   REPLACE nrorecibo WITH mrecibo
                                   REPLACE fecha_ultp WITH mfechap
                              ENDIF
                              EXIT
                         ENDIF
                    ENDFOR
                    UNLOCK
                    EXIT
               ENDIF
          ENDDO
     ELSE
          SELECT 2
          m.planco = plancobro
          p = 1
          DO WHILE .T.
               @ 16, 44 SAY  ;
                 'P/Cobro:' GET  ;
                 m.planco PICTURE  ;
                 '@!'
               READ
               IF LASTKEY() = 27
                    p = salida()
                    IF p = 2
                         EXIT
                    ENDIF
                    LOOP
               ENDIF
               SELECT 8
               IF  .NOT.  ;
                   SEEK(lapmat +  ;
                   m.planco)
                    SELECT 8
                    SET FILTER TO lapso;
= lapmat
                    m.planco = boxfield('plancobr', ;
                               'codigo')
               ENDIF
               mdes1 = ALLTRIM(descripcio)
               @ 16, 44 SAY  ;
                 'P/Cobro:' GET  ;
                 m.planco PICTURE  ;
                 '@!'
               @ 16, COL() + 1  ;
                 SAY mdes1  ;
                 PICTURE '@!s17'
               CLEAR GETS
               EXIT
          ENDDO
          m.plancobro = m.planco
          SELECT 2
          m.cuco = 0
          IF  .NOT. SEEK(xcedula)
               m.cuco = 1
          ENDIF
          DO WHILE .T.
               IF FLOCK()
                    IF m.cuco = 1
                         APPEND BLANK
                    ENDIF
                    GATHER MEMVAR  ;
                           MEMO
                    UNLOCK
                    EXIT
               ENDIF
          ENDDO
     ENDIF
     opc = acepta( ;
           'Ir a Proc. Inscripciขn Mat.Adicional' ;
           )
     IF opc = 1
          DO scp2222b
          ON KEY LABEL tab
          RELEASE WINDOW record1
          CLOSE DATABASES
          RETURN
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
     RELEASE WINDOW reg
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
FUNCTION mbancos
PARAMETER xban
SELECT bancos
IF  .NOT. SEEK(xban)
     mbanco = boxfield('BANCOS', ;
              'CODIGO')
ENDIF
SHOW GET mbanco
RETURN .T.
*
FUNCTION mcobrad
PARAMETER xban
SELECT cobrador
IF  .NOT. SEEK(xban)
     mcobrador = boxfield('COBRADOR', ;
                 'CODIGO')
ENDIF
SHOW GET mcobrador
RETURN .T.
*
FUNCTION deuda_ven1
DEFINE WINDOW deuda FROM 00, 00  ;
       TO 24, 79 SHADOW TITLE  ;
       'Registro de Control de Deuda Anterior'  ;
       COLOR SCHEME 5
ACTIVATE WINDOW deuda
@ 00, 00 SAY  ;
  'ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป'
@ 01, 00 SAY  ;
  'บ C.I. :            Alumno:                                                  บ'
@ 02, 00 SAY  ;
  'บ Menciขn :                  Semestre:    Plan de Cobro:       Lapso:        บ'
@ 03, 00 SAY  ;
  'วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ'
@ 04, 00 SAY  ;
  'บ    Total Deuda Vencida:               Fecha de Pago    :                   บ'
@ 05, 00 SAY  ;
  'บ   Monto Pagado/Abonado:               Tipo Pago (E/D/C):                   บ'
@ 06, 00 SAY  ;
  'บ  Saldo Pendiente Pagar:               Nro. Dep./Cheq   :                   บ'
@ 07, 00 SAY  ;
  'บ            % Descuento:               Banco :                              บ'
@ 08, 00 SAY  ;
  'บ  Monto Cancelar/Abonar:               Cobrador :                           บ'
@ 09, 00 SAY  ;
  'บ  Nro. ณ  Descripciขn de Conceptos           ณ Fecha Vencmto. ณ Monto Cuota บ'  ;
  COLOR N/W 
@ 10, 00 SAY  ;
  'บ       ณ                                     ณ                ณ             บ'
@ 11, 00 SAY  ;
  'บ       ณ                                     ณ                ณ             บ'
@ 12, 00 SAY  ;
  'บ       ณ                                     ณ                ณ             บ'
@ 13, 00 SAY  ;
  'บ       ณ                                     ณ                ณ             บ'
@ 14, 00 SAY  ;
  'บ       ณ                                     ณ                ณ             บ'
@ 15, 00 SAY  ;
  'บ       ณ                                     ณ                ณ             บ'
@ 16, 00 SAY  ;
  'บ       ณ                                     ณ                ณ             บ'
@ 17, 00 SAY  ;
  'บ       ณ                                     ณ                ณ             บ'
@ 18, 00 SAY  ;
  'บ       ณ                                     ณ                ณ             บ'
@ 19, 00 SAY  ;
  'บ       ณ                                     ณ                ณ             บ'
@ 20, 00 SAY  ;
  'บ       ณ                                     ณ                ณ             บ'
@ 21, 00 SAY  ;
  'วฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤถ'
@ 22, 00 SAY  ;
  'บ                                                              ณ             บ'
SELECT 7
SCATTER MEMO MEMVAR
m.saldo = total_d - monto_p
m.lapsomat = lapsomat
SELECT 18
IF m.lapsomat <> SPACE(6)
     xlap = m.lapsomat
     ch1 = LEFT(xlap, 2)
     ch2 = ALLTRIM(STR(YEAR(CTOD( ;
           '01/01/' + RIGHT(xlap,  ;
           2)))))
     use ma&xlap again alias nuevo order;
ced
     SEEK xcedula
     SCATTER MEMO MEMVAR
     m.curso = cur
     DO CASE
          CASE m.curso = '32012'
               m.mencion = 'BASICA'
          CASE m.curso = '31022'
               m.mencion = 'CIENCIAS'
          CASE m.curso = '31023'
               m.mencion = 'HUMANIDADES'
     ENDCASE
ELSE
     m.cur = SPACE(5)
     m.curso = SPACE(5)
     m.mencion = SPACE(11)
     m.semestre = SPACE(1)
     m.plancobro = SPACE(5)
     m.ch1 = SPACE(2)
     m.ch2 = SPACE(4)
     m.turno = SPACE(4)
ENDIF
SELECT 1
@ 01, 08 GET m.cedula PICTURE  ;
  '@!'
@ 01, 27 GET m.nombres PICTURE  ;
  '@!'
@ 02, 11 GET m.mencion PICTURE  ;
  '@!'
@ 02, 38 GET m.semestre PICTURE  ;
  '@!'
@ 02, 56 GET m.plancobro PICTURE  ;
  '@!'
@ 02, 70 GET ch1 PICTURE '@!'
@ 02, 72 SAY '-'
@ 02, 73 GET ch2 PICTURE '@!'
SELECT 7
@ 04, 26 GET m.total_d PICTURE  ;
  '99,999,999.99'
@ 05, 26 GET m.monto_p PICTURE  ;
  '99,999,999.99'
@ 06, 26 GET m.saldo PICTURE  ;
  '99,999,999.99'
CLEAR GETS
RELEASE datos
DIMENSION datos( 7, 4)
EXTERNAL ARRAY datos
FOR q = 1 TO 7
     datos( q, 1) = SPACE(30)
     datos( q, 2) = 0.00 
     datos( q, 3) = SPACE(30)
     datos( q, 4) = 0.00 
ENDFOR
li = 10
w = 1
z = 1
IF m.cur <> SPACE(5)
     FOR q = 1 TO 12
          lq = LTRIM(STR(q))
          if pagado&lq >= cuota&lq
               LOOP
          ENDIF
          mcuota = q
          SELECT 11
          LOCATE FOR lapso = xlap  ;
                 .AND. codigo =  ;
                 m.plancobro  ;
                 .AND. cuota = q
          xfec = fecha_ven
          xcon = concepto1
          SELECT 12
          IF  .NOT. SEEK(xcon)
               xdes = 'SIN DESCRIPCION'
          ELSE
               xdes = ALLTRIM(descripcio)
          ENDIF
          SELECT 18
          @ li, 02 SAY  ;
            LTRIM(STR(q)) PICTURE  ;
            '99'
          @ li, 11 SAY xdes
          @ li, 51 SAY xfec  ;
            PICTURE '@E'
          @li,65 say cuota&lq pict '99,999,999.99'
          li = li + 1
          IF w > 7
               datos( z, 3) =  ;
                    xdes
               datos(z,4)=cuota&lq
               z = z + 1
          ELSE
               datos( w, 1) =  ;
                    xdes
               datos(w,2)=cuota&lq
          ENDIF
          w = w + 1
     ENDFOR
ELSE
     FOR jj = 1 TO 5
          lj = LTRIM(STR(jj))
          if monto&lj=0
               LOOP
          ENDIF
          datos(jj,1)=alltrim(concepto&lj)
          datos(jj,2)=monto&lj
          @ li, 02 SAY jj PICTURE  ;
            '99'
          @li,11 say concepto&lj
          @li,65 say monto&lj pict '99,999,999.99'
          li = li + 1
     ENDFOR
ENDIF
@ 22, 65 SAY m.saldo PICTURE  ;
  '99,999,999.99'
m.fechap = DATE()
m.tipop = 'Efectivo'
m.nrode = SPACE(20)
m.banco = SPACE(2)
m.montop = 0.00 
m.desban = SPACE(20)
m.choice = 1
m.cobrador = SPACE(2)
m.descuento = 0.00 
SAVE SCREEN TO ven
DO WHILE .T.
     RESTORE SCREEN FROM ven
     @ 04, 58 GET m.fechap  ;
       PICTURE '@E'
     @ 05, 58 GET m.tipop DEFAULT  ;
       'Efectivo' PICTURE  ;
       '@!M Deposito,Cheque,Efectivo'
     READ
     IF LASTKEY() = 27
          p = salida()
          IF p = 2
               RELEASE WINDOW  ;
                       deuda
               RETURN .F.
          ENDIF
          LOOP
     ENDIF
     IF m.tipop <> 'Efectivo'
          @ 06, 58 GET m.nrode  ;
            PICTURE '@!S15' VALID  ;
            m.nrode <> SPACE(20)  ;
            ERROR  ;
            'El Nฃmero de la Transacciขn esta en blanco'
          @ 07, 47 GET m.banco  ;
            PICTURE '@!' VALID  ;
            mbancos(m.banco)
          READ
          SELECT 10
          IF  .NOT. SEEK(m.banco)
               m.banco = boxfield('bancos', ;
                         'codigo')
          ENDIF
          SELECT 9
          SET ORDER TO bandepo
          IF SEEK(m.banco +  ;
             m.nrode)
               IF status <> 'D'
                    = msgerro( ;
                      'Ya existe un registro de pagos con este numero, verifique' ;
                      )
                    LOOP
               ENDIF
          ENDIF
          SELECT 10
          @ 07, 47 SAY m.banco  ;
            PICTURE '@!' COLOR W+/ ;
            W 
          @ 07, COL() + 2 SAY  ;
            LEFT(descripcio, 25)
     ENDIF
     IF m.monto_p = 0
          @ 07, 26 GET  ;
            m.descuento PICTURE  ;
            '999' VALID  ;
            m.descuento <= 100  ;
            ERROR  ;
            'El Porcentaje de Descuento es invalido, verifique'
          READ
          IF LASTKEY() = 27
               p = salida()
               IF p = 2
                    RELEASE WINDOW  ;
                            deuda
                    RETURN .F.
               ENDIF
               LOOP
          ENDIF
          m.porc = m.descuento /  ;
                   100
          m.montop = m.saldo -  ;
                     (m.saldo *  ;
                     m.porc)
     ENDIF
     @ 08, 26 GET m.montop  ;
       PICTURE '99,999,999.99'
     @ 08, 50 GET m.cobrador  ;
       PICTURE '@!' VALID  ;
       mcobrad(m.cobrador)
     READ
     IF LASTKEY() = 27
          p = salida()
          IF p = 2
               RELEASE WINDOW  ;
                       deuda
               RETURN .F.
          ENDIF
          LOOP
     ENDIF
     IF m.descuento > 0
          m.porc = m.descuento /  ;
                   100
          nuevosaldo = m.saldo -  ;
                       (m.saldo *  ;
                       m.porc)
          IF m.montop <= 0 .OR.  ;
             m.montop >  ;
             nuevosaldo
               = msgerro( ;
                 'El Monto a Pagar por Concepto de Mensualidad es invalido, verifique' ;
                 )
               LOOP
          ENDIF
          IF nuevosaldo >  ;
             m.montop
               = msgerro( ;
                 'El Monto a cancelar es invalido, no se permite abonos para esta modalidad' ;
                 )
               LOOP
          ENDIF
     ELSE
          IF m.montop = 0 .OR.  ;
             m.montop > m.saldo
               = msgerro( ;
                 'El Monto a Pagar por Concepto de Mensualidad es invalido, verifique' ;
                 )
               LOOP
          ENDIF
     ENDIF
     SELECT cobrador
     IF  .NOT. SEEK(m.cobrador)
          m.cobrador = boxfield('cobrador', ;
                       'codigo')
     ENDIF
     SELECT 14
     @ 08, 50 SAY m.cobrador  ;
       PICTURE '@!' COLOR W+/W 
     @ 08, COL() + 2 SAY  ;
       LEFT(nombres, 20)
     mnomcob = ALLTRIM(nombres)
     opc = acepta( ;
           'Esta correcto este Monto a Cancelar' ;
           )
     IF opc = 2
          LOOP
     ENDIF
     EXIT
ENDDO
IF m.descuento > 0
     m.porc = m.descuento / 100
     nuevosaldo = (m.saldo *  ;
                  m.porc) -  ;
                  m.montop
ELSE
     nuevosaldo = m.saldo -  ;
                  m.montop
ENDIF
opce = acepta( ;
       'Proceso este Pago de Mensualidad' ;
       )
IF opce = 2
     RELEASE WINDOW deuda
     RETURN .F.
ENDIF
mrecibo = 0
SELECT 18
p = printer()
IF p = 2
     SELECT 13
     DO WHILE .T.
          IF RLOCK()
               REPLACE recibo  ;
                       WITH  ;
                       recibo +  ;
                       1
               mrecibo = recibo
               UNLOCK
               EXIT
          ENDIF
     ENDDO
     SELECT 18
     WAIT WINDOW  ;
          'Nro. del Recibo Generado: ' +  ;
          LTRIM(STR(mrecibo)) +  ;
          CHR(13) +  ;
          'Presione Cualquier Tecla para continuar..'
ELSE
     SELECT 13
     DO WHILE .T.
          IF RLOCK()
               REPLACE recibo  ;
                       WITH  ;
                       recibo +  ;
                       1
               mrecibo = recibo
               UNLOCK
               EXIT
          ENDIF
     ENDDO
     SELECT 18
     SET DEVICE TO PRINTER
     @ 0, 1 SAY '  ' + CHR(27) +  ;
       CHR(67) + CHR(33)
     @ 00, 00 SAY CHR(18) +  ;
       CHR(20)
     @ 00, 00 SAY CHR(14) +  ;
       CHR(15) + titulo + CHR(18) +  ;
       CHR(20) + ' '
     @ 01, 00 SAY  ;
       'RIF .                                                   RECIBO Nro.:          '
     @ 01, 04 SAY l_rif
     @ 01, 68 SAY CHR(14) + ng +  ;
       LTRIM(STR(mrecibo)) + ngn
     @ 02, 00 SAY CHR(15) + ' '
     @ 02, 00 SAY l_direccio
     @ 02, PCOL() SAY CHR(18) +  ;
       CHR(20) + ' '
     @ 02, 00 SAY  ;
       '                                                        FECHA PAGO:          '
     @ 02, 68 SAY m.fechap  ;
       PICTURE '@E'
     @ 03, 00 SAY  ;
       '                                                      '
     @ 03, 28 SAY CHR(14) +  ;
       CHR(15) + 'RECIBO DE PAGO' +  ;
       CHR(18) + CHR(20) + ' '
     @ 04, 00 SAY  ;
       '                                                      ษออออออออออออออออออออป '
     @ 05, 00 SAY  ;
       'C.I.:                                                 บ Bs.                บ '
     @ 05, 61 SAY m.montop  ;
       PICTURE '9,999,999.99'
     @ 05, 06 SAY m.cedula  ;
       PICTURE '!-99999999'
     @ 06, 00 SAY  ;
       'ALUMNO:                                               ศออออออออออออออออออออผ '
     @ 06, 08 SAY m.nombres  ;
       PICTURE '@!s45'
     @ 07, 00 SAY 'SEMESTRE: ' +  ;
       m.semestre
     @ 08, 00 SAY 'TURNO   : ' +  ;
       m.turno
     xcantidad = letras(m.montop)
     xcantidad = ALLTRIM(xcantidad)
     @ 09, 00 SAY  ;
       'Hemos recibido la cantidad de:'
     @ 10, 00 SAY xcantidad  ;
       PICTURE '@!'
     li = 11
     @ li, 00 SAY  ;
       'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ' +  ;
       ng
     li = li + 1
     @ li, 00 SAY  ;
       '  C O N C E P T O S                                          M O N T O       ' +  ;
       ngn
     li = li + 1
     @ li, 00 SAY  ;
       'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ'
     li = li + 1
     @ li, 00 SAY CHR(15) + ' '
     FOR s = 1 TO 7
          ls = LTRIM(STR(s))
          IF datos(s,1) =  ;
             SPACE(30)
               LOOP
          ENDIF
          @ li, 01 SAY datos(s,1)  ;
            PICTURE '@!s22'
          @ li, 24 SAY '('
          @ li, 25 SAY datos(s,2)  ;
            PICTURE '999,999.99'
          @ li, 35 SAY ')'
          IF s <= 4
               IF datos(s,3) <>  ;
                  SPACE(30)
                    @ li, 39 SAY  ;
                      datos(s,3)  ;
                      PICTURE  ;
                      '@!s22'
                    @ li, 60 SAY  ;
                      '('
                    @ li, 61 SAY  ;
                      datos(s,4)  ;
                      PICTURE  ;
                      '999,999.99'
                    @ li, 71 SAY  ;
                      ')'
               ENDIF
               IF s = 1
                    IF m.descuento >  ;
                       0
                         @ li,  ;
                           102  ;
                           SAY  ;
                           m.descuento  ;
                           PICTURE  ;
                           '999.99'
                         @ li,  ;
                           111  ;
                           SAY  ;
                           '% Descuento ->'
                         @ li,  ;
                           124  ;
                           SAY  ;
                           (m.saldo *  ;
                           m.porc)  ;
                           PICTURE  ;
                           '999,999.99'
                    ENDIF
                    IF datos(2,2) =  ;
                       0
                         li = li +  ;
                              1
                         @ li,  ;
                           100  ;
                           SAY  ;
                           'Monto Pagado/Abonado ->'
                         @ li,  ;
                           124  ;
                           SAY  ;
                           m.montop  ;
                           PICTURE  ;
                           '999,999.99'
                    ENDIF
               ENDIF
               IF s = 2
                    IF m.descuento >  ;
                       0
                         @ li,  ;
                           102  ;
                           SAY  ;
                           m.descuento  ;
                           PICTURE  ;
                           '999.99'
                         @ li,  ;
                           111  ;
                           SAY  ;
                           '% Descuento ->'
                         @ li,  ;
                           124  ;
                           SAY  ;
                           (m.saldo *  ;
                           m.porc)  ;
                           PICTURE  ;
                           '999,999.99'
                    ELSE
                         @ li,  ;
                           100  ;
                           SAY  ;
                           'Monto Pagado/Abonado ->'
                         @ li,  ;
                           124  ;
                           SAY  ;
                           m.montop  ;
                           PICTURE  ;
                           '999,999.99'
                    ENDIF
               ENDIF
               IF s = 3 .AND.  ;
                  m.descuento >  ;
                  0
                    @ li, 100 SAY  ;
                      'Monto Pagado/Abonado ->'
                    @ li, 124 SAY  ;
                      m.montop  ;
                      PICTURE  ;
                      '999,999.99'
               ENDIF
          ENDIF
          li = li + 1
     ENDFOR
     @ li, 00 SAY CHR(18) + ' '
     @ 21, 00 SAY  ;
       'ษอออออออออออออออออออออออออออออออหอออออออออออออออออออออออออออออออออออออออออออออป'
     @ 22, 00 SAY  ;
       'บ           N O T A             บ  EFECTIVO   [ ]   CHEQ/DEPOST [ ]           บ'
     IF m.tipop <> 'Efectivo'
          @ 22, 65 SAY 'X'
     ELSE
          @ 22, 47 SAY 'X'
     ENDIF
     @ 23, 00 SAY  ;
       'วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ  NRO. :                                     บ'
     @ 23, 41 SAY m.nrode PICTURE  ;
       '@!'
     @ 24, 00 SAY  ;
       'บ                               บ  BANCO:                                     บ'
     @ 24, 41 SAY m.desban
     @ 25, 00 SAY  ;
       'บ                               บ                                             บ'
     @ 26, 00 SAY  ;
       'บ                               บ                        ___________________  บ'
     @ 27, 00 SAY  ;
       'บ                               บ                           RECIBIDO POR:     บ'
     @ 28, 00 SAY  ;
       'ศอออออออออออออออออออออออออออออออสอออออออออออออออออออออออออออออออออออออออออออออผ'
     @ 29, 00 SAY  ;
       ' NOTA: Los Pagos Realizado no estan sujetos a devoluciones'
     @ 30, 00 SAY ' '
     EJECT
     SET DEVICE TO SCREEN
ENDIF
SELECT 9
DO WHILE .T.
     IF FLOCK()
          APPEND BLANK
          REPLACE recibo WITH  ;
                  mrecibo, cedula  ;
                  WITH m.cedula
          REPLACE curso WITH  ;
                  m.curso,  ;
                  plancobro WITH  ;
                  m.plancobro
          REPLACE seccion WITH  ;
                  m.semestre,  ;
                  fecha_pago WITH  ;
                  m.fechap
          REPLACE nrocuota WITH 0,  ;
                  tipo_pago WITH  ;
                  m.tipop,  ;
                  nro_depo WITH  ;
                  m.nrode
          REPLACE cod_banco WITH  ;
                  m.banco, monto  ;
                  WITH m.montop,  ;
                  transa WITH  ;
                  'AT'
          REPLACE cobrador WITH  ;
                  m.cobrador
          REPLACE lapso WITH  ;
                  lapmat
          UNLOCK
          EXIT
     ENDIF
ENDDO
SELECT 7
DO WHILE .T.
     IF RLOCK()
          REPLACE monto_p WITH  ;
                  monto_p +  ;
                  m.montop
          REPLACE descto WITH  ;
                  m.descuento
          UNLOCK
          EXIT
     ENDIF
ENDDO
RELEASE WINDOW deuda
RETURN IIF(nuevosaldo = 0, .T.,  ;
       .F.)
*
