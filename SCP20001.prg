CLOSE DATABASES
SET TALK OFF
SELECT 2
USE predatos AGAIN ALIAS predatos  ;
    ORDER control
SELECT 3
USE regpagos AGAIN ALIAS regpagos  ;
    ORDER codigo
SELECT 4
USE plancobr AGAIN ALIAS placobro  ;
    ORDER lapcod
SELECT 5
USE detacobro AGAIN ALIAS  ;
    detacobro ORDER lapcod
SELECT 6
USE bancos AGAIN ALIAS bancos  ;
    ORDER codigo
SELECT 1
USE datos AGAIN ALIAS datos ORDER  ;
    cedula
SELECT 9
use mp&lapmat
SET ORDER TO ced
DEFINE WINDOW scf FROM 20, 15 TO  ;
       23, 65 FLOAT SHADOW DOUBLE  ;
       COLOR SCHEME 5
DEFINE WINDOW record1 FROM 01, 00  ;
       TO 22, 79 FLOAT NOCLOSE  ;
       SHADOW TITLE  ;
       'Registro de Pre-Inscripci¢n (SCP20001)'  ;
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
            '³ C.I. :             Apellidos/Nombres :                                     ³'
          @ 02, 00 SAY  ;
            '³ Sexo:   Fecha Nac.:            Lugar Nacmto. :                             ³'
          @ 03, 00 SAY  ;
            '³ Estado Nacmto:                                        Ini./Est.:           ³'
          @ 04, 00 SAY  ;
            '³                                                                            ³'
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
            '³ Menci¢n :       (                                )        Semestre: (   )  ³'
          @ 16, 00 SAY  ;
            '³ Plan de Cobro :                                                            ³'
          @ 17, 00 SAY  ;
            '³ Monto Pre-Inscrici¢n:                                                      ³'
          @ 18, 00 SAY  ;
            'ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾'
          m.pote = 2
          SAVE SCREEN TO pelota
     ENDIF
     RESTORE SCREEN FROM pelota
     @ 01, 09 GET m.nac PICTURE  ;
       '@!M V,E,C,R' MESSAGE  ;
       'ingrese la Nacionalidad del Alumno'
     @ 01, 10 SAY '-'
     @ 01, 11 GET m.ced PICTURE  ;
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
     SCATTER MEMO MEMVAR
     m.cedula = xcedula
     m.nac1 = LEFT(m.rep_legal,  ;
              1)
     m.ced1 = VAL(SUBSTR(m.rep_legal,  ;
              3, 10))
     m.saldo = total_d - monto_p
     IF m.saldo > 0
          WAIT WINDOW  ;
               'Este Alumno tiene una Deuda Pendiente Correspondiente al Lapso : ' +  ;
               m.lapsomat +  ;
               CHR(13) +  ;
               'que representa un monto de : ' +  ;
               LTRIM(STR(m.saldo,  ;
               13, 2)) + CHR(13) +  ;
               'verifique'
     ENDIF
     SELECT 9
     IF SEEK(xcedula)
          = msgerro( ;
            'Este alumno ya esta inscrito, verifique' ;
            )
          m.nac = 'V'
          m.ced = 0
          LOOP
     ENDIF
     SELECT 2
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
     m.planco = plancobro
     qcurso = m.cur
     SELECT 1
     SCATTER MEMO MEMVAR
     m.nac1 = LEFT(m.rep_legal,  ;
              1)
     m.ced1 = VAL(SUBSTR(m.rep_legal,  ;
              3, 10))
     @ 01, 41 GET m.nombres  ;
       PICTURE '@!S35' MESSAGE  ;
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
     @ 03, 67 GET m.entidad  ;
       PICTURE '@!' MESSAGE  ;
       'Ingrese El Estado de Nacimiento Abreviado ¢ (DF, EX)'
     CLEAR GETS
     DO WHILE .T.
          @ 01, 41 GET m.nombres  ;
            PICTURE '@!S35'  ;
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
          @ 03, 67 GET m.entidad  ;
            PICTURE '@!' MESSAGE  ;
            'Ingrese El Estado de Nacimiento Abreviado ¢ (DF, EX)'
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
                      'Profesi¢n u Oficio de la Madre'
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
                      'Profesi¢n u Oficio del Padre'
                    @ 11, 26 GET  ;
                      m.direccion1  ;
                      PICTURE  ;
                      '@!'  ;
                      MESSAGE  ;
                      'Direcci¢n o Habitaci¢n'
                    @ 12, 26 GET  ;
                      m.direccion2  ;
                      PICTURE  ;
                      '@!'  ;
                      MESSAGE  ;
                      'Direcci¢n o Habitaci¢n'
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
     qcurso = m.cur
     m.codigo = m.seccionr
     SELECT 2
     IF FOUND()
          = msgerro( ;
            'El Alumno ya esta Pre-Inscrito' ;
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
               ENDIF
               m.nac = 'V'
               m.ced = 0
               LOOP
          ENDIF
     ENDIF
     m.codplan = qcurso
     m.curso = qcurso
     paso = 0
     DO WHILE .T.
          @ 15, 12 GET qcurso  ;
            PICTURE  ;
            '@!M 32012,31022,31023'  ;
            MESSAGE  ;
            'Ingrese la Menci¢n'
          READ
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
          IF SUBSTR(m.codplan, 1,  ;
             2) = '32'
               @ 15, 72 GET  ;
                 m.codigo DEFAULT  ;
                 '07' PICTURE  ;
                 '@!M 07,08,09,10,11,12'  ;
                 MESSAGE  ;
                 'Ingrese el Curso'
          ELSE
               @ 15, 72 GET  ;
                 m.codigo DEFAULT  ;
                 '01' PICTURE  ;
                 '@!M 01,02,03,04'
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
                         SET ORDER TO;
cedula
                         SEEK m.cedula
                         m.cuco =  ;
                          IIF(FOUND(),  ;
                          0, 1)
                         DO WHILE  ;
                            .T.
                              IF FLOCK()
                                   IF m.cuco = 1
                                        APPEND BLANK
                                   ENDIF
                                   GATHER MEMVAR MEMO
                                   UNLOCK
                                   EXIT
                              ENDIF
                         ENDDO
                    ENDIF
                    m.nac = 'V'
                    m.ced = SPACE(10)
                    paso = 1
                    EXIT
               ENDIF
               LOOP
          ENDIF
          opc = acepta( ;
                'Estan Correctos Datos Academicos' ;
                )
          IF opc = 2
               LOOP
          ENDIF
          EXIT
     ENDDO
     IF paso = 1
          m.nac = 'V'
          m.ced = SPACE(10)
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
     m.seccionr = m.codigo
     p = 1
     DO WHILE .T.
          @ 16, 18 GET m.planco  ;
            PICTURE '@!'
          READ
          IF LASTKEY() = 27
               p = salida()
               IF p = 2
                    EXIT
               ENDIF
               LOOP
          ENDIF
          SELECT 4
          IF  .NOT. SEEK(lapmat +  ;
              m.planco)
               SET FILTER TO lapso = lapmat
               m.planco = boxfield('plancobr', ;
                          'codigo')
               SET FILTER TO
          ENDIF
          mdes1 = ALLTRIM(descripcio)
          mmontod = pre_inscri
          @ 16, 18 SAY m.planco  ;
            PICTURE '@!' COLOR W+/ ;
            W 
          @ 16, 24 SAY mdes1  ;
            PICTURE '@!'
          @ 17, 24 SAY mmontod  ;
            PICTURE  ;
            '99,999,999.99'
          opc = acepta( ;
                'Acepta este Plan de Cobro' ;
                )
          IF opc = 2
               LOOP
          ENDIF
          EXIT
     ENDDO
     IF p = 2
          opc = acepta( ;
                'Acepta los Datos personales' ;
                )
          IF opc = 1
               SELECT 1
               SET ORDER TO cedula
               SEEK xcedula
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
               m.nac = 'V'
               m.ced = SPACE(10)
               LOOP
          ENDIF
     ENDIF
     mmontop = 0.00 
     SELECT 2
     IF  .NOT. SEEK(xcedula)
          DEFINE WINDOW reg FROM  ;
                 11, 20 TO 22, 75  ;
                 GROW FLOAT CLOSE  ;
                 ZOOM SHADOW  ;
                 TITLE  ;
                 'Registro de Control de Pago'  ;
                 MINIMIZE SYSTEM  ;
                 COLOR SCHEME 5
          MOVE WINDOW reg CENTER
          ACTIVATE WINDOW reg
          @ 01, 00 SAY  ;
            '  Fecha Pago :                            '
          @ 02, 00 SAY  ;
            '  Tipo Pago (E/D/C):                      '
          @ 03, 00 SAY  ;
            '  Nro. Deposito:                          '
          @ 04, 00 SAY  ;
            '  Banco:                                  '
          @ 05, 00 SAY  ;
            '  Monto  Pre-Inscripci¢n:                 '
          @ 06, 00 SAY  ;
            '  Monto a Cancelar      :                 '
          @ 07, 00 TO 07, WCOLS()
          mfechap = DATE()
          mtipop = 'Deposito'
          mnrode = SPACE(25)
          mbanco = SPACE(2)
          mmontop = 0.00 
          mdesban = SPACE(20)
          m.choice = 1
          swp = 0
          SAVE SCREEN TO hi
          DO WHILE .T.
               RESTORE SCREEN  ;
                       FROM hi
               @ 01, 16 GET  ;
                 mfechap PICTURE  ;
                 '@E'
               @ 02, 21 GET  ;
                 mtipop DEFAULT  ;
                 'Deposito'  ;
                 PICTURE  ;
                 '@!M Deposito,Cheque,Efectivo'
               READ
               IF mtipop <>  ;
                  'Efectivo'
                    @ 03, 18 GET  ;
                      mnrode  ;
                      PICTURE  ;
                      '@!' VALID  ;
                      mnrode <>  ;
                      SPACE(25)  ;
                      ERROR  ;
                      'El Numero del Deposito esta en blanco'
                    @ 04, 10 GET  ;
                      mbanco  ;
                      PICTURE  ;
                      '@!' VALID  ;
                      validar(VARREAD())
                    READ
                    SELECT bancos
                    IF  .NOT.  ;
                        SEEK(mbanco)
                         mbanco =  ;
                          boxfield('bancos', ;
                          'codigo')
                    ENDIF
                    @ 04, 10 SAY  ;
                      mbanco  ;
                      PICTURE  ;
                      '@!' COLOR  ;
                      W+/W 
                    @ 04, COL() +  ;
                      2 SAY  ;
                      LEFT(descripcio,  ;
                      25)
                    mdesban = ALLTRIM(descripcio)
               ENDIF
               @ 05, 26 SAY  ;
                 mmontod PICTURE  ;
                 '99,999,999.99'  ;
                 COLOR W+/W 
               @ 06, 26 GET  ;
                 mmontop PICTURE  ;
                 '99,999,999.99'
               @ 08, 10 GET  ;
                 m.choice DEFAULT  ;
                 1 SIZE 1, 10, 5  ;
                 PICTURE  ;
                 '@*HT \<Aceptar;\<Cancelar'  ;
                 COLOR SCHEME 5
               READ CYCLE
               IF LASTKEY() = 27
                    LOOP
               ENDIF
               EXIT
          ENDDO
          DEACTIVATE WINDOW reg
          IF m.choice = 2
               opc = acepta( ;
                     'Acepta los Datos personales' ;
                     )
               IF opc = 1
                    SELECT 1
                    SET ORDER TO cedula
                    SEEK xcedula
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
          opce = acepta( ;
                 'Acepta la Informaci¢n' ;
                 )
          IF opce = 2
               opc = acepta( ;
                     'Acepta los Datos personales' ;
                     )
               IF opc = 1
                    SELECT 1
                    SET ORDER TO cedula
                    SEEK xcedula
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
          m.curso = qcurso
          m.cedula = xcedula
          m.cur = qcurso
          m.plancobro = m.planco
          m.pre_inscri = mmontop
          SELECT 2
          IF  .NOT. SEEK(xcedula)
               SELECT 8
               USE liceo
               DO WHILE .T.
                    IF RLOCK()
                         REPLACE control  ;
                                 WITH  ;
                                 control +  ;
                                 1
                         REPLACE recibo  ;
                                 WITH  ;
                                 recibo +  ;
                                 1
                         mrecibo =  ;
                          recibo
                         m.control =  ;
                          control +  ;
                          1
                         ACTIVATE  ;
                          WINDOW  ;
                          scf
                         x = 'Nro. de Control asignado: ' +  ;
                             LTRIM(STR(m.control))
                         @ 0, 0  ;
                           SAY  ;
                           SPACE(((WCOLS() -  ;
                           LEN(x) -  ;
                           6) /  ;
                           2) +  ;
                           3) +  ;
                           x
                         x = 'Presione cualquier tecla para continuar'
                         @ 1, 0  ;
                           SAY  ;
                           SPACE(((WCOLS() -  ;
                           LEN(x) -  ;
                           6) /  ;
                           2) +  ;
                           3) +  ;
                           x
                         WAIT ''
                         DEACTIVATE  ;
                          WINDOW  ;
                          scf
                         UNLOCK
                         EXIT
                    ENDIF
               ENDDO
               p = printer()
               IF p = 1 .AND.  ;
                  mmontop > 0
                    SET DEVICE TO PRINTER
                    @ 00, 00 SAY  ;
                      CHR(18) +  ;
                      CHR(20)
                    @ 00, 00 SAY  ;
                      CHR(14) +  ;
                      CHR(15) +  ;
                      titulo +  ;
                      CHR(18) +  ;
                      CHR(20) +  ;
                      ' '
                    @ 01, 00 SAY  ;
                      'C.I.:                                                    RECIBO Nro:           '
                    @ 01, 05 SAY  ;
                      m.cedula  ;
                      PICTURE  ;
                      '!-99999999'
                    @ 01, 69 SAY  ;
                      LTRIM(STR(mrecibo))
                    @ 02, 00 SAY  ;
                      'APELLIDOS/NOMBRES:                                       FECHA PAGO:           '
                    @ 02, 19 SAY  ;
                      m.nombres  ;
                      PICTURE  ;
                      '@!s35'
                    @ 02, 68 SAY  ;
                      mfechap  ;
                      PICTURE  ;
                      '@E'
                    @ 03, 00 SAY  ;
                      'MENCION:       (                                   )  GRADO/SECCION:           '
                    @ 03, 09 SAY  ;
                      m.cur  ;
                      PICTURE  ;
                      '@!'
                    @ 03, 16 SAY  ;
                      mmencion  ;
                      PICTURE  ;
                      '@!'
                    @ 03, 68 SAY  ;
                      m.seccionr  ;
                      PICTURE  ;
                      '@!'
                    @ 04, 00 SAY  ;
                      'PLAN COBRO:                                             Nro.CONTROL:           '
                    @ 04, 11 SAY  ;
                      m.planco  ;
                      PICTURE  ;
                      '@!'
                    @ 04, 19 SAY  ;
                      mdes1
                    @ 04, 68 SAY  ;
                      LTRIM(STR(m.control))
                    @ 05, 00 SAY  ;
                      'ÕÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¸'
                    @ 06, 00 SAY  ;
                      '³ ITEMS ³   C O N C E P T O S                                 ³     MONTO     ³'
                    @ 07, 00 SAY  ;
                      'ÔÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾'
                    @ 08, 00 SAY  ;
                      '    01    PRE-INSCRIPCION'
                    @ 08, 64 SAY  ;
                      mmontop  ;
                      PICTURE  ;
                      '99,999,999.99'
                    @ 20, 00 SAY  ;
                      'ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿'
                    @ 21, 00 SAY  ;
                      ' TIPO PAGO(E/D/C):                                 TOTALES -> ³               ³'
                    @ 21, 19 SAY  ;
                      mtipop
                    @ 21, 65 SAY  ;
                      mmontop  ;
                      PICTURE  ;
                      '99,999,999.99'
                    @ 22, 00 SAY  ;
                      ' Nro. Dep./Cheque:                                            ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ'
                    @ 22, 19 SAY  ;
                      mnrode  ;
                      PICTURE  ;
                      '@!'
                    @ 23, 00 SAY  ;
                      ' BANCO :                                                                       '
                    @ 23, 09 SAY  ;
                      mdesban
                    @ 25, 00 SAY  ;
                      '                                                            RECIBIDO POR:      '
                    @ 26, 00 SAY  ;
                      '                                                                               '
                    @ 27, 00 SAY  ;
                      '                                                           __________________  '
                    @ 28, 00 SAY  ;
                      '  '
                    SET PRINTER TO
                    SET DEVICE TO SCREEN
               ENDIF
          ENDIF
     ENDIF
     SELECT 1
     SET ORDER TO cedula
     SEEK xcedula
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
     IF mmontop > 0
          SELECT 3
          DO WHILE .T.
               IF FLOCK()
                    APPEND BLANK
                    REPLACE codigo  ;
                            WITH  ;
                            m.control,  ;
                            cedula  ;
                            WITH  ;
                            m.cedula
                    REPLACE curso  ;
                            WITH  ;
                            m.cur,  ;
                            plancobro  ;
                            WITH  ;
                            m.planco
                    REPLACE seccion  ;
                            WITH  ;
                            m.seccionr,  ;
                            fecha_pago  ;
                            WITH  ;
                            mfechap
                    REPLACE tipo_pago  ;
                            WITH  ;
                            mtipop,  ;
                            monto  ;
                            WITH  ;
                            mmontop,  ;
                            recibo  ;
                            WITH  ;
                            mrecibo
                    REPLACE lapso  ;
                            WITH  ;
                            lapmat
                    UNLOCK
                    EXIT
               ENDIF
          ENDDO
     ENDIF
     SELECT 2
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
          area = SELECT()
          SELECT 4
          ON KEY LABEL tab
          SET FILTER TO lapso = lapmat
          m.planco = boxfield('PLANCOBR', ;
                     'CODIGO')
          SET FILTER TO
          SHOW GET m.planco
          SELECT (area)
     CASE xvar = 'MBANCO'
          ON KEY LABEL tab
          mbanco = boxfield('BANCOS', ;
                   'CODIGO')
          SHOW GET mbanco
ENDCASE
ON KEY LABEL tab do validar with varread()
RETURN
*
