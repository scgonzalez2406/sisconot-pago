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
       'Eliminaci�n del Registro de Pre-Inscripci�n'  ;
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
            '����������������������������������������������������������������������������͸'
          @ 01, 00 SAY  ;
            '� C.I. :             Apellidos/Nombres :                                     �'
          @ 02, 00 SAY  ;
            '� Sexo:   Fecha Nac.:            Lugar Nacmto. :                             �'
          @ 03, 00 SAY  ;
            '� Estado Nacmto:                                        Ini./Est.:           �'
          @ 04, 00 SAY  ;
            '�                                                                            �'
          @ 05, 00 SAY  ;
            '����������������������������������������������������������������������������͵'
          @ 06, 00 SAY  ;
            '�              D A T O S    D E L    R E P R E S E N T A N T E               �'
          @ 07, 00 SAY  ;
            '����������������������������������������������������������������������������Ĵ'
          @ 08, 00 SAY  ;
            '� Nombre de la Madre:                          � Profesi�n :                 �'
          @ 09, 00 SAY  ;
            '� Nombre  del  Padre:                          � Profesi�n :                 �'
          @ 10, 00 SAY  ;
            '����������������������������������������������������������������������������Ĵ'
          @ 11, 00 SAY  ;
            '� Direcci�n Habitaci�n :                                                     �'
          @ 12, 00 SAY  ;
            '�                                                                            �'
          @ 13, 00 SAY  ;
            '� Telefonos :                                                                �'
          @ 14, 00 SAY  ;
            '����������������������������������������������������������������������������͵'
          @ 15, 00 SAY  ;
            '� Menci�n :       (                                )  Grado/Secci�n : (   )  �'
          @ 16, 00 SAY  ;
            '� Plan de Cobro :                                                            �'
          @ 17, 00 SAY  ;
            '� Monto Pre-Inscrici�n:                                                      �'
          @ 18, 00 SAY  ;
            '����������������������������������������������������������������������������;'
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
       'Ingrese el N�mero del Cedula del Alumno'  ;
       ERROR  ;
       'Ingrese al N�mero de Cedula'
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
          m.nac = 'V'
          m.ced = 0
          LOOP
     ENDIF
     SCATTER MEMO MEMVAR
     m.cedula = xcedula
     m.planco = plancobro
     qcurso = m.cur
     mmontod = pre_inscri
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
       'Ingrese El Estado de Nacimiento Abreviado � (DF, EX)'
     @ 04, 49 GET m.representa  ;
       PICTURE '@!s25' MESSAGE  ;
       'Ingrese el Nombre del Representante del Alumno en la Instituci�n'
     @ 08, 22 GET m.represent1  ;
       PICTURE '@!S24' MESSAGE  ;
       'Nombre de la Madre'
     @ 08, 61 GET m.profesion1  ;
       PICTURE '@!S17' MESSAGE  ;
       'Profesi�n u Oficio de la Madre'
     @ 09, 22 GET m.represent2  ;
       PICTURE '@!S24' MESSAGE  ;
       'Nombre del Padre'
     @ 09, 61 GET m.profesion2  ;
       PICTURE '@!S17' MESSAGE  ;
       'Profesi�n u Oficio del Padre'
     @ 11, 26 GET m.direccion1  ;
       PICTURE '@!' MESSAGE  ;
       'Direcci�n o Habitaci�n'
     @ 12, 26 GET m.direccion2  ;
       PICTURE '@!' MESSAGE  ;
       'Direcci�n o Habitaci�n'
     @ 13, 15 GET m.telefono  ;
       PICTURE '@!' MESSAGE  ;
       'Telefonos'
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
     @ 15, 12 GET qcurso PICTURE  ;
       '@!M 32012,31022,31023'  ;
       MESSAGE  ;
       'Ingrese la Menci�n'
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
     IF SUBSTR(m.codplan, 1, 2) =  ;
        '32'
          @ 15, 72 GET m.codigo  ;
            DEFAULT '07' PICTURE  ;
            '@!M 07,08,09,10,11,12'  ;
            MESSAGE  ;
            'Ingrese el Curso'
     ELSE
          @ 15, 72 GET m.codigo  ;
            DEFAULT '01' PICTURE  ;
            '@!M 01,02,03,04'
     ENDIF
     m.seccionr = m.codigo
     @ 16, 18 GET m.planco  ;
       PICTURE '@!'
     SELECT 4
     SEEK (lapmat + m.planco)
     mdes1 = ALLTRIM(descripcio)
     @ 16, 18 SAY m.planco  ;
       PICTURE '@!' COLOR W+/W 
     @ 16, 24 SAY mdes1 PICTURE  ;
       '@!'
     @ 17, 24 SAY mmontod PICTURE  ;
       '99,999,999.99'
     CLEAR GETS
     opc = acepta( ;
           'Elimino esta Pre-Inscripci�n' ;
           )
     IF opc = 2
          m.nac = 'V'
          m.ced = SPACE(10)
          LOOP
     ENDIF
     SELECT 2
     SET ORDER TO cedula
     SEEK xcedula
     DO WHILE .T.
          IF RLOCK()
               DELETE
               UNLOCK
               EXIT
          ENDIF
     ENDDO
     opc = acepta( ;
           'Elimino los Datos Personales' ;
           )
     IF opc = 2
          m.nac = 'V'
          m.ced = SPACE(10)
          LOOP
     ENDIF
     SELECT 1
     SET ORDER TO cedula
     SEEK xcedula
     DO WHILE .T.
          IF RLOCK()
               DELETE
               UNLOCK
               EXIT
          ENDIF
     ENDDO
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
          SELECT 4
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
ENDCASE
ON KEY LABEL tab do validar with varread()
RETURN
*
