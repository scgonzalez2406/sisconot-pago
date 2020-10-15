CLOSE DATABASES
SET TALK OFF
SET CENTURY ON
CLOSE DATABASES
unidad1 = 'UNO'
unidad2 = 'DOS'
unidad3 = 'TRES'
unidad4 = 'CUATRO'
unidad5 = 'CINCO'
unidad6 = 'SEIS'
unidad7 = 'SIETE'
unidad8 = 'OCHO'
unidad9 = 'NUEVE'
SELECT 2
use mp&lapmat
SET ORDER TO cedcur
SELECT 1
USE datos AGAIN ALIAS datos ORDER  ;
    cedula
DEFINE WINDOW a1 FROM 03, 00 TO  ;
       10, 79 FLOAT NOCLOSE  ;
       SHADOW NOMINIMIZE COLOR  ;
       SCHEME 5
ACTIVATE WINDOW a1
xd = 'Constancia de Inscripci¢n'
@ 0, 0 SAY SPACE(((WCOLS() -  ;
  LEN(xd) - 6) / 2) + 3) + xd
ng = CHR(27) + CHR(69)
ngn = CHR(27) + CHR(70)
unidad0 = ' '
unidad1 = 'UNO'
unidad2 = 'DOS'
unidad3 = 'TRES'
unidad4 = 'CUATRO'
unidad5 = 'CINCO'
unidad6 = 'SEIS'
unidad7 = 'SIETE'
unidad8 = 'OCHO'
unidad9 = 'NUEVE'
@ 01, 00 SAY  ;
  'ÕÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¸'
@ 02, 00 SAY  ;
  '³ C‚dula  :                Apellidos y Nombres :                            ³'
@ 03, 00 SAY  ;
  '³ Sexo:   Fecha Nac.:               Lugar Nac. :                            ³'
@ 04, 00 SAY  ;
  '³ C¢digo:          Secci¢n :           Turno:            Fecha:             ³'
@ 05, 00 SAY  ;
  'ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ'
SAVE SCREEN TO hola
kdirector = l_director
m.cargo = 'DIRECTOR    '
ON KEY LABEL f8 keybo chr(13)
ON KEY LABEL f9 keybo chr(13)
ON KEY LABEL tab do validar with varread()
m.nac = 'V'
m.ced = SPACE(10)
valida1 = 'ABCDEFGHIJKLMNOPQRSTUVXYZ'
m.curso = '32012'
m.codigo = SPACE(2)
m.turno = SPACE(3)
m.turnito = SPACE(12)
m.fecha = DATE()
m.equivale = SPACE(12)
m.semetre = '                '
DO WHILE .T.
     RESTORE SCREEN FROM hola
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
          RELEASE WINDOW a1
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
          p = msgerro( ;
              'Este Alumno no esta registrado en el sistema' ;
              )
          m.nac = 'V'
          m.ced = SPACE(10)
          LOOP
     ENDIF
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
     SELECT 2
     SET ORDER TO ced
     SEEK xcedula
     IF  .NOT. FOUND()
          = msgerro( ;
            'Este alumno no tiene matricula para este lapso' ;
            )
          m.nac = 'V'
          m.ced = SPACE(10)
          LOOP
     ENDIF
     IF status = 'X'
          = msgerro( ;
            'Este Alumno fue egresado de la Matricula, verifique' ;
            )
     ENDIF
     m.curso = cur
     m.codigo = semestre
     @ 04, 10 GET m.curso PICTURE  ;
       '@!M 32012,31022,31023'
     IF SUBSTR(m.curso, 1, 2) =  ;
        '32'
          @ 04, 29 GET m.codigo  ;
            DEFAULT '07' PICTURE  ;
            '@!M 07,08,09,10,11,12'
     ELSE
          @ 04, 29 GET m.codigo  ;
            DEFAULT '01' PICTURE  ;
            '@!M 01,02,03,04'
     ENDIF
     @ 04, 46 GET m.turno PICTURE  ;
       '@!M MAT,VES,NOC,SAB'
     READ
     CLEAR GETS
     @ 04, 64 GET m.fecha PICTURE  ;
       '@E'
     READ
     IF LASTKEY() = 27
          ON KEY LABEL tab
          RELEASE WINDOW a1
          CLOSE DATABASES
          RETURN
     ENDIF
     p = printer()
     IF p = 2
          ON KEY LABEL tab
          RELEASE WINDOW a1
          CLOSE DATABASES
          RETURN
     ENDIF
     m.seccion = m.codigo
     ano1 = SUBSTR(lapmat, 3, 4)
     ano2 = STR(VAL(ano1) + 1)
     ano2 = RIGHT(ano2, 4)
     ch1 = ano1
     ch2 = ano2
     x = VAL(SUBSTR(DTOC(m.fecha),  ;
         8, 1))
     x = LTRIM(STR(x, 1))
     m.year=LOWER(unidad&x)+'.'
     xd = 'const1'
     DO CASE
          CASE m.curso = '32012'
               IF VAL(m.codigo) =  ;
                  07
                    m.semetre = 'SEPTIMO SEMESTRE'
                    m.equivale = '7mo. GRADO'
               ENDIF
               IF VAL(m.codigo) =  ;
                  08
                    m.semetre = 'OCTAVO SEMESTRE'
                    m.equivale = '7mo. GRADO'
               ENDIF
               IF VAL(m.codigo) =  ;
                  09
                    m.semetre = 'NOVENO SEMESTRE'
                    m.equivale = '8vo. GRADO'
               ENDIF
               IF VAL(m.codigo) =  ;
                  10
                    m.semetre = 'DECIMO SEMESTRE'
                    m.equivale = '8vo. GRADO'
               ENDIF
               IF VAL(m.codigo) =  ;
                  11
                    m.semetre = 'DECIMO PRIMER SEMESTRE'
                    m.equivale = '9no. GRADO'
               ENDIF
               IF VAL(m.codigo) =  ;
                  12
                    m.semetre = 'DECIMO SEGUNDO SEMESTRE'
                    m.equivale = '9no. GRADO'
               ENDIF
          CASE m.curso = '31022'  ;
               .OR. m.curso =  ;
               '31023'
               IF VAL(m.codigo) =  ;
                  01
                    m.semetre = 'PRIMER SEMESTRE'
                    m.equivale = '1er. A¥O'
               ENDIF
               IF VAL(m.codigo) =  ;
                  02
                    m.semetre = 'SEGUNDO SEMESTRE'
                    m.equivale = '1er. A¥O'
               ENDIF
               IF VAL(m.codigo) =  ;
                  03
                    m.semetre = 'TERCER SEMESTRE'
                    m.equivale = '2do. A¥O'
               ENDIF
               IF VAL(m.codigo) =  ;
                  04
                    m.semetre = 'CUARTO SEMESTRE'
                    m.equivale = '2do. A¥O'
               ENDIF
     ENDCASE
     IF m.turno = 'MAT'
          m.turnito = 'MATUTINO'
     ENDIF
     IF m.turno = 'VES'
          m.turnito = 'VESPERTINO'
     ENDIF
     IF m.turno = 'NOC'
          m.turnito = 'NOCTURNO'
     ENDIF
     IF m.turno = 'SAB'
          m.turnito = 'SABATINO'
     ENDIF
     SELECT 1
     SET DEVICE TO PRINTER
     y1 = ' '
     y2 = ALLTRIM(l_nombre)
     y3 = ALLTRIM(l_localida)
     y4 = 'INSCRITO EN EL M.E. S-4529N0602'
     @ 00, 00 SAY CHR(18) +  ;
       CHR(20) + ' ' + ng
     @ 01, 00 SAY SPACE(((80 -  ;
       LEN(y1) - 6) / 2) + 3) +  ;
       y1
     @ 02, 00 SAY SPACE(((80 -  ;
       LEN(y2) - 6) / 2) + 3) +  ;
       y2
     @ 03, 00 SAY SPACE(((80 -  ;
       LEN(y3) - 6) / 2) + 3) +  ;
       y3
     @ 04, 00 SAY SPACE(((80 -  ;
       LEN(y4) - 6) / 2) + 3) +  ;
       y4
     centro1 = 'C O N S T A N C I A    D E    I N S C R I P C I O N'
     @ 11, 00 SAY SPACE(((80 -  ;
       LEN(centro1) - 6) / 2) +  ;
       3) + centro1
     @ 18, 05 SAY  ;
       '  Qui‚n suscribe, Directora de la UNIDAD EDUCATIVA COLEGIO "SIGLO XXI",'
     @ 20, 05 SAY  ;
       'certifica   por    medio    de    la   presente   que   el   Alumno (a)'
     @ 22, 05 SAY  ;
       '______________________________________________ , titular de  la  C‚dula'
     @ 22, 05 SAY xnombre
     @ 24, 05 SAY  ;
       'de Identidad Nro.____________,se ha inscrito en esta  instituci¢n  para'
     @ 24, 23 SAY xcedula PICTURE  ;
       '!-9999999999'
     DO CASE
          CASE m.curso = '32012'
               @ 26, 05 SAY  ;
                 'cursar el ________________________ (_____________ de Educaci¢n B sica),'
               @ 26, 16 SAY  ;
                 m.semetre
               @ 26, 42 SAY  ;
                 m.equivale
               @ 28, 05 SAY  ;
                 'del  plan  de  estudio de la Modalidad Educaci¢n de Adultos, durante el'
               @ 30, 05 SAY  ;
                 'Turno ____________ en  el A¤o Escolar : ' +  ;
                 ch1 + '-' + ch2 +  ;
                 '.'
               @ 30, 12 SAY  ;
                 m.turnito
          CASE m.curso = '31022'  ;
               .OR. m.curso =  ;
               '31023'
               @ 26, 05 SAY  ;
                 'cursar el ________________________ (_____________  de  Educaci¢n  Media'
               @ 26, 16 SAY  ;
                 m.semetre
               @ 26, 42 SAY  ;
                 m.equivale
               @ 28, 05 SAY  ;
                 'Diversificada) del plan de estudio de la Modalidad Educaci¢n de Adultos'
               @ 30, 05 SAY  ;
                 'durante el Turno ____________ en el A¤o Escolar :' +  ;
                 ch1 + '-' + ch2 +  ;
                 '.'
               @ 30, 23 SAY  ;
                 m.turnito
     ENDCASE
     @ 32, 05 SAY  ;
       '   Constancia  que  se  expide  a  solicitud  de  parte  interesada en'
     x = VAL(SUBSTR(DTOC(DATE()),  ;
         8, 1))
     x = LTRIM(STR(x, 1))
     m.rtu = ALLTRIM(l_localida) +  ;
             ' , a los ' +  ;
             LOWER(conv(DAY(m.fecha))) +  ;
             ' d¡as del mes de ' +  ;
             LOWER(meses(m.fecha))
     IF YEAR(m.fecha) <= 1999
          m.rtu=m.rtu+' de mil novecientos noventa y '+LOWER(unidad&x)+'.'
     ELSE
          IF YEAR(m.fecha) = 2000
               m.rtu = m.rtu +  ;
                       ' del dos mil. '
          ELSE
               m.rtu=m.rtu+' del dos mil '+LOWER(unidad&x)+'.'
          ENDIF
     ENDIF
     IF LEN(m.rtu) >= 68
          i = 71
          c = 1
          DO WHILE .T.
               IF SUBSTR(m.rtu, i,  ;
                  1) = ' '
                    m.rtu1 = SUBSTR(m.rtu,  ;
                             1,  ;
                             i)
                    rt = LEN(m.rtu1)
                    yu = 1 + rt
                    yu = 72 - yu
                    IF yu > 0
                         m.rtu1 =  ;
                          m.rtu1 +  ;
                          SPACE(yu)
                    ENDIF
                    lou1 = LEN(m.rtu)
                    k = i + lou1
                    m.rtu2 = SUBSTR(m.rtu,  ;
                             i +  ;
                             1,  ;
                             k) +  ;
                             SPACE(2)
                    EXIT
               ENDIF
               i = i - 1
          ENDDO
          m.rtu1 = ALLTRIM(m.rtu1)
          de = AT(',', m.rtu1)
          k = de
          DO WHILE .T.
               IF LEN(m.rtu1) >=  ;
                  70
                    EXIT
               ENDIF
               IF SUBSTR(m.rtu1,  ;
                  de, 1) = ' '
                    m.rtu1 = STUFF(m.rtu1,  ;
                             de,  ;
                             0,  ;
                             ' ' ;
                             )
                    de = de + 2
               ENDIF
               IF LEN(m.rtu1) =  ;
                  de
                    de = AT(',',  ;
                         m.rtu1)
               ENDIF
               de = de + 1
          ENDDO
          @ 34, 05 SAY m.rtu1
          @ 36, 05 SAY m.rtu2
     ELSE
          @ 34, 05 SAY m.rtu
     ENDIF
     xd = '_____________________________'
     @ 45, 00 SAY SPACE(((80 -  ;
       LEN(xd) - 6) / 2) + 3) +  ;
       xd
     xd = l_nom_func
     @ 46, 00 SAY SPACE(((80 -  ;
       LEN(xd) - 6) / 2) + 3) +  ;
       xd
     xd = l_car_func
     @ 47, 00 SAY SPACE(((80 -  ;
       LEN(xd) - 6) / 2) + 3) +  ;
       xd
     @ 48, 01 SAY CHR(18) +  ;
       CHR(20) + ' ' + ngn
     SET DEVICE TO SCREEN
     m.nac = 'V'
     m.ced = SPACE(10)
     valida1 = 'ABCDEFGHIJKLMNOPQRSTUVXYZ'
     m.curso = '32011'
     m.fecha = DATE()
ENDDO
ON KEY LABEL tab
CLOSE DATABASES
RELEASE WINDOW a1
RETURN
*
FUNCTION conv
PARAMETER xmonto
PRIVATE ALL
xnumero = LTRIM(STR(xmonto, 2))
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
b2 = SUBSTR(xnumero, 1, 2)
IF VAL(b2) <= 10
     b2 = SUBSTR(xnumero, 1, 1)
     let1=unidad&b2
ELSE
     b2 = SUBSTR(xnumero, 1, 1)
     b3 = SUBSTR(xnumero, 2, 1)
     IF VAL(b2 + b3) >= 20
          t = b2 + '0'
          let1=trim(decena&t)
          let1=let1+iif(b3<>'0',' Y '+unidad&b3,unidad&b3)
     ELSE
          IF VAL(b2 + b3) < 10
               let1=unidad&b3
          ELSE
               t = b2 + b3
               let1=trim(decena&t)
          ENDIF
     ENDIF
ENDIF
RETURN let1
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
