CLOSE DATABASES
SET DEVICE TO SCREEN
RELEASE WINDOW
RELEASE WINDOW
RELEASE WINDOW
RELEASE WINDOW
RELEASE POPUP ALL
SET COLOR TO
CLEAR
SET TALK OFF
STORE SET('BELL') TO save_bell
STORE SET('STATUS') TO save_stat
STORE SET('SCOREBOARD') TO  ;
      save_score
STORE SET('ESCAPE') TO save_escap
STORE SET('SAFETY') TO save_safe
SET COLOR TO
SET DATE fren
SET SCOREBOARD OFF
SET ECHO OFF
SET BELL OFF
SET STATUS OFF
SET CURSOR ON
SET CENTURY ON
SET SAFETY OFF
SET MESSAGE TO 23 CENTER
SET ESCAPE OFF
SET EXCLUSIVE OFF
SET PROCEDURE TO proce2
SET DELETED ON
SET SYSMENU OFF
SET CLOCK TO 22, 69
SET HELP TO 'orghelp.dbf'
SET HELP ON
SET UDFPARMS TO VALUE
SELECT 1
USE liceo
GOTO TOP
SCATTER MEMO MEMVAR
PUBLIC titulo, empresa, color1,  ;
       tp, xlogin, lapmat
PUBLIC l_nombre, l_nomcort,  ;
       l_omad, l_unico, l_zona,  ;
       l_distrito, l_sector,  ;
       l_actual
PUBLIC l_entidad, l_dependen,  ;
       l_telefono, l_direccio,  ;
       l_director, l_ceddirec
PUBLIC l_contestu, l_cedestud,  ;
       l_funciedu, l_cedfunci,  ;
       l_localida, l_nomext,  ;
       l_nombredd
PUBLIC l_nom_func, l_ced_func,  ;
       l_car_func, ng, ngn,  ;
       l_rif
PUBLIC raya, noraya, ng, ngn,  ;
       scrip, nscrip, cpi12,  ;
       cpi10, italic, nitalic
raya = CHR(27) + CHR(45) + '1'
noraya = CHR(27) + CHR(45) + '0'
ng = CHR(27) + CHR(69)
ngn = CHR(27) + CHR(70)
scrip = CHR(27) + CHR(83) + '1'
nscrip = CHR(27) + CHR(84)
cpi12 = CHR(27) + CHR(77)
cpi10 = CHR(27) + CHR(80)
italic = CHR(27) + CHR(52)
nitalic = CHR(27) + CHR(53)
PUBLIC barm1, barm2, barm3, barm4,  ;
       barm5, barm6, barm7, barm8,  ;
       barm9, barm10, barm11
STORE .F. TO barm1, barm2, barm3,  ;
      barm4, barm5, barm6, barm7,  ;
      barm8, barm9, barm10,  ;
      barm11
PUBLIC entre, entra
STORE .F. TO entre, entra
kuku1 = 'K1ýýýýý1ý:ý¹ýý±ýýý{ýqý{ýqýýý'
kuku2 = 'K2ýýýÿñý{ýqýýý'
kuku3 = 'K3ýýýÿñý{ýqýýý'
kuku4 = 'K4ýýýýýýýýýý'
kuku5 = 'K5ýývýý><ýý'
l_nomext = stringv(SUBSTR(kuku1,  ;
           3, 60))
l_nombre = stringv(SUBSTR(kuku2,  ;
           3, 60))
l_nomcort = stringv(SUBSTR(kuku2,  ;
            3, 60))
l_nombredd = stringv(SUBSTR(kuku2,  ;
             3, 60))
titulo = stringv(SUBSTR(kuku3, 3,  ;
         60))
l_omad = stringv(SUBSTR(kuku4, 3,  ;
         20))
l_rif = stringv(SUBSTR(kuku5, 3,  ;
        20))
l_unico = m.unico
l_zona = m.zona
l_distrito = m.distrito
l_sector = m.sector
l_entidad = m.entidad
l_dependen = m.dependenci
l_telefono = m.telefono
l_direccio = m.direccion
l_director = m.director
l_ceddirec = m.cedula
l_contestu = m.contestudi
l_cedestud = m.ceduestudi
l_funciedu = m.funceducac
l_cedfunci = m.cedueducac
l_localida = m.localidad
l_nom_func = m.nom_func
l_ced_func = m.ced_func
l_car_func = m.car_func
CLOSE DATABASES
ng = CHR(27) + CHR(69)
ngn = CHR(27) + CHR(70)
STORE .F. TO x_sal
STORE l_nomcort TO empresa
STORE 'CONTROL DE PAGOS' TO  ;
      sistema
STORE 0 TO key
scheme5 = SCHEME(8)
IF ISCOLOR()
     SET COLOR OF SCHEME 3 TO BG/W, N/W,;
N/W,, GR+/N, N/BG, R/W
     SET COLOR OF SCHEME 2 TO BG/W, N/W,;
N/W,, GR+/N, N/BG, R/W
     set color of scheme 5 to &scheme5
     color1 = 'N/W,N/BG'
ELSE
     SET COLOR OF SCHEME 3 TO W/N, N+/W,;
W+/N, W+/N, W/N, B+/N, W+/N, -, W+/N,;
W/N
     SET COLOR OF SCHEME 2 TO W/N, N+/W,;
W+/N, W+/N, W/N, B+/N, W+/N, -, W+/N,;
W/N
     SET COLOR OF SCHEME 5 TO W/N, N+/W,;
W+/N, W+/N, W/N, N/W, W+/N, -, W+/N, W/N
     SET COLOR OF SCHEME 7 TO W/N, N+/W,;
W+/N, W+/N, W/N, N/W, W+/N, -, W+/N, W/N
     color1 = 'W/N,N/W'
ENDIF
ON KEY LABEL f1 do phelp with pad()
ON KEY LABEL f2 do calculadora
ON KEY LABEL f7 do calendario
ON ERROR do errorhandler with message(),error(),program(),lineno()
xt = CAPSLOCK()
xy = NUMLOCK()
IF xt = .F.
     = CAPSLOCK( .NOT.  ;
       CAPSLOCK())
ENDIF
IF xy = .F.
     = NUMLOCK( .NOT. NUMLOCK())
ENDIF
= inicio()
IF  .NOT. FILE('ASIGNATU.OVL')
     = msgerro( ;
       'Restringido el acceso al sistema, por fallas' ;
       )
     CLOSE DATABASES
     RELEASE WINDOW
     RETURN
ENDIF
SELECT 1
USE asignatu.ovl
DO WHILE .T.
     IF FLOCK()
          INDEX ON a101 TO  ;
                asignatc
          UNLOCK
          EXIT
     ENDIF
ENDDO
USE asignatu.ovl INDEX asignatc
qi = 0
sswt = 0
DO WHILE .T.
     m.clave = acceso()
     IF m.clave == 'AZX3'
          sswt = 1
          EXIT
     ENDIF
     m.clave = IIF(LEN(m.clave) =  ;
               5, m.clave,  ;
               m.clave + SPACE(5 -  ;
               LEN(m.clave)))
     LOCATE FOR RIGHT(a101, 5) ==  ;
            m.clave
     IF  .NOT. FOUND()
          ?? CHR(7)
          = msgerro( ;
            'Clave de Acceso no Autorizada, Verifique' ;
            )
          qi = qi + 1
          IF qi > 3
               = msgerro( ;
                 'Usuario no Autorizado para el acceso al sistema' ;
                 )
               CLOSE DATABASES
               RELEASE WINDOW
               RETURN
          ENDIF
          LOOP
     ENDIF
     EXIT
ENDDO
IF sswt = 1
     barm1 = .F.
     barm2 = .F.
     barm3 = .F.
     barm4 = .F.
     barm5 = .F.
     barm6 = .F.
     barm7 = .F.
     barm8 = .F.
     barm9 = .F.
     barm10 = .F.
     barm11 = .F.
ELSE
     j = 1
     FOR i = 14 TO 24
          lk = LTRIM(STR(i))
          lp = LTRIM(STR(j))
          barm&lp=a&lk
          j = j + 1
     ENDFOR
ENDIF
CLOSE DATABASES
SELECT 1
USE lapso1
GOTO TOP
SCATTER MEMO MEMVAR
SET CENTURY ON
DEFINE WINDOW record1 FROM 12, 10  ;
       TO 21, 70 FLOAT NOCLOSE  ;
       SHADOW NOMINIMIZE COLOR  ;
       SCHEME 5
ACTIVATE WINDOW record1
x1 = 'Lapso Historico Actual'
@ 00, 00 SAY SPACE(((WCOLS() -  ;
  LEN(x1) - 6) / 2) + 3) + x1
@ 01, 00 TO 01, WCOLS() - 2
xfechaf = SUBSTR(DTOC(m.fechaf),  ;
          4, 7)
xfechar = SUBSTR(DTOC(m.fechar),  ;
          4, 7)
xfechaexta = SUBSTR(DTOC(m.fechaexta),  ;
             4, 7)
m.cod1 = SUBSTR(m.lapso1, 1, 2)
m.cod2 = SUBSTR(m.lapso1, 3, 4)
lapmat = m.cod1 + m.cod2
l_actual = m.cod1 + '-' + m.cod2 +  ;
           '/' + RIGHT(lapso, 4)
DO WHILE .T.
     @ 02, 01 SAY  ;
       'Lapso Actual         : '  ;
       GET m.cod1 PICTURE  ;
       '@M 01,02'
     @ 02, COL() SAY '-' GET  ;
       m.cod2 PICTURE '9999'
     @ 03, 01 SAY  ;
       'Fecha Final          : '  ;
       GET xfechaf PICTURE  ;
       '99/9999'
     @ 04, 01 SAY  ;
       'Fecha Revisiýn       : '  ;
       GET xfechar PICTURE  ;
       '99/9999'
     @ 05, 01 SAY  ;
       'Fecha Extraordinario : '  ;
       GET xfechaexta PICTURE  ;
       '99/9999'
     @ 06, 01 TO 06, WCOLS()  ;
       DOUBLE
     CLEAR GETS
     @ 07, 24 GET m.choice  ;
       DEFAULT 1 SIZE 1, 10, 10  ;
       PICTURE '@*HT \<Ok' COLOR  ;
       SCHEME 5
     READ
     EXIT
ENDDO
SET CENTURY ON
RELEASE WINDOW
CLOSE DATABASES
SAVE SCREEN TO pant1
DO WHILE .T.
     RESTORE SCREEN FROM pant1
     IF ISCOLOR()
          SET COLOR OF SCHEME 3 TO BG/W,;
N/W, N/W,, GR+/N, N/BG, R/W
          SET COLOR OF SCHEME 2 TO BG/W,;
N/W, N/W,, GR+/N, N/BG, R/W
          set color of scheme 5 to &scheme5
     ELSE
          SET COLOR OF SCHEME 3 TO W/N,;
W+/N, W+/N, W+/N, W/N, B+/N, W+/N, -,;
W+/N, W/N
          SET COLOR OF SCHEME 2 TO W/N,;
W+/N, W+/N, W+/N, W/N, B+/N, W+/N, -,;
W+/N, W/N
          SET COLOR OF SCHEME 5 TO W/N,;
W+/N, W+/N, W+/N, W/N, N/W, W+/N, -, W+/N,;
W/N
          SET COLOR OF SCHEME 7 TO W/N,;
W+/N, W+/N, W+/N, W/N, N/W, W+/N, -, W+/N,;
W/N
          color1 = 'W/N,N/W'
     ENDIF
     m.cod1 = SUBSTR(lapmat, 1,  ;
              2)
     m.cod2 = SUBSTR(lapmat, 3,  ;
              4)
     @ 22, 01 SAY  ;
       'Lapso Actual : ' +  ;
       l_actual COLOR W+/B,W+/B 
     DEFINE MENU principal COLOR  ;
            SCHEME 3
     DEFINE PAD tablas OF  ;
            principal PROMPT  ;
            '\<Tablas Basicas' AT  ;
            1, 00 MESSAGE  ;
            'Tablas Býsicas del Sistema'
     DEFINE PAD alumnos OF  ;
            principal PROMPT  ;
            '\<Alýmnos/Pagos' AT  ;
            1, 23 MESSAGE  ;
            'Tratamiento Administrativo de Alumnos'
     DEFINE PAD consulta OF  ;
            principal PROMPT  ;
            '\<Reportes' AT 1, 49  ;
            MESSAGE  ;
            'Menu de Reportes del Sistema'
     DEFINE PAD salir OF  ;
            principal PROMPT  ;
            '\<Salida' AT 1, 68  ;
            MESSAGE  ;
            'Desactivaciýn del Sistema'
     ON SELECTION PAD tablas OF principal;
do p_tablas
     ON SELECTION PAD alumnos OF principal;
do p_alumnos
     ON SELECTION PAD consulta OF principal;
do p_consulta
     ON SELECTION PAD salir OF principal;
activate popup vert6
     DEFINE POPUP vert6 FROM 02,  ;
            66 SHADOW RELATIVE  ;
            MULTI COLOR SCHEME 2
     DEFINE BAR 1 OF vert6 PROMPT  ;
            ' \<Salida  ' MESSAGE  ;
            'Salida del Sistema'  ;
            MARK CHR(251) COLOR  ;
            SCHEME 2
     ON SELECTION BAR 1 OF vert6 do exit
     ACTIVATE MENU principal
     IF LASTKEY() = 27
          LOOP
     ENDIF
     IF x_sal = .F.
          EXIT
     ENDIF
ENDDO
ON ERROR
ON KEY
CLOSE DATABASES
CLEAR
RETURN
*
PROCEDURE exit
DO housekeep
ON KEY
ON ERROR
SET COLOR TO
CLEAR
RELEASE WINDOW
x_sal = .T.
RETURN
*
PROCEDURE housekeep
DEACTIVATE MENU principal
RELEASE WINDOW all
ON KEY
ON ERROR
CLEAR
RETURN
*
PROCEDURE p_tablas
DEFINE POPUP tablita FROM 2, 00  ;
       SHADOW RELATIVE MULTI  ;
       COLOR SCHEME 3
DEFINE BAR 1 OF tablita PROMPT  ;
       ' Conceptos de \<Cobranza                '  ;
       MESSAGE  ;
       'Actualizaciýn de Conceptos de Cobranza y Control de Pagos'  ;
       MARK CHR(251) COLOR SCHEME  ;
       2
DEFINE BAR 2 OF tablita PROMPT  ;
       ' \<Planes de Cobro                      '  ;
       MESSAGE  ;
       'Actualizaciýn Planes de Pagos'  ;
       MARK CHR(251) COLOR SCHEME  ;
       2
DEFINE BAR 3 OF tablita PROMPT  ;
       ' Tabla de \<Bancos                      '  ;
       MESSAGE  ;
       'Actualizaciýn de Tablas de Bancos Comerciales'  ;
       MARK CHR(251) COLOR SCHEME  ;
       2
DEFINE BAR 4 OF tablita PROMPT  ;
       ' Tabla de Cobra\<dores                  '  ;
       MESSAGE  ;
       'Actualizaciýn de Cobradores'  ;
       MARK CHR(251) COLOR SCHEME  ;
       2
DEFINE BAR 5 OF tablita PROMPT  ;
       ' \<Lapso Historico                      '  ;
       MESSAGE  ;
       'Modifica el Lapso Historico'  ;
       MARK CHR(251) COLOR SCHEME  ;
       2
DEFINE BAR 7 OF tablita PROMPT  ;
       '\-'
DEFINE BAR 8 OF tablita PROMPT  ;
       ' \<Reconstrucciýn de Indices            '  ;
       MESSAGE  ;
       'Reconstruciýn de los Archivos Indices'  ;
       MARK CHR(251) COLOR SCHEME  ;
       2
DEFINE BAR 9 OF tablita PROMPT  ;
       ' Confi\<gura el Entorno del Sistema     '  ;
       MESSAGE  ;
       'Configura el Entorno del Sistema'  ;
       MARK CHR(251) COLOR SCHEME  ;
       2
DEFINE BAR 10 OF tablita PROMPT  ;
       ' Respaldar el \<Sistema                 '  ;
       MESSAGE  ;
       'Respalda la Informaciýn del Sistema en un Disco'  ;
       MARK CHR(251) COLOR SCHEME  ;
       2
DEFINE BAR 11 OF tablita PROMPT  ;
       ' Res\<taurar el Sistema                 '  ;
       MESSAGE  ;
       'Restaura la Informaciýn del Sistema de un Disco al PC'  ;
       MARK CHR(251) COLOR SCHEME  ;
       2
DEFINE BAR 12 OF tablita PROMPT  ;
       ' Busca Archi\<vos Daýados               '  ;
       MESSAGE  ;
       'Busca Archivos o Bases de Datos Daýadas'  ;
       MARK CHR(251) COLOR SCHEME  ;
       2
DEFINE BAR 13 OF tablita PROMPT  ;
       '\-'
DEFINE BAR 14 OF tablita PROMPT  ;
       ' Actuali\<zaciýn de Usuarios del Sistema'  ;
       MESSAGE  ;
       'Actualizaciýn de los Usuarios del Sistema (Claves de Acceso)'  ;
       MARK CHR(251) COLOR SCHEME  ;
       2
ON SELECTION POPUP tablita do m_tablas;
with bar()
ACTIVATE POPUP tablita
RELEASE POPUP tablita
RETURN
*
PROCEDURE m_tablas
PARAMETER xbar
DO CASE
     CASE xbar = 1
          SAVE SCREEN TO m_salva
          DO scp10001
          RESTORE SCREEN FROM  ;
                  m_salva
     CASE xbar = 2
          DEFINE POPUP plancob  ;
                 FROM 5, 05  ;
                 SHADOW RELATIVE  ;
                 MULTI COLOR  ;
                 SCHEME 3
          DEFINE BAR 1 OF plancob  ;
                 PROMPT  ;
                 ' Registro de \<Plan de Cobro '  ;
                 MESSAGE  ;
                 'Registra sus Diferentes Planes de Cobro'  ;
                 SKIP FOR barm1  ;
                 MARK CHR(251)  ;
                 COLOR SCHEME 2
          DEFINE BAR 2 OF plancob  ;
                 PROMPT  ;
                 ' \<Listado de Plan de Cobro  '  ;
                 MESSAGE  ;
                 'Emite un Listado Impreso de sus Planes de Cobro'  ;
                 MARK CHR(251)  ;
                 COLOR SCHEME 2
          ON SELECTION POPUP plancob do;
m_plancob with bar()
          ACTIVATE POPUP plancob
          RELEASE POPUP plancob
          RETURN
     CASE xbar = 3
          SAVE SCREEN TO m_salva
          DO scp10004
          RESTORE SCREEN FROM  ;
                  m_salva
     CASE xbar = 4
          SAVE SCREEN TO m_salva
          DO scp10005
          RESTORE SCREEN FROM  ;
                  m_salva
     CASE xbar = 5
          DO scp10006
     CASE xbar = 6
          SAVE SCREEN TO salva
          DO scl80001
          RESTORE SCREEN FROM  ;
                  salva
     CASE xbar = 8
          SAVE SCREEN TO salva
          DO scp10008
          RESTORE SCREEN FROM  ;
                  salva
     CASE xbar = 9
          SAVE SCREEN TO salva
          HIDE POPUP ALL
          HIDE MENU principal
          DO config
          SET MESSAGE TO 23 CENTER
          SHOW MENU principal
          SET COLOR OF SCHEME 3 TO BG/W,;
N/W, N/W,, GR+/N, N/BG, R/W
          SET COLOR OF SCHEME 2 TO BG/W,;
N/W, N/W,, GR+/N, N/BG, R/W
          set color of scheme 5 to &scheme5
          color1 = 'N/W,N/BG'
          set color to &color1
          RESTORE SCREEN FROM  ;
                  salva
     CASE xbar = 10
          SAVE SCREEN TO jol1
          DO bbackup
          RESTORE SCREEN FROM  ;
                  jol1
     CASE xbar = 11
          SAVE SCREEN TO jol1
          DO brestore
          RESTORE SCREEN FROM  ;
                  jol1
     CASE xbar = 12
          SAVE SCREEN TO jol1
          DO scl90002
          RESTORE SCREEN FROM  ;
                  jol1
     CASE xbar = 14
          SAVE SCREEN TO kol
          SELECT 1
          USE asignatu.ovl INDEX  ;
              asignatc
          sswt = 1
          DO WHILE .T.
               m.clave = acceso()
               IF m.clave =  ;
                  'FALSO'
                    CLOSE DATABASES
                    RESTORE SCREEN  ;
                            FROM  ;
                            kol
                    RETURN
               ENDIF
               IF m.clave ==  ;
                  'AZX3'
                    sswt = 1
                    EXIT
               ENDIF
               m.clave = IIF(LEN(m.clave) =  ;
                         5,  ;
                         m.clave,  ;
                         m.clave +  ;
                         SPACE(5 -  ;
                         LEN(m.clave)))
               LOCATE FOR  ;
                      RIGHT(a101,  ;
                      5) =  ;
                      m.clave
               IF  .NOT. FOUND()
                    ?? CHR(7)
                    = msgerro( ;
                      'Usuarios no Autorizada, Verifique' ;
                      )
                    qi = qi + 1
                    IF qi > 3
                         = msgerro( ;
                           'Usuario no Autorizado para el acceso al sistema' ;
                           )
                         CLOSE DATABASES
                         RELEASE WINDOW
                         RETURN
                    ENDIF
                    LOOP
               ENDIF
               IF LEFT(a101, 3) <>  ;
                  'SUP'
                    = msgerro( ;
                      'Este Usuario no tiene acceso permitido, verifique' ;
                      )
                    qi = qi + 1
                    IF qi > 3
                         = msgerro( ;
                           'Usuario no Autorizado para el acceso al sistema' ;
                           )
                         CLOSE DATABASES
                         RELEASE WINDOW
                         RETURN
                    ENDIF
                    LOOP
               ENDIF
               EXIT
          ENDDO
          CLOSE DATABASES
          DO scl10010
          RESTORE SCREEN FROM kol
ENDCASE
RETURN
*
PROCEDURE m_plancob
PARAMETER xbar
DO CASE
     CASE xbar = 1
          SAVE SCREEN TO m_salva
          DO scp10002
          RESTORE SCREEN FROM  ;
                  m_salva
     CASE xbar = 2
          SAVE SCREEN TO m_salva
          DO scp1002a
          RESTORE SCREEN FROM  ;
                  m_salva
ENDCASE
RETURN
*
PROCEDURE p_alumnos
DEFINE POPUP alumnos FROM 1, 19  ;
       SHADOW RELATIVE MULTI  ;
       COLOR SCHEME 3
DEFINE BAR 1 OF alumnos PROMPT  ;
       ' Registro de Control \<Deuda Anterior   '  ;
       MESSAGE  ;
       'Registra por Alumno la Deuda Acumulada hasta el Semestre Anterior'  ;
       MARK CHR(251) COLOR SCHEME  ;
       2
DEFINE BAR 2 OF alumnos PROMPT  ;
       '\-'
DEFINE BAR 3 OF alumnos PROMPT  ;
       ' Registro de \<Inscripciýn              '  ;
       MESSAGE  ;
       'Registra Datos de Inscripciýn por Alumno'  ;
       SKIP FOR barm3 MARK  ;
       CHR(251) COLOR SCHEME 2
DEFINE BAR 4 OF alumnos PROMPT  ;
       '\-'
DEFINE BAR 5 OF alumnos PROMPT  ;
       ' Proceso de Control de \<Pagos          '  ;
       MESSAGE  ;
       'Procesa los Pagos Realizados por el Alumno'  ;
       SKIP FOR barm4 MARK  ;
       CHR(251) COLOR SCHEME 2
*DEFINE BAR 6 OF alumnos PROMPT  ;
*       ' Proceso Control Pa\<gos Mat. Adicional '  ;
*       MESSAGE  ;
*       'Procesa los Pagos por Materias Adicionales del Alumno'  ;
*       SKIP FOR barm4 MARK  ;
*       CHR(251) COLOR SCHEME 2
DEFINE BAR 7 OF alumnos PROMPT  ;
       ' Proceso de \<Otros Ingresos            '  ;
       MESSAGE  ;
       'Procesa el Cobro de Otros Ingresos de la Instituciýn'  ;
       SKIP FOR barm5 MARK  ;
       CHR(251) COLOR SCHEME 2
DEFINE BAR 8 OF alumnos PROMPT  ;
       ' \<Anulaciýn de Recibos de Cobro        '  ;
       MESSAGE  ;
       'Anula el Ultimo Recibo de Cobro por Alumno'  ;
       SKIP FOR barm6 MARK  ;
       CHR(251) COLOR SCHEME 2
DEFINE BAR 10 OF alumnos PROMPT  ;
       ' Resumen de \<Transacciones/Alumno      '  ;
       MESSAGE  ;
       'Resumen de Transacciones de Pago del Alumno'  ;
       MARK CHR(251) COLOR SCHEME  ;
       2
DEFINE BAR 11 OF alumnos PROMPT  ;
       '\-'
DEFINE BAR 40 OF alumnos PROMPT  ;
       ' Control de pagos en linea    '  ;
       MESSAGE  ;
       'Gestionar pagos de la plataforma web'  ;
       MARK CHR(251) COLOR SCHEME 2
DEFINE BAR 41 OF alumnos PROMPT  ;
       '\-'
DEFINE BAR 12 OF alumnos PROMPT  ;
       ' Act\<ualizaciýn Registros de Pago      '  ;
       MESSAGE  ;
       'Actualiza los Registros de Pago por Alumno antes de Instalado el Sistema'  ;
       SKIP FOR barm10 MARK  ;
       CHR(251) COLOR SCHEME 2
DEFINE BAR 13 OF alumnos PROMPT  ;
       '\-'
DEFINE BAR 14 OF alumnos PROMPT  ;
       ' Cierre Diario de \<Caja                '  ;
       MESSAGE  ;
       'Emisiýn del Reporte de Cierre Diario de Caja'  ;
       SKIP FOR barm8 MARK  ;
       CHR(251) COLOR SCHEME 2
DEFINE BAR 15 OF alumnos PROMPT  ;
       '\-'
DEFINE BAR 16 OF alumnos PROMPT  ;
       ' \<Re-Impresiýn de Recibos de Cobros    '  ;
       MESSAGE  ;
       'Reimprime Cualquier Recibo de Cobro Emitido por el Sistema'  ;
       MARK CHR(251) COLOR SCHEME  ;
       2
DEFINE BAR 17 OF alumnos PROMPT  ;
       '\-'
DEFINE BAR 18 OF alumnos PROMPT  ;
       ' \<Eliminar Alumnos Matrýcula de Pagos '  ;
       MESSAGE  ;
       'Eliminar Datos del Alumno de la Matrýcula de Pago'  ;
       SKIP FOR barm9 MARK  ;
       CHR(251) COLOR SCHEME 2
DEFINE BAR 19 OF alumnos PROMPT  ;
       ' Actuali\<zaciýn de Cýdula de Alumnos   '  ;
       MESSAGE  ;
       'Actualizaciýn de Cýdula del Alumno'  ;
       MARK CHR(251) COLOR SCHEME  ;
       2
DEFINE BAR 21 OF alumnos PROMPT  ;
       ' Cierre de \<Matrýcula de Pagos         '  ;
       MESSAGE  ;
       'Cierre de Matrýcula de Pagos al Final del Semestre'  ;
       MARK CHR(251) COLOR SCHEME  ;
       2
ON SELECTION POPUP alumnos do m_alumnos;
with bar()
ACTIVATE POPUP alumnos
RELEASE POPUP alumnos
RETURN
*
PROCEDURE m_alumnos
PARAMETER xbar
DO CASE
     CASE xbar = 1
          DEFINE POPUP alumins  ;
                 FROM 3, 25  ;
                 SHADOW RELATIVE  ;
                 MULTI COLOR  ;
                 SCHEME 3
          DEFINE BAR 1 OF alumins  ;
                 PROMPT  ;
                 ' Registro de \<Deudas                    '  ;
                 MESSAGE  ;
                 'Registro Control de Deuda Vencida por Alumno al Semestre Anterior'  ;
                 SKIP FOR barm2  ;
                 MARK CHR(251)  ;
                 COLOR SCHEME 2
          DEFINE BAR 2 OF alumins  ;
                 PROMPT  ;
                 ' Consulta Re\<gistro Control de Deudas   '  ;
                 MESSAGE  ;
                 'Consula el Control de Deuda Vencida por Alumno al Semestre Anterior'  ;
                 MARK CHR(251)  ;
                 COLOR SCHEME 2
          DEFINE BAR 3 OF alumins  ;
                 PROMPT  ;
                 ' Re\<porte de Control de Deudas          '  ;
                 MESSAGE  ;
                 'Reporte Impreso del Control de Deuda Vencida por Alumno al Semestre Anterior'  ;
                 MARK CHR(251)  ;
                 COLOR SCHEME 2
          DEFINE BAR 4 OF alumins  ;
                 PROMPT '\-' MARK  ;
                 CHR(251) COLOR  ;
                 SCHEME 2
          DEFINE BAR 5 OF alumins  ;
                 PROMPT  ;
                 ' Consulta del Control de Deuda Detallada '  ;
                 MARK CHR(251)  ;
                 COLOR SCHEME 2
          ON SELECTION POPUP alumins do;
m_alumin2 with bar()
          ACTIVATE POPUP alumins
          RELEASE POPUP alumins
          RETURN
     CASE xbar = 3
          DEFINE POPUP alumins  ;
                 FROM 5, 25  ;
                 SHADOW RELATIVE  ;
                 MULTI COLOR  ;
                 SCHEME 3
          DEFINE BAR 1 OF alumins  ;
                 PROMPT  ;
                 ' Registro de \<Inscripciýn            '  ;
                 MESSAGE  ;
                 'Proceso de Registros de Datos de Inscripciýn del Alumno'  ;
                 MARK CHR(251)  ;
                 COLOR SCHEME 2
          DEFINE BAR 2 OF alumins  ;
                 PROMPT  ;
                 ' \<Egresos de Matricula de Pagos      '  ;
                 MESSAGE  ;
                 'Egresa Alumnos de la Matrýcula de Pagos'  ;
                 MARK CHR(251)  ;
                 COLOR SCHEME 2
          DEFINE BAR 3 OF alumins  ;
                 PROMPT '\-'
          DEFINE BAR 4 OF alumins  ;
                 PROMPT  ;
                 ' Registro Inscripciýn \<Mat.Adicional '  ;
                 MESSAGE  ;
                 'Proceso de Inscripciýn de Materia Adicional'  ;
                 MARK CHR(251)  ;
                 COLOR SCHEME 2
          DEFINE BAR 5 OF alumins  ;
                 PROMPT  ;
                 ' E\<gresos de Matricula Mat.Adicional '  ;
                 MESSAGE  ;
                 'Egresa Alumnos de Matricula de Pagos de Materias Adicionales'  ;
                 MARK CHR(251)  ;
                 COLOR SCHEME 2
          DEFINE BAR 6 OF alumins  ;
                 PROMPT  ;
                 ' \<Retiro de Materia Adicional        '  ;
                 MESSAGE  ;
                 'Retiro de Materia Adicional'  ;
                 MARK CHR(251)  ;
                 COLOR SCHEME 2
          DEFINE BAR 7 OF alumins  ;
                 PROMPT  ;
                 ' \<Cambiar Nro.de Materias Adicionales'  ;
                 MESSAGE  ;
                 'Cambia la Cantidad de Materias Adicionales'  ;
                 MARK CHR(251)  ;
                 COLOR SCHEME 2
          ON SELECTION POPUP alumins do;
m_alumins with bar()
          ACTIVATE POPUP alumins
          RELEASE POPUP alumins
          RETURN
     CASE xbar = 5
          SAVE SCREEN TO m_salva
          DO scp20033
          RESTORE SCREEN FROM  ;
                  m_salva
     CASE xbar = 6
          SAVE SCREEN TO m_salva
          DO scp20333
          RESTORE SCREEN FROM  ;
                  m_salva
     CASE xbar = 7
          DEFINE POPUP adic FROM  ;
                 9, 25 SHADOW  ;
                 RELATIVE MULTI  ;
                 COLOR SCHEME 3
          DEFINE BAR 1 OF adic  ;
                 PROMPT  ;
                 ' \<Registro Solicitudes Administrativas'  ;
                 MESSAGE  ;
                 'Registra las Solicitudes Administrativas hechas por Alumnos'  ;
                 MARK CHR(251)  ;
                 COLOR SCHEME 2
          DEFINE BAR 2 OF adic  ;
                 PROMPT  ;
                 ' R\<egistro de Otros Ingresos          '  ;
                 MESSAGE  ;
                 'Registra los Ingresos Adicionales que tiene la Instituciýn'  ;
                 MARK CHR(251)  ;
                 COLOR SCHEME 2
          ON SELECTION POPUP adic do m_adic;
with bar()
          ACTIVATE POPUP adic
          RELEASE POPUP adic
          RETURN
     CASE xbar = 8
          DEFINE POPUP anula FROM  ;
                 9, 25 SHADOW  ;
                 RELATIVE MULTI  ;
                 COLOR SCHEME 3
          DEFINE BAR 1 OF anula  ;
                 PROMPT  ;
                 ' Anula \<Registro de Cobro Mensualidad      '  ;
                 MESSAGE  ;
                 'Anula Registros por Pago Mensualidades'  ;
                 MARK CHR(251)  ;
                 COLOR SCHEME 2
          DEFINE BAR 2 OF anula  ;
                 PROMPT  ;
                 ' Anula R\<egistro de Otros Ingresos         '  ;
                 MESSAGE  ;
                 'Anula Registros Ingresos Adicionales que tiene la Instituciýn'  ;
                 MARK CHR(251)  ;
                 COLOR SCHEME 2
          DEFINE BAR 3 OF anula  ;
                 PROMPT  ;
                 ' \<Anula Registro Solicitudes Administrativas'  ;
                 MESSAGE  ;
                 'Anula Registros por Solicitudes Administrativas hechas por Alumnos'  ;
                 MARK CHR(251)  ;
                 COLOR SCHEME 2
          ON SELECTION POPUP anula do;
m_anula with bar()
          ACTIVATE POPUP anula
          RELEASE POPUP anula
          RETURN
     CASE xbar = 10
          SAVE SCREEN TO m_salva
          DO scp20008
          RESTORE SCREEN FROM  ;
                  m_salva
     CASE xbar = 40
          SAVE SCREEN TO m_salva
          DO sfr1010
          RESTORE SCREEN FROM  ;
                  m_salva
     CASE xbar = 12
          DEFINE POPUP actuali  ;
                 FROM 13, 25  ;
                 SHADOW RELATIVE  ;
                 MULTI COLOR  ;
                 SCHEME 3
          DEFINE BAR 1 OF actuali  ;
                 PROMPT  ;
                 ' Actualizaciýn Registro de \<Inscripciýn'  ;
                 MESSAGE  ;
                 'Proceso Registro de Datos de Inscripcion con Fecha Atrasada'  ;
                 MARK CHR(251)  ;
                 COLOR SCHEME 2
          DEFINE BAR 2 OF actuali  ;
                 PROMPT  ;
                 ' Act\<ualizaciýn Control de Pago        '  ;
                 MESSAGE  ;
                 'Proceso Registro de Pagos por Alumno con Fecha Atrasada'  ;
                 MARK CHR(251)  ;
                 COLOR SCHEME 2
          DEFINE BAR 3 OF actuali  ;
                 PROMPT '\-'
          DEFINE BAR 4 OF actuali  ;
                 PROMPT  ;
                 ' Actualizaciýn Reg.I\<nscripciýn Mat.Adic'  ;
                 MESSAGE  ;
                 'Proceso de Inscripcion de Materias Adicionales con Fecha Atrasada'  ;
                 MARK CHR(251)  ;
                 COLOR SCHEME 2
          DEFINE BAR 5 OF actuali  ;
                 PROMPT  ;
                 ' Ac\<tualizaciýn Control de Pago Mat.Adic'  ;
                 MESSAGE  ;
                 'Proceso de Control de Pago de Materias Adicionales con Fecha Atrasada'  ;
                 MARK CHR(251)  ;
                 COLOR SCHEME 2
          ON SELECTION POPUP actuali do;
m_actuali with bar()
          ACTIVATE POPUP actuali
          RELEASE POPUP actuali
          RETURN
     CASE xbar = 14
          DEFINE POPUP plancob  ;
                 FROM 14, 25  ;
                 SHADOW RELATIVE  ;
                 MULTI COLOR  ;
                 SCHEME 3
          DEFINE BAR 1 OF plancob  ;
                 PROMPT  ;
                 ' \<Entrega Parcial de Caja           '  ;
                 MESSAGE  ;
                 'Registra una Entrega Parcial de Caja'  ;
                 MARK CHR(251)  ;
                 COLOR SCHEME 2
          DEFINE BAR 2 OF plancob  ;
                 PROMPT  ;
                 ' \<Relaciýn de Cierre Diario de Caja '  ;
                 MESSAGE  ;
                 'Relaciýn de Cierre Diario de Caja'  ;
                 MARK CHR(251)  ;
                 COLOR SCHEME 2
          ON SELECTION POPUP plancob do;
m_cierre with bar()
          ACTIVATE POPUP plancob
          RELEASE POPUP plancob
          RETURN
     CASE xbar = 16
          SAVE SCREEN TO kol
          SELECT 1
          USE asignatu.ovl INDEX  ;
              asignatc
          sswt = 1
          DO WHILE .T.
               m.clave = acceso()
               IF m.clave =  ;
                  'FALSO'
                    CLOSE DATABASES
                    RESTORE SCREEN  ;
                            FROM  ;
                            kol
                    RETURN
               ENDIF
               IF m.clave ==  ;
                  'AZX'
                    sswt = 1
                    EXIT
               ENDIF
               m.clave = IIF(LEN(m.clave) =  ;
                         5,  ;
                         m.clave,  ;
                         m.clave +  ;
                         SPACE(5 -  ;
                         LEN(m.clave)))
               LOCATE FOR  ;
                      RIGHT(a101,  ;
                      5) =  ;
                      m.clave
               IF  .NOT. FOUND()
                    ?? CHR(7)
                    = msgerro( ;
                      'Usuarios no Autorizada, Verifique' ;
                      )
                    qi = qi + 1
                    IF qi > 3
                         = msgerro( ;
                           'Usuario no Autorizado para el acceso al sistema' ;
                           )
                         CLOSE DATABASES
                         RELEASE WINDOW
                         RETURN
                    ENDIF
                    LOOP
               ENDIF
               IF LEFT(a101, 3) <>  ;
                  'SUP'
                    = msgerro( ;
                      'Este Usuario no tiene acceso permitido, verifique' ;
                      )
                    qi = qi + 1
                    IF qi > 3
                         = msgerro( ;
                           'Usuario no Autorizado para el acceso al sistema' ;
                           )
                         CLOSE DATABASES
                         RELEASE WINDOW
                         RETURN
                    ENDIF
                    LOOP
               ENDIF
               EXIT
          ENDDO
          SAVE SCREEN TO m_salva
          DO scp20010
          RESTORE SCREEN FROM  ;
                  m_salva
     CASE xbar = 18
          SAVE SCREEN TO m_salva
          DO scp7
          RESTORE SCREEN FROM  ;
                  m_salva
     CASE xbar = 19
          SAVE SCREEN TO m_salva
          DO scp8
          RESTORE SCREEN FROM  ;
                  m_salva
     CASE xbar = 21
          SAVE SCREEN TO kol
          SELECT 1
          USE asignatu.ovl INDEX  ;
              asignatc
          sswt = 1
          DO WHILE .T.
               m.clave = acceso()
               IF m.clave =  ;
                  'FALSO'
                    CLOSE DATABASES
                    RESTORE SCREEN  ;
                            FROM  ;
                            kol
                    RETURN
               ENDIF
               IF m.clave ==  ;
                  'AZX'
                    sswt = 1
                    EXIT
               ENDIF
               m.clave = IIF(LEN(m.clave) =  ;
                         5,  ;
                         m.clave,  ;
                         m.clave +  ;
                         SPACE(5 -  ;
                         LEN(m.clave)))
               LOCATE FOR  ;
                      RIGHT(a101,  ;
                      5) =  ;
                      m.clave
               IF  .NOT. FOUND()
                    ?? CHR(7)
                    = msgerro( ;
                      'Usuarios no Autorizada, Verifique' ;
                      )
                    qi = qi + 1
                    IF qi > 3
                         = msgerro( ;
                           'Usuario no Autorizado para el acceso al sistema' ;
                           )
                         CLOSE DATABASES
                         RELEASE WINDOW
                         RETURN
                    ENDIF
                    LOOP
               ENDIF
               IF LEFT(a101, 3) <>  ;
                  'SUP'
                    = msgerro( ;
                      'Este Usuario no tiene acceso permitido, verifique' ;
                      )
                    qi = qi + 1
                    IF qi > 3
                         = msgerro( ;
                           'Usuario no Autorizado para el acceso al sistema' ;
                           )
                         CLOSE DATABASES
                         RELEASE WINDOW
                         RETURN
                    ENDIF
                    LOOP
               ENDIF
               EXIT
          ENDDO
          SAVE SCREEN TO m_salva
          DO scpm2004
          RESTORE SCREEN FROM  ;
                  m_salva
ENDCASE
RETURN
*
PROCEDURE m_adic
PARAMETER xbar
DO CASE
     CASE xbar = 1
          SAVE SCREEN TO m_salva
          DO solicita
          RESTORE SCREEN FROM  ;
                  m_salva
     CASE xbar = 2
          SAVE SCREEN TO m_salva
          DO scp20044
          RESTORE SCREEN FROM  ;
                  m_salva
ENDCASE
RETURN
*
PROCEDURE m_anula
PARAMETER xbar
DO CASE
     CASE xbar = 1
          SAVE SCREEN TO m_salva
          DO scp20005
          RESTORE SCREEN FROM  ;
                  m_salva
     CASE xbar = 2
          SAVE SCREEN TO m_salva
          DO anular
          RESTORE SCREEN FROM  ;
                  m_salva
     CASE xbar = 3
          SAVE SCREEN TO m_salva
          DO anular1
          RESTORE SCREEN FROM  ;
                  m_salva
ENDCASE
RETURN
*
PROCEDURE m_actuali
PARAMETER xbar
DO CASE
     CASE xbar = 1
          SAVE SCREEN TO m_salva
          DO scp2022b
          RESTORE SCREEN FROM  ;
                  m_salva
     CASE xbar = 2
          SAVE SCREEN TO m_salva
          DO scp2033b
          RESTORE SCREEN FROM  ;
                  m_salva
     CASE xbar = 4
          SAVE SCREEN TO m_salva
          DO scp2222b
          RESTORE SCREEN FROM  ;
                  m_salva
     CASE xbar = 5
          SAVE SCREEN TO m_salva
          DO scp2333b
          RESTORE SCREEN FROM  ;
                  m_salva
ENDCASE
RETURN
*
PROCEDURE m_cierre
PARAMETER xbar
DO CASE
     CASE xbar = 1
          SAVE SCREEN TO m_salva
          DO scp2009b
          RESTORE SCREEN FROM  ;
                  m_salva
     CASE xbar = 2
          SAVE SCREEN TO m_salva
          DO scp20009
          RESTORE SCREEN FROM  ;
                  m_salva
ENDCASE
RETURN
*
PROCEDURE m_alumpre
PARAMETER xbar
DO CASE
     CASE xbar = 1
          SAVE SCREEN TO m_salva
          DO scp20001
          RESTORE SCREEN FROM  ;
                  m_salva
     CASE xbar = 2
          SAVE SCREEN TO m_salva
          DO scp2001a
          RESTORE SCREEN FROM  ;
                  m_salva
     CASE xbar = 3
          SAVE SCREEN TO m_salva
          DO scp2001b
          RESTORE SCREEN FROM  ;
                  m_salva
     CASE xbar = 4
          SAVE SCREEN TO m_salva
          DO scp2001c
          RESTORE SCREEN FROM  ;
                  m_salva
ENDCASE
RETURN
*
PROCEDURE m_alumins
PARAMETER xbar
DO CASE
     CASE xbar = 1
          SAVE SCREEN TO m_salva
          DO scp20022
          RESTORE SCREEN FROM  ;
                  m_salva
     CASE xbar = 2
          SAVE SCREEN TO m_salva
          DO scp2002a
          RESTORE SCREEN FROM  ;
                  m_salva
     CASE xbar = 4
          SAVE SCREEN TO m_salva
          DO scp20222
          RESTORE SCREEN FROM  ;
                  m_salva
     CASE xbar = 5
          SAVE SCREEN TO m_salva
          DO scp2022a
          RESTORE SCREEN FROM  ;
                  m_salva
     CASE xbar = 6
          SAVE SCREEN TO m_salva
          DO scp2222r
          RESTORE SCREEN FROM  ;
                  m_salva
     CASE xbar = 7
          SAVE SCREEN TO m_salva
          DO scp2222c
          RESTORE SCREEN FROM  ;
                  m_salva
ENDCASE
RETURN
*
PROCEDURE m_alumin2
PARAMETER xbar
DO CASE
     CASE xbar = 1
          SAVE SCREEN TO m_salva
          DO scp2002t
          RESTORE SCREEN FROM  ;
                  m_salva
     CASE xbar = 2
          SAVE SCREEN TO m_salva
          DO scp202tb
          RESTORE SCREEN FROM  ;
                  m_salva
     CASE xbar = 3
          SAVE SCREEN TO m_salva
          DO scp202ta
          RESTORE SCREEN FROM  ;
                  m_salva
     CASE xbar = 5
          SAVE SCREEN TO m_salva
          DO scp202vi
          RESTORE SCREEN FROM  ;
                  m_salva
ENDCASE
RETURN
*
PROCEDURE m_alumz
PARAMETER xbar
DO CASE
     CASE xbar = 1
          SAVE SCREEN TO m_salva
          DO scp2010a
          RESTORE SCREEN FROM  ;
                  m_salva
     CASE xbar = 2
          SAVE SCREEN TO m_salva
          DO scp2010b
          RESTORE SCREEN FROM  ;
                  m_salva
ENDCASE
RETURN
*
PROCEDURE p_consulta
DEFINE POPUP p_alumnos FROM 2, 48  ;
       SHADOW RELATIVE MULTI  ;
       COLOR SCHEME 3
DEFINE BAR 1 OF p_alumnos PROMPT  ;
       ' Alumnos Vs. \<Semestre             '  ;
       MESSAGE  ;
       'Emite un Reporte de los Alumnos por Semestre y Turno'  ;
       MARK CHR(251) COLOR SCHEME  ;
       2
DEFINE BAR 2 OF p_alumnos PROMPT  ;
       ' Alumnos Vs  \<Especialidad         '  ;
       MESSAGE  ;
       'Emite un Reporte de los Alumnos por Especialidad y Turno'  ;
       MARK CHR(251) COLOR SCHEME  ;
       2
DEFINE BAR 3 OF p_alumnos PROMPT  ;
       ' Alumnos Ge\<nerales                '  ;
       MESSAGE  ;
       'Emite un Reporte de Todos los Alumnos Inscritos'  ;
       MARK CHR(251) COLOR SCHEME  ;
       2
DEFINE BAR 5 OF p_alumnos PROMPT  ;
       '\-'
DEFINE BAR 6 OF p_alumnos PROMPT  ;
       ' Relaciýn de \<Morosidad            '  ;
       MESSAGE  ;
       'Emite un Reporte de los Alumnos Morosos por Periodos Actualizados'  ;
       MARK CHR(251) COLOR SCHEME  ;
       2
DEFINE BAR 7 OF p_alumnos PROMPT  ;
       ' Relaciýn de \<Cancelaciýn          '  ;
       MESSAGE  ;
       'Emite un Reporte de los Alumnos que Cancelaron por Periodos Establecidos'  ;
       MARK CHR(251) COLOR SCHEME  ;
       2
DEFINE BAR 8 OF p_alumnos PROMPT  ;
       ' Relaciýn de \<Depositos            '  ;
       MESSAGE  ;
       'Emite un Reporte de los Depositos Consignados a las Diferentes Cuentas Bancarias por Periodos'  ;
       MARK CHR(251) COLOR SCHEME  ;
       2
DEFINE BAR 9 OF p_alumnos PROMPT  ;
       ' Relaciýn de Co\<bros por Conceptos '  ;
       MESSAGE  ;
       'Emite un Reporte de los Cobros Diferenciados por Concepto de Otros Ingresos'  ;
       MARK CHR(251) COLOR SCHEME  ;
       2
DEFINE BAR 10 OF p_alumnos PROMPT  ;
       '\-'
DEFINE BAR 12 OF p_alumnos PROMPT  ;
       ' A\<viso de Cobros de Mensualidad   '  ;
       MESSAGE  ;
       'Emite un Aviso General o Individual para Exigir el Pago de las Mensualidades Pendientes'  ;
       MARK CHR(251) COLOR SCHEME  ;
       2
DEFINE BAR 13 OF p_alumnos PROMPT  ;
       '\-'
DEFINE BAR 14 OF p_alumnos PROMPT  ;
       ' Constancia de \<Inscripciýn        '  ;
       MESSAGE  ;
       'Emisiýn de Constancia de Inscripciýn'  ;
       MARK CHR(251) COLOR SCHEME  ;
       2
DEFINE BAR 15 OF p_alumnos PROMPT  ;
       ' Constancia de \<Estudios           '  ;
       MESSAGE  ;
       'Emisiýn de Constancia de Estudio'  ;
       MARK CHR(251) COLOR SCHEME  ;
       2
DEFINE BAR 16 OF p_alumnos PROMPT  ;
       ' Constancia de Buen\<a Conducta     '  ;
       MESSAGE  ;
       'Emisiýn de Constancia de Buena Conducta'  ;
       MARK CHR(251) COLOR SCHEME  ;
       2
DEFINE BAR 17 OF p_alumnos PROMPT  ;
       ' Constancia de Trabajo \<Profesores '  ;
       MESSAGE  ;
       'Emisiýn de Constancia de Buena Conducta'  ;
       MARK CHR(251) COLOR SCHEME  ;
       2
DEFINE BAR 18 OF p_alumnos PROMPT  ;
       '\-'
DEFINE BAR 19 OF p_alumnos PROMPT  ;
       ' Cumplea\<ýos del Mes              '  ;
       MESSAGE  ;
       'Emite un Listado con los Cumpleaýos del Mes Indicado'  ;
       MARK CHR(251) COLOR SCHEME  ;
       2
ON SELECTION POPUP p_alumnos do m_alumn1;
with bar()
ACTIVATE POPUP p_alumnos
RELEASE POPUP p_alumnos
RETURN
*
PROCEDURE m_alumn1
PARAMETER xbar
DO CASE
     CASE xbar = 1
          SAVE SCREEN TO m_salva
          DO scp30011
          RESTORE SCREEN FROM  ;
                  m_salva
     CASE xbar = 2
          SAVE SCREEN TO m_salva
          DO scp30022
          RESTORE SCREEN FROM  ;
                  m_salva
     CASE xbar = 3
          SAVE SCREEN TO m_salva
          DO scp30033
          RESTORE SCREEN FROM  ;
                  m_salva
     CASE xbar = 4
          SAVE SCREEN TO m_salva
          DO scp30044
          RESTORE SCREEN FROM  ;
                  m_salva
     CASE xbar = 6
          DEFINE POPUP alumzle  ;
                 FROM 8, 44  ;
                 SHADOW RELATIVE  ;
                 MULTI COLOR  ;
                 SCHEME 3
          DEFINE BAR 1 OF alumzle  ;
                 PROMPT  ;
                 ' \<Por \<Semestre  '  ;
                 MESSAGE  ;
                 'Emite un Reporte de los Alumnos Morosos por Semestre en Periodos Establecidos'  ;
                 MARK CHR(251)  ;
                 COLOR SCHEME 2
          DEFINE BAR 2 OF alumzle  ;
                 PROMPT  ;
                 ' Por \<Turno '  ;
                 MESSAGE  ;
                 'Emite un Reporte de los Alumnos Morosos por Turno en Periodos Establecidos'  ;
                 MARK CHR(251)  ;
                 COLOR SCHEME 2
          ON SELECTION POPUP alumzle do;
m_alumzle with bar()
          ACTIVATE POPUP alumzle
          RELEASE POPUP alumzle
          RETURN
     CASE xbar = 7
          DEFINE POPUP alumzli  ;
                 FROM 9, 44  ;
                 SHADOW RELATIVE  ;
                 MULTI COLOR  ;
                 SCHEME 3
          DEFINE BAR 1 OF alumzli  ;
                 PROMPT  ;
                 ' \<Por \<Semestre  '  ;
                 MESSAGE  ;
                 'Emite un Reporte de los Alumnos Al Dýa por Semestre en Periodos Establecidos'  ;
                 MARK CHR(251)  ;
                 COLOR SCHEME 2
          DEFINE BAR 2 OF alumzli  ;
                 PROMPT  ;
                 ' Por \<Turno '  ;
                 MESSAGE  ;
                 'Emite un Reporte de los Alumnos Al Dýa por Turno en Periodos Establecidos'  ;
                 MARK CHR(251)  ;
                 COLOR SCHEME 2
          ON SELECTION POPUP alumzli do;
m_alumzli with bar()
          ACTIVATE POPUP alumzli
          RELEASE POPUP alumzli
          RETURN
     CASE xbar = 8
          SAVE SCREEN TO m_salva
          DO scp30006
          RESTORE SCREEN FROM  ;
                  m_salva
     CASE xbar = 9
          SAVE SCREEN TO m_salva
          DO scp30007
          RESTORE SCREEN FROM  ;
                  m_salva
     CASE xbar = 12
          DEFINE POPUP plancob  ;
                 FROM 13, 44  ;
                 SHADOW RELATIVE  ;
                 MULTI COLOR  ;
                 SCHEME 3
          DEFINE BAR 1 OF plancob  ;
                 PROMPT  ;
                 ' \<General    '  ;
                 MESSAGE  ;
                 'Avisos de Cobros General'  ;
                 MARK CHR(251)  ;
                 COLOR SCHEME 2
          DEFINE BAR 2 OF plancob  ;
                 PROMPT  ;
                 ' \<Individual '  ;
                 MESSAGE  ;
                 'Avisos de Cobros Individual'  ;
                 MARK CHR(251)  ;
                 COLOR SCHEME 2
          ON SELECTION POPUP plancob do;
m_aviso with bar()
          ACTIVATE POPUP plancob
          RELEASE POPUP plancob
          RETURN
     CASE xbar = 14
          SAVE SCREEN TO m_salva
          DO scp40008
          RESTORE SCREEN FROM  ;
                  m_salva
     CASE xbar = 15
          SAVE SCREEN TO m_salva
          DO scp40088
          RESTORE SCREEN FROM  ;
                  m_salva
     CASE xbar = 16
          SAVE SCREEN TO m_salva
          DO scp40888
          RESTORE SCREEN FROM  ;
                  m_salva
     CASE xbar = 17
          SAVE SCREEN TO m_salva
          DO scp4008a
          RESTORE SCREEN FROM  ;
                  m_salva
     CASE xbar = 19
          SAVE SCREEN TO m_salva
          DO scp50001
          RESTORE SCREEN FROM  ;
                  m_salva
ENDCASE
RETURN
*
PROCEDURE m_alumzle
PARAMETER xbar
DO CASE
     CASE xbar = 1
          SAVE SCREEN TO m_salva
          DO scp30555
          RESTORE SCREEN FROM  ;
                  m_salva
     CASE xbar = 2
          SAVE SCREEN TO m_salva
          DO scp3055a
          RESTORE SCREEN FROM  ;
                  m_salva
ENDCASE
RETURN
*
PROCEDURE m_alumzli
PARAMETER xbar
DO CASE
     CASE xbar = 1
          SAVE SCREEN TO m_salva
          DO scp30066
          RESTORE SCREEN FROM  ;
                  m_salva
     CASE xbar = 2
          SAVE SCREEN TO m_salva
          DO scp3066a
          RESTORE SCREEN FROM  ;
                  m_salva
ENDCASE
RETURN
*
PROCEDURE m_aviso
PARAMETER xbar
DO CASE
     CASE xbar = 1
          SAVE SCREEN TO m_salva
          DO scp20007
          RESTORE SCREEN FROM  ;
                  m_salva
     CASE xbar = 2
          SAVE SCREEN TO m_salva
          DO scp2007p
          RESTORE SCREEN FROM  ;
                  m_salva
ENDCASE
RETURN
*
PROCEDURE m_famir
PARAMETER xbar
DO CASE
     CASE xbar = 1
          SAVE SCREEN TO m_salva
          DO scp4004a
          RESTORE SCREEN FROM  ;
                  m_salva
     CASE xbar = 2
          SAVE SCREEN TO m_salva
          DO scp40004
          RESTORE SCREEN FROM  ;
                  m_salva
     CASE xbar = 3
          SAVE SCREEN TO m_salva
          RESTORE SCREEN FROM  ;
                  m_salva
     CASE xbar = 4
          SAVE SCREEN TO m_salva
          RESTORE SCREEN FROM  ;
                  m_salva
ENDCASE
RETURN
*
PROCEDURE m_listac_1
PARAMETER xbar
DO CASE
     CASE xbar = 1
          DO carnet1
     CASE xbar = 2
          DO carnet2
ENDCASE
RETURN
*
FUNCTION logo
IF 'MONO' $ UPPER(SYS(2006))
     colorvar = 'n/n'
ELSE
     colorvar = 'n/b'
ENDIF
SET COLOR TO W+/B
@ 09, 06 SAY 'ÜÛ'
@ 09, 08 SAY 'ßßÛ' COLOR W+/N 
@ 09, 11 SAY 'Ü '
@ 10, 06 SAY 'ßÛÛÜÜ'
@ 10, 11 SAY 'ßß' COLOR  ;
  (colorvar)
@ 11, 06 SAY 'Ü  ßÛÛ'
@ 11, 12 SAY 'Û' COLOR (colorvar)
@ 12, 06 SAY 'ßÛÜÜÛ'
@ 12, 11 SAY 'ß ' COLOR W+/N 
@ 13, 06 SAY '  ßßßß' COLOR  ;
  (colorvar)
@ 09, 15 SAY 'ÛÛ'
@ 09, 17 SAY 'Ü' COLOR (colorvar)
@ 10, 15 SAY 'ÛÛ ' COLOR W+/N 
@ 11, 15 SAY 'ÛÛ ' COLOR W+/N 
@ 12, 15 SAY 'ÛÛ ' COLOR W+/N 
@ 13, 15 SAY ' ßß' COLOR  ;
  (colorvar)
@ 09, 20 SAY 'ÜÛ'
@ 09, 22 SAY 'ßßÛ' COLOR W+/N 
@ 09, 25 SAY 'Ü '
@ 10, 20 SAY 'ßÛÛÜÜ'
@ 10, 25 SAY 'ßß' COLOR  ;
  (colorvar)
@ 11, 20 SAY 'Ü  ßÛÛ'
@ 11, 26 SAY 'Û' COLOR (colorvar)
@ 12, 20 SAY 'ßÛÜÜÛ'
@ 12, 25 SAY 'ß ' COLOR W+/N 
@ 13, 20 SAY '  ßßßß' COLOR  ;
  (colorvar)
@ 09, 29 SAY ' ÜÛ'
@ 09, 32 SAY 'ßßÛ' COLOR W+/N 
@ 09, 35 SAY 'Ü'
@ 10, 29 SAY 'ÛÛ ' COLOR W+/N 
@ 10, 32 SAY 'ß' COLOR (colorvar)
@ 10, 33 SAY '  ß'
@ 10, 36 SAY 'Û' COLOR (colorvar)
@ 11, 29 SAY 'ÛÛ ' COLOR W+/N 
@ 11, 32 SAY '   Ü '
@ 12, 29 SAY ' ßÛÜÜÛ'
@ 12, 35 SAY 'ß ' COLOR W+/N 
@ 13, 29 SAY '   ßßßß' COLOR  ;
  (colorvar)
@ 09, 39 SAY ' ýý'
@ 09, 42 SAY 'ý' COLOR W+/N 
@ 09, 43 SAY 'ýý'
@ 10, 39 SAY 'ýý ' COLOR W+/N 
@ 10, 42 SAY 'ý ' COLOR  ;
  (colorvar)
@ 10, 44 SAY 'ýý'
@ 10, 46 SAY 'ý' COLOR (colorvar)
@ 11, 39 SAY 'ýý ' COLOR W+/N 
@ 11, 42 SAY '  ýý'
@ 11, 46 SAY 'ý' COLOR (colorvar)
@ 12, 39 SAY ' ýýýý'
@ 12, 44 SAY 'ý ' COLOR W+/N 
@ 12, 46 SAY 'ý' COLOR (colorvar)
@ 13, 39 SAY '   ýýý' COLOR  ;
  (colorvar)
@ 09, 49 SAY 'ýýý  ýý'
@ 09, 56 SAY 'ý' COLOR (colorvar)
@ 10, 49 SAY 'ýýýý' COLOR W+/N 
@ 10, 53 SAY ' ýý'
@ 10, 56 SAY 'ý' COLOR (colorvar)
@ 11, 49 SAY 'ýý ' COLOR W+/N 
@ 11, 52 SAY 'ý'
@ 11, 53 SAY 'ýýý ' COLOR W+/N 
@ 12, 49 SAY 'ýý ' COLOR W+/N 
@ 12, 52 SAY ' ýýý'
@ 12, 56 SAY ' ' COLOR W+/N 
@ 13, 49 SAY ' ýý   ýý' COLOR  ;
  (colorvar)
@ 09, 59 SAY ' ýý'
@ 09, 62 SAY 'ý' COLOR W+/N 
@ 09, 63 SAY 'ýý'
@ 10, 59 SAY 'ýý ' COLOR W+/N 
@ 10, 62 SAY 'ý ' COLOR  ;
  (colorvar)
@ 10, 64 SAY 'ýý'
@ 10, 66 SAY 'ý' COLOR (colorvar)
@ 11, 59 SAY 'ýý ' COLOR W+/N 
@ 11, 62 SAY '  ýý'
@ 11, 66 SAY 'ý' COLOR (colorvar)
@ 12, 59 SAY ' ýýýý'
@ 12, 64 SAY 'ý ' COLOR W+/N 
@ 12, 66 SAY 'ý' COLOR (colorvar)
@ 13, 59 SAY '   ýýý' COLOR  ;
  (colorvar)
@ 09, 68 SAY 'ý'
@ 09, 69 SAY 'ýýýýý' COLOR W+/N 
@ 09, 74 SAY 'ý' COLOR (colorvar)
@ 10, 70 SAY 'ýý ' COLOR W+/N 
@ 11, 70 SAY 'ýý ' COLOR W+/N 
@ 12, 70 SAY 'ýý ' COLOR W+/N 
@ 13, 68 SAY '   ýý' COLOR  ;
  (colorvar)
@ 7, 2 TO 7, 77 COLOR B+/B 
@ 7, 2 TO 14, 2 COLOR B+/B 
@ 7, 77 TO 14, 77 COLOR N+/B 
@ 7, 2 SAY 'ý' COLOR B+/B 
@ 7, 77 SAY 'ý' COLOR N+/B 
@ 14, 2 TO 14, 77 COLOR N+/B 
@ 14, 2 SAY 'ý' COLOR B+/B 
@ 14, 77 SAY 'ý' COLOR N+/B 
SET COLOR TO W+/B
xz1 = 'Copyright (C) Dise¤o de Sistemas SISCONOT, C.A. 1996'
xz2 = 'Versi¢n 1.1'
xz3 = 'Reservados todos los Derechos'
xz4 = 'Licencia Autorizada a ' +  ;
      ALLTRIM(l_nomext)
xz5 = 'R.I.F. Nro. ' +  ;
      ALLTRIM(l_rif)
xz6 = 'Toda Utilizaci¢n de este sistema que no este'
xz7 = 'Expresamente autorizada, esta prohibida'
@ 15, 0 SAY SPACE((80 - LEN(xz1) -  ;
  6) / 2) + xz1
@ 16, 0 SAY SPACE((80 - LEN(xz2) -  ;
  6) / 2) + xz2
@ 17, 0 SAY SPACE((80 - LEN(xz3) -  ;
  6) / 2) + xz3
@ 18, 0 SAY SPACE((85 - LEN(xz4) -  ;
  6) / 2) + xz4
@ 19, 0 SAY SPACE((80 - LEN(xz5) -  ;
  6) / 2) + xz5
@ 20, 0 SAY SPACE((80 - LEN(xz6) -  ;
  6) / 2) + xz6
@ 21, 0 SAY SPACE((80 - LEN(xz7) -  ;
  6) / 2) + xz7
RETURN .T.
*
FUNCTION inicio
SET COLOR TO GR+/R, W/RB
@ 00, 00 SAY SPACE(80)
@ 00, 01 SAY empresa
xyz = SPACE(((95 - LEN(sistema) -  ;
      6) / 2) + 3)
@ 00, LEN(xyz) SAY sistema
x = fecha(00,63)
SET COLOR TO N/W
@ 01, 00 SAY SPACE(80)
SET COLOR TO W+/B
@ 02, 00 CLEAR TO 23, 80
x = logo()
SET COLOR TO N/W
@ 24, 00
@ 24, 03 SAY  ;
  ' Mover Opci¢n                  Seleccionar Opci¢n'
SET COLOR TO R+/W
@ 24, 20 SAY CHR(27) + '  ' +  ;
  CHR(26)
@ 24, 59 SAY 'ENTER'
SET COLOR TO N/W
@ 24, 68 SAY 'F1 HELP'
SET COLOR TO R+/W
@ 24, 27 SAY CHR(24) + '  ' +  ;
  CHR(25)
set color to &color1
RETURN .T.