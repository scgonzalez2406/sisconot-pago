raya=CHR(27)+CHR(45)+"1"
noraya=CHR(27)+CHR(45)+"0"
ng=CHR(27)+CHR(69)
ngn=CHR(27)+CHR(70)

CLOSE ALL
SET EXCLU OFF
SET DELE ON
SET TALK OFF
SELECT 0
USE PLANCOBR AGAIN ALIAS PLANCOBR ORDER LAPCOD
SELECT 0
USE MP022002 AGAIN ALIAS MAPO ORDER CED
SELECT 0
USE AD022003 AGAIN ALIAS ADIC ORDER CED
SELECT 0
USE datos again alias datos order cedula
SELECT 0
CREATE CURSOR alumno ;
 (  cedula    c(12),;
    nombres   c(40),;
    plancobr  c(5),;
    nomplan   c(25))
INDEX ON plancobr+cedula TAG planalm
SET ORDER TO TAG planalm
SELECT mapo
GO TOP
DO WHILE !EOF()
   IF status = "X"
      SELECT mapo
      SKIP
      LOOP
   ENDIF
   IF !ALLTRI(plancobro) $ "02040608"
      SELECT mapo
      SKIP
      LOOP
   ENDIF
   m.cedula=ced
   m.plancobr=plancobro
   SELECT datos
   SET ORDER TO TAG cedula
   IF !SEEK(m.cedula)
      SELECT mapo
      SKIP
      LOOP
   ENDIF
   m.nombres=ALLTRIM(nombres)
   SELECT plancobr
   IF !SEEK("022003"+m.plancobr)
      SELECT mapo
      SKIP
      LOOP
   ENDIF
   m.nomplan=ALLTRIM(descripcio)
   SELECT alumno
   APPEND BLANK
   GATHER MEMVAR MEMO
   SELECT mapo
   SKIP
ENDDO
SELECT alumno
SET DEVI TO PRINT
GO TOP
LI=89
SW=0
PAG=0
DO WHILE !EOF()
   IF LI>80
      PAG=PAG+1
      @1,01  SAY "UNIDAD EDUCATIVA COLEGIO SIGLO XXI"
      @1,58 SAY "PAGINA: "+LTRIM(STR(pag))
      @2,01 SAY "DPTO. DE ADMINISTRACION"
      @2,58 SAY "FECHA : "+DTOC(DATE())
      @3,21 SAY "LAPSO : 02-2003/2004"
      @4,15 say "ALUMNOS VS. CON PLAN DE COBROS"
      @5,15 SAY "=============================="
      @6,01 SAY REPLIC("-",77)
      @7,01 SAY "CEDULA    ALUMNOS"
      @8,01 SAY REPLIC("-",77)
      LI=9
   ENDIF
   IF SW=0
      XCOD=PLANCOBR   
      @LI,01 SAY NG+PLANCOBR
      @LI,10 SAY NOMPLAN+NGN
      LI=LI+1
      SW=1
   ENDIF
   IF PLANCOBR <> XCOD
      XCOD=PLANCOBR   
      @LI,01 SAY NG+PLANCOBR
      @LI,10 SAY NOMPLAN+NGN
      LI=LI+1
   ENDIF      
   @LI,01 SAY CEDULA
   @LI,12 SAY NOMBRES
   LI=LI+1
   IF LI > 55
      @LI,01 SAY REPLIC("-",77)
      LI=89
   ENDIF
   SKIP
ENDDO
@LI,01 SAY REPLIC("-",77)
SET DEVI TO SCREE
      
      
      
   
   
            
    
