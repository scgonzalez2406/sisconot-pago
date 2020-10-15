CLOSE database 
SET TALK OFF
SET DATE TO DMY
SET MULTILOCKS ON
lapmat="022014"
SELECT 0
USE concepto ;
	AGAIN ALIAS concepto ;
	ORDER TAG "codigo"
SELECT 0
USE detacobr ;
	AGAIN ALIAS detacobr ;
	ORDER TAG "codigo"
SELECT 0
USE plancobr ;
	AGAIN ALIAS plancobr ;
	ORDER TAG "codigo"
SELECT 0
USE mp&lapmat ;
	AGAIN ALIAS mapo ;
	ORDER TAG "ced"
SET MESSAGE TO "SISCONOT, C.A. (c) 2002, Control de Cobranza de Mensualidades"
GO TOP
CLEAR
JH=0

DO WHILE !EOF()
    JH=JH+1
    @10,10 SAY JH
	SCATTER MEMVAR MEMO
	m.codplan=m.plancobro
    *-Bucle de registro de inscripción
	mmontopagado=monto_paga
	m.montopagad=monto_paga
	m.montocan=monto_paga
	FOR i=1 TO 15
		lk=LTRIM(STR(i))
		m.concep&lk=SPACE(3)
		m.montoc&lk=0.00
	ENDFOR
	SELECT plancobr
	SET ORDER TO TAG lapcod
	SEEK lapmat+m.codplan
	IF !FOUND()
		SELECT mapo
		SKIP
		LOOP
	ENDIF
	mhasta=nrocuotas
	sumat=0.00
	FOR im=1 TO mhasta
		lm=LTRIM(STR(im))
		SELECT detacobr
		SET ORDER TO TAG codcuota
		SEEK lapmat+m.codplan+STR(im)
		DO WHILE !EOF()
			IF lapso <> lapmat
				SELECT detacobr
				SKIP
				LOOP
			ENDIF
			IF codigo <> m.codplan
				SELECT detacobr
				SKIP
				LOOP
			ENDIF
			IF cuota <> im
				EXIT
			ENDIF
			mtotalcuota=0.00
			FOR m=1 TO 10
				lk=LTRIM(STR(m))
				IF concepto&lk=SPACE(3)
					LOOP
				ENDIF
				mtotalcuota=mtotalcuota+monto&lk
			ENDFOR
			sumat=sumat+mtotalcuota
			m.montcuota&lm=mtotalcuota
			SELECT detacobr
			SKIP
		ENDDO
	ENDFOR
	*-  aqui
	*- Redistribucion del Monto Pagado
		SELECT mapo
		mpagado=mmontopagado
		FOR i=1 TO 12
			u=LTRIM(STR(i))
			REPLACE pagado&u WITH 0.00
		ENDFOR
		REPLACE ultimacuot WITH 0, total_deud WITH sumat
		SELECT mapo
		Ii=1
		FOR Ii=1 TO mhasta
			lk=LTRIM(STR(Ii))
			SELECT mapo
			REPLACE cuota&lk WITH m.montcuota&lk
			IF cuota&lk > 0
				IF pagado&lk=cuota&lk
					LOOP
				ENDIF
			ENDIF
			IF (mpagado+pagado&lk) >= m.montcuota&lk
				mpagado=mpagado-(m.montcuota&lk-pagado&lk)
				REPLACE ultimacuot WITH ultimacuot+1
				x=(m.montcuota&lk-pagado&lk)
				REPLACE monto_paga WITH monto_paga+x
				REPLACE cuota&lk WITH m.montcuota&lk
				REPLACE pagado&lk WITH m.montcuota&lk
			ELSE
				IF mpagado > 0
					REPLACE cuota&lk WITH m.montcuota&lk
					REPLACE pagado&lk WITH pagado&lk+mpagado
					REPLACE monto_paga WITH monto_paga+mpagado
				ENDIF
				EXIT
			ENDIF
		ENDFOR	
		
	SELECT mapo
	REPLACE monto_paga WITH pagado1+pagado2+pagado3+pagado4+pagado5+pagado6+pagado7+pagado8+pagado9+pagado10+pagado11+pagado12
	*--
	SKIP
ENDDO
CLOSE daTA


