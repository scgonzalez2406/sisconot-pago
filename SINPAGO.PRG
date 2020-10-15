CLOSE ALL
set talk off
SET EXCL OFF
SET DELE ON
SET SAFE OFF
SET DATE FREN
SET CENTUR ON
selec 0
create cursor sinmapo ;
( cedula   c(12),;
  nombre   c(45),;
  recibo   n(10),;
  emision  d(8))
selec 0
use datos again alias da order cedula
SELEC 0
USE mp012015 again alias mp order ced
selec 0
use regpagos again alias re order recibo
SET FILTER TO lapso="012015" and (nrocuota=0 .or. nrocuota=1)
go top
CLEAR
X=0
do while !eof()
   X=X+1
   @10,10 SAY X
   if transa="AT"
      SELE re
      skip
      loop
   endif   
   
   xced=cedula
   xrec=recibo
   xfec=fecha_pago
   selec mp
   if !seeK(xced)
      sele da
      if seek(xced)
         xnom=alltrim(nombres)
         selec sinmapo
         appe blank
         repla cedula with xced
         repla nombre with xnom
         repla recibo with xrec
         repla emision with xfec
      endif
   endif
   sele re
   skip
enddo
sele sinmapo
copy to sinmapo
copy to sinmapo type xls
         
         
            

