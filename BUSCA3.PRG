close data
set talk off
select 1
use AD021998
set order to tag ced
go top
M.SALDO=0 
xced=space(12)
li=01
do while .not. eof()
   m.saldo=CUOTA1*6
   if m.saldo <> TOTAL_DEUD
      SET DEVI TO PRINT
      @LI,02 SAY CED
      @LI,15 SAY M.SALDO
      REPLA TOTAL_DEUD WITH M.SALDO
      LI=LI+1
   endif
   select 1
   skip
enddo
close data
return