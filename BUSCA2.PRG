close data
set talk off
select 1
use contdeud
set order to tag cedula
go top
 
xced=space(12)
li=01
do while .not. eof()
   m.saldo=total_d-monto_p
   if m.saldo <> TOTAL_D
      SET DEVI TO PRINT
      @LI,02 SAY CEDULA
      @LI,15 SAY M.SALDO
      @LI,30 SAY MONTO_P
      @LI,45 SAY LAPSOMAT
      @LI,55 SAY PLANCOBAD
      LI=LI+1
   endif
   select 1
   skip
enddo
close data
return