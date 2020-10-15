close data
set talk off
select 1
use contdeud
set order to tag cedula
go top
select 2
use mp021998
set order to tag ced
go top 

select 1
set order to tag cedula
go top

xced=space(12)
li=01
plan=space(4)
do while .not. eof()
   m.saldo=total_d-monto_p
   if m.saldo <> 0.00 .AND. m.saldo<>total_d .and. lapsomat='021998'
      xced=cedula
      plan=plancobad
      select 2
      set order to tag ced
      go top
      if seek (xced)
         msal=total_deud-monto_paga
         if msal=0.00
            set devi to print
            @li,02 say xced
            @li,15 say m.saldo
            @li,30 say plan
            li=li+1
         endif
      endif   
   endif
   select 1
   skip
enddo
close data
return