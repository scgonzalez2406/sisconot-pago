close data
set talk off
set delete on
select 1
use contdeud
set order to tag cedula
select 2
use mp011998
set order to tag ced

msaldo=0.00
mpagado=0.00
mced=space(12)

select 1
set order to tag cedula
go top
do while .not. eof()
   msaldo=total_d-monto_p
   if msaldo <> total_d .and. lapsomat='011998'
      mpagado=monto_p
      mced=cedula
      select 2
      set order to tag ced
      if seek (mced)
         scatter memvar memo
         do while .t.
            if rlock()
               ii=1
               for ii=1 to 12
                  lk=ltrim(str(ii))
                  if pagado&lk=cuota&lk
                     loop
                  endif
                  if (mpagado+pagado&lk) >= m.cuota&lk
                     mpagado=mpagado-(m.cuota&lk-pagado&lk)
                     repla ultimacuot with ultimacuot+1
                     x=(m.cuota&lk-pagado&lk)
                     repla monto_paga with monto_paga+x
                     repla pagado&lk with m.cuota&lk
                  else
                     repla pagado&lk with pagado&lk+mpagado
                     repla monto_paga with monto_paga+mpagado
                     exit
                  endif
               endfor
               unlock
               exit
            endif
         enddo
      endif
   endif
   select 1
   skip
enddo
close data
return