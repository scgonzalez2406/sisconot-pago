for i in *.PRG; do
  echo $i
  perl -i -p -e 's|[\r\n]+|\r\n|g' $i
done
