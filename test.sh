FILES="../fwtests/good/*"
for f in $FILES.fw
do
   ./interpreter $f
done

