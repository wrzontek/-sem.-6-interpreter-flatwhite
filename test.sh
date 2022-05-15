FILES="../fwtests/bad/*"
for f in $FILES.fw
do
   ./interpreter $f
done

