@echo off

type introduction.txt > email.txt
type copycat_general_description.txt >> email.txt
echo "<!--more-->" >> email.txt
type changelog.txt >> email.txt
echo "[category releases-devtools]" >> email.txt
echo "[category releases-devtools]" >> email.txt
echo "[publicize off]" >> email.txt

echo Sending to copycat.fr...

echo "jn@microtec.fr" > to.txt
echo "CopyCat Developer v.%2%" > subject.txt
CALL email.bat 

