#!/bin/sh

FILES=$(find ./tests -type f -name "*.jakt")
GREEN="\033[0;32m"
RED="\033[0;31m"
WHITE="\033[0;37m"
NC="\033[0m"
ERROR=0

cargo -q build
for FILE in $FILES
do
    RESULT=$(cargo -q run $FILE 2>/dev/null);
    EXPECTED=$(cat $FILE.expected);
    if [ "$EXPECTED" == "$RESULT" ]; then
        printf "${WHITE}[${GREEN}OK${WHITE}]${NC}"
    else
        printf "${WHITE}[${RED}KO${WHITE}]${NC}"
        ERROR=$((ERROR+1))
    fi
    printf " $FILE\n"
done

exit $ERROR