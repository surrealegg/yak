#!/bin/bash

SHOULD_PASS=$(find ./tests/pass -type f -name "*.jakt")
SHOULD_FAIL=$(find ./tests/fail -type f -name "*.jakt")
GREEN="\033[0;32m"
RED="\033[0;31m"
WHITE="\033[0;37m"
NC="\033[0m"
ERROR=0

cargo -q build

for FILE in $SHOULD_PASS; do
    RESULT=$(cargo -q run $FILE 2>/dev/null);
    CODE=$?
    EXPECTED=$(cat $FILE.expected);
    if [ $CODE -eq 0 ] && [ "$EXPECTED" == "$RESULT" ]; then
        printf "${WHITE}[${GREEN}OK${WHITE}]${NC}"
    else
        printf "${WHITE}[${RED}KO${WHITE}]${NC}"
        ERROR=$((ERROR+1))
    fi
    printf " $FILE\n"
done

for FILE in $SHOULD_FAIL; do
    RESULT=$(cargo -q run $FILE 2>/dev/null);
    if [ $? -eq $(cat $FILE.expected) ]; then
        printf "${WHITE}[${GREEN}OK${WHITE}]${NC}"
    else
        printf "${WHITE}[${RED}KO${WHITE}]${NC}"
        ERROR=$((ERROR+1))
    fi
    printf " $FILE\n"
done

exit $ERROR