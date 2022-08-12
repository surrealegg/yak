#!/bin/bash

SHOULD_PASS=$(find ./tests/pass -type f -name "*.jakt")
SHOULD_FAIL=$(find ./tests/fail -type f -name "*.jakt")
GREEN="\033[0;32m"
RED="\033[0;31m"
WHITE="\033[0;37m"
PASS="${WHITE}[${GREEN}PASS${WHITE}]${NC}"
FAIL="${WHITE}[${RED}FAIL${WHITE}]${NC}"
NC="\033[0m"
ERROR=0

cargo -q build

for FILE in $SHOULD_PASS; do
    RESULT=$(cargo -q run $FILE 2>/dev/null);
    CODE=$?
    EXPECTED=$(cat $FILE.expected);
    if [ $CODE -eq 0 ] && [ "$EXPECTED" == "$RESULT" ]; then
        printf $PASS
    else
        printf $FAIL
        ERROR=$((ERROR+1))
    fi
    printf " $FILE\n"
done

for FILE in $SHOULD_FAIL; do
    RESULT=$(cargo -q run $FILE 2>/dev/null);
    if [ $? -eq $(cat $FILE.expected) ]; then
        printf $PASS
    else
        printf $FAIL
        ERROR=$((ERROR+1))
    fi
    printf " $FILE\n"
done

exit $ERROR