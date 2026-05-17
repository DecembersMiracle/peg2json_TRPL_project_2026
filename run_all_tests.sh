#!/bin/bash

GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m'

PASSED=0
FAILED=0
TOTAL=0

echo "Compiling parser..."
ghc Main.hs -o peg-parser src/AST.hs src/PEGParser.hs src/JSONExporter.hs > /dev/null 2>&1

if [ ! -f "./peg-parser" ]; then
    echo -e "${RED}Failed to compile parser${NC}"
    exit 1
fi
echo ""

echo "=== Unit tests ==="
echo ""

ghc tests/tests.hs -o tests/tests_unit -isrc src/AST.hs src/PEGParser.hs src/JSONExporter.hs > /dev/null 2>&1

if [ -f "./tests/tests_unit" ]; then
    ./tests/tests_unit
    if [ $? -eq 0 ]; then
        PASSED=$((PASSED + 1))
    else
        FAILED=$((FAILED + 1))
    fi
    TOTAL=$((TOTAL + 1))
else
    echo -e "${RED}Compilation failed${NC}"
    FAILED=$((FAILED + 1))
    TOTAL=$((TOTAL + 1))
fi

echo ""
echo "=== PEG files tests ==="
echo ""

for file in arithmetic.peg boolean.peg list.peg identifier.peg xml.peg; do
    echo -n "  $file ... "
    TOTAL=$((TOTAL + 1))
    ./peg-parser tests/peg_files/$file > /tmp/output.json 2>&1
    if [ $? -eq 0 ]; then
        echo -e "${GREEN}OK${NC}"
        PASSED=$((PASSED + 1))
    else
        echo -e "${RED}FAIL${NC}"
        FAILED=$((FAILED + 1))
    fi
done

echo "========================================="
echo -e "Passed: ${GREEN}$PASSED${NC}"
echo -e "Failed: ${RED}$FAILED${NC}"
echo -e "Total: $TOTAL"

if [ $PASSED -eq $TOTAL ]; then
    echo -e "${GREEN}All tests passed.${NC}"
fi
echo ""

rm -f tests/tests_unit /tmp/output.json