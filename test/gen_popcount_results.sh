
RED='\033[0;31m'
NC='\033[0m' # No Color
GREEN='\033[0;32m'
PURPLE='\033[0;35m'

# get built
stack build

if [ $# -eq 1 ];
then
    base=$1
    #regenerate the tests!!
    stack exec system-test generatePopCount $base
else
    base=""
    #regenerate the tests!!
    stack exec system-test generatePopCount 4
fi



for file in popCountTests/$base*.cnf;
do
    if [ ! -e $file ]; then
        echo "No test found matching generated_tests/$base*.cnf"
        exit 1
    fi

    file=${file%.*}
    name=${file#popCountTests/}

    cryptominisat5 --verb 0 "$file.cnf" > "./popCountResults/$name.cnf"

done

stack exec system-test testPopCount

echo -e "${GREEN}Done generating results!${NC}"
