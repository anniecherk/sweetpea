
RED='\033[0;31m'
NC='\033[0m' # No Color
GREEN='\033[0;32m'
PURPLE='\033[0;35m'

if [ $# -eq 1 ];
then
    base=$1
else
    base=""
fi

#regenerate the tests!!
stack build
stack exec cnf generateKofN 6

for file in KofNTests/$base*.cnf;
do
    if [ ! -e $file ]; then
        echo "No test found matching generated_tests/$base*.cnf"
        exit 1†s
    fi

    file=${file%.*}
    name=${file#KofNTests/}

    cryptominisat5 --verb 0 "$file.cnf" > "./KofNResults/$name.cnf"

done

stack exec cnf testKofN

echo -e "${GREEN}Done generating results!${NC}"
