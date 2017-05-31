
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
#stack exec cnf generateKofN 6
stack exec cnf generateKlessthanN 6

#for file in KofNTests/$base*.cnf;
for file in KlessthanNTests/$base*.cnf;
do
    if [ ! -e $file ]; then
        echo "No test found matching generated_tests/$base*.cnf"
        exit 1†s
    fi

    file=${file%.*}
    # name=${file#KofNTests/}
    name=${file#KlessthanNTests/}

    # cryptominisat5 --verb 0 "$file.cnf" > "./KofNResults/$name.cnf"
    cryptominisat5 --verb 0 "$file.cnf" > "./KlessthanNResults/$name.cnf"

done

#stack exec cnf testKofN
stack exec cnf testKlessthanN

echo -e "${GREEN}Done generating results!${NC}"
