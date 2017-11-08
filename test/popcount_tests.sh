
RED='\033[0;31m'
NC='\033[0m' # No Color
GREEN='\033[0;32m'
PURPLE='\033[0;35m'



run_fileoutput_tests(){
  echo "I am in run_fileoutput_tests"
  #regenerate the tests!!
  stack build
  stack exec cnf generatePopCount 6

  for file in popCountTests/$base*.cnf;
  do
      if [ ! -e $file ]; then
          echo "No test found matching generated_tests/$base*.cnf"
          exit 1†s
      fi

      file=${file%.*}
      name=${file#popCountTests/}

      cryptominisat5 --verb 0 "$file.cnf" > "./popCountResults/$name.cnf"

  done

  stack exec cnf testPopCount

  echo "I am leaving run_fileoutput_tests"
}



run_adder_tests(){
  echo "I am in adder tests"
  #regenerate the tests!!
  stack build
  stack exec cnf fullAdder
  stack exec cnf rippleCarry 3

  for file in generated_tests/$base*.cnf;
  do
      if [ ! -e $file ]; then
          echo "No test found matching generated_tests/$base*.cnf"
          exit 1†s
      fi

      file=${file%.*}
      name=${file#generated_tests/}


      diff "$file.sol" <(cryptominisat5_simple --verb=0 "$file.cnf") > /dev/null

      if [ $? -ne 0 ];
      then

          echo -e "Oh no, ${RED}$name Failed! ${NC}\n    ${GREEN}TRUE${NC}                                                          ${PURPLE}GENERATED${NC}"
          diff -y "$file.sol" <(cryptominisat5_simple "$file.cnf" | tail -n 2)
          echo ""
        #  exit 1
      fi

  done

  echo "I am leaving adder tests"
}


if [ $# -eq 1 ];
then
    base=$1
else
    base=""
fi




run_fileoutput_tests
#run_adder_tests

echo -e "${GREEN}Done generating results!${NC}"

exit $?
