if [ "$1" = "gen" ]; then
    ./dist/build/LambdaGen/LambdaGen $2 $3
else
    ./dist/build/Homework$1/Homework$1
fi
