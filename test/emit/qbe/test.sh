for file in $(ls -1 *.out); do
    ./$file
    return_code=$?
    echo "Executing:" $file "has returned code: " $return_code
done
