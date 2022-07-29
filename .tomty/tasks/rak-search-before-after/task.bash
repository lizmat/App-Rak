set -e

cat $root_dir/task.bash

lb=$(config 'lines-before')
la=$(config 'lines-after')

touch  $cache_root_dir/file.txt

for (( i=1 ; i<=$lb ; i++ )); 
do
echo "line_before"$i >> $cache_root_dir/file.txt
done

echo "foo" >> $cache_root_dir/file.txt

for (( i=1 ; i<=$la ; i++ )); 
do
echo "line_after"$i >> $cache_root_dir/file.txt
done

cd $cache_root_dir

echo "====="
cat file.txt
echo "====="


echo "<<<"
rak foo --before-context=$lb --after-context=$la --/highlight --human 2>&1
echo ">>>"

