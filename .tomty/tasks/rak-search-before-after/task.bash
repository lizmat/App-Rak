set -e

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

echo "file: "
cat file.txt


echo "run: rak foo --before-context=$lb --after-context=$la --highlight=False --human"

echo "<<<"
rak foo --before-context=$lb --after-context=$la --highlight=False --human 2>&1
echo ">>>"

