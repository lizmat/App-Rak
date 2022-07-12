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

echo "<<<"
echo "execute: rak foo --before=$lb --after=$la"
rak foo --before=$lb --after=$la
echo ">>>"

