set -e

cat $root_dir/task.bash

echo "foo" > $cache_root_dir/file.txt
echo "foo2" >> $cache_root_dir/file.txt
echo "foo3" >> $cache_root_dir/file.txt

cd $cache_root_dir

which rak
rak --version
ls -l
pwd

echo "====="
cat file.txt
echo "====="

echo "<<<"
#rak foo --/highlight --human
rak foo
echo ">>>"

