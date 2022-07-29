set -e

cat $root_dir/task.bash

echo "foo" > $cache_root_dir/file.txt

cd $cache_root_dir

rak --version

echo "====="
cat file.txt
echo "====="

echo "<<<"
rak foo --/highlight --human
echo ">>>"

