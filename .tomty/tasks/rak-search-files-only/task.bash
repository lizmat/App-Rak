set -e

cat $root_dir/task.bash

echo "foo" > $cache_root_dir/file.txt

cd $cache_root_dir

echo "====="
cat file.txt
echo "====="

echo "<<<"
rak foo --files-with-matches 2>&1
echo ">>>"

