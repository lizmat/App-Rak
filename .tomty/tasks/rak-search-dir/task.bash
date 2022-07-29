set -e

cat $root_dir/task.bash

cd $cache_root_dir

mkdir bar

echo "foo" > bar/file.txt

echo "====="
cat  bar/file.txt
echo "====="

echo "<<<"
rak foo bar --/highlight --human 2>&1
echo ">>>"

echo "---"

echo "<<<"
rak foo bar2 --/highlight --human 2>&1
echo ">>>"
