set -e

cd $cache_root_dir

mkdir bar

echo "foo" > bar/file.txt

echo "<<<"
echo "execute: rak foo bar"
rak foo
echo ">>>"

