set -e

cd $cache_root_dir

mkdir bar

echo "foo" > bar/file.txt

echo "<<<"
rak foo --highlight=False --human
echo ">>>"

