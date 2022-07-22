set -e

cd $cache_root_dir

mkdir bar

echo "foo" > bar/file.txt

echo "run: rak foo --highlight=False --human"

echo "<<<"
rak foo --highlight=False --human 2>&1
echo ">>>"

