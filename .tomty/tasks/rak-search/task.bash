set -e

echo "foo" > $cache_root_dir/file.txt
cd $cache_root_dir

echo "run: rak foo --highlight=False --human"

echo "<<<"
rak foo --highlight=False --human
echo ">>>"

