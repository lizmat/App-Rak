set -e

echo "foo" > $cache_root_dir/file.txt
cd $cache_root_dir

echo "run: rak foo --files-with-matches"

echo "<<<"
rak foo --files-with-matches
echo ">>>"

