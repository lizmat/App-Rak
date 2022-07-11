set -e

echo "foo" > $cache_root_dir/file.txt
cd $cache_root_dir

echo "<<<"
echo "execute: rak foo --files-only"
rak foo --files-only
echo ">>>"

