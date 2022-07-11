set -e

echo "line1" > $cache_root_dir/file.txt
echo "line2" >> $cache_root_dir/file.txt
echo "foo" >> $cache_root_dir/file.txt
echo "line3" >> $cache_root_dir/file.txt
echo "line4" >> $cache_root_dir/file.txt

cd $cache_root_dir

echo "<<<"
echo "execute: rak foo --before=2 --after=2"
rak foo --before=2 --after=2
echo ">>>"

