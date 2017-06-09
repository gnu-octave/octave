#! /bin/sh

bug_numbers=$(for file in "$@"; do
  sed -n "s/.*<\([0-9][0-9][0-9][0-9]*\)>.*/\1/p" "$file"
done | sort -u)

fixed_bug_numbers=$(for num in $bug_numbers; do
  status=$(wget -q -O - http://octave.org/testfailure/?$num | sed -n 's/.*>Status:<\/span><\/span>&nbsp;<\/td><td valign="middle" width="35%">\([^<]*\)<.*/\1/p');
  if [ "$status" = "Fixed" ]; then echo "$num"; fi
done)

if [ -z "$fixed_bug_numbers" ]; then
  echo "no change in bug status"
  exit 0;
fi

fixed_bug_pattern=`echo $fixed_bug_numbers | sed 's/ /\\\\|/g; s/^/<\\\\(/; s/$/\\\\)>/'`

for file in "$@"; do
   sed -i "s/$fixed_bug_pattern/<*\1>/" "$file"
done
