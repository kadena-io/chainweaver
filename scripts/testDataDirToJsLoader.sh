#!/usr/bin/env bash
echo "localStorage.clear();"
for i in $(find $1 -type f);
  do echo "localStorage.setItem('$(basename $i)', '$(cat $i | perl -p -e 's|\\n|\\\n|g')');" ;
done;
