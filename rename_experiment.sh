#!/bin/bash

from=$1
to=$2

if [ -d data/$from ]; then 
	git mv data/$from data/$to
fi

if [ -d figures/$from ]; then
	git mv figures/$from figures/$to
fi

if [ -d results/$from ]; then
	git mv results/$from results/$to
fi

if [ -d scripts/$from ]; then
	git mv scripts/$from scripts/$to
fi

if [ -d derived/$from ]; then
	mv derived/$from derived/$to
fi

find . -type f -regex ".*.Rm?d?" -print0 | xargs -0 sed -i "s#$from#$to#g"


