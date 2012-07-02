#!/bin/bash

MathKern='/Applications/Mathematica Home Edition.app/Contents/MacOs/MathKernel'

"$MathKern" -noprompt < tests.nb | grep '.' > testresults.txt
diff -B testresults.txt expectedresults.txt
echo 'DONE'

