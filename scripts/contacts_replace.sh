#!/bin/bash
# This script can be used once in a while to update contacts.
sed -i 's/cocci.systeme.lip6.fr/cocci@inria.fr/g'                                           `git ls-files` # . instead of @ in the pattern just to avoid this script to be affected by itself :-)
sed -i 's/http:\/\/coccinelle.lip6.fr/https:\/\/coccinelle.gitlabpages.inria.fr\/website/g' `git ls-files`
# Perhaps also to convert:
# julia.lawall@lip6.fr
# yoann.padioleau@gmail.com
# nicolas.palix@imag.fr
# peter.senna@gmail.com
