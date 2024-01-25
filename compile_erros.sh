#!/usr/bin/bash

echo -e "\u2713 Removed compilation generated artifacts"
make clean > /dev/null 2>&1 
echo -e "\u203c Compiling and storing stderr to file"
make &> ~/midland/coccinelle_make_warnings.txt
echo -e "\u2713 Warnings written successfully"
read -p "Open the file? [Y/n]: " ans
mod_ans=$(echo "$ans" | tr '[:upper:]' ':lower:')
if [ "$mod_ans" = "y" ] || [ "$mod_ans" = "yes" ]; then
	nvim ~/midland/coccinelle_make_warnings.txt
else
	echo -e "\u2713 Exiting now."
fi
