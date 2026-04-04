#/bin/sh
# This script builds BOXER Godot WASM
#
# TODO
# Clean up the /tmp stuff from asdf-cc.lisp and other non-relative paths to the project

clear; clear
read -p "Starting Godot WASM Build... [ENTER]"
echo $(pwd)

${ECL_TO_RUN} --load ./scripts/static-wasm-lib.lisp

read -p "Moving to /tmp/ecl-cc-cache to copy liblibboxercore.a [ENTER]"
cd /tmp/ecl-cc-cache
cp  home/sgithens/code/boxer-sunrise/embedded/liblibboxercore.a ~/code/wecl/Code/wasm-ecl/

read -p "Going to move to godot-wasm and build now... [ENTER]"
cd ~/code/godot-wasm/
scons custom_modules=/home/sgithens/code/boxer-sunrise/boxwin/godot/modules/ platform=web target=template_release

read -p "Run web export from Godot project [ENTER]"
LD_LIBRARY_PATH=~/code/ecl/ecl-host/lib/ ./bin/godot.linuxbsd.editor.x86_64 --path ~/code/boxer-sunrise/boxwin/godot/Boxer/ --export-release "Web" ~/Desktop/test-binary/Boxer\ Godot\ release.html
