#!/bin/sh
# You'll need to pass in a username and password, either as command line variables or via
# the environment. This should be an app-specific password generated at https://appleid.apple.com

echo "Boxer Sunrise macOS Build and Notarize\n"

# Update the timestamp on the app
touch data/boxersunrise.app

# Because of the issue with not loading cl-freetype2 until application start, we
# are copying this source part to the area in the application bundle where it can
# still be available after delivery
mkdir -p data/boxersunrise.app/Contents/MacOS
mkdir -p data/boxersunrise.app/Contents/PlugIns/shaders
cp src/draw-low-opengl330/shaders/* data/boxersunrise.app/Contents/PlugIns/shaders
cp src/draw-low-opengl330/freetype-fonts.lisp data/boxersunrise.app/Contents/PlugIns


read -p "Lispworks Build and Delivery [enter]"

# /Applications/LispWorks\ 7.1\ \(64-bit\)/LispWorks\ \(64-bit\).app/Contents/MacOS/lispworks-7-1-0-amd64-darwin -build src/delivery-script.lisp
/Applications/LispWorks\ 8.0\ \(64-bit\)/LispWorks\ \(64-bit\).app/Contents/MacOS/lispworks-8-0-0-macos64-universal -build src/delivery-script.lisp

read -p "Code sign with entitlements/hardened runtime [enter]"

codesign --force --verbose --entitlements ./data/boxersunrise.app/Contents/Resources/boxer.entitlements --option runtime --deep -s "Developer ID Application" ./data/boxersunrise.app

read -p "Zip app to send for notarization [enter]"

cd data
/usr/bin/ditto -c -k --keepParent ./boxersunrise.app ./boxersunrise.zip
cd ..

read -p "Send for notarization with notarytool [enter]"

# xcrun altool --notarize-app --password ${password} --username ${username} --file ./data/boxersunrise.zip --primary-bundle-id "boxer.notorize"
# Swithing to notarytool for 2024
xcrun notarytool submit --password ${password} --apple-id ${username} --team-id ${team} --wait ./data/boxersunrise.zip

read -p "Remove the temporary zip file [enter]"

rm ./data/boxersunrise.zip

read -p "Do the dishes while you wait for the verification email from Apple... [enter]"

read -p "Staple the ticket to the binary [enter]"

xcrun stapler staple --file ./data/boxersunrise.app

read -p "Zip again to create the final package [enter]"

cd data
/usr/bin/ditto -c -k --keepParent ./boxersunrise.app ./boxersunrise.zip
cd ..
