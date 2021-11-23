# Code Signing and Notarizing

This includes information, and the exact command line usage to sign the binaries and use
Apples notarization service.

## Apple signing and Notarizing

These examples were constructed on MacOS Catalina using XCode 12.4. Some of the invocations are
likely different on Big Sur and greater versions of the XCode command line tools.

Substitute the relevant sections before for your Signing profile and Apple Id and password.

For the `codesign` invocation the `runtime` option enabled the application runtime hardening. For finer grain
control there is an option that allows passing in an Entitlements plist.  The `deep` option scans
the entire bundle and signs all the binaries. Occasionally this is known to not traverse them
in the correct order, forcing you to run it for each binary inside from lowest to furthest out,
but in general as worked fine for boxer so far.

### Developer Certificates

The `-s` option in `codesign` below needs to be the actual name of the certificate as it is
displayed in the Keychain Access application. The following steps can be used to create a local
signing certificate if one does not exist yet. This requires a paid Apple Developer Program account.

- Follow these steps to create a certificate request:

  https://help.apple.com/developer-account/#/devbfa00fef7
- Create a new "Developer ID Application" certificate on this page using the certificate
  request created in the previous step.

  https://developer.apple.com/account/resources/certificates/list
- After creating the certificate, download it to your Keychain Access and use it's name in
  the `codesign` `-s` option.

You may need to create a separate request and certificate for each build machine to avoid
codesign generating a "The specified item could not be found in the keychain" error.

### Command line examples

```sh
# Signing the entire application bundle
codesign --force --verbose --entitlements ~/code/boxer-sunrise2/data/boxersunrise.app/Contents/Resources/boxer.entitlements --option runtime --deep -s "Developer ID Application" ./current-test-build/boxersunrise-wip.app

# Compress the app into a zip file
cd data
/usr/bin/ditto -c -k --keepParent ./boxersunrise.app ./boxersunrise.zip
cd ..

# Submitting the bundle for notarization
xcrun altool --notarize-app --username 'steve@githens.org' --password ********  --file ./current-test-build/boxersunrise-wip.zip --primary-bundle-id "boxer.notorize"

# Checking the notarization status

xcrun altool --username 'steve@githens.org' --password ******** --notarization-info <UUID-returned-from-notarize-app>

# Stapling the ticket to the app
# We can't stable to a zip file, so we staple back to the original .app we signed,
# and then can zip it again for distribution
xcrun stapler staple --file ./current-test-build/boxersunrise-wip.app
```

### Current structure of binary dependencies

The current (July 2021) layout of the binary dependencies is below.  `Resources` cannot contain any code
binaries in order to sign properly.  We are putting all dependencies in either `Frameworks` or `PlugIns`.

```
data/boxersunrise.app/Contents
├── Frameworks
│   ├── libfreetype.6.dylib
│   └── libfreetype.dylib
├── Info.plist
├── MacOS
│   ├── boxersunrise
│   └── boxersunrise.lwheap
├── PkgInfo
├── PlugIns
│   ├── cl-freetype2
│   │   └── src
│   │       ├── bitmap.64xfasl
│   │       ├── face.64xfasl
│   │       ├── ffi
│   │       │   ├── cffi-cwrap.64xfasl
│   │       │   ├── cffi-defs.64xfasl
│   │       │   ├── ft2-basic-types.64xfasl
│   │       │   ├── ft2-bitmap.64xfasl
│   │       │   ├── ft2-face.64xfasl
│   │       │   ├── ft2-glyph.64xfasl
│   │       │   ├── ft2-init.64xfasl
│   │       │   ├── ft2-lib.64xfasl
│   │       │   ├── ft2-outline.64xfasl
│   │       │   ├── ft2-size.64xfasl
│   │       │   └── grovel
│   │       │       ├── grovel-freetype2.64xfasl
│   │       │       ├── grovel-freetype2.processed-grovel-file
│   │       │       ├── grovel-freetype2__grovel
│   │       │       ├── grovel-freetype2__grovel.c
│   │       │       └── grovel-freetype2__grovel.o
│   │       ├── glyph.64xfasl
│   │       ├── init.64xfasl
│   │       ├── outline.64xfasl
│   │       ├── package.64xfasl
│   │       ├── render.64xfasl
│   │       └── toy.64xfasl
│   └── freetype-fonts.lisp
└── Resources
    ├── Fonts
    │   ├── LICENSE-Liberation-Fonts
    │   ├── LiberationMono-Bold.ttf
    │   ├── LiberationMono-BoldItalic.ttf
    │   ├── LiberationMono-Italic.ttf
    │   ├── LiberationMono-Regular.ttf
    │   ├── LiberationSans-Bold.ttf
    │   ├── LiberationSans-BoldItalic.ttf
    │   ├── LiberationSans-Italic.ttf
    │   ├── LiberationSans-Regular.ttf
    │   ├── LiberationSerif-Bold.ttf
    │   ├── LiberationSerif-BoldItalic.ttf
    │   ├── LiberationSerif-Italic.ttf
    │   ├── LiberationSerif-Regular.ttf
    │   └── info.txt
    ├── boxer-app.icns
    ├── boxer-file.icns
    └── boxer-sunrise.icns
```

### References

- https://developer.apple.com/library/archive/technotes/tn2206/_index.html#//apple_ref/doc/uid/DTS40007919-CH1-TNTAG206
- https://developer.apple.com/documentation/security/notarizing_macos_software_before_distribution/resolving_common_notarization_issues
- https://developer.apple.com/documentation/security/notarizing_macos_software_before_distribution/customizing_the_notarization_workflow#3087720
- https://developer.apple.com/library/archive/documentation/Security/Conceptual/CodeSigningGuide/Procedures/Procedures.html
- https://developer.apple.com/documentation/security/notarizing_macos_software_before_distribution/customizing_the_notarization_workflow/notarizing_apps_from_the_command_line_with_xcode_12_and_earlier
- https://developer.apple.com/library/archive/technotes/tn2206/_index.html#//apple_ref/doc/uid/DTS40007919-CH1-TNTAG309
