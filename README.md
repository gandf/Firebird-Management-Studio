# Firebird Management Studio
Firebird Management Studio target is to migrate old source of IBConsole to lazarus and make compliance with last firebird version and improve functionnality.
Delphi is not open source and I think no open source tools for firebird is supported by community.
I hope to migrate on open source solution help deployment of generic firebird tools.

For now, project compile but fully work. Some code has been disabled to compile.

Win32 EXE file are provided to test without compile for all people. Source and software are provided without any warranty.

Components to install in Lazarus :
- ibx : https://www.mwasoftware.co.uk/ibx

Work in progressor or to target :
- Translation
- Clean
- Fix issue from convertion from Delphi to Lazarus
- Fix issue occured by delete lzRichEdit and replace by SynEdit
- Fix issue occured by delete lnet
- Change ibx to official firebird component already present in Lazarus
