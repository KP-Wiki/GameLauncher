# GameLauncher

## GameLauncher operates in 2 modes:
1. Launcher. Activated by running the exe as usual. Checks game verion, downloads and installs patches.
2. Patchmaker. Activated by dragging a 7z build on to the exe. Creates patches between pairs of builds.

### Launcher
Identifies build version of a folder it is run in.
Queries server for available builds and patches.
Downloads and patches on command. Before patching will verify all the affected files (to avoid data loss).

### Patchmaker
Identifies build version of a dropped file and looks for a previous build version.
Unpacks both of them and creates a diff between them.
Packs the diff into a patch.

## Requirements
1. 7zip exe (path set in TKMSettings.PATH_TO_7ZIP). Only for Patchmaker.
2. Server with API to list available builds and patches (TKMSettings.SERVER_ADDRESS and TKMSettings.SERVER_FILE_LIST_GET). Only for Launcher.
3. hdiffz.dll (included. Compiled from https://github.com/Kromster80/HDiffPatch). For both modes.

