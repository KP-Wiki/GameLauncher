# GameLauncher

Application to keep games up to date.

## Guidelines
1. Everything should be streamlined for the player. First goal of the Launcher is to make players life simpler.
2. If anything is a lossy operation - cancel early and advise player to downloading a full build as usual.
3. Later on we could split assets into app-managed (e.g. stock maps) and user-managed (maps, mods). App-managed will be updated, user-managed will be left untouched.

## GameLauncher operates in 2 modes:
1. Launcher. Activated by running the exe as usual. Checks game verion, downloads and installs patches.
2. Patchmaker. Activated by dragging a 7z build on to the exe. Creates patches between pairs of builds.

### Launcher
1. Identifies build version of a folder it is run in.
2. Queries server for available builds and patches.
3. Downloads and patches on command. Before patching will verify all the affected files (to avoid data loss).

### Patchmaker
1. Identifies build version of a dropped file and looks for a previous build version.
2. Unpacks both of them and creates a diff between them.
3. Packs the diff into a patch.

## Requirements
1. 7zip exe (path set in TKMSettings.PATH_TO_7ZIP). Only for Patchmaker.
2. Server with API to list available builds and patches (TKMSettings.SERVER_ADDRESS and TKMSettings.SERVER_FILE_LIST_GET). Only for Launcher.
3. hdiffz.dll (included. Compiled from https://github.com/Kromster80/HDiffPatch). For both modes.

