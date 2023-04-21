# GameLauncher

Application to keep games up to date by automatically downloading and applying patches.

## Guidelines
1. Everything should be streamlined for the player. First goal of the Launcher is to make players life simpler.
2. If anything is a lossy operation - cancel early and advise player to downloading a full build as usual. Later on we could split assets into app-managed (e.g. stock maps) and user-managed (maps, mods). App-managed will be updated, user-managed will be left untouched.
3. Patchmaker is intended to fit in into build process. Hence no GUI, just the log.
4. Launcher is transitioning from Prototype to Beta stage, so many things are still in the code, rather than configurable from UI or commandline.
5. We have agreed to use Sem-Ver-based numbering for builds (Major.Minor.Patch.Rev-Branch). For patching we really need just the Rev and Branch though.

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

## Workflow
1. Game build should consist of 1 folder containing all the games data.
2. It should also include "version" file stating the gamea build branch and version.
3. It should be packed into 7zip right after generation, to avoid any temp/OS files being generated inside.
4. Package should be named according to rules (specified in TKMSettings) with branch marker (optional) and revision number.
5. Patchmaker needs newer version 7zip package to start. It will try to find previous version 7zip package.
6. If previous version is found, Patchmaker will generate a patch.
7. Generated patch needs to be uploaded to the webserver.
8. Patcher needs to be launched from the games folder. It will detect games version from "version" file.
9. It will query webserver for list of available builds and patches and determine if patching can be performed.
10. On players command, Patcher will download and apply 1 or more consequtive patches till the latest version.
11. If any patch could result in data loss, patching will be calcelled.

## Configuring
1. Open src\KM_Settings.pas and fill in constants.
2. Update game build process to generate "version" file in the games root folder containing game version string.
3. Change graphics in the src\Form_Main.dfm image.
4. You can tweak diff settings in the TKMHDiffPatch constats (affects diff generation speed vs size)
