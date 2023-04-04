unit KM_Settings;
interface


type
  TKMSettings = record
  public const
    GAME_NAME = 'Knights Province Alpha';
    GAME_EXE_NAME = 'KnightsProvince.exe';

    SERVER_ADDRESS = 'https://release.knightsprovince.com/index.php/apps/kpautomation/api/1.0/';
    FILE_LIST_GET = 'kp_files';

    // Proposed versioning scheme inspired by semver, but based on players experience more than on programmatic API.
    // Major version - unused, since we are not aiming to create radicaly big changes (think Warcraft 1 vs Warcraft 2 and 3)
    //                 KMR is at major 1. KP is major 0 (until Alpha/Beta stage is complete)
    // Minor version - milestone release (think KMR r6720, KP Alpha 12) with big new features and noticeable changes to gameplay (TownHall, Pastures, MP, KT)
    // Patch         - milestone release patch (think hotfixes or big after-release patches)
    // Revision      - internal development counter (always grows)
    // Branch        - marker of stable/unstable branch

    //    Existing naming pattern                     SemVer-ish naming pattern
    //  - [Knights Province Alpha] 11.3 r11234        0.11.3.11234
    //  - [Knights Province Alpha] 12 wip r12544      0.12.0.12544-unstable
    //  - [Knights Province Alpha] 12 wip r12602      0.12.0.12602-unstable
    //  - [Knights Province Alpha] 12 r12630          0.12.0.12630
    //  - [Knights Province Alpha] 12.1 r12646        0.12.1.12646
    //  - [Knights Province Alpha] 12.2 r12666        0.12.2.12666
    //  - [Knights Province Alpha] 13 wip r12700      0.13.0.12700-unstable
    //  - [Knights Province Alpha] 13 wip r12900      0.13.0.12900-unstable
    //  - [Knights Province Alpha] 13 r14400          0.13.0.14400
    //  - [Knights Province Beta] 1 r18800            0.1.0.18800
    //  - [Knights Province Beta] 5 r19800            0.5.0.19800
    //  - [Knights Province Beta] 5.1 r19850          0.5.1.19850
    //  - [Knights Province] r21300                   1.0.0.21300

    //  - [KaM Remake Demo] r2173                     0.5.0.2173
    //  - [KaM Remake Demo] r5503                     0.9.0.5503
    //  - [KaM Remake] r6720                          1.0.0.6720
    //  - [KaM Remake] r14787                         1.1.0.14787-unstable
    //  - [KaM Remake (RC1)] r15180                   1.1.0.15180-unstable
    //  - [KaM Remake (RC2)] r15190                   1.1.0.15190-unstable
    //  - [KaM Remake] r15200                         1.1.0.15200
    //  - [KaM Remake] r15201                         1.2.0.15201-unstable
    //  - [KaM Remake] r16600                         1.2.0.16600

    // At any given time, project is assumed to have just 2 Branches and correspondingly 2 Heads: stable and unstable
    // Note that Revision is growing in each branch separately, meaning that sometimes stable branch can have a higher Revision than unstable
    // We dont parse Major.Minor.Patch since knowing Revision and Branch is enough for the Updater to update

    //VERSION_MAJOR = '';                 // Can be assumed to be always the same for the same product (KMR, KP Alpha, KP Beta, KP)
    //VERSION_MINOR_REGEX = ' \K\d+';     // Find " 12" and trim " "
    //VERSION_PATCH_REGEX = '[.]\K\d+';   // Find ".1" and trim "."
    VERSION_REVISION_REGEX = 'r\K\d+';  // Find "r123456" and trim "r"
    VERSION_BRANCH_SUFFIX_UNSTABLE = 'wip';      // Checked first
    VERSION_BRANCH_SUFFIX_STABLE = ' ';           // Anything else
  end;


implementation


end.
