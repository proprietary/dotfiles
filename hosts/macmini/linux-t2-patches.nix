{
  config,
  lib,
  pkgs,
  buildLinux,
  fetchzip,
  runCommand,
  ...
}@args:
let
  kernelVersion = "6.11";
  kernelMajorVersion = with lib; (elemAt (take 1 (splitVersion kernelVersion)) 0);
  patchRepo = pkgs.fetchFromGitHub {
    owner = "t2linux";
    repo = "linux-t2-patches";
    rev = "889654917244606a3893518d4e5e5ac2355af93f";
    hash = "sha256-PHhJnOFTA9B9MMbyQZONGuOd4nZP2sVZIbjZ0pVfz+8=";

  };
  kernel = fetchzip {
    url = "mirror://kernel/linux/kernel/v${kernelMajorVersion}.x/linux-${kernelVersion}.tar.xz";
    hash = "sha256-QIbHTLWI5CaStQmuoJ1k7odQUDRLsWNGY10ek0eKo8M=";
  };
in
buildLinux (
  args
  // {
    version = kernelVersion;

    modDirVersion = with lib; "${concatStringsSep "." (take 3 (splitVersion "${kernelVersion}.0"))}";

    src = runCommand "patched-source" { } ''
      cp -r ${kernel} $out
      chmod -R u+w $out
      cd $out
      while read -r patch; do
        echo "Applying patch $patch";
        patch -p1 < $patch;
      done < <(find ${patchRepo} -type f -name "*.patch" | sort)
    '';

    structuredExtraConfig = with lib.kernel; {
      APPLE_BCE = module;
      APPLE_GMUX = module;
      APFS_FS = module;
      BRCMFMAC = module;
      BT_BCM = module;
      BT_HCIBCM4377 = module;
      BT_HCIUART_BCM = yes;
      BT_HCIUART = module;
      HID_APPLETB_BL = module;
      HID_APPLETB_KBD = module;
      HID_APPLE = module;
      DRM_APPLETBDRM = module;
      HID_SENSOR_ALS = module;
      SND_PCM = module;
      STAGING = yes;
    };

    kernelPatches = [ ];

  }
  // (args.argsOverride or { })
)
