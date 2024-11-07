{
  config,
  lib,
  pkgs,
  ...
}@args:
let
  apple-set-os-loader = pkgs.stdenv.mkDerivation {
    name = "apple-set-os";
    # Tiny EFI program for unlocking the Intel IGD (working on Macmini8,1)
    src = pkgs.fetchFromGitHub {
      owner = "Redecorating";
      repo = "apple_set_os-loader";
      rev = "r33.9856dc4";
      sha256 = "sha256-hvwqfoF989PfDRrwU0BMi69nFjPeOmSaD6vR6jIRK2Y";
    };
    buildInputs = [ pkgs.gnu-efi ];
    buildPhase = ''
      substituteInPlace Makefile --replace "/usr" '$(GNU_EFI)'
      export GNU_EFI=${pkgs.gnu-efi}
      make
    '';
    installPhase = ''
      install -D bootx64_silent.efi $out/bootx64.efi
    '';
  };
in
{
  config = lib.mkMerge [
    {
      boot.kernelPackages = pkgs.linuxPackagesFor (pkgs.callPackage ./linux-t2-patches.nix { });

      # Make sure post-resume.service exists
      powerManagement.enable = true;

      # Unlock the iGPU
      system.activationScripts.appleSetOsLoader = ''
        if [[ -e /boot/EFI/BOOT/BOOTX64.EFI.ORIG ]]; then
          true # It's already installed, no action required
        elif [[ -e /boot/EFI/BOOT/BOOTX64.EFI ]]; then
          # Copy the new bootloader to a temporary location
          cp ${apple-set-os-loader}/bootx64.efi /boot/EFI/BOOT/BOOTX64_TEMP.EFI

          # Rename the original bootloader
          mv /boot/EFI/BOOT/BOOTX64.EFI /boot/EFI/BOOT/BOOTX64.EFI.ORIG

          # Move the new bootloader to the final destination
          mv /boot/EFI/BOOT/BOOTX64_TEMP.EFI /boot/EFI/BOOT/BOOTX64.EFI
        else
          echo "Error: /boot/EFI/BOOT/BOOTX64.EFI is missing" >&2
          exit 1
        fi
      '';

      # Enable the iGPU by default if present
      environment.etc."modprobe.d/apple-gmux.conf".text = ''
        options apple-gmux force_igd=y
      '';
    }
  ];
}
