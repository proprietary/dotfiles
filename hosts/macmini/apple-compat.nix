{ config, lib, pkgs, ... } @ args:
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
    buildInputs = [
      pkgs.gnu-efi
    ];
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
        if [[ -e /boot/efi/efi/boot/bootx64_original.efi ]]; then
          true # It's already installed, no action required
        elif [[ -e /boot/efi/efi/boot/bootx64.efi ]]; then
          # Copy the new bootloader to a temporary location
          cp ${apple-set-os-loader}/bootx64.efi /boot/efi/efi/boot/bootx64_temp.efi

          # Rename the original bootloader
          mv /boot/efi/efi/boot/bootx64.efi /boot/efi/efi/boot/bootx64.efi.orig

          # Move the new bootloader to the final destination
          mv /boot/efi/efi/boot/bootx64_temp.efi /boot/efi/efi/boot/bootx64.efi
        else
          echo "Error: /boot/efi/efi/boot/bootx64.efi is missing" >&2
        fi
      '';

      # Enable the iGPU by default if present
      environment.etc."modprobe.d/apple-gmux.conf".text = ''
        options apple-gmux force_igd=y
      '';
    }
  ];
}
