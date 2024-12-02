{ config, lib, pkgs, modulesPath, ... }:

{
  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usb_storage" "usbhid" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-amd" ];
  boot.extraModulePackages = [ ];

  # Use Disko to partition the boot drive.
  disko.devices = {
    disk = {
      vdb = {
        device = "/dev/disk/by-id/nvme-CT1000P5SSD8_20282B783631";
        type = "disk";
        content = {
          type = "gpt";
          partitions = {
            ESP = {
              type = "EF00";
              size = "1G";
              content = {
                type = "filesystem";
                format = "vfat";
                mountpoint = "/boot";
              };
            };
            root = {
              size = "100%";
              content = {
                type = "btrfs";
                subvolumes = {
                  # Impermanence needs a root filesystem to wipe.
                  root = {
                    name = "@root";
                    mountpoint = "/";
                  };
                  # It obviously needs a place to persistently store data.
                  "@persistent" = {
                    mountpoint = "/@persistent";
                  };
                  # Keep the Nix store separate as well.
                  nix = {
                    name = "@nix";
                    mountpoint = "/nix";
                  };
                };
              };
            };
          };
        };
      };
    };
  };

  # Impermanence won't work properly if we don't mark the persistent volume as needed for boot.
  fileSystems."/@persistent".neededForBoot = true;

  boot.initrd.postDeviceCommands = lib.mkAfter ''
    mkdir /btrfs_tmp
    mount /dev/nvme1n1p2 /btrfs_tmp
    if [[ -e /btrfs_tmp/@root ]]; then
        mkdir -p /btrfs_tmp/old_roots
        timestamp=$(date --date="@$(stat -c %Y /btrfs_tmp/@root)" "+%Y-%m-%-d_%H:%M:%S")
        mv /btrfs_tmp/@root "/btrfs_tmp/old_roots/@$timestamp"
    fi

    delete_subvolume_recursively() {
        IFS=$'\n'
        for i in $(btrfs subvolume list -o "$1" | cut -f 9- -d ' '); do
            delete_subvolume_recursively "/btrfs_tmp/$i"
        done
        btrfs subvolume delete "$1"
    }

    for i in $(find /btrfs_tmp/old_roots/ -maxdepth 1 -mtime +30); do
        delete_subvolume_recursively "$i"
    done

    btrfs subvolume create /btrfs_tmp/@root
    umount -f /btrfs_tmp
  '';

  # Mount secondary drives
  fileSystems."/mnt/nvme-ssd" = {
    device = "/dev/disk/by-uuid/4f799226-f09b-576a-ce56-416f30fed919";
    fsType = "btrfs";
  };

  fileSystems."/mnt/external-ssd" = {
    device = "/dev/disk/by-uuid/53989e66-e678-4438-b3cd-6e28f5d647f1";
    fsType = "btrfs";
  };

  swapDevices = [ ];

  # Enables DHCP on each ethernet and wireless interface.
  networking.useDHCP = lib.mkDefault true;

  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
