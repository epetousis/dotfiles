{ config, lib, pkgs, modulesPath, ... }:

{
  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usb_storage" "usbhid" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-amd" ];
  boot.extraModulePackages = [ ];

  # Use Disko to partition the boot drive.
  disko.devices = {
    disk = {
      sdb = {
        device = "/dev/disk/by-id/ata-Samsung_SSD_860_EVO_M.2_500GB_S5GCNJ0N603853K";
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
                type = "filesystem";
                format = "btrfs";
                mountpoint = "/";
              };
            };
          };
        };
      };
    };
  };

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
