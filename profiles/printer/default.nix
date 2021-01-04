{ pkgs, ... }: {
  hardware = {
    sane.enable = true;
    sane.brscan4 = {
      enable = true;
      netDevices = {
        office-printer = {
          model = "HL-2380DW";
          ip = "192.168.1.166";
        };
      };
    };
  };
  services.printing = {
    enable = true;
    drivers = [ pkgs.hll2390dw-cups ];
  };

  modules.unfree.allowList = [
    "hll2390dw-cups"
    "brscan4"
    "brother-udev-rule-type1"
    "brscan4-etc-files"
  ];
}
