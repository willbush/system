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

  # Note: first time setup requires going to http://localhost:631 and adding the
  # printer to the cups service. In addition, in order to find the printer, I
  # have to temporarily disable the firewall because it seems to use a randomly
  # assigned port when discovering the network printer.
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
