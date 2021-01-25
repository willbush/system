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

  services = {
    # Note: first time setup requires going to http://localhost:631 and adding
    # the printer to the cups service.
    printing = {
      enable = true;
      drivers = [ pkgs.hll2390dw-cups ];
    };

    # Without avahi I find that I have temporarily disable the firewall to
    # discover my printer because it seems to use a randomly assigned port when
    # discovering the network printer. Using avahi gets around that issue.
    avahi = {
      enable = true;
      nssmdns = true;
    };
  };

  modules.unfree.allowList = [
    "hll2390dw-cups"
    "brscan4"
    "brother-udev-rule-type1"
    "brscan4-etc-files"
  ];
}
