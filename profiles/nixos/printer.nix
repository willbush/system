{
  hardware = {
    sane.enable = true;
    sane.brscan4 = {
      enable = true;
      netDevices = {
        office-printer = {
          model = "HL-2380DW";
          ip = "192.168.68.62";
        };
      };
    };
  };

  services = {
    # Note: first time setup requires going to http://localhost:631 and adding
    # the printer to the cups service. Also, to get an lpd:// connection to the
    # printer I have to temporarily disable the firewall because it seems to use
    # a randomly assigned port when discovering the network printer. Enabling
    # avahi gets around that issue by connecting via a different means.
    printing = {
      enable = true;
      logLevel = "notice"; # one level below "info"
    };

    avahi = {
      enable = true;
      nssmdns4 = true;
    };
  };

  modules.unfree.allowList = [
    "hll2390dw-cups"
    "brscan4"
    "brother-udev-rule-type1"
    "brscan4-etc-files"
  ];
}
