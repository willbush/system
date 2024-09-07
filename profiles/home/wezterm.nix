{ inputs, pkgs, ... }:
{
  # No automatic theme stylix
  # stylix.targets.wezterm.enable = false;
  programs = {
    wezterm = {
      enable = true;
      package = inputs.wezterm.packages.${pkgs.system}.default;
      # extraConfig = ''
      #   return {}
      # '';
    };
  };
}
