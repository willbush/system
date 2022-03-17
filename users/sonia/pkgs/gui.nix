{ pkgs, ... }:
{
  home.packages = with pkgs; [
    # java
    jdk
    jetbrains.idea-community
  ];
}
